#!/usr/bin/env scsh
; -*- mode: scheme -*-
!#

(define --verbose   #f)
(define --help      #f)
(define --porcelain #f)
(define --force     #f)

; Main entry-point. Parses arguments and forwards to the command dispatcher.
(define (main raw-args)
  (define args (parse-options raw-args))
  (if (or (< (length args) 2) --help)
      (exit-with-usage)
      (let ((command (cadr args))
            (command-args (cddr args)))
        (run-command command command-args))))

; Parses the options understood by this tool.
(define parse-options
  (option-parser
    (--verbose
      (set! --verbose (not --verbose)))
    (--porcelain
      (set! --porcelain (not --porcelain)))
    (--force
      (set! --force (not --force)))
    (--help
      (set! --help (not --help)))))

; Dispatches based on which operation was specified.
(define (run-command command args)
  (case (string->symbol command)
    ('start     (within-workspace run-start args))
    ('drop      (within-workspace run-drop args))
    ('commit    (within-workspace run-commit args))
    ('uncommit  (within-workspace run-uncommit args))
    ('export    (within-workspace run-export args))
    ('submit    (within-workspace run-submit args))
    ('rename    (within-workspace-lenient run-rename args))
    ('status    (within-workspace-lenient run-status args))
    ('branch    (within-workspace-lenient run-branch args))
    ('diff      (within-workspace-lenient run-diff args))
    ('git       (within-workspace-lenient run-git args))
    ('squash    (within-workspace run-squash args))
    ('revert    (within-workspace run-revert args))
    ('pullall   (within-workspace run-pullall args))
    (else       (exit-with-usage))))

; Executes a 'start' command.
(define (run-start new-branch)
  (define current-branch (git-current-branch))
  (run
    (git checkout -b ,new-branch -t ,current-branch)))

; Executes a 'drop' command.
(define (run-drop)
  (define current-branch (git-current-branch))
  (if (is-master? current-branch)
      (fail "Can't drop master"))
  (&&
    (git checkout master)
    (git branch -D ,current-branch)))

; Is the given branch the master?
(define (is-master? branch)
  (string=? branch "master"))

; Runs a 'commit' command.
(define (run-commit)
  (define branch (git-current-branch))
  (if (is-master? branch)
      (fail "Don't commit on master"))
  ; Only try to commit if there are outstanding changes.
  (if (git-has-outstanding-changes?)
      (run (git commit -a))))

; Runs an 'uncommit' command.
(define (run-uncommit)
  (run (git reset --soft HEAD^)))

; Runs an 'export' command.
(define (run-export)
  (define branch (git-current-branch))
  (if (is-master? branch)
      (fail "Don't export master"))
  ; Commit outstanding changes.
  (run-commit)
  ; Push to the remote branch, possibly creating it.
  (if (and
        (= 0 (run (git push -u export ,branch ,@(if --force '(-f) '()))))
	(not (git-has-pull-request? branch)))
      ; There is no pull-request so make one.
      (and
        ; Try creating the pull request...
        (= 0 (run (hub pull-request)))
	; ...if successful create a marker on this branch.
        (git-mark-has-pull-request branch))))

; Runs a 'submit' command.
(define (run-submit)
  (define branch (git-current-branch))
  (if (is-master? branch)
      (fail "Don't submit master"))
  ; Commit outstanding changes. This shouldn't really happen, they should have
  ; been exported earlier, but there are exceptions.
  (run-commit)
  (let ((master (@git-branch "master")))
    (within master
      (&&
        ; Merge the branch into local master.
        (git merge ,branch)
	; Push the local master to the origin.
	(git push origin master)
	; Delete the pull request branch.
	(git push export --delete ,branch)))))

(define (run-rename new-name)
  (run (git branch -m ,new-name)))

(define (run-pullall)
  (&&
    ; Update the current branch to the current version.
    (git pull origin master)
    ; Move all the submodules to the master branch since otherwise
    ; they'll land on a detached branch.
    (git submodule foreach --recursive git checkout master)
    ; Update the submodules
    (git submodule update --recursive)))

(define (run-status)
  (run (git status ,@(if --porcelain '(--porcelain) '()))))

(define (run-branch)
  (run (git branch)))

(define (run-diff)
  (run (git diff)))

(define (run-git . args)
  (run (git ,@args)))

(define (run-squash count)
  (run (git rebase --interactive ,(string-append "HEAD~" count))))

(define (run-revert)
  (run (git reset --hard HEAD)))

; Executes the given thunk with the given arguments within the current
; workspace.
(define (within-workspace thunk args)
  (let ((workspace (@current-workspace)))
    (if workspace
        (within workspace
          (apply thunk args))
        (fail "Couldn't find current workspace"))))

; Executes the given thunk with the given arguments within the current
; workspace or, if no workspace could be found, the current directory.
(define (within-workspace-lenient thunk args)
  (let ((workspace (@current-workspace)))
    (if workspace
        (within workspace
          (apply thunk args))
	(apply thunk args))))

; Prints all options understood by this tool and exits with an error.
(define (exit-with-usage)
  (for-each println
    '("Usage: ash COMMAND OPTIONS"
      ""
      "Where COMMAND is one of the following:"
      "  * start      Starts a new git branch tracking the current one."
      "  * drop       Kills the current branch."
      "  * commit     Commit outstanding changes locally."
      "  * uncommmit  Roll back the last commit."
      "  * export     Push the current changes to a pull-request."
      "  * submit     Push the current changes to the origin."
      "  * rename     Rename the current branch."
      "  * status     Print git status."
      "  * branch     Print git branch."
      "  * diff       Print git diff."
      "  * git        Go to the workspace and run the given command."
      "  * squash     Interactively squash the given number of commits."
      "  * revert     Revert the current branch to HEAD."
      "  * pullall    Pull this workspace and its deps recursively."
      ""
      "and OPTIONS include the following:"
      "  --verbose            Print the actions performed"
      "  --help               Display this information"))
  (exit 1))
