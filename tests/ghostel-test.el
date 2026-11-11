;;; ghostel-test.el --- Logic tests of plan 110 -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests of the configuration code that plan 110 added with
;; ghostel, run in the batch session of `tests/run' with the
;; configuration loaded.  The safe update's step that keeps ghostel on
;; its releases runs against git repositories each spec makes, with a
;; scratch HOME and no git configuration of the user's; `my/btop' runs
;; with no program to find.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar my/straight-safe-update-boundaries)
(defvar straight-base-dir)
(defvar ghostel-module-auto-install)
(declare-function my/btop "init")

(defun ghostel-test--release (checkout &optional local)
  "Run the safe update's :release step on a clone at CHECKOUT.
The upstream holds the tags v0.1.0, v0.9.0 and v0.10.0, then a commit
tagged v0.11.0-rc1, and a side branch tagged v0.12.0; each commit
writes its label to the file version.  LOCAL, when `commit', adds a
commit to the clone; when `change', leaves an edit of version
uncommitted; when `detach', detaches HEAD from its branch.  Return a
plist: :value, what the step returned; :moved, whether HEAD moved;
:tag, the tag at HEAD, if any; :edit, whether the uncommitted edit is
still there."
  (let* ((base (file-name-as-directory (make-temp-file "ghostel-test-release-" t)))
         (upstream (expand-file-name "upstream" base))
         (repository (expand-file-name "straight/repos/ghostel" base))
         (process-environment
          (append (list (concat "HOME=" base) "GIT_CONFIG_NOSYSTEM=1"
                        "GIT_CONFIG_GLOBAL=/dev/null"
                        "GIT_AUTHOR_NAME=Tests" "GIT_AUTHOR_EMAIL=tests@example.invalid"
                        "GIT_COMMITTER_NAME=Tests" "GIT_COMMITTER_EMAIL=tests@example.invalid")
                  process-environment))
         (git (lambda (directory &rest arguments)
                (let ((default-directory directory))
                  (apply #'process-lines "git" arguments))))
         (commit (lambda (directory label)
                   (with-temp-file (expand-file-name "version" directory)
                     (insert label "\n"))
                   (funcall git directory "add" "version")
                   (funcall git directory "commit" "-q" "-m" label))))
    (unwind-protect
        (progn
          (make-directory upstream)
          (funcall git upstream "init" "-q" "-b" "main")
          ;; v0.1.0 sorts first as text, v0.10.0 is the newest release.
          (dolist (tag '("v0.1.0" "v0.9.0" "v0.10.0" "v0.11.0-rc1"))
            (funcall commit upstream tag)
            (funcall git upstream "tag" tag))
          (funcall git upstream "switch" "-q" "-c" "side")
          (funcall commit upstream "v0.12.0")
          (funcall git upstream "tag" "v0.12.0")
          (funcall git upstream "switch" "-q" "main")
          (funcall git base "clone" "-q" upstream repository)
          (funcall git repository "reset" "-q" "--hard" checkout)
          (pcase local
            ('commit (funcall commit repository "local"))
            ('change (with-temp-file (expand-file-name "version" repository)
                       (insert "edited\n")))
            ('detach (funcall git repository "switch" "-q" "--detach")))
          (let* ((before (funcall git repository "rev-parse" "HEAD"))
                 (value (let ((straight-base-dir base))
                          (funcall (plist-get my/straight-safe-update-boundaries :release)))))
            (list :value value
                  :moved (not (equal before (funcall git repository "rev-parse" "HEAD")))
                  :tag (car (funcall git repository "tag" "--points-at" "HEAD"))
                  :edit (with-temp-buffer
                          (insert-file-contents (expand-file-name "version" repository))
                          (equal (buffer-string) "edited\n")))))
      (delete-directory base t))))

(describe "Plan 110 D11: the safe update keeps ghostel on a release"

  (it "moves ghostel forward to its newest release tag, not to the tip of its branch"
    (expect (ghostel-test--release "v0.9.0")
            :to-equal '(:value nil :moved t :tag "v0.10.0" :edit nil)))

  (it "moves ghostel back to its newest release tag from the tip of its branch"
    (expect (ghostel-test--release "origin/main")
            :to-equal '(:value nil :moved t :tag "v0.10.0" :edit nil)))

  (it "leaves a ghostel with a commit of its own where it is"
    (expect (ghostel-test--release "v0.9.0" 'commit)
            :to-equal '(:value nil :moved nil :tag nil :edit nil)))

  (it "keeps a local change, and returns ghostel's recipe"
    (expect (ghostel-test--release "v0.9.0" 'change)
            :to-equal '(:value ((:package "ghostel")) :moved nil :tag "v0.9.0" :edit t)))

  (it "returns ghostel's recipe when HEAD is on no branch"
    (expect (ghostel-test--release "v0.9.0" 'detach)
            :to-equal '(:value ((:package "ghostel")) :moved nil :tag "v0.9.0" :edit nil)))

  (it "returns ghostel's recipe when there is no repository to move"
    (let ((straight-base-dir (file-name-as-directory
                              (make-temp-file "ghostel-test-release-" t))))
      (unwind-protect
          (expect (funcall (plist-get my/straight-safe-update-boundaries :release))
                  :to-equal '((:package "ghostel")))
        (delete-directory straight-base-dir t)))))

(describe "Plan 110 D10: M-x my/btop"

  ;; Without btop on `exec-path', or without ghostel's module, which the
  ;; tests never download, starting btop fails.  Ghostel loads first, so
  ;; that its options are not defined inside the `let' that binds one.
  (it "starts btop again after a start that failed"
    (require 'ghostel)
    (let ((exec-path nil)
          (ghostel-module-auto-install nil))
      (unwind-protect
          (let ((first (condition-case failure (my/btop) (error failure)))
                (second (condition-case failure (my/btop) (error failure))))
            ;; The second start fails as the first did, instead of
            ;; showing the buffer the first one left.
            (expect (car-safe first) :to-be-truthy)
            (expect second :to-equal first))
        (when-let* ((buffer (get-buffer "*ghostel* btop")))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))))))

(provide 'ghostel-test)

;;; ghostel-test.el ends here
