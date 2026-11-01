;;; emacs-capabilities-test.el --- An Emacs that lacks a capability -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests of plan 108: the configuration asks what this Emacs can
;; do, and a capability it lacks costs that capability alone.  Each
;; early-init case runs in its own `emacs --batch -Q', with a scratch
;; HOME and --init-directory, as it changes `features' and redefines
;; what Emacs starts with.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'jinx)

(defconst emacs-capabilities-test--early-init
  (expand-file-name "../early-init.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The early-init.el of the checkout under test.")

(defun emacs-capabilities-test--early-init (directory before after)
  "Load early-init.el in a fresh Emacs started on DIRECTORY.
Evaluate the form BEFORE first and the form AFTER last.  Return the
exit status and the output, as a cons."
  (let ((default-directory directory)
        (process-environment (cons (format "HOME=%s" directory) process-environment)))
    (with-temp-buffer
      (let ((status (call-process
                     (expand-file-name invocation-name invocation-directory)
                     nil t nil "--batch" "-Q"
                     (format "--init-directory=%s" directory)
                     "--eval" (prin1-to-string before)
                     "--load" emacs-capabilities-test--early-init
                     "--eval" (prin1-to-string after))))
        (cons status (buffer-string))))))

(describe "early-init.el, on native code"
  :var (directory)
  (before-each (setq directory (file-name-as-directory
                                (make-temp-file "emacs-capabilities-" t))))
  (after-each (delete-directory directory t))

  (it "reaches its end in an Emacs without native code that defines the redirect"
    (expect (emacs-capabilities-test--early-init
             directory
             '(progn
                (setq features (delq 'native-compile features))
                (fset 'startup-redirect-eln-cache
                      (lambda (&rest _) (error "A variable this Emacs lacks"))))
             '(princ initial-major-mode))
            :to-equal '(0 . "fundamental-mode")))

  (it "still redirects native code under the directory Emacs started on"
    (expect (emacs-capabilities-test--early-init
             directory
             '(progn
                (provide 'native-compile)
                (fset 'startup-redirect-eln-cache
                      (lambda (cache) (setq emacs-capabilities-test-cache cache))))
             '(princ emacs-capabilities-test-cache))
            :to-equal (cons 0 (concat directory "eln-cache/")))))

(describe "jinx, on its module"
  :var (buffers)
  (before-each
    (setq buffers nil)
    (spy-on 'message))
  (after-each
    (global-jinx-mode -1)
    (mapc #'kill-buffer buffers))

  (cl-flet ((text-buffer ()
              (let ((buffer (generate-new-buffer "emacs-capabilities-text")))
                (push buffer buffers)
                (with-current-buffer buffer (text-mode))
                buffer))
            (off-messages ()
              (seq-filter (lambda (args) (string-prefix-p "Spell checking is off" (car args)))
                          (spy-calls-all-args 'message))))

    (it "costs spell checking alone when the module fails"
      (spy-on 'jinx--load-module :and-throw-error 'error)
      (let ((buffer (text-buffer))
            (noninteractive nil))
        (global-jinx-mode 1)
        (expect 'jinx--load-module :to-have-been-called-times 1)
        (expect global-jinx-mode :not :to-be-truthy)
        (expect (buffer-local-value 'jinx-mode buffer) :not :to-be-truthy)
        (expect (length (off-messages)) :to-equal 1)
        (spy-calls-reset 'jinx--load-module)
        (text-buffer)
        (expect 'jinx--load-module :not :to-have-been-called)
        (expect (length (off-messages)) :to-equal 1)))

    (it "turns on as before when the module works"
      (spy-on 'jinx--load-module)
      (spy-on 'jinx--load-dicts)
      (let ((buffer (text-buffer))
            (noninteractive nil))
        (global-jinx-mode 1)
        (expect global-jinx-mode :to-be-truthy)
        (expect (buffer-local-value 'jinx-mode buffer) :to-be-truthy)
        (expect (off-messages) :to-equal nil)))))

;;; emacs-capabilities-test.el ends here
