;;; treemacs-helpers-test.el --- Logic tests of how Treemacs runs its helpers -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests (plan 119): Treemacs runs its Python helpers through
;; `pfuture-new' and reads their whole output once they exit.  On
;; macOS a pty loses what it still holds when the child exits, so the
;; configuration gives these processes a pipe.  A process with a pipe
;; has no tty; one with a pty has one, on Linux as on macOS.

;;; Code:

(require 'buttercup)
(require 'treemacs)

(describe "A process Treemacs starts through pfuture"
  (it "writes to a pipe, not a pty"
    (let ((future (pfuture-new "true")))
      (unwind-protect
          (expect (process-tty-name future) :to-be nil)
        (pfuture-await-to-finish future))))

  (it "still gives its whole output once it exits"
    (let ((future (pfuture-new "printf" "(a . b)")))
      (expect (read (pfuture-await-to-finish future)) :to-equal '(a . b)))))

(provide 'treemacs-helpers-test)

;;; treemacs-helpers-test.el ends here
