;;; t1-gui.el --- The session checks (T1) that need a graphical frame -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Run by `tests/integrity/session/run' beside `t1.el', in a session of
;; its own with a client frame on the runner's private display, and
;; skipped as a whole without one (EMACS_TEST_GUI=0, or no Xvfb): faces
;; are what the user sees in a graphical frame, and without one Emacs
;; computes them differently.  The requirement is D10: comments are
;; italic in both themes, after every theme change.

;;; Code:

(require 'buttercup)
(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path directory)
  (add-to-list 'load-path (expand-file-name ".." directory)))
(require 't1-checks)

(defvar my/test-session-frame)

(let ((entries (my/integrity-ledger-entries
                (expand-file-name "tests/integrity/known-defects/" my/t1-root))))
  (describe "In a graphical frame"
    (it "shows comments in italic after every theme change"
      ;; The faces are those of the runner's client frame.
      (expect (display-graphic-p my/test-session-frame) :to-be-truthy)
      (buttercup-suppress-warning-capture
        (my/t1-judge 'italic-comments (my/t1-italic-subjects my/test-session-frame) entries)
        ;; With the slant applied after every theme change (D10), the
        ;; check goes silent.
        (let ((italic (lambda (_theme)
                        (set-face-attribute 'font-lock-comment-face nil :slant 'italic))))
          (add-hook 'enable-theme-functions italic)
          (unwind-protect
              (expect (my/t1-italic-subjects my/test-session-frame) :to-equal nil)
            (remove-hook 'enable-theme-functions italic)))))))

;;; t1-gui.el ends here
