;;; l1-scenario-language-gui.el --- Self-tests of outcomes that need a display -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Emacs formats no mode or header line, and redisplays nothing, in
;; batch or in the initial frame of a daemon, so the outcomes and steps
;; that need redisplay are checked here, in the graphical client frame.

;;; Code:

(require 'scenario)

(describe "SL-GUI: the header-line outcome"

  (it "reads the header line of the selected window"
    (let ((buffer (get-buffer-create "scenario-language-gui")))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buffer)
            (setq header-line-format '("project " (:eval (buffer-name))))
            (expect (my/scenario-outcome-verdict
                     '(:header-line-matches "\\`project scenario-language-gui\\'") nil)
                    :to-be nil)
            (expect (my/scenario-outcome-verdict
                     '(:header-line-matches "elsewhere") nil)
                    :to-equal "the header line is \"project scenario-language-gui\"")
            (setq header-line-format nil)
            (expect (my/scenario-outcome-verdict
                     '(:header-line-matches "project") nil)
                    :to-equal "the header line is nil"))
        (kill-buffer buffer)))))

(describe "SL-GUI: faces as redisplay shows them"

  (it ":face-shown reads the faces redisplay gave the text at point"
    (let ((buffer (get-buffer-create "scenario-language-gui.el")))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buffer)
            (insert "(defun example () nil)\n")
            (emacs-lisp-mode)
            (goto-char 2)
            (expect (my/scenario-outcome-verdict
                     '(:face-shown font-lock-keyword-face) nil)
                    :to-equal "the text at point is not fontified")
            (redisplay)
            (expect (my/scenario-outcome-verdict
                     '(:face-shown font-lock-keyword-face) nil)
                    :to-be nil)
            (expect (my/scenario-outcome-verdict
                     '(:face-shown font-lock-string-face) nil)
                    :to-match "font-lock-keyword-face"))
        (kill-buffer buffer))))

  (it "(:redisplay-while-typing) leaves the text unfontified until a pause"
    ;; jit-lock defers only when `jit-lock-defer-time' is set, as plan
    ;; 036 sets it; l1-performance-behavior.el pauses after this step.
    (expect (condition-case failure
                (progn
                  (my/scenario-run-row
                   '(:name "r" :file "sample.el" :text "(-!-defun example () nil)\n")
                   (my/scenario-compile-steps
                    '((:type "x") (:keys "<backspace>") (:redisplay-while-typing)))
                   '((:face-shown font-lock-keyword-face)) nil)
                  nil)
              (buttercup-failed (cdr failure)))
            :to-equal "(:face-shown font-lock-keyword-face): \
the text at point is not fontified")))

(describe "SL-GUI: the expected-skip reason (:no-gui)"

  (it "is false in a session with a graphical test frame"
    (expect (display-graphic-p my/test-session-frame) :to-be-truthy)
    (expect (my/test-session-reason-holds-p '(:no-gui)) :to-be nil)))

(provide 'l1-scenario-language-gui)

;;; l1-scenario-language-gui.el ends here
