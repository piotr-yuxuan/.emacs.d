;;; l1-table-outcomes-gui.el --- Self-test of a table outcome that needs a display -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; `:header-label-above-cell' reads the columns where redisplay drew a
;; header label and a cell, so it is checked here, in the graphical
;; client frame.  The plausible wrong header is one that does not
;; scroll with the text: the outcome must fail on it.

;;; Code:

(require 'scenario)

(describe "TBL-GUI: the outcome :header-label-above-cell"

  (it "compares the columns redisplay drew, after horizontal scrolling"
    (let ((buffer (get-buffer-create "table-outcomes-gui")))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buffer)
            (setq truncate-lines t
                  header-line-format (list (propertize " " 'display '(space :align-to 0))
                                           (make-string 30 ?\s)
                                           "label"))
            (insert (make-string 30 ?.) "cell" (make-string 100 ?.) "\n")
            ;; Point stays visible at every scroll below, so redisplay
            ;; keeps the scroll it is given.
            (goto-char 60)
            (redisplay t)
            (expect (my/scenario-outcome-verdict '(:header-label-above-cell "label" "cell") nil)
                    :to-be nil)
            (set-window-hscroll (selected-window) 10)
            (redisplay t)
            (expect (my/scenario-outcome-verdict '(:header-label-above-cell "label" "cell") nil)
                    :to-match "\"cell\" starts at column 20\\'")
            (set-window-hscroll (selected-window) 40)
            (redisplay t)
            (expect (my/scenario-outcome-verdict '(:header-label-above-cell "label" "cell") nil)
                    :to-equal "the line at point does not show \"cell\""))
        (kill-buffer buffer)))))

(provide 'l1-table-outcomes-gui)

;;; l1-table-outcomes-gui.el ends here
