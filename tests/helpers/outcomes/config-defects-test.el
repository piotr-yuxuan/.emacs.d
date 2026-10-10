;;; outcomes/config-defects-test.el --- Self-tests of the outcomes of plan 088 -*- lexical-binding: t -*-

;;; Commentary:
;; Each outcome must hold on the buffer it describes and fail on a
;; buffer just outside it.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defun outcomes-config-defects-test--verdict (text clause)
  "Return the verdict of CLAUSE in a buffer holding TEXT."
  (my/scenario-check-clause clause)
  (with-temp-buffer
    (insert text)
    (my/scenario-outcome-verdict clause nil)))

(describe "Outcomes of plan 088"

  (it ":longest-line-within measures the widest line, in columns"
    (let ((line (lambda (width) (make-string width ?a))))
      (expect (outcomes-config-defects-test--verdict
               (concat (funcall line 50) "\n" (funcall line 118) "\n" (funcall line 3))
               '(:longest-line-within 110 120))
              :to-be nil)
      (expect (outcomes-config-defects-test--verdict
               (concat (funcall line 70) "\n" (funcall line 69))
               '(:longest-line-within 110 120))
              :to-match "70 columns")
      (expect (outcomes-config-defects-test--verdict
               (concat (funcall line 115) "\n" (funcall line 121))
               '(:longest-line-within 110 120))
              :to-match "121 columns")))

  (it ":initial-frame-lacks fails when the initial frame shows a matching buffer"
    ;; In batch, the selected frame is `terminal-frame'.
    (let ((window (frame-selected-window terminal-frame))
          (shown (get-buffer-create "*outcomes-initial-frame*")))
      (save-window-excursion
        (set-window-buffer window shown)
        (expect (outcomes-config-defects-test--verdict
                 "" '(:initial-frame-lacks "\\`\\*outcomes-initial-frame\\*\\'"))
                :to-match "initial frame shows")
        (expect (outcomes-config-defects-test--verdict
                 "" '(:initial-frame-lacks "\\`\\*outcomes-other\\*\\'"))
                :to-be nil))
      (kill-buffer shown))))

;;; outcomes/config-defects-test.el ends here
