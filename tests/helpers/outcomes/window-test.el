;;; outcomes/window-test.el --- Self-tests of the window outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; Window layouts built with the split functions the keys C-x 2 and
;; C-x 3 call; each outcome must fail on the other layout.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'scenario/outcome)
(require 'outcomes)

(defun outcomes-window-test--verdict (clause &optional context)
  "Return the verdict of CLAUSE with CONTEXT."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause context))

(describe "Window outcomes"

  (it ":window-split tells side by side from stacked windows"
    (save-window-excursion
      (delete-other-windows)
      (expect (outcomes-window-test--verdict '(:window-split side-by-side))
              :to-match "1")
      (split-window-right)
      (expect (outcomes-window-test--verdict '(:window-split side-by-side)) :to-be nil)
      (expect (outcomes-window-test--verdict '(:window-split stacked)) :to-be-truthy)
      (delete-other-windows)
      (split-window-below)
      (expect (outcomes-window-test--verdict '(:window-split stacked)) :to-be nil)
      (expect (outcomes-window-test--verdict '(:window-split side-by-side))
              :to-be-truthy)))

  (it ":windows-count and :window-buffers-equal"
    (save-window-excursion
      (delete-other-windows)
      (expect (outcomes-window-test--verdict '(:windows-count 1)) :to-be nil)
      (let ((other (get-buffer-create " outcomes-window-test")))
        (unwind-protect
            (progn
              (split-window-right)
              (expect (outcomes-window-test--verdict '(:windows-count 2)) :to-be nil)
              (expect (outcomes-window-test--verdict '(:windows-count 1)) :to-be-truthy)
              (expect (outcomes-window-test--verdict '(:window-buffers-equal)) :to-be nil)
              (set-window-buffer (next-window) other)
              (expect (outcomes-window-test--verdict '(:window-buffers-equal))
                      :to-be-truthy))
          (kill-buffer other)))))

  (it ":selected-window-changed compares with the window before the keys"
    (save-window-excursion
      (delete-other-windows)
      (let ((before (selected-window)))
        (split-window-right)
        (expect (outcomes-window-test--verdict '(:selected-window-changed)
                                               (list :before-window before))
                :to-be-truthy)
        (other-window 1)
        (expect (outcomes-window-test--verdict '(:selected-window-changed)
                                               (list :before-window before))
                :to-be nil))))

  (it ":cursors-count counts point and the fake cursors"
    (cl-letf (((symbol-function 'mc/all-fake-cursors) (lambda () '(fake-1 fake-2))))
      (expect (outcomes-window-test--verdict '(:cursors-count 3)) :to-be nil)
      (expect (outcomes-window-test--verdict '(:cursors-count 1)) :to-match "3"))))

(provide 'outcomes/window-test)

;;; outcomes/window-test.el ends here
