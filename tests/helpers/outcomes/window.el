;;; outcomes/window.el --- Outcomes about windows and cursors -*- lexical-binding: t -*-

;;; Commentary:
;; Window layout and multiple cursors, as the user sees them.  See
;; `scenario/outcome.el' for the contract.

;;; Code:

(require 'scenario/outcome)

(declare-function mc/all-fake-cursors "multiple-cursors-core")

(my/scenario-define-outcome :windows-count (_context count)
  "The selected frame shows COUNT windows, not counting the minibuffer."
  (let ((actual (length (window-list nil 'no-minibuffer))))
    (unless (= actual count)
      (format "the frame shows %d windows" actual))))

(my/scenario-define-outcome :window-split (_context direction)
  "The selected frame shows two windows split in DIRECTION.
DIRECTION is `side-by-side' or `stacked'."
  (let* ((windows (window-list nil 'no-minibuffer))
         (edges (mapcar #'window-edges windows))
         (actual (cond ((/= (length windows) 2) (length windows))
                       ((= (nth 1 (car edges)) (nth 1 (cadr edges))) 'side-by-side)
                       ((= (nth 0 (car edges)) (nth 0 (cadr edges))) 'stacked))))
    (unless (eq actual direction)
      (format "the windows are %S" actual))))

(my/scenario-define-outcome :window-buffers-equal (_context)
  "Every window of the selected frame shows the same buffer."
  (let ((buffers (delete-dups (mapcar #'window-buffer
                                      (window-list nil 'no-minibuffer)))))
    (when (cdr buffers)
      (format "the windows show %S" buffers))))

(my/scenario-define-outcome :selected-window-changed (context)
  "The keys selected another window than the one selected before them."
  (when (eq (selected-window) (plist-get context :before-window))
    "the same window is still selected"))

(my/scenario-define-outcome :cursors-count (_context count)
  "The current buffer has COUNT cursors: point and the fake cursors."
  (let ((actual (1+ (if (fboundp 'mc/all-fake-cursors)
                        (length (mc/all-fake-cursors))
                      0))))
    (unless (= actual count)
      (format "the buffer has %d cursors" actual))))

(provide 'outcomes/window)

;;; outcomes/window.el ends here
