;;; l1-window-vanilla.el --- Vanilla window management (C-x 2/3/1/0/o) -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 073 / vanilla window-management slice.  Presses the four
;; built-in window commands plus `other-window':
;;
;;   C-x 2  split-window-below       -> two windows, stacked.
;;   C-x 3  split-window-right       -> two windows, side by side.
;;   C-x 1  delete-other-windows     -> one window remains.
;;   C-x 0  delete-window            -> the active window goes away.
;;   C-x o  other-window             -> selected window changes.
;;
;; Each row starts from a single-window frame, which the cleanup of the
;; previous row restores, on a freshly opened file.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-WINDOW-SPLIT-RIGHT: C-x 3 splits the current window side by side"
 :given ((:name "any-file" :file "src/sample.clj" :text "(ns sample)"))
 :when ((:keys "C-x 3"))
 :then ((:windows-count 2)
        (:window-split side-by-side)
        ;; Both windows show the same buffer (Emacs default).
        (:window-buffers-equal)))

(scenario
 :describe "BL1-WINDOW-SPLIT-BELOW: C-x 2 splits the current window into stacked windows"
 :given ((:name "any-file" :file "src/sample.clj" :text "(ns sample)"))
 :when ((:keys "C-x 2"))
 :then ((:windows-count 2)
        (:window-split stacked)
        (:window-buffers-equal)))

(scenario
 :describe "BL1-WINDOW-ONE: C-x 3 then C-x 1 returns to a single window"
 :given ((:name "any-file" :file "src/sample.clj" :text "(ns sample)"))
 :when ((:keys "C-x 3 C-x 1"))
 :then ((:windows-count 1)))

(scenario
 :describe "BL1-WINDOW-DELETE: C-x 3 then C-x 0 closes the active window"
 :given ((:name "any-file" :file "src/sample.clj" :text "(ns sample)"))
 :when ((:keys "C-x 3 C-x 0"))
 :then ((:windows-count 1)))

(scenario
 :describe "BL1-WINDOW-OTHER: C-x 3 then C-x o switches to the other window"
 :given ((:name "any-file" :file "src/sample.clj" :text "(ns sample)"))
 :when ((:keys "C-x 3 C-x o"))
 :then ((:windows-count 2)
        (:selected-window-changed)))

(provide 'l1-window-vanilla)

;;; l1-window-vanilla.el ends here
