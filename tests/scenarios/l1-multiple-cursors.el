;;; l1-multiple-cursors.el --- C-> / C-< / C-c C-< add cursors -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 073 / multiple-cursors slice.  The live config binds:
;;
;;   C->        mc/mark-next-like-this
;;   C-<        mc/mark-previous-like-this
;;   C-c C-<    mc/mark-all-like-this
;;
;; The user-visible outcome is "there are now N cursors in the
;; buffer".  Each scenario first selects a symbol with C-SPC M-f, the
;; region multiple-cursors replicates.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-MC-NEXT: C-> adds a cursor on the next match"
 :given ((:name "two-foos"
          :file "scratch.el"
          :text "-!-foo foo bar"))
 :when ((:keys "C-SPC M-f C->"))
 :then ((:cursors-count 2)))

(scenario
 :describe "BL1-MC-PREV: C-< adds a cursor on the previous match"
 :given ((:name "two-foos-prev"
          :file "scratch.el"
          :text "foo -!-foo bar"))
 :when ((:keys "C-SPC M-f C-<"))
 :then ((:cursors-count 2)))

(scenario
 :describe "BL1-MC-ALL: C-c C-< marks all matches in the buffer"
 :given ((:name "three-foos"
          :file "scratch.el"
          :text "-!-foo foo foo"))
 :when ((:keys "C-SPC M-f C-c C-<"))
 :then ((:cursors-count 3)))

(provide 'l1-multiple-cursors)

;;; l1-multiple-cursors.el ends here
