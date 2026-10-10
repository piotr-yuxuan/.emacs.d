;;; l1-restored-vanilla-keys.el --- C-x t 1, C-x t t, C-x t d and M-0 are vanilla again -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089, decision D4: the Treemacs keys moved to the C-c F prefix,
;; which gives back the tab keys and the digit argument M-0.  A row
;; may leave a tab open; each row names its own file, so the tabs left
;; by another row never match its outcomes.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-VANILLA-M-0: M-0 is a digit argument"
 :given ((:name "text" :file "digits.txt" :text "abc-!-def"))
 ;; C-k with the argument 0 kills the text before point on the line.
 :when ((:keys "M-0 C-k"))
 :then ((:buffer-text "-!-def")))

(scenario
 :describe "BL1-VANILLA-TAB-CLOSE-OTHER: C-x t 1 closes the other tabs"
 :given ((:name "text" :file "tab-close.txt" :text "x"))
 :when ((:keys "C-x t 2 C-x t 1"))
 :then ((:tabs-count 1)))

(scenario
 :describe "BL1-VANILLA-DIRED-OTHER-TAB: C-x t d opens a directory in a new tab"
 :given ((:name "text" :file "tab-dired.txt" :text "x"))
 :when ((:keys "C-x t d <return>"))
 :then ((:mode dired-mode)
        (:other-tab-shows "\\`tab-dired\\.txt\\'")))

(scenario
 :describe "BL1-VANILLA-OTHER-TAB-PREFIX: C-x t t shows the next command's buffer in a new tab"
 :given ((:name "text" :file "tab-prefix.txt" :text "x"))
 :when ((:keys "C-x t t C-x d <return>"))
 :then ((:mode dired-mode)
        (:other-tab-shows "\\`tab-prefix\\.txt\\'")))

(provide 'l1-restored-vanilla-keys)

;;; l1-restored-vanilla-keys.el ends here
