;;; l1-magit-companions.el --- magit's companions start with magit -*- lexical-binding: t -*-

;;; Commentary:
;; magit-prime caches what magit-status needs, and difftastic adds
;; its structural diffs to magit's transients (M-d and M-c in the diff
;; menu, M-RET in blame).  Both must be active as soon as magit is
;; (084 CF-07, D8): the first C-x g of a session loads magit.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-07: the first magit-status turns on magit-prime and difftastic"
 :given ((:name "first magit" :file "notes.txt" :text "notes-!-\n"))
 :when ((:keys "C-x g"))
 :then ((:mode magit-status-mode)
        (:minor-modes-on magit-prime-mode difftastic-bindings-mode)))

(provide 'l1-magit-companions)

;;; l1-magit-companions.el ends here
