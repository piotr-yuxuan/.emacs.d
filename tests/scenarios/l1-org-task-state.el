;;; l1-org-task-state.el --- Change an Org task state by keystroke -*- lexical-binding: t; -*-

;;; Commentary:
;; Plan 079.  The compact task state flow must remain available through
;; Org's normal state chooser and must not add retired metadata.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-ORG-TASK-STATE: C-c C-t selects the next task state"
 :given ((:name "next action"
          :file "todo.org"
          :text "* TODO Prepare-!- the release\n"))
 :when ((:keys "C-c C-t n"))
 :then ((:buffer-text "* NEXT Prepare the release\n")))

(provide 'l1-org-task-state)

;;; l1-org-task-state.el ends here
