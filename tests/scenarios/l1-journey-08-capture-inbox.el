;;; l1-journey-08-capture-inbox.el --- Journey 8: C-c i captures a task under Inbox -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 8, and U09 of the 085 mutation baseline.  C-c i
;; opens the quick-task capture: a TODO heading, then a link to the
;; file the user was in.  The user types the task and C-c C-c files
;; it under the Inbox heading of the task file, and the user is back
;; in the file.  When the template targets another heading, the
;; mutation, the task lands under that heading instead.
;;
;; The session keeps the real HOME, and `my/gtd-file' names the user's
;; personal task file, which a test must never write.  mcp-server
;; loads org-capture once the session is idle, before the runner loads
;; this file, so its templates already hold that path.  This file
;; points every template that targets it, and `my/gtd-file', at a
;; fresh file of the session's temporary directory, as `local.el' would
;; on another machine; the headings and template text stay the
;; configuration's.  It refuses to run if any template still names the
;; personal file.

;;; Code:

(require 'scenario)
(require 'org-capture)

(let ((personal my/gtd-file)
      (file (make-temp-file "journey-gtd-" nil ".org" "* Inbox\n* Projects\n")))
  (setq org-capture-templates
        (mapcar (lambda (template)
                  (let ((copy (copy-tree template)))
                    (when (equal (nth 1 (nth 3 copy)) personal)
                      (setf (nth 1 (nth 3 copy)) file))
                    copy))
                org-capture-templates))
  (when (string-search personal (format "%S" org-capture-templates))
    (error "A capture template still targets %s" personal))
  (setopt my/gtd-file file))

(scenario
 :describe "J-08: C-c i captures a task under Inbox"
 :given ((:name "from a file" :file "notes.txt" :text "Some notes.\n"))
 :when ((:keys "C-c i") (:sample "capture") (:type "Buy milk") (:keys "C-c C-c"))
 :then ((:sampled "capture" :buffer "\\`CAPTURE-journey-gtd-")
        (:sampled "capture" :line "\\`\\*\\* TODO \\'")
        (:buffer-name "\\`notes\\.txt\\'")
        (:buffer-named-matches
         "\\`journey-gtd-"
         "\\`\\* Inbox\n\n\\*\\* TODO Buy milk\n\\[\\[file:[^]]*/notes\\.txt\\]\\]\n\n\\* Projects\n\\'")))

(provide 'l1-journey-08-capture-inbox)

;;; l1-journey-08-capture-inbox.el ends here
