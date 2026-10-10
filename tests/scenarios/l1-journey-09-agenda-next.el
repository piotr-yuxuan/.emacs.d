;;; l1-journey-09-agenda-next.el --- Journey 9: C-c a g shows the NEXT actions -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094, journey 9.  C-c a g opens the daily task overview, a
;; block agenda whose "Next actions" section lists the NEXT tasks of
;; the task file and nothing else.  Without that section, the
;; mutation, the NEXT task appears nowhere: it is not scheduled.
;;
;; The session keeps the real HOME, and `my/gtd-file' names the user's
;; personal task file, which a test must not read.  This file points
;; `my/gtd-file' at a fresh file of the session's temporary directory,
;; as `local.el' would on another machine, and lets the configuration
;; recompute `org-agenda-files' from it, as it does after init.  It
;; refuses to run if the agenda would read any other file.
;;
;; The block separator is `org-agenda-block-separator', whose default
;; Org picks when org-agenda loads: a line of ─ when a frame can show
;; it, of = otherwise.  Since plan 098, org-agenda loads once the
;; session is idle, after the client frame exists, or with no frame at
;; all: either line separates the sections.

;;; Code:

(require 'scenario)

(setopt my/gtd-file (make-temp-file "journey-gtd-" nil ".org"
                                    "* Inbox\n** NEXT Call the plumber\n** TODO Water the plants\n"))
(my/org-set-agenda-files)
(unless (equal org-agenda-files (list my/gtd-file))
  (error "The agenda would read other task files: %S" org-agenda-files))

(scenario
 :describe "J-09: C-c a g shows the NEXT items in the daily agenda"
 :given ((:name "from a file" :file "notes.txt" :text "Some notes.\n"))
 :when ((:keys "C-c a g"))
 :then ((:buffer-name "\\`\\*Org Agenda\\*\\'")
        (:buffer-matches "^Next actions\n[^\n]*Inbox->NEXT Call the plumber\n\n[=─]+\nWaiting on others\n")))

(provide 'l1-journey-09-agenda-next)

;;; l1-journey-09-agenda-next.el ends here
