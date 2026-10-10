;;; l3-journey-05-switch-project.el --- Journey 5: s-p p then f opens a file of another project -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 5.  Project switching offers the projects the
;; user has worked in.  The user first opens a file of journey-project
;; with s-p f, which makes that project known, then moves to a file of
;; another project (the sandbox around it), and from there switches
;; back with s-p p, picks journey-project by name, presses f and opens
;; one of its files.  When no project is remembered, the mutation, the
;; name matches no known project and the prompt stays open.  project.el
;; lists the files with git, a real process, so the file is L3.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-05: s-p p, then f, opens a file in another project"
 :given ((:name "switch" :project "journey-project" :entry "README.txt"))
 :when ((:keys "s-p f") (:type "meeting") (:keys "<return>")
        (:keys "C-x C-f") (:type "../../elsewhere.txt") (:keys "<return>")
        (:keys "s-p p") (:type "journey-project") (:sample "projects") (:keys "<return>")
        (:keys "f") (:type "checklist") (:keys "<return>"))
 :then ((:sampled "projects" :buffer "\\`elsewhere\\.txt\\'")
        (:sampled "projects" :minibuffer "Select project.*: journey-project *\n.*/journey-project/")
        (:file-suffix "/journey-project/docs/release-checklist.txt")))

(provide 'l3-journey-05-switch-project)

;;; l3-journey-05-switch-project.el ends here
