;;; l3-journey-04-wgrep-edit.el --- Journey 4: edit search results, save the files -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, journey 4.  From consult-ripgrep's matches, C-c e
;; (embark-export) opens them in a grep buffer.  C-c C-p makes it
;; editable with wgrep; the user finds the match with C-s, edits it,
;; and C-c C-c writes the edit back to the file, which the
;; configuration saves (`wgrep-auto-save-buffer').  Without that, the
;; mutation, the edit stays in an unsaved buffer and the file on disk
;; is unchanged.  ripgrep is a real process, so the file is L3.

;;; Code:

(require 'scenario)

(scenario
 :describe "J-04: C-c e exports the matches, wgrep edits them, the files are saved"
 :given ((:name "export" :project "journey-project" :entry "README.txt"))
 :when ((:keys "C-c s r") (:type "frobnicate") (:idle 2000) (:keys "C-c e")
        (:keys "C-c C-p C-s") (:type "frobnicate") (:keys "<return>") (:type "de")
        (:keys "C-c C-c"))
 :then ((:buffer-matches "^src/greeting\\.el:4:.*look for defrobnicate\\.")
        (:saved-file-matches "src/greeting.el" "journeys look for defrobnicate\\.\"\n")))

(provide 'l3-journey-04-wgrep-edit)

;;; l3-journey-04-wgrep-edit.el ends here
