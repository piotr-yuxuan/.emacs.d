;;; l1-mark-set-mark-command.el --- C-SPC and C-@ activate the region (SC-6, SC-7) -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 073 / mark slice.  Carries forward two audit rows from the
;; deleted `tests/test-scarlet-changes.el':
;;
;;   SC-6: C-SPC sets the mark and activates the region.
;;   SC-7: C-@ does the same (vanilla equivalent).
;;
;; Moving forward a word after setting the mark makes the region hold
;; that word, which is what the user sees highlighted.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-MARK-C-SPC: C-SPC activates the region"
 :given ((:name "any-file" :file "src/sample.clj" :text "(+ -!-one two)"))
 :when ((:keys "C-SPC M-f"))
 :then ((:region-text "one")))

(scenario
 :describe "BL1-MARK-C-AT: C-@ activates the region (vanilla equivalent of C-SPC)"
 :given ((:name "any-file" :file "src/sample.clj" :text "(+ -!-one two)"))
 :when ((:keys "C-@ M-f"))
 :then ((:region-text "one")))

(provide 'l1-mark-set-mark-command)

;;; l1-mark-set-mark-command.el ends here
