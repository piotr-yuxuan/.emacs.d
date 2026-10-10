;;; l3-dirvish-fd.el --- RET in an fd listing enters the listed directory -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 089.  dirvish draws no collapsed chain in an fd listing (the
;; `:when' of its `collapse' attribute), so `my/dirvish-collapse-shown-p'
;; must keep RET and <tab> from following one there.  C-c f runs a real
;; fd over the fixture file-tree, hence the L3 tier.  The scenario
;; language skips only on a missing language server, so a missing fd
;; fails the row instead of skipping it.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL3-DIRVISH-FD-RET: <return> on a directory of an fd listing enters it, not its chain"
 ;; src/ heads the chain src/main/java/com/example/App.java.
 :given ((:name "fd from notes.txt" :project "file-tree" :entry "notes.txt"))
 :when ((:keys "C-c f") (:idle 2000) (:keys "C-s s r c <return> <return>"))
 :then ((:buffer-name "\\`src\\'")))

(provide 'l3-dirvish-fd)

;;; l3-dirvish-fd.el ends here
