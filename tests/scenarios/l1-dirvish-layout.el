;;; l1-dirvish-layout.el --- The dirvish layout key goes through the configured recipes -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089.  M-t toggles the full-frame layout, which starts as
;; `dirvish-default-layout', (1 0.11 0.55), the second recipe of
;; `dirvish-layout-recipes'.  M-T moves to the next recipe, wrapping
;; round.  With two recipes, next, previous and a flip between them
;; look the same, so these rows cannot verify the order of the cycle.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-DIRVISH-LAYOUT-NEXT: M-T moves to the next configured layout"
 :given ((:name "file-tree root" :project "file-tree" :entry "."))
 :when ((:keys "M-t M-T"))
 :then ((:dirvish-layout (0 0.11 0.55))))

(scenario
 :describe "BL1-DIRVISH-LAYOUT-CYCLE: M-T twice comes back to the default layout"
 :given ((:name "file-tree root" :project "file-tree" :entry "."))
 :when ((:keys "M-t M-T M-T"))
 :then ((:dirvish-layout (1 0.11 0.55))))

(provide 'l1-dirvish-layout)

;;; l1-dirvish-layout.el ends here
