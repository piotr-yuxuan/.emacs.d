;;; l1-indent-bars-yaml-gui.el --- Plan 115: YAML bars every 2 columns -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; `yaml-ts-mode' has no indentation offset, so indent-bars would guess
;; `standard-indent', 4, and miss every other level of 2-space YAML.
;; The bars of a YAML buffer are drawn every 2 columns.  Graphical: in
;; a daemon, indent-bars sets itself up, spacing included, only for a
;; graphical frame (plan 116).

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-INDENT-BARS-YAML-GUI: a YAML buffer draws indent-bars every 2 columns"
 :given ((:name "workflow"
          :file "workflow.yml"
          :text "on:\n  push:\n    branches:\n      - main\n-!-"))
 :when ((:open))
 :then ((:mode yaml-ts-mode)
        (:minor-modes-on indent-bars-mode)
        (:variable-equals indent-bars-spacing 2)))

(provide 'l1-indent-bars-yaml-gui)

;;; l1-indent-bars-yaml-gui.el ends here
