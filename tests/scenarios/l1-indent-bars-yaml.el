;;; l1-indent-bars-yaml.el --- Plan 115: indent-bars in YAML buffers -*- lexical-binding: t -*-

;;; Commentary:
;; `yaml-ts-mode' derives from `text-mode', so the `prog-mode-hook'
;; entry of indent-bars never reached it.  Opening a YAML file must
;; give a `yaml-ts-mode' buffer (the grammar loaded, not the
;; fundamental-mode fallback) where indent-bars is on and draws a bar
;; every 2 columns, the indentation of this file.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-INDENT-BARS-YAML: opening a YAML file draws indent-bars every 2 columns"
 :given ((:name "workflow"
          :file "workflow.yml"
          :text "on:\n  push:\n    branches:\n      - main\n-!-"))
 :when ((:open))
 :then ((:mode yaml-ts-mode)
        (:minor-modes-on indent-bars-mode)
        (:variable-equals indent-bars-spacing 2)))

(provide 'l1-indent-bars-yaml)

;;; l1-indent-bars-yaml.el ends here
