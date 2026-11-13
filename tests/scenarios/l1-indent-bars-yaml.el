;;; l1-indent-bars-yaml.el --- Plan 115: indent-bars in YAML buffers -*- lexical-binding: t -*-

;;; Commentary:
;; `yaml-ts-mode' derives from `text-mode', so the `prog-mode-hook'
;; entry of indent-bars never reached it.  Opening a YAML file must
;; give a `yaml-ts-mode' buffer (the grammar loaded, not the
;; fundamental-mode fallback) where indent-bars is on.  The spacing of
;; its bars is checked in l1-indent-bars-yaml-gui.el: in a daemon,
;; indent-bars sets itself up only for a graphical frame (plan 116).

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-INDENT-BARS-YAML: opening a YAML file activates indent-bars-mode"
 :given ((:name "workflow"
          :file "workflow.yml"
          :text "on:\n  push:\n    branches:\n      - main\n-!-"))
 :when ((:open))
 :then ((:mode yaml-ts-mode)
        (:minor-modes-on indent-bars-mode)))

(provide 'l1-indent-bars-yaml)

;;; l1-indent-bars-yaml.el ends here
