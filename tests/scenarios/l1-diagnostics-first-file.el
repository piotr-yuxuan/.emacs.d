;;; l1-diagnostics-first-file.el --- Flycheck is on in the first file -*- lexical-binding: t -*-
;; scenario-startup: first-file diagnostics-first-file/settings.conf

;;; Commentary:
;; Plan 090, DD-04.  The runner opens this file right after startup, as
;; `emacsclient FILE' does.  Flycheck must already be on there.  A
;; configuration file starts no language server and no LTeX+, which
;; would turn Flycheck on in their own buffers: only the global mode
;; can.  The runner lets the idle timers of startup run before the
;; specs, so the mode alone would also hold for Flycheck started from an
;; idle timer (DD-04's defect).  `after-init-hook' is what starts it
;; before the first file, so the spec checks that wiring too, until the
;; runner can observe the first file before its idle timers (085).

;;; Code:

(require 'scenario)

(scenario
 :describe "DIAG-FIRST-FILE: a file opened first has Flycheck on"
 :given ((:name "opened at startup" :existing-buffer "\\`settings\\.conf\\'"))
 :when ((:open))
 :then ((:mode conf-mode)
        (:minor-modes-on flycheck-mode)
        (:variable-contains after-init-hook global-flycheck-mode)))

(provide 'l1-diagnostics-first-file)

;;; l1-diagnostics-first-file.el ends here
