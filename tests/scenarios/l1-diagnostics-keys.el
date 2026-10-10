;;; l1-diagnostics-keys.el --- Flycheck keeps right-click -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 090.  flycheck-grammalecte's setup binds its correction menu to
;; <mouse-3> in `flycheck-mode-map', and Flycheck is on in every file
;; buffer, so right-click would lose `mouse-save-then-kill' everywhere,
;; code included.  Mouse input is a documented edge of keystroke
;; dispatch: the outcome reads the binding the dispatch would use.
;; Grammalecte's registration shows that its setup ran in the session.

;;; Code:

(require 'scenario)

(scenario
 :describe "DIAG-MOUSE: right-click keeps mouse-save-then-kill in Flycheck buffers"
 :given ((:name "elisp" :file "tool.el" :text "(message \"hello\")\n")
         (:name "conf" :file "settings.conf" :text "[main]\nname = value\n"))
 :when ((:open))
 :then ((:minor-modes-on flycheck-mode)
        (:variable-contains flycheck-checkers grammalecte)
        (:key-binding "<mouse-3>" mouse-save-then-kill)))

(provide 'l1-diagnostics-keys)

;;; l1-diagnostics-keys.el ends here
