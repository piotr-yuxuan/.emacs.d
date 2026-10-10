;;; dangling-binding-clean.el --- Bindings into keymaps of commands only -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-clean-map (live.el) holds commands only; the keymap of a
;; package not loaded is skipped.
(keymap-set t1fx-clean-map "C-c a" #'t1fx-command)
(keymap-set t1fx-not-loaded-map "C-c a" #'t1fx-missing)

;;; dangling-binding-clean.el ends here
