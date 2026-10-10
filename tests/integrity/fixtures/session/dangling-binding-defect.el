;;; dangling-binding-defect.el --- Bindings into a keymap with dangling keys -*- lexical-binding: t -*-
;; Read by tests/integrity/session/t1-checks-test.el, never evaluated.

;; t1fx-dangling-map (live.el) holds a binding to an undefined
;; command, an autoload its loaded library does not define, a lambda
;; that is not a command, and a function that is not a command under
;; a prefix.
(keymap-set t1fx-dangling-map "C-c b" 't1fx-missing)

;;; dangling-binding-defect.el ends here
