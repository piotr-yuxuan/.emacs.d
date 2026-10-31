;;; l1-session-warning-gui.el --- Self-test: a warning ends no file -*- lexical-binding: t -*-
;; scenario-display: graphic

;;; Commentary:
;; Plan 105.  Buttercup prints the warnings a spec logs after the spec,
;; with `send-string-to-terminal', which signals on a graphical
;; terminal: the rest of the file never ran, and the session waited for
;; its deadline.  The harness now records them in the spec's result
;; line, and the runner prints them.  The first spec logs a warning as
;; a user meets one, from M-:; the second must still run.

;;; Code:

(require 'scenario)

(scenario
 :describe "SL-WARNING: a warning logged in a graphical session ends nothing"
 :given ((:name "warn" :file "notes.txt" :text "-!-\n"))
 :when ((:keys "M-:") (:type "(display-warning 'probe \"a probe warning\")")
        (:keys "<return>"))
 :then ((:buffer-name "\\`notes\\.txt\\'")))

(scenario
 :describe "SL-WARNING-NEXT: the spec after a warning still runs"
 :given ((:name "next" :file "next.txt" :text "-!-\n"))
 :when ((:open))
 :then ((:buffer-name "\\`next\\.txt\\'")))

(provide 'l1-session-warning-gui)

;;; l1-session-warning-gui.el ends here
