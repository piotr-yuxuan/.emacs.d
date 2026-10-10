;;; keystrokes-test.el --- Self-tests of the :when grammar -*- lexical-binding: t -*-

;;; Commentary:
;; The grammar is validated when a scenario expands, so these specs
;; call `my/scenario-compile-steps' directly.  Expectations come from
;; the grammar's contract: one key vector for the whole `:when', GUI
;; key names only, printable text only, probes in order, and a final
;; probe that checks no prompt is left open.  The behaviour of the
;; probes in a live session is covered by
;; `tests/scenarios/l1-scenario-language.el'.

;;; Code:

(require 'buttercup)
(require 'keystrokes)

(describe "The :when grammar"

  (it "compiles all steps into one key vector ending with the closing probe"
    (expect (my/scenario-compile-steps
             '((:keys "C-x b") (:type "ab") (:idle 300) (:sample "open")
               (:keys "<return>")))
            :to-equal
            '([?\C-x ?b ?a ?b scenario-probe scenario-probe return scenario-probe]
              (:idle 300) (:sample "open") (:end))))

  (it "keeps keys of several steps in the order written"
    (expect (car (my/scenario-compile-steps '((:keys "C-<right>") (:keys "M-s"))))
            :to-equal [C-right ?\M-s scenario-probe]))

  (it "compiles (:redisplay-while-typing) to a probe"
    (expect (my/scenario-compile-steps '((:keys "a") (:redisplay-while-typing)))
            :to-equal '([?a scenario-probe scenario-probe]
                        (:redisplay-while-typing) (:end))))

  (it "compiles (:open) to the closing probe alone"
    (expect (my/scenario-compile-steps '((:open)))
            :to-equal '([scenario-probe] (:end))))

  (it "rejects terminal key names, alone or with modifiers"
    (dolist (keys '("RET" "C-x TAB" "DEL" "ESC" "M-RET" "C-M-TAB"))
      (expect (my/scenario-compile-steps `((:keys ,keys))) :to-throw 'error)))

  (it "accepts the graphical key names"
    (expect (car (my/scenario-compile-steps
                  '((:keys "<tab> <return> <backspace> <escape> C-<tab>"))))
            :to-equal [tab return backspace escape C-tab scenario-probe]))

  (it "rejects keys that are not key-parse syntax"
    (expect (my/scenario-compile-steps '((:keys "C-x  b"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:keys kbd))) :to-throw 'error))

  (it "rejects control characters in typed text"
    (expect (my/scenario-compile-steps '((:type "a\nb"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:type "\t"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:type ""))) :to-throw 'error))

  (it "rejects a quoted list, a bare string and an evaluated form as a step"
    (expect (my/scenario-compile-steps ''((:keys "C-x b"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((kbd "C-x b"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '("C-x b")) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:keys . "C-x b"))) :to-throw 'error))

  (it "rejects wrong arguments"
    (expect (my/scenario-compile-steps '((:keys "a" "b"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:open t))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:redisplay-while-typing 1))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:idle 0))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:idle "400"))) :to-throw 'error)
    (expect (my/scenario-compile-steps '((:sample sym))) :to-throw 'error))

  (it "rejects (:open) together with other steps"
    (expect (my/scenario-compile-steps '((:open) (:keys "a"))) :to-throw 'error))

  (it "rejects two samples with one name"
    (expect (my/scenario-compile-steps '((:sample "s") (:keys "a") (:sample "s")))
            :to-throw 'error)))

(provide 'keystrokes-test)

;;; keystrokes-test.el ends here
