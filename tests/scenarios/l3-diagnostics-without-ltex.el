;;; l3-diagnostics-without-ltex.el --- Prose findings without LTeX+ -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 090.  Where LTeX+ does not check a buffer (ltex-ls-plus is not
;; installed, its server has gone, or the buffer visits no file),
;; Flycheck selects the buffer's checker from `flycheck-checkers' by
;; itself.  An Org buffer must then still show org-lint's findings, and
;; Grammalecte's after them when it declares French.  This file makes
;; the session's ltex-ls-plus missing, as on a machine without it.

;;; Code:

(require 'scenario)

(setq lsp-ltex-plus-ls-plus-executable "/nonexistent/ltex-ls-plus")

(scenario
 :describe "DIAG-ORG-WITHOUT-LTEX: org-lint's and Grammalecte's findings show together"
 :given ((:name "org" :file "notes.org"
          :text "#+LANGUAGE: fr\n\nSee [[file:missing.org][the plan]].\n\nIls a mangé des pommes.\n"))
 :when ((:open))
 :then ((:executables-found "grammalecte-cli.py" "python3")
        (:diagnostic-at 3 "missing\\.org")
        (:diagnostic-at 5 "Conjugaison erronée")
        (:minor-modes-off lsp-ltex-plus-mode)))

(provide 'l3-diagnostics-without-ltex)

;;; l3-diagnostics-without-ltex.el ends here
