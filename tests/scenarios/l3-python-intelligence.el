;;; l3-python-intelligence.el --- Python navigation and import actions -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Real BasedPyright and Ruff processes exercise the configured
;; commands; no LSP response is mocked.  The Python configuration
;; guesses the project root itself; each row still adds its project to
;; the private lsp session, so that its cleanup shuts the servers down.

;;; Code:

(require 'scenario)

(scenario
 :describe "Python F9 opens LSP Treemacs"
 :given ((:name "source symbols" :project "python-intelligence" :entry "example.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "<f9>"))
 :then ((:window-shows "\\`\\*LSP Symbols List\\*\\'")))

(scenario
 :describe "Python organize imports sorts through Ruff"
 :given ((:name "unsorted imports" :project "python-intelligence" :entry "example.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-attached ruff) (:lsp-indexed))
 :when ((:keys "C-c l r o"))
 :then ((:buffer-text-changed)
        (:buffer-matches "import os\nimport sys\nfrom abc import ABC, abstractmethod")))

(scenario
 :describe "Python hint toggle disables hints without editing source"
 :given ((:name "hint recovery" :project "python-intelligence" :entry "example.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "C-c l T H"))
 :then ((:minor-modes-off lsp-inlay-hints-mode)
        (:buffer-text-unchanged)))

(scenario
 :describe "Python F9 closes the symbol tree on a second press"
 :given ((:name "toggle symbols" :project "python-intelligence" :entry "example.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "<f9> <f9>"))
 :then ((:file-suffix "example.py")
        (:windows-count 1)))

(scenario
 :describe "Python file-symbol search works before first using Consult"
 :given ((:name "module symbol" :project "python-intelligence" :entry "example.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-indexed))
 :when ((:keys "C-c l s f") (:type "GLOBAL_VALUE") (:keys "<return>"))
 :then ((:line-at-point "\\`GLOBAL_VALUE = 1\\'")))

(provide 'l3-python-intelligence)

;;; l3-python-intelligence.el ends here
