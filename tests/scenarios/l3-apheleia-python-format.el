;;; l3-apheleia-python-format.el --- C-c r runs the configured Python formatters -*- lexical-binding: t -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 094, K18 of the 085 mutation baseline.  The configuration maps
;; Python to isort then black, where Apheleia's own default is black
;; alone.  Unsorted imports and a badly spaced call tell them apart:
;; only isort reorders the imports, only black spaces the arguments.
;; isort and black are real processes; without them the spec skips
;; (tests/expected-skips/094.el).
;;
;; The file belongs to a project with a pyproject.toml, as the user's
;; Python files do.  The user waits for the language servers, as
;; lsp-deferred starts them once Emacs is idle, before pressing C-c r:
;; pressed earlier, the formatting races their start, Ruff's server
;; exits with code 2 and lsp-mode's restart prompt blocks the session
;; (see the plan's open questions).  The row adds the project to the
;; private lsp session, so that its clean-up shuts the servers down; a
;; missing server skips the spec.

;;; Code:

(require 'scenario)

(scenario
 :describe "K18-APHELEIA-PYTHON: C-c r sorts the imports and formats the code"
 :given ((:name "py" :project "journey-python-format" :entry "unformatted.py"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright) (:lsp-attached ruff) (:lsp-indexed))
 :when ((:keys "C-c r") (:idle 5000))
 :then ((:executables-found "isort" "black")
        (:buffer-text "import os\nimport sys\n\nprint(os.sep, sys.argv)\n")))

(provide 'l3-apheleia-python-format)

;;; l3-apheleia-python-format.el ends here
