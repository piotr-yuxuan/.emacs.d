;;; l3-diagnostics.el --- Each fixture's known problem is shown once -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 090.  Each fixture has one known problem, found by the real
;; checker the configuration runs for its language.  The diagnostic
;; outcomes count Flycheck and Flymake together, once every checker of
;; the buffer has finished, so these scenarios hold whichever front end
;; shows a finding and fail when two show it (DD-07, decision D13).
;; Content is untrusted here, as for the user, whose `trusted-content'
;; is empty; l3-diagnostics-trusted.el runs the shell fixture trusted.
;;
;; The CHAIN scenarios show two checkers' findings in one buffer, all
;; through Flycheck: lsp-mode and LTeX+ each select their own Flycheck
;; checker, so the other's findings show only through the chain of next
;; checkers that the configuration links.
;;
;; Rows that start a language server import their sandbox as the
;; project root with (:lsp-root "."), as the user once answers
;; lsp-mode's prompt.  `:executables-found' skips a row whose programs
;; are missing; tests/expected-skips/090.el names them.

;;; Code:

(require 'scenario)

(scenario
 :describe "DIAG-SHELL: shellcheck's quoting warning shows once"
 :given ((:name "bash" :file "deploy.sh"
          :text "#!/usr/bin/env bash\ntarget=$1\nls $target\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "bash-language-server" "shellcheck")
        (:diagnostic-at 3 "SC2086")
        ;; Emacs's own sh-mode backend stays listed; with Flymake off it
        ;; never runs (see l3-diagnostics-flymake-mutation.el).
        (:minor-modes-off flymake-mode)
        (:variable-excludes flymake-diagnostic-functions flymake-shellcheck--backend)))

(scenario
 :describe "DIAG-NEXT: C-c ! n moves point to the shell warning"
 :given ((:name "bash" :file "deploy.sh"
          :text "-!-#!/usr/bin/env bash\ntarget=$1\nls $target\n"
          :system (:lsp-root ".")))
 ;; The user waits for the server's first report, then asks for it.
 :when ((:idle 10000) (:keys "C-c ! n"))
 :then ((:executables-found "bash-language-server" "shellcheck")
        (:line-at-point "\\`ls \\$target\\'")
        (:diagnostic-at 3 "SC2086")))

(scenario
 :describe "DIAG-PYTHON: BasedPyright's argument type error shows once"
 :given ((:name "python" :file "app.py"
          :text "def double(x: int) -> int:\n    return 2 * x\n\n\nRESULT = double(\"two\")\n"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached pyright))
 :when ((:open))
 :then ((:diagnostic-at 5 "reportArgumentType")))

(scenario
 :describe "DIAG-TERRAFORM: tofu-ls's undeclared variable shows once"
 :given ((:name "opentofu" :file "main.tf"
          :text "output \"greeting\" {\n  value = var.missing\n}\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "tofu-ls")
        (:diagnostic-at 2 "No declaration found for \"var\\.missing\"")))

(scenario
 :describe "DIAG-TERRAFORM-PROJECT: C-c ! P validates the project with tofu"
 :given ((:name "opentofu" :file "main.tf"
          :text "output \"greeting\" {\n  value = var.missing\n}\n"
          :system (:lsp-root ".")))
 ;; The user checks the project, then reads the project's errors.
 :when ((:idle 3000) (:keys "C-c ! P") (:idle 5000) (:keys "C-c ! l C-x o P"))
 :then ((:executables-found "tofu" "tofu-ls")
        (:buffer-name "\\`\\*Flycheck errors\\*\\'")
        (:buffer-matches "Reference to undeclared input variable")))

(scenario
 :describe "DIAG-ORG: org-lint's broken link shows once"
 :given ((:name "org" :file "notes.org"
          :text "* Notes\n\nSee [[file:missing.org][the plan]].\n"))
 :when ((:open))
 :then ((:diagnostic-at 3 "missing\\.org")))

(scenario
 :describe "DIAG-MARKDOWN: an LTeX+ grammar rule shows once"
 :given ((:name "markdown" :file "notes.md"
          :text "# Notes\n\nThis is a example of a sentence.\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "ltex-ls-plus" "marksman")
        (:diagnostic-at 3 "EN_A_VS_AN")))

(scenario
 :describe "DIAG-MARKDOWN-CHAIN: marksman's and LTeX+'s findings show together"
 ;; The second row opens with LTeX+ already running.  In English prose
 ;; the chain is synchronous, so no publish lands during a check here;
 ;; tests/diagnostics-test.el covers the publish during a check.
 :given ((:name "markdown" :file "notes.md"
          :text "# Notes\n\nThis is a example of a sentence.\n\nSee [the plan](missing.md).\n"
          :system (:lsp-root "."))
         (:name "readme" :file "README.md"
          :text "# Notes\n\nThis is a example of a sentence.\n\nSee [the plan](missing.md).\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "ltex-ls-plus" "marksman")
        (:diagnostic-at 3 "EN_A_VS_AN")
        (:diagnostic-at 5 "missing\\.md")
        (:minor-modes-off flymake-mode)))

(scenario
 :describe "DIAG-MARKDOWN-RESTART: marksman's findings stay after LTeX+ restarts"
 ;; Restarting LTeX+, as its doctor or a changed setting asks, turns it
 ;; on again where lsp-mode already serves the buffer: lsp-mode's
 ;; checker must stay selected, or marksman's broken link goes.
 :given ((:name "markdown" :file "notes.md"
          :text "# Notes\n\nThis is a example of a sentence.\n\nSee [the plan](missing.md).\n"
          :system (:lsp-root ".")))
 ;; The user waits for both servers, then restarts LTeX+.  Vacuous if
 ;; marksman attaches only after the restart: lsp-mode then selects its
 ;; checker by itself.  Needs 085's :when step that waits on a condition.
 :when ((:idle 15000) (:keys "M-x") (:type "lsp-ltex-plus-restart-server") (:keys "<return>"))
 :then ((:executables-found "ltex-ls-plus" "marksman")
        (:minor-modes-on lsp-diagnostics-mode lsp-ltex-plus-mode)
        (:diagnostic-at 3 "EN_A_VS_AN")
        (:diagnostic-at 5 "missing\\.md")))

(scenario
 :describe "DIAG-ORG-CHAIN: org-lint's and LTeX+'s findings show together"
 :given ((:name "org" :file "notes.org"
          :text "* Notes\n\nThis is a example of a sentence.\n\nSee [[file:missing.org][the plan]].\n"))
 :when ((:open))
 :then ((:executables-found "ltex-ls-plus")
        (:diagnostic-at 3 "EN_A_VS_AN")
        (:diagnostic-at 5 "missing\\.org")
        (:minor-modes-off flymake-mode)))

(scenario
 :describe "DIAG-FRENCH: Grammalecte's conjugation error shows once"
 ;; The file declares French, as Grammalecte requires.
 :given ((:name "french" :file "lettre.txt"
          :text "-*- jinx-languages: \"fr_FR\" -*-\n\nIls a mangé des pommes.\n"))
 :when ((:open))
 :then ((:executables-found "grammalecte-cli.py" "python3")
        (:diagnostic-at 3 "Conjugaison erronée")))

(scenario
 :describe "DIAG-ENGLISH: Grammalecte leaves English prose alone"
 ;; Grammalecte's French typography rules flag a colon and straight
 ;; quotes; a buffer that does not declare French must show none.
 :given ((:name "org" :file "notes.org"
          :text "* Notes\n\nNote: the plan is \"simple\"; see the README.\n")
         (:name "markdown" :file "notes.md"
          :text "# Notes\n\nNote: the plan is \"simple\"; see the README.\n"
          :system (:lsp-root "."))
         (:name "text" :file "notes.txt"
          :text "Notes\n\nNote: the plan is \"simple\"; see the README.\n"))
 :when ((:open))
 :then ((:executables-found "grammalecte-cli.py" "python3")
        (:diagnostic-count 3 "insécable\\|uillemets" 0)))

(provide 'l3-diagnostics)

;;; l3-diagnostics.el ends here
