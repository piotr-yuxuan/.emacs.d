;;; l3-diagnostics-flymake-mutation.el --- Flymake back in shell buffers -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 090, the mutation test of DD-07.  This file puts Flymake back
;; in shell buffers, as the removed flymake-shellcheck configuration
;; did, and trusts the sandboxes as l3-diagnostics-trusted.el does.
;; The shell warning must then show twice, through Flycheck and
;; Flymake, so the trusted scenario would fail with a count of 2.
;; flymake-shellcheck is no longer installed, so Flymake runs the
;; shellcheck backend that `sh-mode' installs itself.  A count of 1
;; here means the diagnostic outcomes no longer see Flymake.

;;; Code:

(require 'scenario)

(add-to-list 'trusted-content
             (file-name-as-directory
              (abbreviate-file-name (file-truename temporary-file-directory))))
(add-hook 'sh-base-mode-hook #'flymake-mode)

(scenario
 :describe "DIAG-SHELL-MUTATION: with Flymake back, the trusted shell warning shows twice"
 :given ((:name "bash" :file "deploy.sh"
          :text "#!/usr/bin/env bash\ntarget=$1\nls $target\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "bash-language-server" "shellcheck")
        (:minor-modes-on flymake-mode)
        (:diagnostic-count 3 "SC2086" 2)))

(provide 'l3-diagnostics-flymake-mutation)

;;; l3-diagnostics-flymake-mutation.el ends here
