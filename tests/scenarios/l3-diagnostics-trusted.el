;;; l3-diagnostics-trusted.el --- Shell findings in trusted content -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 090, DD-07.  Emacs 31's Flymake runs a backend in a buffer
;; only when its content is trusted (`trusted-content'), so in the
;; user's sessions, where nothing is trusted, a Flymake shellcheck
;; backend is disabled and a double report stays hidden.  This file
;; trusts the session's temporary directory, where every sandbox lives,
;; as a user who trusts a project does.

;;; Code:

(require 'scenario)

(add-to-list 'trusted-content
             (file-name-as-directory
              (abbreviate-file-name (file-truename temporary-file-directory))))

(scenario
 :describe "DIAG-SHELL-TRUSTED: in trusted content, shellcheck's warning shows once"
 :given ((:name "bash" :file "deploy.sh"
          :text "#!/usr/bin/env bash\ntarget=$1\nls $target\n"
          :system (:lsp-root ".")))
 :when ((:open))
 :then ((:executables-found "bash-language-server" "shellcheck")
        (:diagnostic-at 3 "SC2086")))

(provide 'l3-diagnostics-trusted)

;;; l3-diagnostics-trusted.el ends here
