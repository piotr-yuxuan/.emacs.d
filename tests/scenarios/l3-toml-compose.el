;;; l3-toml-compose.el --- tombi and Docker's Compose server report their findings -*- lexical-binding: t; -*-
;; scenario-tier: l3

;;; Commentary:
;; Plan 120.  A TOML buffer starts tombi, and a Compose file starts
;; Docker's docker-language-server beside yaml-language-server; each
;; shows its known problem once.  docker-language-server checks Compose
;; files at the YAML level only (yaml-language-server validates them
;; against the Compose schema), so its fixture holds a duplicate key.
;; A row whose server is missing skips; tests/expected-skips/120.el
;; names them.

;;; Code:

(require 'scenario)

(scenario
 :describe "TOML: tombi's type error in Cargo.toml shows once"
 :given ((:name "rust" :file "Cargo.toml"
          :text "[package]\nname = \"fixture\"\nversion = 1\nedition = \"2021\"\n"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached tombi))
 :when ((:open))
 :then ((:diagnostic-at 3 "type String")))

(scenario
 :describe "COMPOSE: docker-language-server's duplicate key shows once"
 :given ((:name "compose" :file "compose.yaml"
          :text "services:\n  web:\n    image: nginx\n    image: redis\n"
          :system (:lsp-root ".")))
 :wait-for ((:lsp-attached docker-language-server))
 :when ((:open))
 :then ((:diagnostic-at 4 "already defined")))

;;; l3-toml-compose.el ends here
