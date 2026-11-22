;;; language-servers-test.el --- Logic tests of plan 120 -*- lexical-binding: t -*-

;;; Commentary:
;; Logic tests of plan 120: the expressions nixd is given, the language
;; id and the server Compose files get, with Docker's telemetry off,
;; the YAML settings lsp-mode sends,
;; and the order in which the Terraform server is read from local.el.
;; The servers themselves run in tests/scenarios/l3-toml-compose.el.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'lsp-mode)

(defvar my/terraform-language-server)
(defvar lsp-terraform-ls-server)
(declare-function my/nixd-expressions "init")
(declare-function lsp-buffer-language--configured-id "lsp-mode")
(declare-function lsp-configuration-section "lsp-mode")
(declare-function lsp--json-serialize "lsp-mode")
(declare-function my/compose-language-server "init")

(defconst language-servers-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "The worktree whose tests/ holds this file.")

(describe "nixd's expressions"
  (it "takes NixOS and Home Manager options from nixosConfigurations on Linux"
    (let ((expressions (my/nixd-expressions "/flakes/fleet" "laptop"
                                            "nixpkgs-unstable" 'gnu/linux)))
      (expect (plist-get expressions :nixos)
              :to-equal "(builtins.getFlake \"git+file:///flakes/fleet\").nixosConfigurations.laptop.options")
      (expect (plist-get expressions :home-manager)
              :to-equal "(builtins.getFlake \"git+file:///flakes/fleet\").nixosConfigurations.laptop.options.home-manager.users.type.getSubOptions [ ]")
      (expect (plist-member expressions :nix-darwin) :to-be nil)))

  (it "takes nix-darwin and Home Manager options from darwinConfigurations on macOS"
    (let ((expressions (my/nixd-expressions "/flakes/mac" "macOS"
                                            "base.inputs.nixpkgs-unstable" 'darwin)))
      (expect (plist-get expressions :nix-darwin)
              :to-equal "(builtins.getFlake \"git+file:///flakes/mac\").darwinConfigurations.macOS.options")
      (expect (plist-get expressions :home-manager)
              :to-equal "(builtins.getFlake \"git+file:///flakes/mac\").darwinConfigurations.macOS.options.home-manager.users.type.getSubOptions [ ]")
      (expect (plist-member expressions :nixos) :to-be nil)))

  (it "imports the nixpkgs input that Home Manager takes its packages from"
    (expect (plist-get (my/nixd-expressions "/flakes/mac" "macOS"
                                            "base.inputs.nixpkgs-unstable" 'darwin)
                       :nixpkgs)
            :to-equal "import (builtins.getFlake \"git+file:///flakes/mac\").inputs.base.inputs.nixpkgs-unstable { }"))

  (it "percent-encodes a space, which a git+file reference rejects"
    (expect (plist-get (my/nixd-expressions "/flakes/my fleet" "h" "nixpkgs-unstable"
                                            'gnu/linux)
                       :nixos)
            :to-match "git\\+file:///flakes/my%20fleet\""))

  (it "refuses a system that is neither Linux nor macOS"
    (expect (my/nixd-expressions "/flakes/fleet" "h" "nixpkgs-unstable" 'windows-nt)
            :to-throw 'error)))

(defun language-servers-test--language (file-name)
  "Return the language id lsp-mode gives a buffer visiting FILE-NAME."
  (with-temp-buffer
    (setq buffer-file-name file-name)
    (lsp-buffer-language--configured-id)))

(defmacro language-servers-test--with-compose-server (executable &rest body)
  "Run BODY with lsp-mode's tables as Compose's setup leaves them for EXECUTABLE.
The tables are copies, without what the session's own setup added."
  (declare (indent 1))
  `(let ((lsp-language-id-configuration
          (cl-remove "dockercompose" lsp-language-id-configuration
                     :key #'cdr :test #'equal))
         (lsp-clients (make-hash-table :test #'eq)))
     (my/compose-language-server ,executable)
     ,@body))

(describe "Compose files"
  (it "stay yaml, for yaml-language-server, when Docker's server is missing"
    (language-servers-test--with-compose-server nil
      (expect (language-servers-test--language "/srv/app/compose.yaml") :to-equal "yaml")
      (expect (gethash 'docker-language-server lsp-clients) :to-be nil)))

  (it "get dockercompose, which Docker's server checks, when it is found"
    (language-servers-test--with-compose-server "/profile/bin/docker-language-server"
      (dolist (name '("/srv/app/compose.yaml" "/srv/app/compose.yml"
                      "/srv/app/docker-compose.yml" "/srv/app/compose.override.yaml"))
        (expect (language-servers-test--language name) :to-equal "dockercompose"))
      (expect (gethash 'docker-language-server lsp-clients) :to-be-truthy)))

  (it "leave other YAML yaml, and a Dockerfile, where no Scout lookup may start, alone"
    (language-servers-test--with-compose-server "/profile/bin/docker-language-server"
      (expect (language-servers-test--language "/srv/compose/values.yaml") :to-equal "yaml")
      (expect (language-servers-test--language "/srv/php/composer.yaml") :to-equal "yaml")
      (expect (language-servers-test--language "/srv/app/compose-values.yml") :to-equal "yaml")
      (expect (language-servers-test--language "/srv/app/.github/workflows/ci.yml")
              :to-equal "yaml")
      (expect (language-servers-test--language "/srv/app/Dockerfile")
              :not :to-equal "dockercompose")))

  (it "turn Docker's telemetry off, at start and in its configuration reply"
    (language-servers-test--with-compose-server "/profile/bin/docker-language-server"
      (let* ((client (gethash 'docker-language-server lsp-clients))
             (reply (gethash "workspace/configuration"
                             (lsp--client-request-handlers client)))
             (params (lsp-make-configuration-params
                      :items (vector (lsp-make-configuration-item :section "docker")
                                     (lsp-make-configuration-item :section "telemetry")))))
        (expect (lsp--json-serialize (lsp--client-initialization-options client))
                :to-equal "{\"telemetry\":\"off\"}")
        (expect (lsp--json-serialize (funcall reply nil params))
                :to-equal "[{\"telemetry\":\"off\"},{\"telemetry\":\"off\"}]")))))

(describe "yaml-language-server's settings"
  (it "encode as JSON, so that lsp-mode can send them"
    ;; With a string as a schema's key, lsp-mode failed to encode them
    ;; ("Wrong type argument: symbolp"), and no YAML server received any.
    (require 'lsp-yaml)
    (expect (lsp--json-serialize (lsp-configuration-section "yaml")) :to-be-truthy)
    (expect (lsp--json-serialize (lsp-configuration-section "yaml"))
            :to-match "github-workflow")))

(describe "The Terraform language server"
  (it "is the one local.el names, read when lsp-terraform loads"
    ;; lsp-terraform loads with the first Terraform buffer, after local.el.
    (expect (featurep 'lsp-terraform) :to-be nil)
    (let ((my/terraform-language-server "tofu-ls"))
      (load (expand-file-name "tests/fixtures/local-terraform/local.el"
                              language-servers-test--root)
            nil t)
      (require 'lsp-terraform)
      (expect lsp-terraform-ls-server :to-equal "terraform-ls"))))

;;; language-servers-test.el ends here
