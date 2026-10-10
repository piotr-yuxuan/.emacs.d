;;; local.el --- Fixture machine-local settings for nixd -*- lexical-binding: t -*-

;; tests/config-defects-test.el loads this file as the configuration
;; loads the user's local.el, then loads lsp-nix.  The flake need not
;; exist: nixd evaluates the expressions, Emacs only builds them.

(setq my/nixd-flake "/flakes/fleet/"
      my/nixd-flake-host "fixture-host")

;;; local.el ends here
