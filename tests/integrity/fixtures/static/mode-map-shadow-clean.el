;;; mode-map-shadow-clean.el --- Fixture: mode maps beside global keys -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(global-set-key (kbd "C-c t") #'fixture-terminal)
(global-set-key (kbd "C-c D") #'fixture-docker)
(global-set-key (kbd "C-c d") #'fixture-docker-old)

;; The debugger and test-runner prefixes are per language.
(define-key fixture-mode-map (kbd "C-c d") 'fixture-debug-map)
(define-key fixture-mode-map (kbd "C-c d b") #'fixture-breakpoint)
(define-key fixture-mode-map (kbd "C-c k t") #'fixture-run-test)

;; A key added under a global prefix keymap.
(use-package fixture-lsp
  :bind-keymap ("C-c l" . fixture-command-map)
  :bind (:map fixture-lsp-mode-map ("C-c l s s" . fixture-symbols)))

;; A map that is not a mode map, and a key no global binding uses.
(define-key fixture-prompt-map (kbd "C-c t") #'fixture-send)
(define-key fixture-mode-map (kbd "C-c x") #'fixture-run)
