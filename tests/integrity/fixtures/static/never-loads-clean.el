;;; never-loads-clean.el --- Fixture: blocks that load -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.  Each block would
;; never load without the trigger its comment names.

(setq use-package-always-defer t)

;; :demand loads it once magit has loaded.
(use-package fixture-demand :after magit :demand t :config (fixture-demand-mode))

;; :defer with a number loads it when Emacs is idle.
(use-package fixture-idle :defer 2 :config (fixture-idle-mode))

;; :defer nil overrides use-package-always-defer.
(use-package fixture-eager :after magit :defer nil)

;; :hook, :bind and :commands autoload the commands they name.
(use-package fixture-hook :hook (prog-mode . fixture-hook-mode) :config (fixture-hook-setup))
(use-package fixture-bind :bind ("C-c x" . fixture-bind-command) :config (fixture-bind-setup))
(use-package fixture-commands :after magit :commands fixture-commands-list)

;; :init runs at startup and calls an autoloaded function.
(use-package fixture-init :init (fixture-init-mode) :config (fixture-init-setup))

;; :custom sets an option its autoloads declare, which loads it.
(use-package fixture-custom :custom (fixture-custom-mode t) :config (fixture-custom-setup))

;; Another form uses an autoloaded function.
(use-package fixture-elsewhere :config (fixture-elsewhere-setup))
(add-hook 'text-mode-hook #'fixture-elsewhere-mode)

;; Another form requires it.
(use-package fixture-required :config (fixture-required-setup))
(with-eval-after-load 'org (require 'fixture-required))

;; Its autoloads load it: it is absent from the autoloads alist.
(use-package fixture-itself :config (fixture-itself-setup))

;; Nothing waits for it to load: :init runs at startup.
(use-package fixture-plain :init (setq fixture-plain-variable 1))

;; Disabled blocks do nothing.
(use-package fixture-disabled :disabled :config (fixture-disabled-mode))
