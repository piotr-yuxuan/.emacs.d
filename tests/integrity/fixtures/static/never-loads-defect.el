;;; never-loads-defect.el --- Fixture: blocks that nothing loads -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(setq use-package-always-defer t)

;; Waits for magit, but nothing loads the package, even after magit.
(use-package fixture-after
  :after magit
  :custom (fixture-after-width 50)
  :config (fixture-after-mode))

;; Its :config waits for a load that never comes.  The block names its
;; own mode, but only in :config, which runs after the load.
(use-package fixture-config
  :config (global-fixture-config-mode 1))

;; Waiting for a package does not load it: neither an after-load form
;; nor another block's :after, even with :demand.
(with-eval-after-load 'fixture-after
  (setq fixture-after-ready t))
(use-package fixture-companion
  :after fixture-config
  :demand t)
