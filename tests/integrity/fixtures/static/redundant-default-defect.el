;;; redundant-default-defect.el --- Fixture: keywords that repeat defaults -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(setq straight-use-package-by-default t
      use-package-always-defer t)

(use-package fixture-noisy :ensure t :defer t)
