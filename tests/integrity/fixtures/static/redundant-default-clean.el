;;; redundant-default-clean.el --- Fixture: keywords that change defaults -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.  Nothing sets
;; use-package-always-defer or use-package-always-ensure, and straight
;; does not install by default.

(setq straight-use-package-by-default nil)

(use-package fixture-quiet :ensure t :defer t)
(use-package fixture-idle :defer 2 :ensure nil)
