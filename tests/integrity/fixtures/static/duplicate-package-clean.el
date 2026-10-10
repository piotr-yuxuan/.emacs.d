;;; duplicate-package-clean.el --- Fixture: packages declared once -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.  Each name is
;; mentioned twice, but declared once.

(use-package fixture-base)

(use-package fixture-extension
  :after fixture-base
  :config (require 'fixture-base))
