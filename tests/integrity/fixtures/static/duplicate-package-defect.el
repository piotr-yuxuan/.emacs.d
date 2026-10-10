;;; duplicate-package-defect.el --- Fixture: a package declared twice -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(use-package fixture-twice :custom (fixture-twice-lines 5))

(with-eval-after-load 'org
  (use-package fixture-twice :config (fixture-twice-mode)))
