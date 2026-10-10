;;; duplicate-binding-defect.el --- Fixture: a key bound twice in one map -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(global-set-key (kbd "C-c f") #'fixture-find)

(use-package fixture
  :bind ("C-c f" . fixture-find))
