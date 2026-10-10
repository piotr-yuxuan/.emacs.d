;;; protected-key-defect.el --- Fixture: protected vanilla keys rebound -*- lexical-binding: t -*-
;; Read as data by t0-checks-test.el, never evaluated.

(global-set-key (kbd "C--") #'fixture-contract)

(use-package fixture
  :bind (("M-0" . fixture-select-window))
  :bind* (("C-x f" . fixture-files)))

;; A command on a prefix of protected keys hides them all.
(global-set-key (kbd "C-x t") #'fixture-tabs)
