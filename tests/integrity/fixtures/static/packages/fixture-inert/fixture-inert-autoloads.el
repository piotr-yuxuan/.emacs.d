;;; fixture-inert-autoloads.el --- Fixture: autoloads that only declare -*- lexical-binding: t -*-
(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))
(autoload 'fixture-inert-command "fixture-inert" "Run the fixture." t)
(defvar fixture-inert-mode nil "Non-nil if Fixture-Inert mode is enabled.")
(custom-autoload 'fixture-inert-mode "fixture-inert" nil)
(autoload 'fixture-inert-mode "fixture-inert" "Toggle the fixture mode." t)
(register-definition-prefixes "fixture-inert" '("fixture-inert-"))
(provide 'fixture-inert-autoloads)
