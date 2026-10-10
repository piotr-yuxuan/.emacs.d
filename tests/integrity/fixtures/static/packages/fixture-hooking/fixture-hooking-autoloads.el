;;; fixture-hooking-autoloads.el --- Fixture: autoloads that hook the package in -*- lexical-binding: t -*-
(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))
(autoload 'fixture-hooking-mode "fixture-hooking" "Toggle the fixture mode." t)
(add-hook 'prog-mode-hook #'fixture-hooking-mode)
(provide 'fixture-hooking-autoloads)
