;;; readers.el --- Fixture: every form the readers know -*- lexical-binding: t -*-
;; Read as data by readers-test.el, never evaluated.

(setq fixture-a 1
      fixture-b 2)
(setq-default fixture-c 3)

(global-set-key (kbd "C-c a") #'fixture-agenda)
(keymap-global-set "C-c b" 'fixture-buffers)
(define-key global-map "\C-cc" #'fixture-capture)
(define-key fixture-mode-map [remap undo] #'fixture-undo)
(keymap-set fixture-mode-map "M-<left>" (lambda () (interactive)))
(bind-key "C-c e" #'fixture-eval fixture-mode-map)
(bind-key* "C-c f" #'fixture-find)

(defun fixture-function (fixture-argument)
  (let ((fixture-local 1))
    (setq fixture-local 2
          fixture-argument 3
          fixture-d 4)))

(use-package fixture-package
  :after (:all fixture-base fixture-extra)
  :bind (("C-c g" . fixture-go)
         :map fixture-mode-map
         ("C-c h" . fixture-here))
  :bind* ("C-c i" . fixture-important)
  :bind-keymap ("C-c j" . fixture-command-map)
  :custom
  (fixture-e 5)
  (fixture-f 6 "With a comment.")
  :config
  (setopt fixture-g 7)
  (customize-set-variable 'fixture-h 8)
  (with-eval-after-load 'fixture-later
    (define-key fixture-later-mode-map (kbd "C-c k") #'fixture-later-command))
  (use-package fixture-nested :custom ((fixture-i 9) (fixture-j 10))))

(with-eval-after-load 'fixture-feature
  (local-set-key (kbd "C-c l") #'fixture-local-command))

(eval-after-load "fixture-file" '(fixture-setup))

(define-key (current-local-map) (kbd "C-c m") #'fixture-menu)

(use-package fixture-package)

(dolist (map (list fixture-mode-map fixture-other-mode-map))
  (define-key map (kbd "C-c t") #'fixture-test))

(defvar-keymap fixture-new-map "C-c n" #'fixture-new)

(add-hook 'fixture-mode-hook (lambda () (setq-local fixture-k 11)))

(seq-doseq (map (vector fixture-mode-map))
  (define-key map (kbd "C-c u") #'fixture-u))
