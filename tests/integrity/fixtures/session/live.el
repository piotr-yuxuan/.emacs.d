;;; live.el --- Live state for the fixtures of the T1 self-tests -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by tests/integrity/session/t1-checks-test.el, with lib/ on
;; the load path.  It defines what the CHECK-defect.el and
;; CHECK-clean.el fixtures name; those are only read, never evaluated.

;;; Code:

(require 't1fx-library)

(defun t1fx-command ()
  "Do nothing, as a documented command."
  (interactive))

(defun t1fx-undocumented ()
  (interactive))

(defun t1fx-plain ()
  "Do nothing, as a function that is not a command.")

;; An autoload into a library that is loaded and does not define it.
(autoload 't1fx-ghost "t1fx-library" nil t)

;; Autoloads into libraries nothing loads: lib/t1fx-menu.el,
;; lib/t1fx-hooked.el, and t1fx-absent-library, which does not exist.
(autoload 't1fx-menu-command "t1fx-menu" nil t)
(autoload 't1fx-hooked-mode "t1fx-hooked" nil t)
(autoload 't1fx-absent-command "t1fx-absent-library" nil t)

(defun t1fx-keyed ()
  "Call it with \\[t1fx-keyed]."
  (interactive))

(defvar t1fx-dangling-map
  (define-keymap
    "C-c a" #'t1fx-command
    "C-c b" 't1fx-missing
    "C-c d" 't1fx-ghost
    "C-c l" (lambda () nil)
    "C-c p q" #'t1fx-plain
    "<f19> d" #'t1fx-command
    "<menu-bar> <t1fx>" (list 'menu-item "Fixture" (lambda () nil))
    "C-<down-mouse-2>" (lambda () nil))
  "A keymap holding four dangling bindings, a menu and a mouse binding.")

(defvar t1fx-clean-map
  (define-keymap
    "C-c a" #'t1fx-command
    "C-c k x" #'t1fx-command
    "C-c s" #'t1fx-keyed
    "<f19> c" #'t1fx-command)
  "A keymap holding only commands, one under a prefix.")

(defun t1fx-reader ()
  "Read a key, as the agenda dispatcher does."
  (interactive))

(defvar t1fx-reading-map
  (define-keymap
    "<f19> y" #'t1fx-reader
    "<f19> f" '(menu-item "Filtered" t1fx-command :filter ignore))
  "A keymap with a command that reads keys, and a menu item its filter hides.")

(define-minor-mode t1fx-on-mode
  "A global fixture mode, turned on."
  :global t
  :keymap (define-keymap "<f19> m" #'t1fx-command))

(define-minor-mode t1fx-off-mode
  "A global fixture mode, left off."
  :global t
  :keymap (define-keymap "<f19> o" #'t1fx-command))

(define-minor-mode t1fx-local-mode
  "A buffer-local fixture mode, left off."
  :keymap (define-keymap "<f19> l" #'t1fx-command))

(defvar t1fx-package-mode-map
  (define-keymap
    "C-c q" #'t1fx-command
    "C-c y" #'t1fx-command
    "C-c P x" #'t1fx-command
    "C-c u" #'t1fx-command
    "C-c e" #'t1fx-command
    "<f19> r" #'t1fx-command)
  "A package's mode map binding keys of the space reserved for users.")

(defvar t1fx-child-package-mode-map
  (define-keymap :parent t1fx-package-mode-map)
  "A mode map that only inherits the keys of its parent.")

(defvar t1fx-greedy-mode-map
  (define-keymap "C-c" #'t1fx-command)
  "A mode map that takes the whole C-c prefix, as cdlatex can.")

(t1fx-on-mode 1)

(define-derived-mode t1fx-parent-mode fundamental-mode "Parent"
  "A fixture major mode.")

(define-derived-mode t1fx-child-mode t1fx-parent-mode "Child"
  "A fixture mode whose hook repeats a function of its parent's.")

(define-derived-mode t1fx-other-mode t1fx-parent-mode "Other"
  "A fixture mode whose hook adds a function of its own.")

(define-derived-mode t1fx-old-mode fundamental-mode "Old"
  "A fixture mode the self-tests remap away.")

(defalias 't1fx-alias-mode #'t1fx-child-mode
  "An alias of a mode: calling it runs the hooks of t1fx-child-mode.")

(defalias 't1fx-hooked-alias-mode #'t1fx-child-mode
  "An alias of a mode whose hook is an alias of the mode's hook.")

(defvaralias 't1fx-hooked-alias-mode-hook 't1fx-child-mode-hook)

(define-derived-mode t1fx-legacy-mode fundamental-mode "Legacy"
  "A fixture mode the self-tests remap to `t1fx-ts-mode'.")

(define-derived-mode t1fx-ts-mode fundamental-mode "TS"
  "A fixture mode that names `t1fx-legacy-mode' an extra parent only.")

(derived-mode-add-parents 't1fx-ts-mode '(t1fx-legacy-mode))

(define-derived-mode t1fx-legacy-child-mode t1fx-legacy-mode "Legacy child"
  "A fixture mode derived from `t1fx-legacy-mode' that opens files.
The self-tests remap it away too.")

(define-derived-mode t1fx-slides-mode t1fx-legacy-mode "Slides"
  "A fixture mode derived from `t1fx-legacy-mode' that opens no file.
As slitex-mode for latex-mode: no buffer starts it but M-x.")

(define-derived-mode t1fx-styles-mode fundamental-mode "Styles"
  "A fixture mode the self-tests remap away, as css-mode.")

(define-derived-mode t1fx-styles-ts-mode fundamental-mode "Styles TS"
  "The mode the self-tests remap `t1fx-styles-mode' to.")

(define-derived-mode t1fx-scss-mode t1fx-styles-mode "SCSS"
  "A fixture mode derived from `t1fx-styles-mode' and not remapped, as scss-mode.")

(defun t1fx-hook-function ()
  "Run from two hooks of the same mode chain.")

(add-hook 't1fx-parent-mode-hook #'t1fx-hook-function)
(add-hook 't1fx-child-mode-hook #'t1fx-hook-function)
(add-hook 't1fx-other-mode-hook #'ignore)
(add-hook 't1fx-child-mode-hook #'t1fx-local-mode)
(add-to-list 'auto-mode-alist '("\\.t1fx-child\\'" . t1fx-child-mode))
(add-to-list 'auto-mode-alist '("\\.t1fx-other\\'" . t1fx-other-mode))
(add-to-list 'auto-mode-alist '("\\.t1fx-legacy-child\\'" . t1fx-legacy-child-mode))
(add-to-list 'auto-mode-alist '("\\.t1fx-scss\\'" . t1fx-scss-mode))

(defvar t1fx-declared 1
  "A declared variable.")

(defvar t1fx-current 1
  "The current name of an option.")

(define-obsolete-variable-alias 't1fx-old-variable 't1fx-current "1.0")

;;; live.el ends here
