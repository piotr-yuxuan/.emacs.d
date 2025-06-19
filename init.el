;;; package -- Summary
;;; Commentary:
;;;   Emacs configuration generated from =init.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   litterate documentation file.

;;; Code:

(when-let (library (locate-library "org" nil load-path))
  (setq-default load-path (delete (substring (file-name-directory library) 0 -1)
                                  load-path)))

(defvar default-gc-cons-threshold 16777216 ; 16mb
  "My default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; make garbage collector less invasive during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Don't check outdated compiled lisp files.
(setq load-prefer-newer noninteractive)

(add-hook
 'emacs-startup-hook
 (lambda (&rest _)
   (setq gc-cons-threshold default-gc-cons-threshold
         gc-cons-percentage 0.1)))

;; Asynchronous package compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation t)

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t))
      backup-directory-alist `((".*" . "~/.emacs.d/backups/")))

(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(setq straight-check-for-modifications '(find-when-checking check-on-save))
(setq straight-cache-autoloads t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package general
  :ensure t)

(setq initial-scratch-message nil ;; No need to remind me what a scratch buffer is.
      ;; Double-spaces after periods is morally wrong.
      sentence-end-double-space nil
      ;; Save existing clipboard text into the kill ring before replacing it.
      save-interprogram-paste-before-kill t
      ;; Prompts should go in the minibuffer, not in a GUI.
      use-dialog-box nil
      ;; accept 'y' or 'n' instead of yes/no
      use-short-answers t
      ;; eke out a little more scrolling performance
      fast-but-imprecise-scrolling t
      ;; if native-comp is having trouble, there's not very much I can do
      native-comp-async-report-warnings-errors 'silent
      ;; unicode ellipses are better
      truncate-string-ellipsis "…"
      ;; keep the point in the same place while scrolling
      scroll-preserve-screen-position t
      ;; don't keep duplicate entries in kill ring
      kill-do-not-save-duplicates t)

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(use-package eval-sexp-fu
  :hook (prog-mode . turn-on-eval-sexp-fu-flash-mode))

(setq visible-bell t
      column-number-mode t
      mouse-wheel-tilt-scroll t)

(when (eq system-type 'darwin)
  (setq mac-function-modifier 'super)
  ;; Sometimes we need # for comments
  (setq mac-right-option-modifier 'none))

(when (eq system-type 'gnu/linux)
  ;; TODO: I'd like to ignore the left control to preserve my hand.
  (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier))

(global-auto-revert-mode)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package crux
  :config (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(use-package which-key
  :hook (emacs-startup . which-key-mode))

(use-package color-theme-sanityinc-tomorrow
  :init
  ;; Load themes but leave activation to auto-dark.
  (load-theme 'sanityinc-tomorrow-night t nil)
  (load-theme 'sanityinc-tomorrow-day t nil))

(defun my/apply-theme-customizations ()
  "Apply face customizations after theme change."
  (when (facep 'eglot-highlight-symbol-face)
    (let ((adjust-fn (if (eq auto-dark-mode 'dark)
                         #'color-lighten-name
                       #'color-darken-name))
          (adjust-val (if (eq auto-dark-mode 'dark) 42 30)))
      (set-face-attribute 'eglot-highlight-symbol-face nil
                          :background (funcall adjust-fn (face-background 'highlight) adjust-val)))))

(defun my/load-theme (theme)
  "Load THEME and apply customizations."
  (load-theme theme t nil)
  (my/apply-theme-customizations))

(defun my/auto-dark ()
  "Enable auto-dark-mode and set theme hooks."
  (auto-dark-mode t))

(use-package auto-dark
  :config
  (setq auto-dark-dark-theme 'sanityinc-tomorrow-night
        auto-dark-light-theme 'sanityinc-tomorrow-day)

  (add-hook 'auto-dark-dark-mode-hook
            (lambda () (my/load-theme 'sanityinc-tomorrow-night)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda () (my/load-theme 'sanityinc-tomorrow-day)))

  :hook ((emacs-startup . my/auto-dark)
         (focus-in . my/auto-dark)))

(set-face-attribute 'default nil
                    :family (cond ((eq system-type 'darwin) "Iosevka Term")
                                  ((eq system-type 'gnu/linux) "Iosevka Term")
                                  (t "Fira Code"))
                    :weight 'regular
                    :height 180
                    :width 'normal)

(use-package fringe-scale
  :straight (fringe-scale :type git :host github :repo "blahgeek/emacs-fringe-scale"))

(set-fringe-mode 16)
(fringe-scale-setup)

;; If you use `emacs-mac' Mac port then ligatures are handled for you.
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)

  (defconst known-ligatures
    '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
      "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
      "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
      ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))

  (use-package ligature
    :config
    (ligature-set-ligatures 'prog-mode known-ligatures)
    (ligature-set-ligatures 'text-mode known-ligatures)
    (ligature-set-ligatures 'eat-mode known-ligatures)
    (ligature-set-ligatures 'comint-mode known-ligatures)
    (ligature-set-ligatures 'special-mode known-ligatures)
    ;; Enables ligature checks globally in all buffers. You can also
    ;; do it per mode with `ligature-mode'.
    :hook (emacs-startup . global-ligature-mode)))

(add-hook 'auto-dark-dark-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil
                                :weight 'light
                                :slant 'italic)))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini) ;; more useful than helm-buffers-list
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-x k" . kill-buffer))
  :config
  (helm-mode 1)
  :custom
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-move-to-line-cycle-in-source nil))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t))

(use-package moody
  :demand t
  :custom (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :hook (emacs-startup . minions-mode))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package svg-clock)

(use-package ivy
  :init (ivy-mode 1))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package imenu
  :hook ((prog-mode text-mode org-mode) . imenu-add-menubar-index))

(winner-mode 1)

(defun my/other-window-reverse ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(general-define-key
 :prefix "C-x"
 "O" 'my/other-window-reverse)

(global-set-key (kbd "M-o") 'ace-window)

(use-package exec-path-from-shell
  :config
  (customize-set-variable
   'exec-path-from-shell-arguments
   (remove "-i" exec-path-from-shell-arguments))
  :hook (emacs-startup . (lambda ()
                           ;; Can be quite slow.
                           (when (memq window-system '(mac ns x))
                             (exec-path-from-shell-initialize))
                           (when (daemonp)
                             (exec-path-from-shell-initialize)))))

(use-package dashboard
  :custom ((dashboard-startup-banner 'logo)
           (dashboard-items '(;; (bookmarks . 15)
                              (projects . 10)
                              ;; (recents  . 10)
                              ;; (agenda . 15)
                              ;; (registers . 15)
                              ))
           (dashboard-center-content t)
           (dashboard-vertically-center-content t)
           (inhibit-startup-screen t)
           (dashboard-projects-switch-function
            (lambda (project)
              (magit-status project)
              (delete-other-windows)))))
(dashboard-setup-startup-hook)

(use-package edit-indirect)

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines))
  :custom ((mc/always-run-for-all t)))

(use-package paredit
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode)
  :bind (("C-s-s" . paredit-splice-sexp)
         ("C-s-<up>" . paredit-splice-sexp-killing-backward)
         ("C-s-<down>" . paredit-splice-sexp-killing-forward)
         ("C-<right>" . paredit-forward-slurp-sexp)
         ("C-<left>" . paredit-forward-barf-sexp)
         ("C-s-<left>" . paredit-backward-slurp-sexp)
         ("C-s-<right>" . paredit-backward-barf-sexp)
         :map paredit-mode-map
         ("RET" . nil))
  :config
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  ;; (put 'paredit-newline 'delete-selection t)
  )

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package undo-tree
  :demand t
  :defer nil
  :commands undo-tree-visualize
  :hook ((prog-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :bind ([remap undo] . undo-tree-visualize)
  :custom ((undo-tree-history-directory-alist '(("." . "~/.emacs.d/backups/undo-tree")))
           (undo-tree-visualizer-timestamps t)
           (undo-tree-auto-save-history nil)
           (undo-tree-visualizer-diff t)))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(global-set-key (kbd "s-SPC") 'cycle-spacing)
(add-hook 'before-save-hook 'whitespace-cleanup)

(delete-selection-mode 1)

(use-package writeroom-mode)
(use-package centered-cursor-mode)

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  ((visual-fill-column-center-text 1)
   (visual-fill-column-width 85)))

(defalias 'zsh-mode 'shell-script-mode)

(use-package org
  :hook ((org-mode . git-gutter-mode))
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  :config
  ;; So I can execute shell code from org-mode babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (mermaid . t)))
  :hook
  (org-mode . auto-fill-mode))

(use-package org-contrib)

(use-package toc-org
  :straight (toc-org :host github :repo "snosov1/toc-org")
  :hook ((org-mode . toc-org-enable)))

(defun org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))

(use-package svg-tag-mode
  :commands org-mode
  :config
  (defun my/svg-tag-mode-hook ()
    (interactive)
    (setq svg-tag-tags '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
                         (":FIXME:" . ((lambda (tag) (svg-tag-make "FIXME"  :face 'org-todo :inverse t :margin 0))))))
    (svg-tag-mode))

  :hook ((org-mode . my/svg-tag-mode-hook)
         (prog-mode . my/svg-tag-mode-hook)
         (org-agenda-finalize . org-agenda-show-svg)))

(use-package ob-mermaid
  :custom
  (cond ((eq system-type 'darwin) (ob-mermaid-cli-path "/opt/homebrew/bin/mmdc"))
        ((eq system-type 'gnu/linux) (ob-mermaid-cli-path "/usr/bin/mmdc"))))

(use-package dirvish
  :after magit
  :init
  (dirvish-override-dired-mode)
  :custom ((dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
           (dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
           (delete-by-moving-to-trash t)
           (dired-mouse-drag-files t)
           (mouse-drag-and-drop-region-cross-program t))
  :config
  (dirvish-side-follow-mode) ;; similar to `treemacs-follow-mode'
  :bind ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ;; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ;; remapped `describe-mode'
   ("s"   . dirvish-quicksort) ;; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu) ;; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package projectile
  :config (projectile-mode +1)
  :bind-keymap ("s-p" . projectile-command-map))

(use-package helm-projectile)

(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package treemacs
  :bind (("<f8>" . treemacs))
  :config
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
  (treemacs-git-mode 'extended)
  (treemacs-project-follow-mode)
  (treemacs-git-commit-diff-mode))

(use-package treemacs-projectile)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(setq mode-require-final-newline t)

(use-package helm-ag
  :after (helm-projectile)
  :custom ((helm-follow-mode-persistent t)
           (helm-ag-fuzzy-match t)
           (helm-ag-use-grep-ignore-list t)
           (helm-ag-insert-at-point 'symbol)))

(use-package helm-rg
  :after (helm-projectile))

(use-package swiper-helm
  ;; i-search is still available with S-f
  ;; i-search* families are still powerful when needed
  :bind (("S-f" . isearch-forward)
         ("C-s" . swiper)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package forge
  :after magit
  :straight (forge :host github :repo "magit/forge"))

(use-package pinentry
  :commands magit
  :config (pinentry-start)
  :custom (epg-pinentry-mode 'loopback))

(use-package eldoc-box
  :after (eldoc eglot)
  :ensure t
  ;;:hook (eglot-managed-mode . eldoc-box-hover-mode)
  :bind* (("C-h C-d" . eldoc-box-help-at-point)
          ;; "C-h ." is already defined.
          ))

(defun my/eglot-x-on-enter-hook ()
  "Use eglot-x-on-enter as RET in eglot-managed buffers. We don't want to
map it to a global or keymap-scoped binding so we can't use `:bind` from
`use-package`."
  (local-set-key (kbd "RET") #'eglot-x-on-enter))

(use-package eglot
  :after which-key
  :hook (prog-mode . eglot-ensure)
  :bind (:map prog-mode-map
              ;; ("M-." . xref-find-definitions)
              ;; ("M-?" . xref-find-references)
              ("C-c e r" . eglot-rename)
              ("C-c e o" . eglot-code-action-organize-imports)
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format)
              ("C-c e i" . eglot-find-implementation)
              ("C-c e d" . eglot-find-declaration)
              ("C-c e t" . eglot-find-typeDefinition)
              ("C-c e x" . eglot-reconnect) ; or eglot-shutdown if preferred
              ("C-c e q" . eglot-shutdown)
              ("C-c e l" . eglot-stderr-buffer))
  :custom
  (eglot-sync-connect nil)
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil :background (color-darken-name (face-background 'highlight) 5))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio"))))

(use-package eglot-x
  :straight (eglot-x :type git :host github :repo "nemethf/eglot-x")
  :after eglot
  :hook ((eglot-managed-mode . eglot-x-setup)
         (eglot-managed-mode . my/eglot-x-on-enter-hook))
  :bind (:map eglot-mode-map
              ("C-c e E" . eglot-x-execute-command)
              ("C-c e S" . eglot-x-workspace-restart)
              ("C-c e C" . eglot-x-inlay-hints-mode)
              ("C-c e D" . eglot-x-show-documentation)))

(which-key-add-key-based-replacements
  "C-c e" "eglot")

(use-package tree-sitter
  :defer t
  :hook (;; Here put the modes for which you want tree sitter syntax
         ;; highlighting and fold indicators.
         (emacs-lisp-mode . tree-sitter-mode)
         (prog-mode . tree-sitter-mode)))

(setq my/tree-sitter-langs-grammar-straight-dir "~/.emacs.d/straight/build/tree-sitter-langs/bin")
(setq my/tree-sitter-langs-grammar-dir "~/.emacs.d/tree-sitter")
(setq my/tree-sitter-langs-grammar-prefix "libtree-sitter-")

(defun rename-tree-sitter-grammars (orig-fun &rest args)
  "Prefix tree-sitter grammar files with `treesit-`, as expected by tree-sitter."
  (dolist (file (directory-files my/tree-sitter-langs-grammar-straight-dir t "\\.so$"))
    (let ((new-name (concat my/tree-sitter-langs-grammar-dir "/"
                            my/tree-sitter-langs-grammar-prefix
                            (file-name-nondirectory file))))
      (copy-file file new-name t))))

(use-package tree-sitter-langs
  :after tree-sitter
  :defer t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (setq treesit-extra-load-path (list my/tree-sitter-langs-grammar-straight-dir))
  (advice-add 'tree-sitter-langs-install-grammars :after #'rename-tree-sitter-grammars))

(use-package company-mode
  :hook (prog-mode . company-mode))

(use-package company-flx
  :after company-mode
  :hook (prog-mode . company-flx-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package ido-completing-read+)

(use-package flx-ido
  :hook ((prog-mode . ido-mode)
         ;; Smarter fuzzy matching for ido.
         (prog-mode . flx-ido-mode))
  :bind ("C-x k" . kill-buffer)
  :custom
  (ido-enable-flex-matching t)
  ;; Disable ido faces to see flx highlights.
  (ido-use-faces nil))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package ts-fold-indicators
  :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook ((tree-sitter-mode . ts-fold-indicators-mode)
         (tree-sitter-mode . ts-fold-line-comment-mode))
  :bind (("C-s-<tab>" . ts-fold-toggle))
  :custom ((ts-fold-line-count-show t))
  :config
  (fringe-helper-define 'ts-fold-indicators-fr-minus-tail nil
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

  (fringe-helper-define 'ts-fold-indicators-fr-center nil "")
  (fringe-helper-define 'ts-fold-indicators-fr-end-left nil "")
  (fringe-helper-define 'ts-fold-indicators-fr-end-right nil ""))

;; (defun my/highlight-indent-guides-dark ()
;;   ;; The rightmost indent guide currently active.
;;   (set-face-foreground 'highlight-indent-guides-top-character-face "red")
;;   (set-face-background 'highlight-indent-guides-top-character-face "red")
;;   ;; The lower indent guides in the current stack.
;;   (set-face-foreground 'highlight-indent-guides-stack-character-face "blue")
;;   (set-face-background 'highlight-indent-guides-stack-character-face "blue")
;;   ;; Everything else that is not active.
;;   (set-face-foreground 'highlight-indent-guides-character-face "orange")
;;   (set-face-background 'highlight-indent-guides-character-face "orange"))

;; (defun my/highlight-indent-guides-light ()
;;   ;; The rightmost indent guide currently active.
;;   (set-face-foreground 'highlight-indent-guides-top-character-face "red")
;;   (set-face-background 'highlight-indent-guides-top-character-face "red")
;;   ;; The lower indent guides in the current stack.
;;   (set-face-foreground 'highlight-indent-guides-stack-character-face "blue")
;;   (set-face-background 'highlight-indent-guides-stack-character-face "blue")
;;   ;; Everything else that is not active.
;;   (set-face-foreground 'highlight-indent-guides-character-face "orange")
;;   (set-face-background 'highlight-indent-guides-character-face "orange"))

(defun my/hide-leftmost-indent-guide (level responsive display)
  (if (< level 1)
      nil
    (highlight-indent-guides--highlighter-default level responsive display)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-auto-enabled nil)
  ;;(highlight-indent-guides-highlighter-function 'my/hide-leftmost-indent-guide)
  :hook
  (tree-sitter-mode . highlight-indent-guides-mode))

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

(general-define-key
 :keymaps 'prog-mode-map
 :prefix "C-c"
 "r" #'format-all-region-or-buffer)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load)
         (sh-mode . flymake-mode))
  :custom (flymake-shellcheck-allow-external-files variable t))

(use-package clojure-mode)
(use-package cider)

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)        ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :hook (clojure-mode . my/clojure-mode-hook))

(use-package python
  :after eglot
  :custom (python-indent-guess-indent-offset-verbose nil)
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server"))))

(use-package poetry
  :hook (python-mode . poetry-tracking-mode)
  ;:custom (poetry-tracking-strategy 'switch-buffer)
  )

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package auto-virtualenv
  :custom (auto-virtualenv-verbose t)
  :config (auto-virtualenv-setup))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package auctex
  :hook ((LaTeX-mode . tree-sitter-mode)
         (LaTeX-mode . eglot-ensure)
         (LaTeX-mode . git-gutter-mode)
         (LaTeX-mode . turn-on-auto-fill))
  :custom ((TeX-engine 'xetex)
           (TeX-PDF-mode t)))

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex))

(use-package rust-mode
  :after eglot
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer"
                  :initializationOptions
                  (
                   :cargo (
                           :allFeatures t
                           :loadOutDirsFromCheck t
                           :runBuildScripts t)
                   :procMacro (:enable t)
                   :check (:command "clippy")
                   :inlayHints (
                                :bindingModeHints t
                                :chainingHints t
                                :closingBraceHints t
                                :parameterHints t
                                :typeHints t
                                :maxLength 25
                                :enabled t)
                   :lens (
                          :enable t
                          :implementations t
                          :references t
                          :run t
                          :debug t))))))

(use-package cargo-mode
  :hook (rust-mode . cargo-minor-mode)
  :custom ((compilation-scroll-output t)))

(require 'server)
(unless (server-running-p)
    (server-start))

(use-package devdocs)

(use-package restclient)

(use-package company-restclient
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package eat
  :straight (eat :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  ;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu
  :custom ((process-adaptive-read-buffering nil)
           (read-process-output-max (* 4 1024 1024))
           (eat-enable-directory-tracking t)
           (eat-enable-shell-prompt-annotation nil))
  :bind (:map eat-mode-map
              ("C-k" . eat-reset)))

(use-package vterm)

(defun my/btop ()
  "Open a vterm buffer, run the `btop` command, and rename the
buffer to '*vterm* btop'. Exit on quit."
  (interactive)
  (let ((buffer-name "*vterm* btop"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((vterm-buffer (vterm buffer-name)))
        (with-current-buffer vterm-buffer
          (vterm-send-string "btop; exit 0")
          (vterm-send-return))))))

(use-package daemons)

(defun my/garamond-font ()
  "Set a Garamond font for nov-mode."
  (face-remap-add-relative 'variable-pitch :family "Garamond" :height 160))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . centered-cursor-mode)
         (nov-mode . my/garamond-font)))

(use-package esup
  ;; Sometimes an error happens. It is fixed with:
  :custom (esup-depth 0))

(provide 'init)
;;; init.el ends here
