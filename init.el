;;; package -- Summary
;;; Commentary:
;;;   Emacs configuration generated from =init.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   litterate documentation file.

;;; Code:

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

(use-package org
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

(use-package dashboard
  :custom ((dashboard-startup-banner 'logo)
	   (dashboard-items '(;; (bookmarks . 15)
			      ;; (projects . 15)
			      ;; (recents  . 15)
			      ;; (agenda . 15)
			      ;; (registers . 15)
			      ))
	   (dashboard-center-content t)
	   (dashboard-vertically-center-content t)))
(dashboard-setup-startup-hook)

(use-package edit-indirect)

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-c C-S-c" . mc/edit-lines))
  :custom ((mc/always-run-for-all t)))

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode))
  :bind (("A-s" . paredit-splice-sexp)
	 ("A-<up>" . paredit-splice-sexp-killing-backward)
	 ("A-<down>" . paredit-splice-sexp-killing-forward)
	 ("A-<right>" . paredit-forward-slurp-sexp)
	 ("A-<left>" . paredit-forward-barf-sexp)
	 ("A-S-<left>" . paredit-backward-slurp-sexp)
	 ("A-S-<right>" . paredit-backward-barf-sexp)
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

(delete-selection-mode 1)

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
  (load-theme 'sanityinc-tomorrow-day t))

(use-package auto-dark
  :ensure t
  :custom ((auto-dark-dark-theme 'sanityinc-tomorrow-night)
	   (auto-dark-light-theme 'sanityinc-tomorrow-day))
  :config
  (auto-dark-mode t))

;; If you use `emacs-mac' Mac port then ligatures are handled for you.
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)

  (defconst known-ligatures
    '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>"
      "-->" "///" "/=" "/==" "/>" "//" "/*" "*>" "***" ",*/" "<-" "<<-"
      "<=>" "<=" "<|" "<||" "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<=="
      "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$"
      "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--" ":>" ":<" ":::" "::"
      ":?" ":?>" ":=" "::=" "=>>" "==>" "=/=" "=!=" "=>" "===" "=:="
      "==" "!==" "!!" "!=" ">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">="
      "&&&" "&&" "|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-"
      "||=" "||" ".." ".?" ".=" ".-" "..<" "..." "+++" "+>" "++" "[||]"
      "[<" "[|" "{|" "??" "?." "?=" "?:" "##" "###" "####" "#[" "#{"
      "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_" "__" "~~" "~~>"
      "~>" "~-" "~@" "$>" "^=" "]#" "www" "**" "**/" "*/" "\\\\"
      "\\\\\\" "{-" "-}" "----" "/**" "=<<" ">>" "<--" "%%"))

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

(set-face-attribute 'font-lock-comment-face nil
		    :weight 'light
		    :slant 'italic)

(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x b" . helm-mini) ;; more useful than helm-buffers-list
	 ("C-x C-f" . helm-find-files)
	 ("C-x f" . helm-recentf)
	 ("C-x k" . kill-buffer))
  :config (helm-mode 1)
  :custom (helm-mode-fuzzy-match t)
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

(use-package git-gutter+
  :bind (:repeat-map git-gutter+-repeat-map
		     ("n" . git-gutter+-next-hunk)
		     ("p" . git-gutter+-previous-hunk)
		     ("s" . git-gutter+-stage-hunks)
		     ("r" . git-gutter+-revert-hunk)
		     :exit
		     ("c" . magit-commit-create)
		     ("C" . magit-commit)
		     ("b" . magit-blame))
  :hook (emacs-startup . global-git-gutter+-mode))

(use-package forge
  :after magit
  :straight (forge :host github :repo "magit/forge"))

(use-package pinentry
  :commands magit
  :config (pinentry-start)
  :custom (epg-pinentry-mode 'loopback))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
	 (clojure-mode . eglot-ensure)
	 (terraform-mode . eglot-ensure)
	 (prog-mode . eglot-ensure)
	 (sh-mode . eglot-ensure)
	 (latex-mode . eglot-ensure))
  :bind (:map prog-mode-map
         ;("M-." . xref-find-definitions)
         ;("M-?" . xref-find-references)
	 )
  :custom (eglot-sync-connect nil)
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil
		      :background (color-lighten-name
				   (face-background 'highlight)
				   42))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio"))))

(use-package dtrt-indent
  :custom (dtrt-indent-max-merge-deviation 0.0)
  :hook (eglot--managed-mode . dtrt-indent-global-mode))

(defun format-or-indent-region (orig &rest args)
  (interactive)
  (if (and (eglot-current-server) (region-active-p))
      (eglot-format (region-beginning) (- (region-end) 1))
    (apply orig args)))

(advice-add 'indent-region :around #'format-or-indent-region)

(defun adjust-tab-width ()
  "`tab-width' is used by eglot for LSP formatting; adjust it with dtrt."
  (let ((indent-offset-variables
         (nth 2 (dtrt-indent--search-hook-mapping major-mode))))
    (let ((indent-offset-variable
           (if (listp indent-offset-variables)
               (nth 0 indent-offset-variables)
             indent-offset-variables)))
      (if (boundp indent-offset-variable)
          (setq tab-width (symbol-value indent-offset-variable))))))

(advice-add 'dtrt-indent-try-set-offset :after #'adjust-tab-width)
(advice-add 'dtrt-indent-set :after #'adjust-tab-width)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load)
	 (sh-mode . flymake-mode))
  :custom (flymake-shellcheck-allow-external-files variable t))

(use-package clojure-mode)
(use-package cider)
(use-package clj-refactor)

(use-package python
  :hook ((python-mode . eglot-ensure))
  :custom (python-indent-guess-indent-offset-verbose nil))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package poetry
  :hook (python-mode . poetry-tracking-mode)
  ;:custom (poetry-tracking-strategy 'switch-buffer)
)

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package terraform-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(defun my/post-startup-hook ()
  (add-hook 'prog-mode #'tree-sitter-mode))

(use-package tree-sitter
  :ensure t
  :hook (;; Enable syntax highlighting based on tree-sitter wherever possible:
	 (tree-sitter-after-on . tree-sitter-hl-mode)
	 (emacs-startup . my/post-startup-hook)))

(setq my/tree-sitter-langs-grammar-dir "~/.emacs.d/straight/build/tree-sitter-langs/bin/")
(setq my/tree-sitter-langs-grammar-prefix "libtree-sitter-")

(defun rename-tree-sitter-grammars (orig-fun &rest args)
  "Prefix tree-sitter grammar files with `treesit-`, as expected by tree-sitter."
  (dolist (file (directory-files my/tree-sitter-langs-grammar-dir t "\\.so$"))
    (let ((new-name (concat my/tree-sitter-langs-grammar-dir
			    my/tree-sitter-langs-grammar-prefix
			    (file-name-nondirectory file))))
      (rename-file file new-name t))))

(use-package tree-sitter-langs
  :ensure t
  :init
  (setq treesit-extra-load-path (list my/tree-sitter-langs-grammar-dir))
  (advice-add 'tree-sitter-langs-install-grammars :after #'rename-tree-sitter-grammars))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(defun my/overload-fix-visual-glitch ()
  (fringe-helper-define 'ts-fold-indicators-fr-plus nil
  "XXXXXXX"
  "X.....X"
  "X..X..X"
  "X.XXX.X"
  "X..X..X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'ts-fold-indicators-fr-minus-tail nil
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'ts-fold-indicators-fr-center nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'ts-fold-indicators-fr-end-left nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XXXXX" "...XXXXX"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(fringe-helper-define 'ts-fold-indicators-fr-end-right nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "XXXXX..." "XXXXX..."
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook ((tree-sitter-after-on . ts-fold-mode)
	 (ts-fold-mode . ts-fold-indicators-mode)
	 (ts-fold-indicators-mode . my/overload-fix-visual-glitch))
  :bind (("C-S-<tab>" . ts-fold-close-all)))

(use-package highlight-indent-guides
  :custom ((highlight-indent-guides-method 'character)
	   (highlight-indent-guides-auto-enabled nil))
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (set-face-foreground 'highlight-indent-guides-character-face "gray"))

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

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package devdocs)

(use-package gptel
  :custom
  (gptel-model "gemma2:27b")
  (gptel-default-mode "markdown-mode")
  :config
  (setq gptel-backend (gptel-make-ollama "Ollama (local)"
    :host "localhost:11434"
    :stream t
    :models '("gemma2:27b"))))

(use-package gptel-extensions
  :after gptel
  :straight (gptel-extensions :type git
			      :host github
			      :repo "kamushadenes/gptel-extensions.el"))

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
	   (eat-term-name "xterm-256color"))
  :bind (:map eat-mode-map
	      ("C-k" . eat-reset)))

(use-package vterm)

(defun my/btop ()
  "Open a vterm buffer, run the `btop` command, and rename the
buffer to '*vterm-btop*'. Exit on quit."
  (interactive)
  (let ((buffer-name "*vterm-btop*"))
    (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
      (let ((vterm-buffer (vterm buffer-name)))
        (with-current-buffer vterm-buffer
          (vterm-send-string "btop; exit 0")
          (vterm-send-return))))))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package daemons)

(use-package esup
  ;; Sometimes an error happens. It is fixed with:
  :custom (esup-depth 0))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(provide 'init)
;;; init.el ends here
