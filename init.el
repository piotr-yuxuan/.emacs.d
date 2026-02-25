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

(use-package compile-angel
  :demand t
  :custom
  ;; Default is already nil, but be explicit: no per-file messages.
  (compile-angel-verbose nil)
  :config
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/init.el"       compile-angel-excluded-files)
  ;; no-littering's state dirs contain generated/data files, not source.
  ;; etc/custom.el is the main one that gets load-ed and would otherwise
  ;; be compiled unnecessarily.
  (add-to-list 'compile-angel-excluded-files-regexps
               (regexp-quote (expand-file-name "tests/" user-emacs-directory)))
  (add-to-list 'compile-angel-excluded-files-regexps
               (regexp-quote (expand-file-name "var/" user-emacs-directory)))
  (add-to-list 'compile-angel-excluded-files-regexps
               (regexp-quote (expand-file-name "etc/" user-emacs-directory)))
  ;; Suppress the *Compile-Log* buffer that byte-compile opens on its own.
  (setq byte-compile-verbose nil)
  (compile-angel-on-load-mode 1))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "autosaves/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el")
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backups/"))))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package benchmark-init
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package general
  :demand t)

(setq initial-scratch-message nil ;; No need to remind me what a scratch buffer is.
      ;; Avoid loading lisp-interaction machinery for the scratch buffer.
      initial-major-mode 'fundamental-mode
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

(delete-selection-mode t)
(global-hl-line-mode)
(column-number-mode)
(savehist-mode)
(save-place-mode 1)

(use-package eval-sexp-fu
  :hook (prog-mode . turn-on-eval-sexp-fu-flash-mode))

(setq visible-bell t
      mouse-wheel-tilt-scroll t)

(when (eq system-type 'darwin)
  (setq mac-function-modifier 'super)
  ;; Sometimes we need # for comments
  (setq mac-right-option-modifier 'none))

(when (eq system-type 'gnu/linux)
  ;; TODO: I'd like to ignore the left control to preserve my hand.
  (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier))

;; Wayland clipboard for terminal Emacs (emacs -nw).
;; PGTK GUI Emacs has native Wayland clipboard — this only applies when
;; window-system is nil (terminal) and WAYLAND_DISPLAY is set.
(when (and (eq system-type 'gnu/linux)
           (getenv "WAYLAND_DISPLAY")
           (not (display-graphic-p)))
  (defun my/wl-copy (text)
    (let ((proc (make-process :name "wl-copy"
                              :buffer nil
                              :command '("wl-copy" "-f" "-n")
                              :connection-type 'pipe)))
      (process-send-string proc text)
      (process-send-eof proc)))
  (defun my/wl-paste ()
    (shell-command-to-string "wl-paste --no-newline"))
  (setq interprogram-cut-function   #'my/wl-copy
        interprogram-paste-function #'my/wl-paste))

(global-auto-revert-mode)

(use-package crux
  :config (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(setq jit-lock-defer-time 0)
;; Stealth-fontify the rest of the buffer during idle time, reducing
;; the chance of a pause when you jump to an unfontified region.
(setq jit-lock-stealth-time 0.2
      jit-lock-stealth-nice 0.1)
;; Auto vertical scroll computes margins on every scroll event — disable it.
(setq auto-window-vscroll nil)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(global-so-long-mode 1)

(use-package which-key
  :hook (emacs-startup . which-key-mode))

(setq custom-safe-themes t)

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
         ;; This makes the window to flicker on each focus.
         ;(focus-in . my/auto-dark)
         ))

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
    '("www" "-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
      "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
      "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
      "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
      "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
      "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
      "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
      "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
      ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
      "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
      "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
      "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
      "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
      "&="))

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

;; Vertical completion UI
(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15))

;; Orderless matching: space-separated patterns
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Rich annotations in minibuffer
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

;; Enhanced search/navigation commands
(use-package consult
  :bind (("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)         ;; buffers + recent files + bookmarks
         ("C-x B"   . consult-project-buffer) ;; project-scoped buffer list
         ("C-x f"   . consult-recent-file)
         ("C-x k"   . kill-buffer)
         ("C-s"     . consult-line)
         ("S-f"     . isearch-forward)
         ("C-c s r" . consult-ripgrep)        ;; ripgrep (fast, project-wide)
         ("C-c s g" . consult-grep)           ;; grep (slower, supports -C context)
         ("C-c s G" . consult-git-grep)       ;; git grep
         ("C-c s f" . consult-find)
         ("M-g i"   . consult-imenu)))

;; Act on completion candidates
(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ;; Export candidates to a buffer (grep, occur, …) from minibuffer.
         ;; In a consult search: C-c e → embark-export → wgrep to bulk-edit.
         :map minibuffer-local-map
         ("C-c e" . embark-export)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Edit grep/ripgrep results in-place (works with embark exports and consult-ripgrep)
(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  ;; Show icons when running in a graphical frame (nerd-icons must be installed).
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  ;; Truncate file path relative to the project root.
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Show minor modes collapsed by minions.
  (doom-modeline-minor-modes t)
  ;; Only show encoding when it differs from UTF-8-unix.
  (doom-modeline-buffer-encoding 'nondefault)
  ;; Git branch + diff stats (±lines).
  (doom-modeline-vcs-max-length 25)
  ;; Show eglot/LSP connection status.
  (doom-modeline-lsp t)
  ;; Show mu4e unread count.
  (doom-modeline-mu4e t)
  ;; Show separate error and warning counts, not a single summary.
  (doom-modeline-checker-simple-format nil)
  ;; Use built-in project.el for project name.
  (doom-modeline-project-detection 'project)
  ;; Show language environment version (e.g. Python 3.12, Node 20).
  (doom-modeline-env-version t)
  ;; Don't show the clock — the system bar already has it.
  (doom-modeline-time nil))

(use-package minions
  :hook (window-setup . minions-mode))

(use-package svg-clock)

(winner-mode 1)

(defun my/other-window-reverse ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(general-define-key
 :prefix "C-x"
 "O" 'my/other-window-reverse)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package exec-path-from-shell
  :config
  (customize-set-variable
   'exec-path-from-shell-arguments
   (remove "-i" exec-path-from-shell-arguments))
  :hook (emacs-startup . (lambda ()
                           ;; Can be quite slow. `pgtk' covers Wayland GUI Emacs.
                           (when (memq window-system '(mac ns x pgtk))
                             (exec-path-from-shell-initialize))
                           (when (daemonp)
                             (exec-path-from-shell-initialize)))))

;; Per-directory environment variables via direnv.
;; A project-local .envrc that sets JAVA_HOME (e.g. via jenv or SDKMAN)
;; is applied to every subprocess including JDTLS, giving automatic
;; per-project Java runtime selection.  Run M-x envrc-allow once to
;; whitelist a new .envrc.  direnv itself is installed via nix/home.nix.
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package dashboard
  :custom ((dashboard-startup-banner 'logo)
           (dashboard-items '((bookmarks . 15)
                              (projects . 10)
                              (recents  . 10)
                              (agenda . 15)
                              (registers . 15)))
           (dashboard-center-content t)
           (dashboard-vertically-center-content t)
           (inhibit-startup-screen t)
           (dashboard-projects-switch-function
            (lambda (project)
              (magit-status project)
              (delete-other-windows))))
  ;; Hide the mode-line on the dashboard buffer to avoid triggering
  ;; doom-modeline's expensive icon/segment rendering during startup.
  ;; The setting is buffer-local; switching away naturally shows the
  ;; modeline in whatever buffer you land in.
  :hook (dashboard-mode . (lambda () (setq-local mode-line-format nil))))
(dashboard-setup-startup-hook)

(use-package markdown-toc)

(use-package edit-indirect)

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines))
  :custom ((mc/always-run-for-all t)))

(use-package paredit
  :hook ((emacs-lisp-mode) . paredit-mode)
  :bind (("C-s-s" . paredit-splice-sexp)
         ("C-s-<up>" . paredit-splice-sexp-killing-backward)
         ("C-s-<down>" . paredit-splice-sexp-killing-forward)
         ("C-<right>" . paredit-forward-slurp-sexp)
         ("C-<left>" . paredit-forward-barf-sexp)
         ("C-s-<left>" . paredit-backward-slurp-sexp) ;; Also ESC C-<left>
         ("C-s-<right>" . paredit-backward-barf-sexp) ;; Also ESC C-<right>
         :map paredit-mode-map
         ("RET" . nil))
  :config
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  ;; (put 'paredit-newline 'delete-selection t)
  )

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package vundo
  :bind ([remap undo] . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(global-set-key (kbd "s-SPC") 'cycle-spacing)
(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package writeroom-mode)
(use-package centered-cursor-mode)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  ((visual-fill-column-center-text 1)
   (visual-fill-column-width 85)))

(defalias 'zsh-mode 'shell-script-mode)

(use-package org
  :hook ((org-mode . git-gutter-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-cycle-global-at-bob t)
  :custom-face
  ;; Scale headings so each level is visually distinct beyond colour alone
  (org-level-1 ((t (:height 1.35 :weight bold))))
  (org-level-2 ((t (:height 1.2  :weight bold))))
  (org-level-3 ((t (:height 1.1  :weight semi-bold))))
  (org-level-4 ((t (:height 1.0  :weight normal))))
  :config
  ;; So I can execute shell code from org-mode babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (mermaid . t))))

(use-package org-contrib)

(use-package toc-org
  :straight (toc-org :host github :repo "snosov1/toc-org")
  :hook ((org-mode . toc-org-enable)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  ;; Leave heading stars as plain *** — the star count is the clearest
  ;; possible depth indicator and needs no decoration.
  (org-modern-star nil)
  (org-modern-block-fringe t)
  (org-modern-table t)
  (org-modern-keyword "‣ ")
  ;; Emphasis / entities
  (org-hide-emphasis-markers t)   ;; hidden by default, shown by org-appear
  (org-pretty-entities nil)
  (org-ellipsis " ▾")
  ;; Tags and headings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'show)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t))

;; Show emphasis markers only when cursor is inside the delimiters
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t))

;; Show current heading breadcrumb in the header line
(use-package breadcrumb
  :hook (org-mode . breadcrumb-local-mode))

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
  (ob-mermaid-cli-path (cond ((eq system-type 'darwin)   "/opt/homebrew/bin/mmdc")
                             ((eq system-type 'gnu/linux) "/usr/bin/mmdc")
                             (t                           "mmdc"))))

(use-package recentf
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

(use-package dirvish
  ;; dirvish-override-dired-mode must be active before any dired call.
  ;; :commands dired loads dirvish too late — dired is already opening when
  ;; :config runs, so the override misses the first invocation. Hooking to
  ;; emacs-startup loads dirvish eagerly and installs the override in time.
  :after magit
  :custom
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; Three-pane layout: 1 parent dir | current | preview (55% width).
  ;; Format: (depth top-height-ratio preview-width-ratio)
  (dirvish-default-layout '(1 0.11 0.55))
  ;; Cycle through these layouts with M-t (dirvish-layout-toggle)
  (dirvish-layout-switch-recipes
   '((0 0 0)        ;; plain dired — no frills
     (0 0.11 0.55)  ;; preview pane only
     (1 0.11 0.55))) ;; parent + preview (default)
  ;; Preview dispatchers: images, video, audio, archives, PDF, text.
  (dirvish-preview-dispatchers
   (list 'image 'video 'audio 'epub 'pdf-preface 'archive 'tramp 'default))
  (delete-by-moving-to-trash t)
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
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

(dirvish-override-dired-mode)

(use-package project
  :straight nil
  :bind-keymap ("s-p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file" "f")
     (consult-ripgrep "Ripgrep" "r")
     (project-find-dir "Find dir" "d")
     (magit-status "Magit" "g"))))

(use-package treemacs
  :bind (("<f8>"    . treemacs)
         ("C-c t t" . treemacs)
         ("C-c t s" . treemacs-select-window)
         ("C-c t f" . treemacs-find-file)
         ("C-c t p" . treemacs-add-project-to-workspace))
  :custom
  (treemacs-width 35)
  (treemacs-is-never-other-window t)
  (treemacs-show-hidden-files nil)
  (treemacs-follow-recenter-distance 0.1)
  :config
  (treemacs-git-mode 'deferred)          ;; async git status, faster than 'extended
  (treemacs-follow-mode t)               ;; auto-highlight current file in tree
  (treemacs-project-follow-mode t)       ;; auto-switch project in tree
  (treemacs-filewatch-mode t)            ;; auto-refresh on filesystem changes
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-commit-diff-mode t)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)))

;; Sync magit operations (commit, stage, etc.) into the treemacs git indicators
(use-package treemacs-magit
  :after (treemacs magit))

;; Use nerd-icons for file/directory icons (consistent with the rest of the config)
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config (treemacs-load-theme "nerd-icons"))

(use-package editorconfig
  :config (editorconfig-mode 1))

(setq mode-require-final-newline t)

(use-package magit
  :bind ("C-x g" . magit-status))

;; git-gutter-fringe must be loaded first so its fringe bitmaps exist
;; before git-gutter-mode activates in any buffer.
(use-package git-gutter-fringe
  :demand t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [128 192 224 240] nil nil 'bottom))

;; global-git-gutter-mode (rather than a prog-mode hook) handles:
;;   • initial buffer load via find-file-hook
;;   • mode changes from treesit-auto via after-change-major-mode-hook
;;   • indicators disappearing on window/buffer switch via window-configuration-change-hook
(use-package git-gutter
  :after git-gutter-fringe
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g s" . git-gutter:stage-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g d" . git-gutter:popup-hunk))
  :config
  (global-git-gutter-mode t)
  :custom
  (git-gutter:update-interval 0.1))

(use-package magit-todos
  :after magit
  :commands (magit-todos-list magit-todos-mode))

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package forge
  :after magit
  :straight (forge :host github :repo "magit/forge"))

(use-package pinentry
  :commands magit
  :config (pinentry-start)
  :custom (epg-pinentry-mode 'loopback))

(use-package eldoc-box
  :after (eldoc eglot)
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
              ("C-c e x" . eglot-reconnect)
              ("C-c e q" . eglot-shutdown)
              ("C-c e l" . eglot-stderr-buffer))
  :custom
  (eglot-sync-connect nil)
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil :background (color-darken-name (face-background 'highlight) 5)))

;; flycheck-eglot bridges eglot's flymake backend into flycheck so that
;; flycheck is the single diagnostic frontend in prog-mode buffers as well.
;; With flycheck-eglot-exclusive t, the raw flymake backend is disabled
;; inside eglot-managed buffers — only flycheck overlays appear.
(use-package flycheck-eglot
  :after (flycheck eglot)
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  :custom
  (flycheck-eglot-exclusive t))

(use-package eglot-x
  :straight (eglot-x :type git :host github :repo "nemethf/eglot-x")
  :after eglot
  :hook ((eglot-managed-mode . eglot-x-setup)
         (eglot-managed-mode . my/eglot-x-on-enter-hook)
         ;; Inlay hints off by default — toggle with C-c e C
         ;; (eglot-managed-mode . eglot-x-inlay-hints-mode)
         )
  :bind (:map eglot-mode-map
              ("C-c e E" . eglot-x-execute-command)
              ("C-c e S" . eglot-x-workspace-restart)
              ("C-c e C" . eglot-x-inlay-hints-mode)
              ("C-c e D" . eglot-x-show-documentation)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c e" "eglot"))

(when (fboundp 'treesit-parser-create)
  (use-package treesit-auto
    :demand t
    :custom
    (treesit-auto-install 'prompt)
    :config
    ;; Remove bash from the recipe list so global-treesit-auto-mode's
    ;; after-change-major-mode-hook never remaps sh-mode → bash-ts-mode.
    ;; That remapping breaks org-mode src block fontification of zsh/sh
    ;; blocks, which run in temporary buffers where bash-ts-mode fails.
    ;; File extensions are handled separately below via auto-mode-alist.
    (setq treesit-auto-recipe-list
          (cl-remove 'bash treesit-auto-recipe-list
                     :key #'treesit-auto-recipe-lang))
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)
    ;; Re-add bash-ts-mode for actual shell script files via extension,
    ;; since we removed bash from the recipe list above.
    (when (treesit-language-available-p 'bash)
      (add-to-list 'auto-mode-alist '("\\.sh\\'"   . bash-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.zsh\\'"  . bash-ts-mode)))))

(use-package corfu
  :hook (emacs-startup . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Icons in corfu completion popups (requires nerd-icons fonts)
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode  . yas-minor-mode)))

;; Large community snippet collection covering Python, Emacs Lisp, shell, etc.
(use-package yasnippet-snippets
  :after yasnippet)

;; Clojure-specific snippets (defn, ns, let, threading macros, etc.)
(use-package clojure-snippets
  :after (yasnippet clojure-mode))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook ((prog-mode . treesit-fold-mode)
         (prog-mode . treesit-fold-indicators-mode))
  :bind (("C-s-<tab>" . treesit-fold-toggle)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-auto-enabled nil))

(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt)))

(general-define-key :keymaps 'prog-mode-map :prefix "C-c"
  "r" #'apheleia-format-buffer)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load)
         (sh-mode . flymake-mode)
         (sh-mode . highlight-indent-guides-mode))
  :custom (flymake-shellcheck-allow-external-files variable t))

;; clojure-mode covers .clj / .cljs / .cljc / .edn
(use-package clojure-mode
  :hook ((clojure-mode       . paredit-mode)
         (clojurec-mode      . paredit-mode)
         (clojurescript-mode . paredit-mode)))

;; Tree-sitter variant — treesit-auto switches to this automatically
;; when the Clojure grammar is available.
(use-package clojure-ts-mode
  :after clojure-mode)

(use-package cider
  :after eglot
  :hook ((cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file (no-littering-expand-var-file-name "cider-repl-history"))
  (cider-repl-pop-to-buffer-on-connect 'display-only) ;; show REPL, don't steal focus
  (cider-repl-use-pretty-printing t)
  (cider-result-overlay-position 'at-eol)             ;; inline results after expression
  (cider-use-overlays t)
  (cider-show-error-buffer 'except-in-repl)
  (cider-auto-select-error-buffer t)
  (cider-eldoc-display-for-symbol-at-point t)
  (cider-test-show-report-on-success nil)
  :config
  (add-to-list 'eglot-server-programs '(clojure-mode       . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojure-ts-mode    . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojurec-mode      . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojurescript-mode . ("clojure-lsp"))))

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  ;; cljr-add-keybindings-with-prefix installs refactoring commands under C-c C-m
  ;; (this leaves cider-macroexpand-1 unbound, use M-x cider-macroexpand-1 instead)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :hook (clojure-mode . my/clojure-mode-hook))

;; kaocha test runner integration: run tests without leaving Emacs
(use-package kaocha-runner
  :after cider
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(use-package python
  :after eglot
  :hook ((python-mode    . highlight-indent-guides-mode)
         (python-ts-mode . highlight-indent-guides-mode))
  :custom (python-indent-guess-indent-offset-verbose nil)
  :config
  (let ((pyright-server '("pyright-langserver" "--stdio"
                          :initializationOptions
                          (:useLibraryCodeForTypes t
                           :typeCheckingMode "basic"
                           :autoImportCompletions t
                           :diagnosticMode "workspace"
                           :disableOrganizeImports nil))))
    (add-to-list 'eglot-server-programs `(python-mode    . ,pyright-server))
    (add-to-list 'eglot-server-programs `(python-ts-mode . ,pyright-server))))

;; poetry-tracking-mode auto-activates the project virtualenv
(use-package poetry
  :hook ((python-mode    . poetry-tracking-mode)
         (python-ts-mode . poetry-tracking-mode)))

;; elpy: interactive Python development (REPL + rope refactoring).
;; Modules that duplicate eglot/corfu/flycheck are stripped out:
;;   eldoc       → eglot handles eldoc
;;   flymake     → flycheck-eglot handles diagnostics
;;   highlight-indentation → highlight-indent-guides handles it
;;   yasnippet   → already loaded globally
(use-package elpy
  :hook ((python-mode    . elpy-mode)
         (python-ts-mode . elpy-mode))
  :bind (:map elpy-mode-map
              ("C-c C-c" . elpy-shell-send-region-or-buffer)
              ("C-c C-z" . elpy-shell-switch-to-shell)
              ("C-c C-d" . elpy-doc)
              ("C-c C-r e" . elpy-refactor-extract-method)
              ("C-c C-r v" . elpy-refactor-extract-variable)
              ("C-c C-r i" . elpy-refactor-inline-variable)
              ("C-c C-r r" . elpy-refactor-rename))
  :custom
  (elpy-rpc-python-command "python3")
  :config
  (setq elpy-modules
        (cl-set-difference elpy-modules
                           '(elpy-module-eldoc
                             elpy-module-flymake
                             elpy-module-highlight-indentation
                             elpy-module-yasnippet)))
  (elpy-enable))

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio"))))

(use-package auctex
  :hook ((LaTeX-mode . eglot-ensure)
         (LaTeX-mode . git-gutter-mode)
         (LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . highlight-indent-guides-mode))
  :custom ((TeX-engine 'xetex)
           (TeX-PDF-mode t)))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :custom ((cdlatex-math-modify-prefix ?\C-c)))

(use-package rust-mode
  :after eglot
  :hook (rust-mode . highlight-indent-guides-mode)
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

(use-package dockerfile-mode
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerignore\\'" . dockerfile-mode)))

(use-package docker
  :bind ("C-c d" . docker)
  :config
  (setf docker-command "podman"
        docker-compose-command "podman-compose"
        docker-container-tramp-method "podman"))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-hook 'yaml-ts-mode-hook 'highlight-indent-guides-mode)

(setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
(defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  "Custom options that will be merged with any default settings."
  '(:settings (:java
               (:format
                (:settings
                 (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
                 :enabled t)))))

(use-package eglot-java
  :hook ((java-ts-mode . eglot-java-mode)
         (java-ts-mode . highlight-indent-guides-mode))
  :custom
  ;; JVM args — enables Lombok annotation processing.
  (eglot-java-eclipse-jdt-args
   '("--add-modules=ALL-SYSTEM"
     "--add-opens" "java.base/java.util=ALL-UNNAMED"
     "--add-opens" "java.base/java.lang=ALL-UNNAMED"))
  ;; eglot-java-lombok-jar-path is machine-specific — set it in local.el.
  ;; See local.el.example for the recommended snippet.
  :config
  ;; Register JDTLS with java-debug bundle for DAP support via dape.
  ;; The bundle path below is Arch Linux specific; override in local.el on other systems.
  (add-to-list 'eglot-server-programs
               '(java-ts-mode .
                 ("jdtls"
                  :initializationOptions
                  (:bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]))))
  :bind (:map java-ts-mode-map
              ("C-c j n" . eglot-java-file-new)
              ("C-c j x" . eglot-java-run-main)
              ("C-c j t" . eglot-java-run-test)
              ("C-c j N" . eglot-java-project-new)))

(use-package dape
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints when the first debug session starts (not at init,
  ;; to avoid the ~0.5 s startup cost of eagerly requiring dape).
  (dape-start . dape-breakpoint-load)

  :custom
  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)

  :config
  ;; Enable global mouse bindings for setting breakpoints.
  (dape-breakpoint-global-mode +1)

  ;; Save buffers before starting a session.
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success.
  (add-hook 'dape-compile-hook #'kill-buffer)

  ;; Java debug session via JDTLS — invoke with M-x dape, select java-attach
  ;; or configure a named config in dape-configs for your project.
  :bind (:map java-ts-mode-map
              ("C-c j d" . dape)
              ("C-c j b" . dape-breakpoint-toggle)
              ("C-c j c" . dape-continue)
              ("C-c j i" . dape-step-in)
              ("C-c j o" . dape-step-out)
              ("C-c j s" . dape-next)
              ("C-c j e" . dape-evaluate-expression)))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :hook (after-init . repeat-mode))

;; Left and right side windows occupy full frame height
(use-package emacs
  :custom
  (window-sides-vertical t))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$"   . jinx-correct)    ; correct word at point
         ("C-M-$" . jinx-languages)) ; change active languages
  :custom
  ;; Space-separated enchant locale codes checked simultaneously.
  ;; Latin uses aspell-la; Chinese has no hunspell dict (handled below).
  (jinx-languages "en_GB fr_FR pl_PL es_ES la"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((text-mode org-mode markdown-mode LaTeX-mode)
                 . ("ltex-ls")))
  ;; Workspace-level LTeX settings (also overridable in .dir-locals.el):
  ;;   (setq-default eglot-workspace-configuration
  ;;     '((:ltex . (:language "auto"
  ;;                 :additionalRules (:motherTongue "fr")
  ;;                 :diagnosticSeverity "information"))))
  )

;; global-flycheck-mode activates flycheck in every buffer that has a
;; suitable checker.  In prog-mode buffers flycheck-eglot takes over
;; (see the LSP section).  In prose buffers flycheck-grammalecte
;; provides French grammar checks.
(use-package flycheck
  :hook (emacs-startup . global-flycheck-mode)
  :bind (("C-c ! n" . flycheck-next-error)
         ("C-c ! p" . flycheck-previous-error)
         ("C-c ! l" . flycheck-list-errors)
         ("C-c ! v" . flycheck-verify-setup))
  :config
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c !" "flycheck")))

(use-package flycheck-grammalecte
  :after flycheck
  :custom
  ;; Spelling is handled by jinx; disable grammalecte's spell pass.
  (flycheck-grammalecte-report-spellcheck nil)
  (flycheck-grammalecte-report-grammar    t)
  (flycheck-grammalecte-report-apos       t)
  :config
  (grammalecte-setup))

;; Pinyin input method for Simplified and Traditional Chinese.
;; Activate/deactivate with C-\, or set as the session default below.
(use-package pyim
  :custom
  (pyim-default-scheme 'quanpin)  ; full pinyin; alternatives: 'wubi, 'cangjie
  (default-input-method "pyim")
  :config
  ;; pyim-basedict: a minimal Pinyin dictionary (~100 k entries).
  ;; For a richer experience install pyim-greatdict separately.
  (use-package pyim-basedict
    :config (pyim-basedict-enable)))

;; Insert correct whitespace between CJK and Latin/ASCII characters.
;; "Hello世界" becomes "Hello 世界" on save.
(use-package pangu-spacing
  :hook ((text-mode . pangu-spacing-mode)
         (org-mode  . pangu-spacing-mode))
  :custom
  ;; Write real spaces to the file rather than display-only overlays
  (pangu-spacing-real-insert-separtor t))

;; Extend the font fallback chain so Han characters render with Noto.
;; Traditional Chinese (zh-TW) is listed first; the first matching font
;; for each codepoint wins.
(set-fontset-font t 'han (font-spec :family "Noto Sans CJK TC") nil 'append)
(set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC") nil 'append)

(add-hook 'window-setup-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(use-package devdocs)

(use-package restclient)

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
           (eat-term-scrollback-size (* 1024 1024 1024))
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
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . centered-cursor-mode)
         (nov-mode . my/garamond-font)))

(use-package ready-player
  :config (ready-player-mode +1)
  :custom ((ready-player-my-media-collection-location "~/Music/")))

(use-package time-zones)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  :custom
  ((claude-code-ide-terminal-backend 'eat)))

(use-package mu4e
  :straight nil
  :load-path (lambda ()
               (cond ((eq system-type 'darwin)
                      '("/opt/homebrew/share/emacs/site-lisp/mu/mu4e"))
                     ((eq system-type 'gnu/linux)
                      '("/usr/share/emacs/site-lisp/mu4e"))))
  :commands (mu4e mu4e-compose-new)
  :bind ("C-c M" . mu4e)
  :custom
  ;; ── Sync ──────────────────────────────────────────────────────────
  (mu4e-maildir          "~/Mail")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval  300)            ; background sync every 5 min
  ;; ── Threads: include own replies for context ─────────────────────
  (mu4e-headers-include-related t)
  (mu4e-headers-skip-duplicates t)
  ;; ── View ──────────────────────────────────────────────────────────
  (mu4e-view-show-images   t)
  (mu4e-view-image-max-width 800)
  (shr-use-colors nil)                   ; avoid garish HTML colours
  ;; ── Compose ───────────────────────────────────────────────────────
  (mu4e-compose-format-flowed      nil)  ; hard-wrap for consistency
  (mu4e-compose-context-policy     'pick-first)
  (mu4e-compose-reply-include-address t)
  (mu4e-compose-dont-reply-to-self t)
  ;; ── Headers columns ───────────────────────────────────────────────
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-headers-fields '((:human-date    . 12)
                          (:flags         .  6)
                          (:from          . 22)
                          (:thread-subject . nil)))
  :config
  ;; HTML rendering via Emacs built-in shr — no external dependencies
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; ── Zero-inbox bookmarks (named search queries) ───────────────────
  ;; Each bookmark is a virtual folder: a saved mu query language expression.
  ;; Press `b' in the headers view then the key to jump to a bookmark.
  (setq mu4e-bookmarks
        '((:name "Inbox (unprocessed)"
           :query "maildir:/*/Inbox AND NOT flag:trashed"
           :key ?i)
          (:name "Unread"
           :query "flag:unread AND NOT flag:trashed AND NOT maildir:/*/Trash"
           :key ?u)
          (:name "Flagged"
           :query "flag:flagged AND NOT flag:trashed"
           :key ?f)
          (:name "Today"
           :query "date:today..now AND NOT flag:trashed"
           :key ?t)
          (:name "Last 7 days"
           :query "date:7d..now AND NOT flag:trashed"
           :key ?w)
          (:name "Sent"
           :query "maildir:/*/Sent"
           :key ?s)))

  ;; ── Automatic classification rules (refile = tagging in mu4e) ────
  ;; mu4e-refile-folder is called for each message when you press `r'.
  ;; Maildirs act as tags; nesting like /Work/ProjectA is fully supported.
  ;; Safe default: archive everything to /Archive.
  ;; Override mu4e-refile-folder in local.el for your personal maildir layout.
  (setq mu4e-refile-folder "/Archive")

  ;; ── Sending via SMTP (STARTTLS) ───────────────────────────────────
  ;; smtpmail-smtp-server is machine-specific — set it in local.el.
  ;; Credentials are read from ~/.authinfo.gpg — never stored in plain text.
  (setq send-mail-function    'smtpmail-send-it
        smtpmail-smtp-port    587
        smtpmail-stream-type  'starttls
        message-kill-buffer-on-exit t))

(use-package org-mime
  :after (mu4e org)
  :hook (mu4e-compose-mode . org-mime-edit-mail-in-org-mode)
  :custom
  (org-mime-default-header "#+OPTIONS: toc:nil\n")
  (org-mime-export-options '(:with-toc nil :section-numbers nil :with-author nil))
  :bind (:map org-mode-map
              ("C-c M o" . org-mime-org-buffer-htmlize)
              ("C-c M r" . org-mime-htmlize)))

(use-package mu4e-alert
  :after mu4e
  :hook (emacs-startup . (lambda ()
                           (mu4e-alert-set-default-style
                            (if (eq system-type 'darwin) 'notifier 'libnotify))
                           (mu4e-alert-enable-notifications)
                           (mu4e-alert-enable-mode-line-display))))

(use-package mu4e-dashboard
  :straight (:type git :host github :repo "rougier/mu4e-dashboard")
  :after mu4e
  :custom
  (mu4e-dashboard-file (expand-file-name "mu4e-dashboard.org"
                                          user-emacs-directory))
  :config
  (mu4e-dashboard-mode 1))

(use-package esup
  ;; Sometimes an error happens. It is fixed with:
  :custom (esup-depth 0))

;; BDD testing framework — loaded on demand by tests/run.sh
(use-package buttercup)

;; Load machine-local overrides — gitignored, never committed.
(let ((f (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p f)
    (load f nil :nomessage)))

(provide 'init)
;;; init.el ends here
