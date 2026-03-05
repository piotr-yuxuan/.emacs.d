;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Emacs configuration generated from =init.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   literate documentation file.

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

(straight-use-package
 '(use-package
    :type git
    :host github
    :repo "emacs-straight/use-package"
    :branch "master"))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "autosaves/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el")
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backups/"))))
  ;; Ensure target directories exist — auto-save and backup fail silently
  ;; if the directory is missing, especially on a fresh install.
  (make-directory (no-littering-expand-var-file-name "autosaves/") t)
  (make-directory (no-littering-expand-var-file-name "backups/") t)
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package benchmark-init
  ;; Only activate when explicitly profiling — benchmark-init wraps every
  ;; `require' call with timing advice, which itself adds latency to each
  ;; require during normal startup.  Set the EMACS_PROFILE env variable to
  ;; opt in: EMACS_PROFILE=1 emacs
  :if (getenv "EMACS_PROFILE")
  :demand t
  :init (benchmark-init/activate)
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

;; Defer minor-mode activations to emacs-startup-hook so they don't
;; slow down the critical init path.  All eight are cheap once Emacs
;; is up, but calling them at top-level adds latency before the frame
;; is shown for the first time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (delete-selection-mode t)
            (global-hl-line-mode)
            (column-number-mode)
            (savehist-mode)
            (save-place-mode 1)
            (global-auto-revert-mode)
            (when (fboundp 'pixel-scroll-precision-mode)
              (pixel-scroll-precision-mode 1))
            (global-so-long-mode 1)
            (message "Emacs ready in %.3f s with %d GC%s."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done
                     (if (= gcs-done 1) "" "s"))))

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

(use-package crux
  :config (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(setq jit-lock-defer-time 0)
;; Stealth-fontify the rest of the buffer during idle time, reducing
;; the chance of a pause when you jump to an unfontified region.
(setq jit-lock-stealth-time 0.2
      jit-lock-stealth-nice 0.1)
;; Auto vertical scroll computes margins on every scroll event — disable it.
(setq auto-window-vscroll nil)

(use-package which-key
  :hook (emacs-startup . which-key-mode))

(setq custom-safe-themes t)

(use-package color-theme-sanityinc-tomorrow
  :init
  ;; Pre-load only the night theme; auto-dark loads the day theme via its
  ;; light-mode hook when the OS switches.  Loading both eagerly here adds
  ;; ~100 ms with no benefit since only one is active at startup.
  (load-theme 'sanityinc-tomorrow-night t nil))

(defun my/auto-dark ()
  "Enable auto-dark-mode and set theme hooks."
  (auto-dark-mode t))

(use-package auto-dark
  :config
  (setq auto-dark-dark-theme 'sanityinc-tomorrow-night
        auto-dark-light-theme 'sanityinc-tomorrow-day)

  (add-hook 'auto-dark-dark-mode-hook
            (lambda () (load-theme 'sanityinc-tomorrow-night t nil)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda () (load-theme 'sanityinc-tomorrow-day t nil)))

  :init
  ;; Defer auto-dark activation to an idle timer so it doesn't add
  ;; latency before the first frame is shown.  0.5 s is enough for
  ;; the OS dark-mode query (dbus on Linux, AppleScript on macOS) to
  ;; complete without blocking the user.
  (run-with-idle-timer 0.5 nil #'my/auto-dark))

(set-face-attribute 'default nil
                    :family (cond ((eq system-type 'darwin) "Iosevka Term")
                                  ((eq system-type 'gnu/linux) "Iosevka Term")
                                  (t "Fira Code"))
                    :weight 'regular
                    :height 180
                    :width 'normal)

(use-package fringe-scale
  :straight (fringe-scale :type git :host github :repo "blahgeek/emacs-fringe-scale"))

;; Defer fringe setup to emacs-startup-hook: these are cosmetic display
;; operations that do not need to run during init file evaluation.
;; fringe-scale-setup rescales every fringe bitmap to match the 16 px
;; fringe width; wrapping it in inhibit-message silences the per-bitmap
;; "Scaling fringe bitmap …" noise.
(add-hook 'emacs-startup-hook
          (lambda ()
            (set-fringe-mode 16)
            (let ((inhibit-message t))
              (fringe-scale-setup))))

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

(add-hook 'emacs-startup-hook (lambda () (winner-mode 1)))

(defun my/other-window-reverse ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(keymap-set global-map "C-x O" #'my/other-window-reverse)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package exec-path-from-shell
  :config
  (customize-set-variable
   'exec-path-from-shell-arguments
   (remove "-i" exec-path-from-shell-arguments))
  :hook (emacs-startup . (lambda ()
                           ;; `pgtk' covers Wayland GUI Emacs.
                           (when (memq window-system '(mac ns x pgtk))
                             (exec-path-from-shell-initialize))
                           (when (daemonp)
                             (exec-path-from-shell-initialize)))))

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
  :hook ((org-mode . auto-fill-mode))
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-cycle-global-at-bob t)
  ;; org-modules defaults to a large set that loads many rarely-used
  ;; libraries at startup.  Keep only the ones actually used here.
  (org-modules '(ol-doi ol-info))
  (org-export-in-background t)
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
  ;; Activate recentf lazily on the very first interactive command rather
  ;; than at startup.  Loading recentf reads var/recentf from disk; there is
  ;; no reason to pay that cost before the user presses a key.  Using
  ;; `pre-command-hook' (rather than a :before advice on `find-file') ensures
  ;; that commands like `consult-recent-file' also see recentf as active on
  ;; their first invocation.  The hook removes itself after one call so there
  ;; is zero per-command overhead afterwards.
  (defun my/recentf-enable-once ()
    "Enable `recentf-mode' before the first interactive command, then remove self."
    (remove-hook 'pre-command-hook #'my/recentf-enable-once)
    (recentf-mode 1))
  (add-hook 'pre-command-hook #'my/recentf-enable-once)
  :custom
  (recentf-max-saved-items 200))

;; straight's transient shadows Emacs 30's weak built-in transient.
;; dirvish-subtree calls transient--set-layout at load time, a function
;; missing from the Emacs 30.2 built-in.  Loading transient eagerly here
;; costs < 10 ms and decouples dirvish's startup dependency from magit.
(use-package transient
  :demand t)

(use-package dirvish
  ;; :after transient ensures straight's newer transient is loaded and its
  ;; symbols (transient--set-layout, etc.) are available before dirvish-subtree
  ;; is configured.  Magit no longer needs :demand t just for this.
  :after transient
  :custom
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; Sidebar-specific: only nerd-icons, subtree-state, vc-state.
  ;; collapse is intentionally absent: it is a flat-listing feature that
  ;; misfires when subtrees are expanded, rendering intermediate dirs as
  ;; redundant collapsed entries (a›b›c then b›c then c).
  ;; git-msg (commit shortlog) and file-size are hidden to save width;
  ;; toggle any attribute on the fly with M-s → Attributes.
  (dirvish-side-attributes '(nerd-icons subtree-state vc-state))
  ;; Directories first, no . or .. entries (-A), long format required by dired.
  (dired-listing-switches "-l -A --group-directories-first")
  ;; Three-pane layout: 1 parent dir | current | preview (55% width).
  ;; Format: (depth top-height-ratio preview-width-ratio)
  (dirvish-default-layout '(1 0.11 0.55))
  ;; Cycle through these layouts with M-t (dirvish-layout-toggle)
  (dirvish-layout-switch-recipes
   '((0 0 0)         ;; plain dired — no frills
     (0 0.11 0.55)   ;; preview pane only
     (1 0.11 0.55))) ;; parent + preview (default)
  ;; dirvish-preview-dispatchers is left at its built-in default
  ;; (video image gif audio epub archive font pdf).
  ;; 'default, 'tramp, 'pdf-preface are not valid symbols and cause errors.
  (delete-by-moving-to-trash t)
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  (dirvish-emerge-groups
   '(("Directories" (predicate . directories))
     ("JVM"         (extensions "java" "kt" "clj" "cljs" "cljc"))
     ("Web"         (extensions "ts" "tsx" "js" "jsx" "css" "html"))
     ("Data"        (extensions "json" "yaml" "yml" "edn" "sql"))
     ("Docs"        (extensions "org" "md" "rst" "txt"))
     ("Config"      (extensions "toml" "ini" "properties" "conf"))
     ("Tests"       (regex ".*[Tt]est.*"))
     ("Build"       (extensions "xml" "gradle" "nix" "lock"))))
  :config
  (dirvish-override-dired-mode) ;; override dired globally before any C-x d
  (dirvish-side-follow-mode) ;; sidebar auto-highlights the active file
  ;; Use a nerd-icons right-chevron as the path collapse separator
  ;; (single-child directories are joined as  foo›bar›Baz.java).
  ;; Set here in :config so nerd-icons is guaranteed to be loaded.
  (setq dirvish-collapse-separator
        (concat (nerd-icons-codicon "nf-cod-chevron_right") " "))
  ;; Expand the directory at point and all its nested sub-directories.
  ;; Guard with when-let: dired-get-filename returns nil on blank/header lines.
  (defun my/dirvish-subtree-expand-all-children ()
    "Expand directory at point and every nested sub-directory within it."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (when (file-directory-p file)
        (unless (dirvish-subtree--expanded-p)
          (dirvish-subtree-toggle))
        (save-excursion
          (forward-line 1)
          (while (and (not (eobp)) (dirvish-subtree--parent))
            (when-let ((f (dired-get-filename nil t)))
              (when (and (file-directory-p f)
                         (not (dirvish-subtree--expanded-p)))
                (dirvish-subtree-toggle)))
            (forward-line 1))))))
  ;; Collapse every expanded subtree in the buffer.
  ;; copy-sequence snapshots the overlay list so toggling doesn't corrupt iteration.
  (defun my/dirvish-subtree-collapse-all ()
    "Collapse every expanded subtree in the buffer."
    (interactive)
    (save-excursion
      (dolist (ov (copy-sequence dirvish-subtree--overlays))
        (when (overlay-buffer ov)
          (goto-char (max (point-min) (1- (overlay-start ov))))
          (beginning-of-line)
          (dirvish-subtree-toggle)))))
  ;; S-TAB: collapse if directory at point is expanded; otherwise expand it and
  ;; all its descendants via my/dirvish-subtree-expand-all-children.
  (defun my/dirvish-subtree-toggle-all ()
    "Collapse directory at point if expanded; otherwise expand it and all children."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (when (file-directory-p file)
        (if (dirvish-subtree--expanded-p)
            (dirvish-subtree-toggle)
          (my/dirvish-subtree-expand-all-children)))))
  ;; Bind dirvish-mode-map keys here in :config so dirvish-mode-map is
  ;; guaranteed to exist.  Global bindings stay in :bind below.
  (define-key dirvish-mode-map (kbd "a")         #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f")         #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y")         #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N")         #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "b")         #'dired-up-directory) ;; easier than ^ (same command)
  (define-key dirvish-mode-map (kbd "h")         #'dired-omit-mode) ;; toggle dotfiles
  (define-key dirvish-mode-map (kbd "H")         #'dirvish-history-jump) ;; history list
  (define-key dirvish-mode-map (kbd "s")         #'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v")         #'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd ">")         #'dirvish-side-increase-width)
  (define-key dirvish-mode-map (kbd "<")         #'dirvish-side-decrease-width)
  ;; TAB: toggle the subtree under point.
  ;; S-TAB: on a directory, collapse if open; expand recursively if closed.
  (define-key dirvish-mode-map (kbd "<tab>")     #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "<backtab>") #'my/dirvish-subtree-toggle-all)
  (define-key dirvish-mode-map (kbd "M-f")       #'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b")       #'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l")       #'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m")       #'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t")       #'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-s")       #'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e")       #'dirvish-emerge-menu)
  (define-key dirvish-mode-map (kbd "M-j")       #'dirvish-fd-jump)
  :bind
  (("C-c f" . dirvish-fd)
   ("<f8>"  . dirvish-side)))

(use-package project
  :straight nil
  :bind-keymap ("s-p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file" "f")
     (consult-ripgrep "Ripgrep" "r")
     (project-find-dir "Find dir" "d")
     (magit-status "Magit" "g"))))

(use-package imenu-list
  ;; imenu-list activates hs-minor-mode on its buffer hook.
  ;; RET → imenu-list-ret-dwim: jump if leaf; fold/unfold if parent.
  ;; (TAB → forward-button from button-map; not overridable without patching.)
  :bind ("<f9>" . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize t)
  (imenu-list-focus-after-activation t)
  (imenu-list-size 0.25))

(use-package breadcrumb
  ;; Activate only in eglot-managed buffers — eglot 1.14+ attaches
  ;; breadcrumb-region metadata to every imenu node for precise paths.
  ;; doom-modeline already shows project/file, so show only imenu crumbs.
  :hook (eglot-managed-mode . my/breadcrumb-setup)
  :config
  (defun my/breadcrumb-setup ()
    "Show nerd-icon + imenu breadcrumb in header line for eglot-managed buffers."
    (setq-local header-line-format
                '((:eval
                   (when-let* ((crumbs (breadcrumb-imenu-crumbs)))
                     (concat (nerd-icons-icon-for-buffer) " " crumbs)))))))

(use-package editorconfig
  :init
  ;; Defer editorconfig activation to an idle timer — it parses .editorconfig
  ;; on each file open, but activating the mode itself has overhead at startup.
  (run-with-idle-timer 0.3 nil #'editorconfig-mode))

(setq mode-require-final-newline t)

(use-package magit
  :bind ("C-x g" . magit-status))

;; git-gutter-fringe must be loaded before git-gutter-mode activates its
;; first buffer — otherwise git-gutter silently falls back to character
;; display.  :after git-gutter :demand t loads git-gutter-fringe eagerly the
;; moment git-gutter itself is first required (which is lazy — triggered by
;; the first prog/conf/text buffer hook), guaranteeing the fringe bitmaps and
;; side configuration are in place before any buffer's mode hook fires.
;; treesit-fold-indicators occupies the left fringe; put git-gutter on the
;; right so the two don't overwrite each other.
;; [255] = 11111111 fills the full fringe width → solid colour rectangle.
(use-package git-gutter-fringe
  :after git-gutter
  :demand t
  :config
  (setq git-gutter-fr:side 'right-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added    [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [255 255 255 255 255 255 255 255] nil nil 'bottom))

;; Enable git-gutter in every programming-language, config-file, and
;; text/org buffer.  Use-package defers loading until the first hook
;; fires, so there is no startup overhead.
(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (conf-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g s" . git-gutter:stage-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g d" . git-gutter:popup-hunk))
  :custom (git-gutter:update-interval 0.02))

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
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background (color-darken-name (face-background 'highlight) 5)
                      :underline t))

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
  ;; treesit-auto is kept for on-demand grammar installation only
  ;; (M-x treesit-auto-install-all).  It does NOT need to be loaded at
  ;; startup: major-mode-remap-alist is populated directly below via dolist,
  ;; and auto-install prompting is handled lazily when the package first loads.
  (use-package treesit-auto
    :config
    (setq treesit-auto-install 'prompt))

  ;; Remap only languages we actually use via major-mode-remap-alist.
  ;; This intercepts after auto-mode-alist selects a mode, with zero
  ;; per-file overhead (no treesit-language-available-p calls at file open).
  ;; bash is excluded: global-treesit-auto-mode remapping breaks org-mode's
  ;; temporary sh-mode buffers used for src-block fontification.
  (dolist (pair '((python-mode     . python-ts-mode)
                  (java-mode       . java-ts-mode)
                  (js-json-mode    . json-ts-mode)
                  (css-mode        . css-ts-mode)
                  (html-mode       . html-ts-mode)
                  (mhtml-mode      . html-ts-mode)
                  (dockerfile-mode . dockerfile-ts-mode)))
    (add-to-list 'major-mode-remap-alist pair))

  ;; Modes without a non-ts entry in default auto-mode-alist.
  (dolist (entry '(("\\.ya?ml\\'"                                              . yaml-ts-mode)
                   ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
                   ("\\.sh\\'"   . bash-ts-mode)
                   ("\\.bash\\'" . bash-ts-mode)
                   ("\\.zsh\\'"  . bash-ts-mode)))
    (add-to-list 'auto-mode-alist entry)))

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
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

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
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))
  ;; Java is intentionally excluded: JDTLS drives formatting via
  ;; textDocument/willSaveWaitUntil; adding google-java-format as a second
  ;; pass re-indents the entire file on every save regardless of project
  ;; style.  Use C-c e f (eglot-format) for explicit on-demand formatting.
  (setf apheleia-mode-alist
        (assoc-delete-all 'java-mode apheleia-mode-alist))
  (setf apheleia-mode-alist
        (assoc-delete-all 'java-ts-mode apheleia-mode-alist)))

(keymap-set prog-mode-map "C-c r" #'apheleia-format-buffer)

(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :custom
  ;; Require at least 4 indented lines before adapting, to avoid
  ;; mis-inference on short or generated files.
  (dtrt-indent-min-relevant-lines 4)
  ;; Do not scan beyond 5000 lines in very large files.
  (dtrt-indent-max-lines 5000)
  ;; Suppress the "indentation NN columns" message in the echo area.
  (dtrt-indent-verbosity 0))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load)
         (sh-mode . flymake-mode)
         (sh-mode . highlight-indent-guides-mode))
  :custom (flymake-shellcheck-allow-external-files t))

;; clojure-mode covers .clj / .cljs / .cljc / .edn
(use-package clojure-mode
  :hook ((clojure-mode       . paredit-mode)
         (clojurec-mode      . paredit-mode)
         (clojurescript-mode . paredit-mode))
  :config
  ;; Register clojure-lsp here so it is available whenever clojure-mode
  ;; loads — independent of whether CIDER is loaded.
  (add-to-list 'eglot-server-programs '(clojure-mode       . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojure-ts-mode    . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojurec-mode      . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs '(clojurescript-mode . ("clojure-lsp"))))

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
  (cider-test-show-report-on-success nil))

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

;; yaml auto-mode-alist entries are handled by the treesit-auto block
;; (via the dolist above) — no duplicates needed here.
(add-hook 'yaml-ts-mode-hook 'highlight-indent-guides-mode)

(setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
(defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  "Custom options merged with default JDTLS settings. Key settings:
  bundles:                                                  java-debug plugin (Arch: /usr/share/java-debug/)
  java.format.settings.url:                                 Google style XML (java.format.enabled t)
  java.maven.downloadSources / java.gradle.downloadSources: fetch -sources.jar
  java.signatureHelp.enabled:                               parameter hints on method calls
  java.contentProvider.preferred:                           decompiler fallback (fernflower)
  java.import.gradle.enabled / java.autobuild.enabled:      explicit defaults"
  '(
    :bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]
    :settings (:java
               (:format
                (
                 :settings (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
                 :enabled t))
               :maven (:downloadSources t)
               :gradle (:downloadSources t)
               :signatureHelp (:enabled t)
               :contentProvider (:preferred "fernflower")
               :import (:gradle (:enabled t))
               :autobuild (:enabled t))))

(use-package eglot-java
  :hook ((java-mode . eglot-java-mode)
         (java-ts-mode . eglot-java-mode)
         (java-mode . highlight-indent-guides-mode)
         (java-ts-mode . highlight-indent-guides-mode))
  :custom
  ;; Default 2-space indentation (JDTLS / Google Java Style Guide default).
  ;; dtrt-indent will override this for files with a different native style.
  (java-ts-mode-indent-offset 2)
  ;; JVM args — enables Lombok annotation processing.
  (eglot-java-eclipse-jdt-args
   '("--add-modules=ALL-SYSTEM"
     "--add-opens" "java.base/java.util=ALL-UNNAMED"
     "--add-opens" "java.base/java.lang=ALL-UNNAMED"))
  ;; eglot-java-lombok-jar-path is machine-specific — set it in local.el.
  ;; See local.el.example for the recommended snippet.
  ;;
  ;; Prevent eglot-java from managing eglot-server-programs automatically.
  ;; Its internal use of `mapcan' (which calls `nconc', a destructive
  ;; concatenation) mutates the car lists of matching alist entries each time
  ;; eglot-java-mode activates.  With our `(java-mode java-ts-mode)' list key,
  ;; repeated activations eventually make the list circular, causing:
  ;;   eglot--lookup-mode: List contains a loop: (java-mode java-ts-mode …)
  ;; Setting this to t skips the entire mapcan block; our add-to-list below
  ;; becomes the sole authoritative registration.
  (eglot-java-eglot-server-programs-manual-updates t)
  :config
  ;; 2-space indent for java-mode (cc-mode uses c-basic-offset).
  ;; dtrt-indent will override buffer-locally for existing files.
  (add-hook 'java-mode-hook (lambda () (setq-local c-basic-offset 2)))
  ;; Register JDTLS using the eglot-java-eclipse-jdt server class with the
  ;; system `jdtls' binary.  A plain string contact ("jdtls" ...) would create
  ;; a generic eglot-lsp-server instead.  Without the typed class:
  ;;   • eglot-java--find-server returns nil (it only finds eglot-java-eclipse-jdt)
  ;;   • eglot-java--jdt-uri-handler can't request :java/classFileContents
  ;;   • M-. on library classes (ArrayList, List, …) fails with
  ;;       xref--not-found-error: No definitions found for: LSP identifier at point
  ;; Initialization options (including :bundles) come from
  ;; eglot-initialization-options dispatched on eglot-java-eclipse-jdt, which
  ;; in turn calls `custom-eglot-java-init-opts' via eglot-java-user-init-opts-fn.
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) eglot-java-eclipse-jdt "jdtls"))
  (with-eval-after-load 'cc-mode
    (define-key java-mode-map (kbd "C-c j n") #'eglot-java-file-new)
    (define-key java-mode-map (kbd "C-c j x") #'eglot-java-run-main)
    (define-key java-mode-map (kbd "C-c j t") #'eglot-java-run-test)
    (define-key java-mode-map (kbd "C-c j N") #'eglot-java-project-new))
  (with-eval-after-load 'java-ts-mode
    (define-key java-ts-mode-map (kbd "C-c j n") #'eglot-java-file-new)
    (define-key java-ts-mode-map (kbd "C-c j x") #'eglot-java-run-main)
    (define-key java-ts-mode-map (kbd "C-c j t") #'eglot-java-run-test)
    (define-key java-ts-mode-map (kbd "C-c j N") #'eglot-java-project-new)))

(use-package dape
  ;; Only load when first invoked — avoids the 0.5 s eager-require cost.
  :commands dape
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
  ;; Save buffers before starting a session.
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success.
  (add-hook 'dape-compile-hook #'kill-buffer)

  ;; Java debug session via JDTLS — invoke with M-x dape, select java-attach
  ;; or configure a named config in dape-configs for your project.
  (with-eval-after-load 'cc-mode
    (define-key java-mode-map (kbd "C-c j d") #'dape)
    (define-key java-mode-map (kbd "C-c j b") #'dape-breakpoint-toggle)
    (define-key java-mode-map (kbd "C-c j c") #'dape-continue)
    (define-key java-mode-map (kbd "C-c j i") #'dape-step-in)
    (define-key java-mode-map (kbd "C-c j o") #'dape-step-out)
    (define-key java-mode-map (kbd "C-c j s") #'dape-next)
    (define-key java-mode-map (kbd "C-c j e") #'dape-evaluate-expression))
  (with-eval-after-load 'java-ts-mode
    (define-key java-ts-mode-map (kbd "C-c j d") #'dape)
    (define-key java-ts-mode-map (kbd "C-c j b") #'dape-breakpoint-toggle)
    (define-key java-ts-mode-map (kbd "C-c j c") #'dape-continue)
    (define-key java-ts-mode-map (kbd "C-c j i") #'dape-step-in)
    (define-key java-ts-mode-map (kbd "C-c j o") #'dape-step-out)
    (define-key java-ts-mode-map (kbd "C-c j s") #'dape-next)
    (define-key java-ts-mode-map (kbd "C-c j e") #'dape-evaluate-expression)))

;; Enable global mouse bindings for breakpoints once dape is loaded —
;; kept outside :config so it does not prevent dape from being deferred.
(with-eval-after-load 'dape
  (dape-breakpoint-global-mode +1))

(use-package emacs
  :custom
  ;; Left and right side windows occupy full frame height.
  (window-sides-vertical t)
  ;; Always resolve symbolic links when visiting files so that buffer-file-name
  ;; contains the canonical real path.  Without this, following an LSP
  ;; cross-reference can produce two buffers for the same physical file — one
  ;; with the symlinked path, one with the real path — and lsp-mode treats them
  ;; as different workspaces, triggering a duplicate JDTLS launch.
  (find-file-visit-truename t))

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
  :custom
  ;; Raise the error threshold for large generated files (e.g. minified
  ;; JS or huge Python modules) so flycheck doesn't give up early.
  (flycheck-checker-error-threshold 10000)
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
;; Deferred to emacs-startup-hook: fontset modifications are display-only
;; and safe to apply after after-init-time is recorded.  Traditional
;; Chinese (zh-TW) is listed first; the first matching font per codepoint wins.
(add-hook 'emacs-startup-hook
          (lambda ()
            (set-fontset-font t 'han (font-spec :family "Noto Sans CJK TC") nil 'append)
            (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC") nil 'append)))

;; Defer server-start to an idle timer so it doesn't add latency before
;; the first frame is shown.  1 s is enough for all startup hooks to
;; complete; the server is ready before the user can type a command.
(run-with-idle-timer 1 nil
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
