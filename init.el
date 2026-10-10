;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Emacs configuration generated from =README.org=.
;;;
;;;   You can modify this file as you wish so, but keep in mind in the
;;;   long term it's better to keep a clean generation from the
;;;   literate documentation file.

;;; Code:

(defconst my/macos-p (eq system-type 'darwin)
  "Non-nil when Emacs runs on macOS.")

(defconst my/linux-p (eq system-type 'gnu/linux)
  "Non-nil when Emacs runs on GNU/Linux.")

(defgroup my nil
  "Personal settings of this Emacs configuration."
  :group 'convenience)

(defconst my/line-width 120
  "Preferred line width for both hard wrapping (fill-column) and visual wrapping.")

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

(dolist (package '(use-package which-key editorconfig))
  (when (locate-library (symbol-name package))
    (add-to-list 'straight-built-in-pseudo-packages package)))

(straight-use-package
 (if (memq 'use-package straight-built-in-pseudo-packages)
     'use-package
   '(use-package
      :type git
      :host github
      :repo "emacs-straight/use-package"
      :branch "master")))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(defvar my/straight-safe-update-boundaries
  ;; `straight-pull-all' returns only the merge failures.
  (list :pull (lambda () (delete-dups (append (straight-fetch-all) (straight-merge-all))))
        :rebuild #'straight-check-all
        :test (lambda (runner done)
                ;; Run RUNNER fast asynchronously; call DONE with its exit
                ;; status.  Signal when it cannot start.
                (let ((default-directory user-emacs-directory)
                      (process-environment
                       (if my/macos-p
                           (cons "EMACS_TEST_GUI=0" process-environment)
                         process-environment))
                      (buffer (get-buffer-create "*Package update tests*")))
                  (with-current-buffer buffer (erase-buffer))
                  (display-buffer buffer)
                  (make-process
                   :name "package-update-tests"
                   :buffer buffer
                   :command (list runner "fast")
                   :sentinel (lambda (process _event)
                               (unless (process-live-p process)
                                 (funcall done (process-exit-status process)))))))
        :freeze #'straight-freeze-versions
        :thaw #'straight-thaw-versions)
  "The effects of `my/straight-safe-update', as a plist of functions.
:pull pulls every package and returns the recipes it could not pull;
:rebuild rebuilds the changed packages; :test takes the test runner
and a function it calls with the exit status of `RUNNER fast'; :freeze
writes the lockfile; :thaw checks out the lockfile versions and returns
the recipes it could not check out.")

(defun my/straight-safe-update ()
  "Update every package, keeping the update only if the fast tests pass.
Pull every package and rebuild the changed ones, then run `tests/run
fast'.  When the tests pass, write the new versions to the lockfile;
when a pull fails, the pull or rebuild signals or is quit, or the tests
fail or cannot start, restore the lockfile versions and rebuild."
  (interactive)
  (let* ((effect (lambda (key &rest arguments)
                   (apply (plist-get my/straight-safe-update-boundaries key) arguments)))
         (packages (lambda (recipes)
                     (mapconcat (lambda (recipe) (plist-get recipe :package)) recipes ", ")))
         (runner (expand-file-name "tests/run" user-emacs-directory))
         (restore (lambda (reason)
                    (let ((unthawed (funcall effect :thaw)))
                      (funcall effect :rebuild)
                      (if unthawed
                          (message "Package update undone: %s; %s not checked out, run M-x straight-thaw-versions"
                                   reason (funcall packages unthawed))
                        (message "Package update undone: %s; the lockfile versions are back"
                                 reason))))))
    (unless (file-executable-p runner)
      (user-error "Package update not started: %s is not an executable file" runner))
    (when (process-live-p (get-process "package-update-tests"))
      (user-error "Package update not started: the tests of the previous one still run"))
    (when-let* ((reason
                 (condition-case err
                     (if-let* ((unpulled (funcall effect :pull)))
                         (format "%s not pulled" (funcall packages unpulled))
                       (funcall effect :rebuild)
                       ;; Freeze and thaw run from the sentinel, where Emacs
                       ;; inhibits C-g: a half-done thaw is worse than a wait.
                       (funcall effect :test runner
                                (lambda (status)
                                  (if (not (eql status 0))
                                      (funcall restore (format "the tests failed (exit %s)" status))
                                    (message "Package update: the tests passed, freezing the lockfile")
                                    (funcall effect :freeze))))
                       nil)
                   ((error quit)
                    (format "the update stopped (%s)" (error-message-string err))))))
      (funcall restore reason))))

(keymap-global-set "C-c P u" #'my/straight-safe-update)

(use-package buttercup)

(use-package benchmark-init
  ;; Only activate when explicitly profiling — benchmark-init wraps every
  ;; `require' call with timing advice, which itself adds latency to each
  ;; require during normal startup.  Set the EMACS_PROFILE env variable to
  ;; opt in: EMACS_PROFILE=1 emacs
  :if (getenv "EMACS_PROFILE")
  :demand t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

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

(setq ;; Double-spaces after periods is morally wrong.
      sentence-end-double-space nil
      ;; Save existing clipboard text into the kill ring before replacing it.
      save-interprogram-paste-before-kill t
      ;; Prompts should go in the minibuffer, not in a GUI.
      use-dialog-box nil
      ;; accept 'y' or 'n' instead of yes/no
      use-short-answers t
      ;; compilation buffers (cargo, pytest-watch, ...) follow new output
      compilation-scroll-output t
      ;; C-\ toggles the Pinyin input method, see the Chinese section
      default-input-method "pyim"
      ;; if native-comp is having trouble, there's not very much I can do
      native-comp-async-report-warnings-errors 'silent
      ;; unicode ellipses are better
      truncate-string-ellipsis "…"
      ;; keep the point in the same place while scrolling
      scroll-preserve-screen-position t
      ;; don't keep duplicate entries in kill ring
      kill-do-not-save-duplicates t)

;; M-q and auto-fill wrap at my/line-width columns in every buffer.
;; fill-column is buffer-local: setq would change only the buffer
;; current while init runs.
(setq-default fill-column my/line-width)

;; Standard region selection keys.
;; Note: On macOS, System Settings can steal C-SPC for input source switching.
(keymap-global-set "C-SPC" #'set-mark-command)
(keymap-global-set "C-@" #'set-mark-command)
(transient-mark-mode 1)

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
;; Disable hl-line in every terminal / shell mode so the
;; highlight bar does not interfere with the cursor line.
(defun my/disable-hl-line ()
  "Disable `hl-line-mode' in the current buffer."
  (hl-line-mode -1))
(dolist (hook '(mistty-mode-hook
              gterm-mode-hook
              term-mode-hook
              shell-mode-hook
              eshell-mode-hook))
  (add-hook hook #'my/disable-hl-line))

(defun my/enable-global-modes ()
  "Turn on the global editing modes, once startup has finished."
  (delete-selection-mode t)
  (global-hl-line-mode)
  (column-number-mode)
  (savehist-mode)
  (save-place-mode 1)
  (setq auto-revert-avoid-polling t   ; rely on file notifications (kqueue on macOS)
        auto-revert-check-vc-info nil) ; VC info polling is expensive; magit handles it
  (global-auto-revert-mode)
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (global-so-long-mode 1))

(add-hook 'emacs-startup-hook #'my/enable-global-modes)

(use-package eval-sexp-fu
  :hook (prog-mode . turn-on-eval-sexp-fu-flash-mode))

(setq visible-bell t)
;; mouse-wheel-tilt-scroll enables horizontal scrolling via trackpad
;; tilt/swipe.  On macOS this causes the viewport to drift sideways on
;; long lines (e.g. single-line src blocks) during vertical scrolling,
;; because the trackpad reports diagonal movement.  Keep it off;
;; hscroll is still available via C-x < / C-x >.
(setq mouse-wheel-tilt-scroll nil)

(when my/macos-p
  (setq mac-function-modifier 'super)
  ;; Sometimes we need # for comments
  (setq mac-right-option-modifier 'none))

(when my/linux-p
  ;; TODO: I'd like to ignore the left control to preserve my hand.
  (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier))

;; Wayland clipboard for terminal frames (emacs -nw, emacsclient -t).
;; Graphical PGTK frames have the native Wayland clipboard.  One daemon
;; serves both kinds of frame, so the selected frame decides at each
;; copy and paste, not the frame, if any, that exists at startup.
(when (and my/linux-p
           (getenv "WAYLAND_DISPLAY"))
  (defun my/wl-copy (text)
    "Copy TEXT to the clipboard: with wl-copy from a terminal frame."
    (if (display-graphic-p)
        (gui-select-text text)
      (let ((proc (make-process :name "wl-copy"
                                :buffer nil
                                :command '("wl-copy" "-f" "-n")
                                :connection-type 'pipe)))
        (process-send-string proc text)
        (process-send-eof proc))))
  (defun my/wl-paste ()
    "Return the clipboard text: with wl-paste from a terminal frame."
    (if (display-graphic-p)
        (gui-selection-value)
      (shell-command-to-string "wl-paste --no-newline")))
  (setq interprogram-cut-function   #'my/wl-copy
        interprogram-paste-function #'my/wl-paste))

(use-package crux
  :bind ([remap move-beginning-of-line] . crux-move-beginning-of-line))

;; Defer fontification while input is pending (jit-lock-defer-time 0).
;; See the detailed explanation above.  This is the single most
;; effective interactive-latency improvement: it prevents font-lock,
;; pangu-spacing, and other jit-lock clients from blocking keystrokes.
(setq jit-lock-defer-time 0)
;; Stealth-fontify the rest of the buffer during idle time, reducing
;; the chance of a pause when you jump to an unfontified region.
(setq jit-lock-stealth-time 0.2
      jit-lock-stealth-nice 0.1)
;; When scrolling rapidly past unfontified regions, only fontify the
;; portion actually displayed — skip intermediate screens entirely.
(setq fast-but-imprecise-scrolling t)
;; Skip fontification_functions when input is already pending.  This
;; prevents the first window-scroll during held-down arrow keys from
;; blocking: without it, the C redisplay engine calls jit-lock on the
;; entire newly visible region synchronously before reading the next
;; input event.  With it, that fontification is skipped when Emacs is
;; behind on input, letting the scroll catch up first.
(setq redisplay-skip-fontification-on-input t)
;; Auto vertical scroll computes margins on every scroll event — disable it.
(setq auto-window-vscroll nil)

(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.5)
  (which-key-max-display-columns 2)
  (which-key-show-docstrings t)
  (which-key-max-description-length 0.42)
  :config
  (which-key-add-key-based-replacements
    "C-c d"   "Debug"
    "C-c k"   "Run tests"
    "C-c C"   "Cargo"
    "C-c F"   "File tree"
    "C-c g"   "Change hunks"
    "C-c n"   "Nix"
    "C-c P"   "Manage Emacs packages"
    "C-c s"   "Search files"
    "C-c v"   "View"
    "C-c z"   "Fold code"
    "C-c &"   "Snippets"
    "C-x a"   "Abbreviations"
    "C-x n"   "Narrow view"
    "C-x p"   "Project"
    "C-x r"   "Registers and rectangles"
    "C-x t"   "Tabs"
    "C-x w"   "Window layout"
    "C-x x"   "Buffer actions"
    "C-x 8"   "Insert Unicode"
    "C-x RET" "Encoding"
    "C-x C-a" "Debug"
    "C-x 4"   "Other window"
    "C-x 5"   "Other frame"
    "C-x C-k" "Keyboard macros"
    "C-x v"   "Version control")
  ;; which-key matches a major mode exactly: python-mode runs without
  ;; the python grammar.
  (dolist (mode '(python-mode python-ts-mode))
    (which-key-add-major-mode-key-based-replacements mode
      "C-c C-t" "Insert Python templates"
      "C-c TAB" "Manage Python imports")))

(setq custom-safe-themes t)

(use-package color-theme-sanityinc-tomorrow
  :init
  ;; Pre-load only the night theme, for the frames shown before auto-dark
  ;; starts; auto-dark loads the day theme once it starts, from
  ;; `auto-dark-themes'.  Loading both eagerly here adds ~100 ms with no
  ;; benefit since only one is active at startup.
  (load-theme 'sanityinc-tomorrow-night t nil))

(defun my/auto-dark ()
  "Enable auto-dark-mode and set theme hooks."
  (auto-dark-mode t))

(use-package auto-dark
  :custom
  ;; The themes of the dark appearance, then those of the light one.
  ;; auto-dark enables exactly one list when the appearance changes.
  (auto-dark-themes '((sanityinc-tomorrow-night) (sanityinc-tomorrow-day)))
  :hook (;; Defer auto-dark activation to an idle timer so it
         ;; doesn't add latency before the first frame is shown. 0.5 s is
         ;; enough for the OS dark-mode query (dbus on Linux, AppleScript on
         ;; macOS) to complete without blocking the user.
         (emacs-startup . my/auto-dark)))

(defun my/first-installed-font (families)
  "Return the first installed font family in FAMILIES.
FAMILIES is a list of font family names (strings)."
  (catch 'found
    (dolist (family families)
      (when (and (stringp family)
                 (find-font (font-spec :family family)))
        (throw 'found family)))
    nil))

(defun my/apply-default-font (&optional frame)
  "Apply a sane default monospaced font if available on FRAME.
FRAME defaults to the selected frame."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (when-let ((family (my/first-installed-font
                          (cond
                           (my/macos-p
                            '("Iosevka Nerd Font Mono" "Iosevka Term" "Iosevka Term NFM" "Iosevka" "JetBrains Mono" "Menlo"))
                           (t
                            '("Iosevka Nerd Font Mono" "Iosevka Term" "Iosevka" "JetBrains Mono" "DejaVu Sans Mono"))))))
        (set-face-attribute 'default nil
                            :family family
                            :weight 'regular
                            :height 140
                            :width 'normal)))))

;; Apply fonts for the initial frame and any later GUI frames.
(my/apply-default-font)

(add-hook 'after-make-frame-functions #'my/apply-default-font)

(use-package fringe
  :straight nil
  :demand t
  :config
  (fringe-mode 8))

;; If you use `emacs-mac' Mac port then ligatures are handled for you.
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)

  (defconst my/known-ligatures
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
    (ligature-set-ligatures 'prog-mode my/known-ligatures)

    (ligature-set-ligatures 'text-mode my/known-ligatures)
    (ligature-set-ligatures 'special-mode my/known-ligatures)
    ;; Enables ligature checks globally in all buffers. You can also
    ;; do it per mode with `ligature-mode'.
    :hook (emacs-startup . global-ligature-mode)))

(defun my/italic-comments (&rest _)
  "Show comments in a light italic, whichever theme is enabled."
  (set-face-attribute 'font-lock-comment-face nil
                      :weight 'light
                      :slant 'italic))

(add-hook 'enable-theme-functions #'my/italic-comments)
(add-hook 'disable-theme-functions #'my/italic-comments)

;; Braille patterns (U+2800–U+28FF): prepend Iosevka Nerd Font Mono so it stays
;; authoritative.  Iosevka covers Braille natively; the explicit entry prevents
;; any other font from grabbing these codepoints (e.g. a fallback serif font).
(defun my/prefer-iosevka-for-braille ()
  "Draw Braille patterns with Iosevka Nerd Font Mono first."
  (set-fontset-font t '(#x2800 . #x28ff)
                    (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend))

(add-hook 'emacs-startup-hook #'my/prefer-iosevka-for-braille)

;; Vertical completion UI
(use-package vertico
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15))

;; Orderless matching: space-separated patterns
(use-package orderless
  ;; Completion styles must register before the first in-buffer popup.
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))
     ;; lsp-mode installs a buffer-local passthrough default. An explicit
     ;; category override keeps substring and out-of-order matching available
     ;; in Corfu, while the language server still supplies the candidates.
     (lsp-capf (styles orderless)))))

;; Rich annotations in minibuffer
(use-package marginalia
  :hook (emacs-startup . marginalia-mode))

;; Kind icons in the minibuffer (consult-lsp-file-symbols,
;; consult-imenu, M-x, file pickers, …).  Mirrors what
;; `nerd-icons-corfu' does in the in-buffer popup: same source of truth
;; (`marginalia' annotations + LSP `SymbolKind'), same nerd-icons
;; glyphs.  Requires fonts from `M-x nerd-icons-install-fonts'.
(use-package nerd-icons-completion
  ;; Turned on and off with marginalia-mode, whichever loads first.
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Enhanced search/navigation commands
(use-package consult
  :bind (("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)         ;; buffers + recent files + bookmarks
         ("C-x B"   . consult-project-buffer) ;; project-scoped buffer list
         ("C-x f"   . consult-recent-file)
         ("C-x k"   . kill-buffer)
         ("C-s"     . consult-line)
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

(use-package nerd-icons)

(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  ;; Icons in every frame, a daemon's client frames included: a daemon
  ;; starts without a graphical frame, so a test of the frame when this
  ;; loads would turn them off.  The icons need a Nerd Font.
  (doom-modeline-icon t)
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
  ;; Show LSP connection status (lsp-mode).
  (doom-modeline-lsp t)
  ;; Use built-in project.el for project name.
  (doom-modeline-project-detection 'project)
  ;; Show language environment version (e.g. Python 3.12, Node 20).
  (doom-modeline-env-version t)
  ;; Don't show the clock — the system bar already has it.
  (doom-modeline-time nil))

(use-package minions
  :hook (window-setup . minions-mode))

(use-package svg-clock)

(defun my/initial-buffer ()
  "Return the buffer a new frame shows first: the dashboard, built for it.
A daemon's initial frame, which no one sees, gets *scratch* instead."
  (if (and (daemonp) (eq (selected-frame) terminal-frame))
      (get-scratch-buffer-create)
    (dashboard-open)))

(use-package dashboard
  :demand t
  :custom
  (initial-buffer-choice #'my/initial-buffer)
  (dashboard-items nil)
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "Welcome to Emacs")
  (dashboard-center-content t)
  :config
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100))

(add-hook 'emacs-startup-hook #'winner-mode)

(defun my/other-window-reverse ()
  "Switch to the previous window."
  (interactive)
  (other-window -1))

(keymap-set global-map "C-x O" #'my/other-window-reverse)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-ignored-buffers '("*Calc Trail*" " *LV*" "*Ilist*" "*symbols-outline*")))

(defun my/symbol-outline-toggle ()
  "Toggle the source-symbol tree in the current frame.
Use LSP Treemacs for managed buffers, or Imenu otherwise.
The tree contains source symbols, not runtime variable values."
  (interactive)
  (if-let* ((window (get-buffer-window "*LSP Symbols List*" (selected-frame))))
      (delete-window window)
    (if (bound-and-true-p lsp-managed-mode)
        (lsp-treemacs-symbols)
      (imenu-list-smart-toggle))))

(global-set-key (kbd "<f8>") #'treemacs)
(global-set-key (kbd "<f9>") #'my/symbol-outline-toggle)

(use-package treemacs
  :bind (("C-c F F" . treemacs)
         ("C-c F s" . treemacs-select-window)
         ("C-c F 1" . treemacs-delete-other-windows)
         ("C-c F d" . treemacs-select-directory)
         ("C-c F b" . treemacs-bookmark)
         ("C-c F f" . treemacs-find-file)
         ("C-c F t" . treemacs-find-tag))
  :config
  ;; Show a chain of directories that each hold one directory as one
  ;; node, as IntelliJ shows a Java package: five levels cover
  ;; src/main/java/com/example/app.  Treemacs sets 3 when it loads, so
  ;; :custom, which runs before, would be overwritten.
  (setq treemacs-collapse-dirs 5)
  ;; Select the current file in the tree, and show the project of the
  ;; current buffer.  Treemacs turns the first on when it loads.
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1))

;; Refresh the tree's git colours after magit changes the repository.
;; :demand loads it once both treemacs and magit have loaded.
(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)

(use-package exec-path-from-shell
  :demand t
  :config
  (customize-set-variable
   'exec-path-from-shell-arguments
   (remove "-i" exec-path-from-shell-arguments))
  ;; A terminal Emacs keeps the PATH of the shell that started it.
  (when (or (memq window-system '(mac ns x pgtk)) (daemonp))
    (exec-path-from-shell-initialize)))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package markdown-toc)

(use-package edit-indirect)

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines))
  :custom ((mc/always-run-for-all t)))

(use-package paredit
  :straight (paredit :host github :repo "emacsmirror/paredit")
  :hook ((emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
         ("C-s-s" . paredit-splice-sexp)
         ("C-s-<up>" . paredit-splice-sexp-killing-backward)
         ("C-s-<down>" . paredit-splice-sexp-killing-forward)
         ("C-s-<left>" . paredit-backward-slurp-sexp) ;; Also ESC C-<left>
         ("C-s-<right>" . paredit-backward-barf-sexp) ;; Also ESC C-<right>
         ("RET" . nil))
  :config
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  ;; (put 'paredit-newline 'delete-selection t)
  )

(use-package rainbow-delimiters
  :defer 1 ;; load before org buffers render so depth faces are available for heading inheritance
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package vundo
  :bind ([remap undo] . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package expreg
  ;; C-- stays the negative argument.
  :bind (("C-=" . expreg-expand)
         ("C-M-=" . expreg-contract)))

(global-set-key (kbd "s-SPC") 'cycle-spacing)

(use-package writeroom-mode)

(use-package visual-fill-column
  :custom
  ((visual-fill-column-center-text 1)
   (visual-fill-column-width my/line-width)))

(defalias 'zsh-mode 'shell-script-mode)

(defun my/org-follow-codex-link (path _argument)
  "Open the Codex link at PATH through the standard URL dispatcher."
  (browse-url (concat "codex:" path)))

(use-package org
  :hook ((org-mode . auto-fill-mode))
  :custom
  ;; Enable standard Shift+arrow region selection in Org buffers.
  (org-support-shift-select t)
  (org-src-preserve-indentation nil)
  (org-src-content-indentation 0)
  (org-cycle-global-at-bob t)
  ;; org-modules defaults to a large set that loads many rarely-used
  ;; libraries at startup.  Keep only the ones actually used here.
  (org-modules '(ol-doi ol-info))
  (org-export-in-background t)
  ;; Emphasis / entities
  (org-hide-emphasis-markers t)   ;; hidden by default, shown by org-appear
  (org-pretty-entities nil)
  (org-ellipsis " ▾")
  ;; Tags and headings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'show)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  :custom-face
  ;; Inherit foreground from rainbow-delimiters so heading colours and
  ;; parenthesis depth colours form a single, consistent palette.
  (org-level-1 ((t (:inherit rainbow-delimiters-depth-1-face :height 1.35 :weight bold))))
  (org-level-2 ((t (:inherit rainbow-delimiters-depth-2-face :height 1.2  :weight bold))))
  (org-level-3 ((t (:inherit rainbow-delimiters-depth-3-face :height 1.1  :weight semi-bold))))
  (org-level-4 ((t (:inherit rainbow-delimiters-depth-4-face :height 1.05 :weight semi-bold))))
  (org-level-5 ((t (:inherit rainbow-delimiters-depth-5-face :height 1.0  :weight normal))))
  (org-level-6 ((t (:inherit rainbow-delimiters-depth-6-face :height 1.0  :weight normal))))
  (org-level-7 ((t (:inherit rainbow-delimiters-depth-7-face :height 1.0  :weight normal))))
  (org-level-8 ((t (:inherit rainbow-delimiters-depth-8-face :height 1.0  :weight normal))))
  :config
  ;; So I can execute shell code from org-mode babel.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (mermaid . t)))
  (org-link-set-parameters "codex" :follow #'my/org-follow-codex-link))

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
  (org-modern-keyword "‣ "))

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

;; svg-tag-mode renders SVG images inside font-lock, enables
;; cursor-sensor-mode (post-command-hook), and advises org-fontify on
;; every fontification cycle.  That is too expensive to run by default
;; in every org and prog buffer.  Keep it available as M-x svg-tag-mode
;; for the rare buffers that benefit from it.
(use-package svg-tag-mode
  :commands svg-tag-mode
  :config
  (setq svg-tag-tags '((":TODO:" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
                       (":FIXME:" . ((lambda (tag) (svg-tag-make "FIXME"  :face 'org-todo :inverse t :margin 0)))))))

(require 'seq)

(defcustom my/gtd-file (expand-file-name "~/org/todo.org")
  "Path to the personal task and journal file."
  :type 'file
  :group 'my)

(defcustom my/org-agenda-source-files
  (list (expand-file-name "todo.org" user-emacs-directory))
  "Canonical Org task files after `my/gtd-file', in portfolio display order.
The default is this configuration's own task file; `local.el' lists
the repositories of the machine."
  :type '(repeat file)
  :group 'my)

(defcustom my/org-source-label-alist
  '(("org" . "Personal")
    (".emacs.d" . "Emacs"))
  "Optional display aliases keyed by repository directory name."
  :type '(alist :key-type string :value-type string)
  :group 'my)

(defun my/org-set-agenda-files ()
  "Set `org-agenda-files' to the existing canonical task files.
They are `my/gtd-file', then `my/org-agenda-source-files', as
absolute paths."
  (setq org-agenda-files
        (seq-filter #'file-exists-p
                    (delete-dups (mapcar #'expand-file-name
                                         (cons my/gtd-file my/org-agenda-source-files))))))

(defun my/org-source-label (file)
  "Return the repository-derived display label for FILE."
  (let* ((directory (file-name-directory (file-truename file)))
         (root (or (locate-dominating-file directory ".git") directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (or (cdr (assoc name my/org-source-label-alist)) name)))

(defun my/org-agenda-file-p (file)
  "Return non-nil when FILE is one of `org-agenda-files'."
  (and file
       (seq-some (lambda (candidate)
                   (and (file-exists-p candidate)
                        (file-equal-p file candidate)))
                 org-agenda-files)))

(defun my/org-set-source-category ()
  "Set the current agenda file's category from its repository name."
  (when (my/org-agenda-file-p buffer-file-name)
    (setq-local org-category (my/org-source-label buffer-file-name))
    (org-refresh-category-properties)))

(add-hook 'after-init-hook #'my/org-set-agenda-files)
(add-hook 'org-mode-hook #'my/org-set-source-category)

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)"
                    "|" "DONE(d!)" "CANCELLED(c@)"))
        org-log-into-drawer t
        org-log-done 'time
        org-archive-location "%s_archive::"))

(defun my/gtd-today-heading ()
  "Position point on today's `** <date>' heading inside `* Journal'.
Create the journal anchor and today's subheading when missing.  Used
by the journal capture template with a `file+function' target.

NOTE: do not wrap the body in `org-with-wide-buffer' — that macro
restores point on exit, which defeats the whole point of positioning
it for the caller."
  ;; `file+function' targets already enter the file's buffer before
  ;; calling us; the `set-buffer' here is for direct callers (tests,
  ;; manual invocations).
  (set-buffer (find-file-noselect my/gtd-file))
  (widen)
  (goto-char (point-min))
  (unless (re-search-forward "^\\* Journal\\s-*$" nil t)
    ;; No anchor yet. Create `* Journal' at end of file with a blank
    ;; line above (if there's any content) and one below.
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (unless (or (bobp) (save-excursion (forward-line -1) (looking-at-p "^\\s-*$")))
      (insert "\n"))
    (insert "* Journal\n\n"))
  (org-back-to-heading t)
  (let* ((journal-position (point))
         (journal-end (save-excursion (org-end-of-subtree t t) (point)))
         (today (format-time-string "<%Y-%m-%d %a>"))
         (heading (concat "** " today)))
    (if (re-search-forward
         (concat "^" (regexp-quote heading) "\\s-*$") journal-end t)
        ;; Existing heading — re-search-forward leaves point at end of
        ;; match (which on a `^...$' regexp is the start of the *next*
        ;; line); use match-beginning to land back on the heading itself.
        (goto-char (match-beginning 0))
      ;; Missing. Append at the end of `* Journal' with one blank line
      ;; before and after, then land on the new heading line.
      (goto-char journal-position)
      (org-end-of-subtree t t)
      ;; Ensure exactly one blank line between previous content and the
      ;; new heading.
      (unless (bolp) (insert "\n"))
      (unless (save-excursion (forward-line -1) (looking-at-p "^\\s-*$"))
        (insert "\n"))
      (insert heading "\n\n")
      (forward-line -2))))

(with-eval-after-load 'org-capture
  (setq org-default-notes-file my/gtd-file)
  (setq org-capture-templates
        `(("t" "Quick task" entry
           (file+headline ,my/gtd-file "Inbox")
           "* TODO %?\n%a"
           :empty-lines 1)
          ("n" "Journal note" entry
           (file+function ,my/gtd-file my/gtd-today-heading)
           "* %?\n/Entered on/ %U\n"
           :empty-lines 1))))

(defun my/org-agenda-skip-non-task ()
  "Skip the current Agenda entry when its heading has no task state."
  (unless (org-get-todo-state)
    (save-excursion
      (or (outline-next-heading) (point-max)))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-prefix-format
        '((agenda . "  %-16:c%?-12t% s %b")
          (todo . "  %-16:c %b")
          (search . "  %-16:c %b")
          (tags . "  %-16:c %b"))
        org-agenda-custom-commands
        '(("g" "Daily task overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Today")
                        (org-agenda-skip-function #'my/org-agenda-skip-non-task)
                        (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next actions")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on others")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "Closed today"))))))))

(with-eval-after-load 'org-refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

(defun my/gtd-save-buffers (&rest _)
  "Save buffers visiting `org-agenda-files' (used as :after advice on `org-refile')."
  (save-some-buffers t (lambda ()
                         (my/org-agenda-file-p buffer-file-name))))
(advice-add 'org-refile :after #'my/gtd-save-buffers)

(defun my/org-capture-quick-task ()
  "Capture an unscheduled task in the personal inbox."
  (interactive)
  (org-capture nil "t"))

(define-key global-map (kbd "C-c c") #'org-capture)
(define-key global-map (kbd "C-c a") #'org-agenda)
(define-key global-map (kbd "C-c i") #'my/org-capture-quick-task)

(defcustom my/org-dashboard-directory
  (expand-file-name "org-dashboard/" user-emacs-directory)
  "Directory containing the local task dashboard assets."
  :type 'directory
  :group 'my)

(defcustom my/org-dashboard-data-file
  (expand-file-name "tmp/org-dashboard/tasks.json" user-emacs-directory)
  "Generated task data read by the local dashboard."
  :type 'file
  :group 'my)

(defcustom my/org-dashboard-port 8765
  "Loopback port used by the local task dashboard."
  :type 'integer
  :group 'my)

(defconst my/org-dashboard-active-states '("TODO" "NEXT" "WAIT")
  "Task states exported to the local dashboard.")

(defvar my/org-dashboard-process nil
  "Process serving the local task dashboard.")

(defun my/org-dashboard--entry-timestamp ()
  "Return the first active timestamp in the current entry body.
Planning and closing timestamps are returned through their dedicated
fields instead."
  (save-excursion
    (let ((end (org-entry-end-position))
          found)
      (beginning-of-line)
      (while (and (not found)
                  (re-search-forward org-ts-regexp-both end t))
        (let ((value (match-string-no-properties 0)))
          (unless (or (string-prefix-p "[" value)
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (beginning-of-line)
                        (looking-at-p
                         "[[:space:]]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):")))
            (setq found value))))
      found)))

(defun my/org-dashboard--collect-tasks (generation)
  "Return active task records for export GENERATION.
Generation-scoped identifiers make stale browser cards fail closed."
  (require 'org)
  (let ((counter 0)
        tasks)
    (dolist (file org-agenda-files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((state (org-get-todo-state)))
              (when (and (member state my/org-dashboard-active-states)
                         (not (org-in-archived-heading-p))
                         (not (org-in-commented-heading-p)))
                (setq counter (1+ counter))
                (push `((id . ,(format "%s-%d" generation counter))
                        (title . ,(org-get-heading t t t t))
                        (state . ,state)
                        (source . ,(my/org-source-label file))
                        (file . ,(file-truename file))
                        (line . ,(line-number-at-pos))
                        (breadcrumbs . ,(vconcat (org-get-outline-path)))
                        (scheduled . ,(or (org-entry-get nil "SCHEDULED") :null))
                        (deadline . ,(or (org-entry-get nil "DEADLINE") :null))
                        (timestamp . ,(or (my/org-dashboard--entry-timestamp) :null)))
                      tasks))))
          nil 'file))))
    (nreverse tasks)))

(defun my/org-dashboard-export ()
  "Atomically export active Org tasks for the local dashboard.
Return `my/org-dashboard-data-file'."
  (interactive)
  (let* ((target (expand-file-name my/org-dashboard-data-file))
         (directory (file-name-directory target))
         (generation
          (substring
           (secure-hash 'sha256
                        (format "%s:%s:%s" (current-time) (emacs-pid) (random)))
           0 16))
         (payload `((schemaVersion . 1)
                    (generation . ,generation)
                    (generatedAt . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                    (tasks . ,(vconcat (my/org-dashboard--collect-tasks generation)))))
         temporary)
    (make-directory directory t)
    (setq temporary (make-temp-file (expand-file-name ".tasks-" directory)))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (insert (json-serialize payload)))
          (rename-file temporary target t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))
    (when (called-interactively-p 'interactive)
      (message "Exported Org tasks to %s" target))
    target))

(defun my/org-dashboard--start-server ()
  "Start the loopback dashboard server unless it is already running."
  (unless (process-live-p my/org-dashboard-process)
    (let* ((python (or (executable-find "python3")
                       (user-error "The Org dashboard requires python3")))
           (script (expand-file-name "server.py" my/org-dashboard-directory)))
      (unless (file-readable-p script)
        (user-error "Dashboard server not found: %s" script))
      (setq my/org-dashboard-process
            (make-process
             :name "org-dashboard"
             :buffer "*Org dashboard*"
             :command (list python script
                            "--data" (expand-file-name my/org-dashboard-data-file)
                            "--port" (number-to-string my/org-dashboard-port))
             :connection-type 'pipe
             :noquery t))))
  my/org-dashboard-process)

(defun my/org-dashboard-open ()
  "Export current tasks and open the local dashboard in a browser."
  (interactive)
  (my/org-dashboard-export)
  (let ((already-running (process-live-p my/org-dashboard-process))
        (url (format "http://127.0.0.1:%d/" my/org-dashboard-port)))
    (my/org-dashboard--start-server)
    (if already-running
        (browse-url url)
      (run-at-time 0.3 nil #'browse-url url))))

(use-package ob-mermaid
  :custom
  ;; mmdc is installed cross-platform by home/base.nix of the Nix
  ;; configuration.  It lives in the Home Manager profile
  ;; (~/.nix-profile/bin or /etc/profiles/...), so let exec-path
  ;; resolve it instead of hardcoding absolute paths.
  (ob-mermaid-cli-path "mmdc"))

(defcustom my/vellum-directory nil
  "Directory of the vellum LaTeX class and its exporter, or nil.
Set it in `local.el' on a machine where vellum is installed."
  :type '(choice (const :tag "Not installed" nil) directory)
  :group 'my)

(with-eval-after-load 'ox-latex
  (when my/vellum-directory
    (add-to-list 'load-path (expand-file-name my/vellum-directory))
    (require 'vellum)))

(put 'scroll-left 'disabled nil)

(defun my/table-view-org-rows ()
  "Return the rows of the Org table at point, or nil when there is none.
A row is `hline' for a rule, or for a row of width and alignment
cookies such as <20> or <>, which Org export hides too.  Any other row is
\(MARKER . CELLS): a marker at the start of its line, and its cells
as `org-table-to-lisp' reads them, without text properties."
  (when (org-at-table-p)
    (save-excursion
      (goto-char (org-table-begin))
      ;; `org-table-to-lisp' reads one row per line from the beginning.
      (mapcar (lambda (row)
                (prog1 (if (or (eq row 'hline)
                               (and (seq-every-p (lambda (cell)
                                                   (string-match-p
                                                    "\\`\\(?:<[lrc]?[0-9]*>\\)?\\'" cell))
                                                 row)
                                    (seq-some (lambda (cell) (string-prefix-p "<" cell))
                                              row)))
                           'hline
                         (cons (copy-marker (point)) (mapcar #'substring-no-properties row)))
                  (forward-line)))
              (org-table-to-lisp)))))

(defun my/table-view-markdown-rows ()
  "Return the rows of the Markdown pipe table at point, or nil.
The table is the run of lines that start with a pipe, after any
indentation.  A row is `hline' for the alignment row, such as |:--|--:|,
which is the second line: dashes on any other line are data.
Any other row is (MARKER . CELLS): a marker at the start of its line,
and its cells, trimmed.  A pipe after a backslash belongs to its cell,
without the backslash; the pipe that ends a line is optional."
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "[ \t]*|")
      ;; The table starts after the closest line above that is not a
      ;; table line: a blank line, or one starting with another character.
      (goto-char (if (re-search-backward "^[ \t]*[^| \t]" nil t) (pos-bol 2) (point-min)))
      (let ((rows nil))
        (while (looking-at "[ \t]*|")
          (let ((start (match-end 0))
                (cells nil))
            (goto-char start)
            (while (search-forward "|" (pos-eol) t)
              (unless (eq (char-before (1- (point))) ?\\)
                (push (buffer-substring-no-properties start (1- (point))) cells)
                (setq start (point))))
            (push (buffer-substring-no-properties start (pos-eol)) cells)
            (setq cells (mapcar (lambda (cell) (string-replace "\\|" "|" (string-trim cell)))
                                (nreverse (if (string-blank-p (car cells)) (cdr cells) cells))))
            (push (if (and cells
                           (length= rows 1)
                           (seq-every-p (lambda (cell) (string-match-p "\\`:?-+:?\\'" cell))
                                        cells))
                      'hline
                    (cons (copy-marker (pos-bol)) cells))
                  rows))
          (forward-line))
        (nreverse rows)))))

(defcustom my/table-view-min-width 10
  "Narrowest width, in columns, to which the table view fits a column.
A table wider than its window even with its columns this narrow
scrolls sideways."
  :type 'natural
  :group 'my)

(defvar-local my/table-view--natural-widths nil
  "Natural width of each column of the table view: its widest cell or label.")

(defun my/table-view-widths ()
  "Return the widths of the columns of the table view, each at least 2.
Below 2 columns a wide character could not fit, and cells and borders
would no longer line up."
  (mapcar (lambda (column) (max 2 (cadr column))) tabulated-list-format))

(defun my/table-view-wrap (text width)
  "Return the lines of TEXT broken between words, at most WIDTH columns wide.
WIDTH is at least 2, the width of the widest character.  Runs of blanks
become one space.  A word wider than WIDTH is cut into pieces of WIDTH
columns.  An empty TEXT gives one empty line."
  (let ((lines nil)
        (line nil))
    (dolist (word (split-string text "[ \t]+" t))
      (if (and line (<= (+ (string-width line) 1 (string-width word)) width))
          (setq line (concat line " " word))
        (when line
          (push line lines))
        (while (> (string-width word) width)
          (let ((head (truncate-string-to-width word width)))
            (push head lines)
            (setq word (substring word (length head)))))
        (setq line word)))
    (nreverse (cons (or line "") lines))))

(defun my/table-view-rule (widths names left middle right)
  "Return a horizontal rule of the table view, with its newline.
WIDTHS and NAMES are those of the columns; LEFT, MIDDLE and RIGHT are
the characters at the left edge, between two columns and at the right
edge.  The part of each column carries its name, as its cells do, so
that sorting and moving by column also work on a rule."
  (concat
   (cl-loop for width in widths
            for name in names
            for edge = left then middle
            concat (propertize (concat edge (make-string (+ width 2) ?─))
                               'tabulated-list-column-name name 'face 'shadow))
   (propertize right 'tabulated-list-column-name (car (last names)) 'face 'shadow)
   "\n"))

(defun my/table-view-print-entry (id cells)
  "Insert the row ID of CELLS as lines of a box, under the rule above it.
This is the `tabulated-list-printer' of `my/table-view-mode'.  Each
cell wraps to the width of its column, so a row is as tall as its
tallest cell.  A row ends the table with the bottom rule, which the
next row turns into the rule between the two.  Every line of the row,
the rule above it included, carries the row's id and cells; each
column's part of a line, its left border included, carries the
column's name.  Padding aligns each border with `:align-to', so the
borders stay in line whatever the width of the glyphs."
  (let* ((widths (my/table-view-widths))
         (names (mapcar #'car tabulated-list-format))
         (pieces (seq-mapn #'my/table-view-wrap cells widths))
         (next-borders (let ((x 0))
                         (mapcar (lambda (width) (setq x (+ x width 3))) widths)))
         (first (bobp))
         (start (if first (point) (pos-bol 0))))
    (delete-region start (point))
    (insert (my/table-view-rule widths names
                                (if first "┌" "├") (if first "┬" "┼") (if first "┐" "┤")))
    (dotimes (line (apply #'max (mapcar #'length pieces)))
      (insert (cl-loop for name in names
                       for column in pieces
                       for border in next-borders
                       concat (propertize
                               (concat (propertize "│" 'face 'shadow) " "
                                       (or (nth line column) "")
                                       (propertize " " 'display `(space :align-to ,border)))
                               'tabulated-list-column-name name))
              (propertize "│" 'face 'shadow 'tabulated-list-column-name (car (last names)))
              "\n"))
    (add-text-properties start (point)
                         `(tabulated-list-id ,id tabulated-list-entry ,cells))
    (insert (my/table-view-rule widths names "└" "┴" "┘"))))

(defun my/table-view-header-line ()
  "Return the header line of the table view: the labels, between bars.
Each label is drawn with `:align-to' where the cells of its column
start, so that later labels stay above their cells whatever the width
of the glyphs, and shifts left with `window-hscroll'; the part of a
label left of the window is cut off.  A label wider than its column
ends with an ellipsis.  The header line is a mode-line construct, so a
% in a label is escaped."
  (let* ((hscroll (window-hscroll))
         (widths (my/table-view-widths))
         (bar (propertize "│" 'face 'shadow))
         (x 0)
         (borders (mapcar (lambda (width) (prog1 x (setq x (+ x width 3)))) widths)))
    (cl-flet ((at (column)
                (propertize " " 'display `(space :align-to (+ header-line-indent-width
                                                              ,(max 0 (- column hscroll)))))))
      (concat
       (cl-loop for width in widths
                for border in borders
                for column across tabulated-list-format
                concat (concat
                        (when (>= border hscroll)
                          (concat (at border) bar))
                        (at (+ border 2))
                        (string-replace
                         "%" "%%"
                         (truncate-string-to-width
                          (truncate-string-to-width (car column) width nil nil t)
                          width (max 0 (- hscroll border 2))))))
       (when (>= x hscroll)
         (concat (at x) bar))))))

(defconst my/table-view-header-line-format '(:eval (my/table-view-header-line))
  "Header line format of `my/table-view-mode'.")

(defun my/table-view-fit ()
  "Fit the columns of the table view to the width of the selected window.
When the natural widths, those of the widest cell or label of each
column, fit, the columns keep them.  Otherwise the widest columns
narrow to the largest common width that fits, but not below
`my/table-view-min-width', and their cells wrap; a table wider still
scrolls sideways."
  (let* ((naturals my/table-view--natural-widths)
         (available (- (window-body-width) 2 (* 3 (length naturals))))
         (limit (apply #'max naturals)))
    (while (and (> limit my/table-view-min-width)
                (> (apply #'+ (mapcar (lambda (natural) (min natural limit)) naturals))
                   available))
      (setq limit (1- limit)))
    (cl-loop for column across tabulated-list-format
             for natural in naturals
             do (setf (cadr column) (min natural limit)))))

(define-derived-mode my/table-view-mode tabulated-list-mode "Table"
  "Major mode viewing an Org or Markdown table as a box of wrapped cells.
Every cell is shown in full, wrapped to the width of its column; the
columns fit the window when they can.  The view is read-only: edit the
table in its source.  The header line follows horizontal scrolling.

\\{my/table-view-mode-map}"
  (setq-local tabulated-list-printer #'my/table-view-print-entry)
  (setq header-line-format my/table-view-header-line-format)
  ;; `revert-buffer' (g) fits the columns to a window that changed size.
  (add-hook 'tabulated-list-revert-hook #'my/table-view-fit nil t)
  ;; Sorting and the column-width commands set the header line of
  ;; `tabulated-list-init-header', which does not scroll horizontally.
  (add-hook 'post-command-hook
            (lambda () (setq header-line-format my/table-view-header-line-format))
            nil t))

(defun my/table-view ()
  "Show the Org or Markdown table at point in a read-only box of cells.
Each row of the table is a row of the box, with every cell in full,
wrapped to fit the window.
\\<my/table-view-mode-map>In the box, \\[tabulated-list-sort] sorts by the column at point,
and \\[my/table-view-goto-source] goes back to the cell in the table."
  (interactive)
  (let* ((rows (seq-drop-while
                (lambda (row) (eq row 'hline))
                (or (cond ((derived-mode-p 'org-mode) (my/table-view-org-rows))
                          ((derived-mode-p 'markdown-mode) (my/table-view-markdown-rows)))
                    (user-error "No Org or Markdown table at point"))))
         (header (and (eq (cadr rows) 'hline) (cdar rows)))
         (data (seq-remove (lambda (row) (eq row 'hline)) (if header (cddr rows) rows)))
         (count (apply #'max (length header) (mapcar (lambda (row) (length (cdr row))) data)))
         (entries (mapcar (lambda (row)
                            (list (car row)
                                  (vconcat (cdr row)
                                           (make-list (- count (length (cdr row))) ""))))
                          data)))
    (when (zerop count)
      (user-error "No row in the table at point"))
    (with-current-buffer (get-buffer-create (format "*Table: %s*" (buffer-name)))
      (my/table-view-mode)
      (setq tabulated-list-entries entries
            tabulated-list-format
            (vconcat
             (mapcar (lambda (column)
                       ;; Sorting finds a column by its label: make labels unique.
                       (let ((label (or (nth column header) "")))
                         (when (or (string-empty-p label)
                                   (member label (cdr (member label header))))
                           (setq label (format "$%d" (1+ column))))
                         (list label
                               (apply #'max (string-width label)
                                      (mapcar (lambda (entry)
                                                (string-width (aref (cadr entry) column)))
                                              entries))
                               t)))
                     (number-sequence 0 (1- count))))
            my/table-view--natural-widths (mapcar #'cadr tabulated-list-format))
      (my/table-view-fit)
      (tabulated-list-print)
      (pop-to-buffer-same-window (current-buffer)))))

(defun my/table-view-goto-source ()
  "Go to the cell at point in the source table."
  (interactive)
  (let ((line (tabulated-list-get-id))
        (column (seq-position (mapcar #'car tabulated-list-format)
                              ;; At the end of a line, the last cell.
                              (or (get-text-property (point) 'tabulated-list-column-name)
                                  (get-text-property (max (pos-bol) (1- (point)))
                                                     'tabulated-list-column-name)))))
    (unless (and line (buffer-live-p (marker-buffer line)))
      (user-error "No row of a live table at point"))
    (pop-to-buffer-same-window (marker-buffer line))
    (goto-char line)
    (if (derived-mode-p 'org-mode)
        (org-table-goto-column (1+ (or column 0)))
      (markdown-table-goto-column (1+ (or column 0))))))

(keymap-global-set "C-c v t" #'my/table-view)
(keymap-set my/table-view-mode-map "RET" #'my/table-view-goto-source)

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
    (let ((inhibit-message t))
      (recentf-mode 1)))
  (add-hook 'pre-command-hook #'my/recentf-enable-once)
  :custom
  (recentf-max-saved-items 200))

(use-package dired
  :straight nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  ;; Directories first, no . or .. entries (-A), long format required by dired.
  (dired-listing-switches "-l -A --group-directories-first"))

(use-package transient)

(use-package dirvish
  :custom
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; Three-pane layout: 1 parent dir | current | preview (55% width).
  ;; Format: (depth max-parent-width preview-width), widths as frame ratios.
  (dirvish-default-layout '(1 0.11 0.55))
  ;; Full-frame layouts that M-T (dirvish-layout-switch) goes through, in
  ;; order.  M-t (dirvish-layout-toggle) leaves them for a single window.
  (dirvish-layout-recipes
   '((0 0.11 0.55)   ;; preview pane only
     (1 0.11 0.55))) ;; parent + preview (default)
  ;; dirvish-preview-dispatchers is left at its built-in default
  ;; (video image gif audio epub archive font pdf).
  ;; 'default, 'tramp, 'pdf-preface are not valid symbols and cause errors.
  (dirvish-emerge-groups
   '(("Directories" (predicate . directories))
     ("JVM"         (extensions "java" "kt" "clj" "cljs" "cljc"))
     ("Web"         (extensions "ts" "tsx" "js" "jsx" "css" "html"))
     ("Data"        (extensions "json" "yaml" "yml" "edn" "sql"))
     ("Docs"        (extensions "org" "md" "rst" "txt"))
     ("Config"      (extensions "toml" "ini" "properties" "conf"))
     ("Tests"       (regex ".*[Tt]est.*"))
     ("Build"       (extensions "xml" "gradle" "nix" "lock"))))
  :bind
  (("C-c f" . dirvish-fd)))

;; Eager activation at emacs-startup-hook.  The previous one-shot
;; dired-mode-hook approach had a race: dired-mode-hook fires /during/
;; dired-noselect's setup of the buffer being constructed, so the
;; first dired buffer was always built without the override advice.
;; Activating before the user's first dired action eliminates the race
;; entirely; the cost is one require + one mode activation at the end
;; of startup (~25 ms — the load that the first dired action used to
;; pay, now paid earlier).
(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'dirvish)
            (unless (bound-and-true-p dirvish-override-dired-mode)
              (dirvish-override-dired-mode 1))))

(with-eval-after-load 'dirvish-collapse
  (require 'nerd-icons)
  (setq dirvish-collapse-separator
        (concat (nerd-icons-codicon "nf-cod-chevron_right") " ")))

(defun my/dirvish-collapse-chain (dir)
  "Return the chain of single visible entries under DIR, top first.
A directory continues the chain when it holds exactly one visible
entry: not a dotfile, and not hidden by `dired-omit-mode' when it is
on in the current buffer.  The chain ends at a file, at a directory
with no or several visible entries, before an entry it already went
through, as in a link to a parent, and before a link that does not
resolve, as in a link to itself.  It is nil when DIR is not a
directory holding exactly one visible entry, and for the . and ..
entries of a listing, as upstream: they name the current directory
and its parent, not a chain."
  (when (and (file-directory-p dir)
             (not (member (file-name-nondirectory (directory-file-name dir))
                          '("." ".."))))
    (let ((omit (let ((regexp (and (bound-and-true-p dired-omit-mode)
                                   (dired-omit-regexp))))
                  (if (seq-empty-p regexp) regexp-unmatchable regexp)))
          (seen (list (file-truename dir)))
          (chain nil))
      ;; `directory-files' fails on a file or a link loop: the chain ends.
      (while-let ((entries (seq-remove (lambda (name)
                                         (or (string-prefix-p "." name)
                                             (string-match-p omit name)))
                                       (ignore-errors (directory-files dir))))
                  ((null (cdr entries)))
                  ;; Not `expand-file-name', which reads an entry named
                  ;; ~ as the home directory.
                  (next (concat (file-name-as-directory dir) (car entries)))
                  (truename (ignore-errors (file-truename next)))
                  ((not (member truename seen))))
        (push truename seen)
        (push next chain)
        (setq dir next))
      (nreverse chain))))

(with-eval-after-load 'dirvish-collapse
  (define-advice dirvish-collapse--cache (:override (f-name) my/visible-chain)
    "Describe the chain `my/dirvish-collapse-chain' finds under F-NAME.
Return what upstream returns: (HEAD . TAIL) for a chain, where TAIL
is its last entry and HEAD the separators and directories before it;
\(empty . t) for an empty directory; (nil . nil) otherwise."
    (dirvish-attribute-cache f-name
        (if (bound-and-true-p dired-omit-mode) :my/collapse-omit :collapse)
      (let* ((chain (my/dirvish-collapse-chain f-name))
             (names (mapcar #'file-name-nondirectory chain))
             (head (apply #'concat dirvish-collapse-separator
                          (mapcar (lambda (name) (concat name dirvish-collapse-separator))
                                  (butlast names))))
             (tail (car (last names))))
        (cond (chain
               (add-face-text-property 0 (length head) 'dirvish-collapse-dir-face nil head)
               (add-face-text-property 0 (length tail)
                                       (if (file-directory-p (car (last chain)))
                                           'dirvish-collapse-dir-face
                                         'dirvish-collapse-file-face)
                                       nil tail)
               (cons head tail))
              ((ignore-errors (directory-empty-p f-name)) (cons 'empty t))
              (t (cons nil nil)))))))

(defun my/dirvish-collapse-shown-p ()
  "Non-nil when the current dirvish buffer draws the `collapse' attribute.
dirvish draws it while it is among the session's attributes, which
\\[dirvish-setup-menu] changes, in a local listing that is not an fd
result, as the attribute's `:when' says."
  (and (assq 'collapse (dirvish-prop :attrs))
       (not (dirvish-prop :fd-info))
       (not (dirvish-prop :remote))))

(defun my/dirvish-subtree-toggle-chain ()
  "Expand or collapse the directory at point, a collapsed chain at once.
On a directory whose line shows a chain, as `my/dirvish-collapse-chain'
finds it, expand every level of the chain, down to the contents of
its last directory.  Elsewhere, and to collapse, act as
`dirvish-subtree-toggle'."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (chain (and file (not (dirvish-subtree--expanded-p))
                     (my/dirvish-collapse-shown-p)
                     (my/dirvish-collapse-chain file)))
         (end (car (last chain))))
    (if (not chain)
        (dirvish-subtree-toggle)
      (save-excursion
        (dirvish-subtree-expand-to end)
        (when (file-directory-p end)
          (dirvish-subtree-toggle))))))

(defun my/dirvish-collapse-find-entry ()
  "Visit the file or directory at point, or the end of its collapsed chain.
On a directory whose line shows a chain, as `my/dirvish-collapse-chain'
finds it, visit the chain's last entry, the one its line shows last, as
`dired-find-file' visits the entry at point: through dirvish, which
leaves its full-frame layout for a file.  When that entry does not
exist, a dangling link, act as `dired-find-file'."
  (interactive)
  (if-let* ((file (dired-get-filename nil t))
            ((my/dirvish-collapse-shown-p))
            (end (car (last (my/dirvish-collapse-chain file))))
            ((file-exists-p end)))
      (dired--find-possibly-alternative-file end)
    (call-interactively #'dired-find-file)))

(with-eval-after-load 'dirvish
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
  (defun my/dirvish-subtree-toggle-all ()
    "Collapse directory at point if expanded; otherwise expand it and all children."
    (interactive)
    (when-let ((file (dired-get-filename nil t)))
      (when (file-directory-p file)
        (if (dirvish-subtree--expanded-p)
            (dirvish-subtree-toggle)
          (my/dirvish-subtree-expand-all-children))))))

(with-eval-after-load 'dirvish
  (define-key dirvish-mode-map (kbd "a")         #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "f")         #'dirvish-file-info-menu)
  (define-key dirvish-mode-map (kbd "y")         #'dirvish-yank-menu)
  (define-key dirvish-mode-map (kbd "N")         #'dirvish-narrow)
  (define-key dirvish-mode-map (kbd "b")         #'dired-up-directory)
  (define-key dirvish-mode-map (kbd "H")         #'dirvish-history-jump)
  (define-key dirvish-mode-map (kbd "s")         #'dirvish-quicksort)
  (define-key dirvish-mode-map (kbd "v")         #'dirvish-vc-menu)
  (define-key dirvish-mode-map (kbd "<tab>")     #'my/dirvish-subtree-toggle-chain)
  (define-key dirvish-mode-map (kbd "TAB")       #'my/dirvish-subtree-toggle-chain)
  (define-key dirvish-mode-map (kbd "<backtab>") #'my/dirvish-subtree-toggle-all)
  (define-key dirvish-mode-map (kbd "RET")       #'my/dirvish-collapse-find-entry)
  (define-key dirvish-mode-map (kbd "M-f")       #'dirvish-history-go-forward)
  (define-key dirvish-mode-map (kbd "M-b")       #'dirvish-history-go-backward)
  (define-key dirvish-mode-map (kbd "M-l")       #'dirvish-ls-switches-menu)
  (define-key dirvish-mode-map (kbd "M-m")       #'dirvish-mark-menu)
  (define-key dirvish-mode-map (kbd "M-t")       #'dirvish-layout-toggle)
  (define-key dirvish-mode-map (kbd "M-T")       #'dirvish-layout-switch)
  (define-key dirvish-mode-map (kbd "M-s")       #'dirvish-setup-menu)
  (define-key dirvish-mode-map (kbd "M-e")       #'dirvish-emerge-menu))

(use-package project
  :straight nil
  :bind-keymap ("s-p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file" "f")
     (consult-ripgrep "Ripgrep" "r")
     (project-find-dir "Find dir" "d")
     (magit-project-status "Magit" "g")
     (eat-project "Eat" ?e))))

(use-package imenu-list
  :custom
  (imenu-list-auto-resize t)
  (imenu-list-focus-after-activation t)
  (imenu-list-size 0.25))

(use-package editorconfig
  ;; On before the first file buffer, which command-line files and
  ;; emacsclient open once init has finished.
  :hook (after-init . editorconfig-mode))

(setq mode-require-final-newline t)

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom (magit-format-file-function #'magit-format-file-nerd-icons))

(defun my/diff-hl-hash-temp-file-name (name)
  "Return NAME with its base name replaced by its SHA-1.
diff-hl names its copy of a revision after the full path of the
file; for a deep path that name passes the 255-byte limit of a
file name, so the copy is never written and the diff fails."
  (expand-file-name (concat "#" (sha1 (file-name-nondirectory name)) "#")
                    (file-name-directory name)))

;; diff-hl: fringe indicators for VCS changes.
;; Keybindings preserve the C-c g prefix from the previous git-gutter config.
;; Keep Git change markers on the right fringe.
(use-package diff-hl
  :hook ((prog-mode  . diff-hl-mode)
         (conf-mode  . diff-hl-mode)
         (text-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :bind (("C-c g n" . diff-hl-next-hunk)
         ("C-c g p" . diff-hl-previous-hunk)
         ("C-c g s" . diff-hl-stage-current-hunk)
         ("C-c g r" . diff-hl-revert-hunk)
         ("C-c g d" . diff-hl-show-hunk))
  :custom
  (diff-hl-side 'right)
  :config
  ;; Live diff against HEAD for unsaved changes (idle timer, async).
  ;; It also replaces diff-hl's teardown of a hunk's overlay on each
  ;; edit by its idle refresh.
  (diff-hl-flydiff-mode 1)
  (advice-add 'diff-hl-make-temp-file-name :filter-return
              #'my/diff-hl-hash-temp-file-name))

(use-package magit-todos
  :after magit
  :commands (magit-todos-list magit-todos-mode))

(defun my/magit-delta-follow-appearance ()
  "Make delta colour diffs for the current appearance, light or dark.
The appearance is `frame-background-mode', which auto-dark sets; dark
when it is unset."
  (when (boundp 'magit-delta-delta-args)
    (setq magit-delta-delta-args
          (cons (if (eq frame-background-mode 'light) "--light" "--dark")
                (seq-difference magit-delta-delta-args '("--light" "--dark"))))))

(add-hook 'auto-dark-dark-mode-hook #'my/magit-delta-follow-appearance)
(add-hook 'auto-dark-light-mode-hook #'my/magit-delta-follow-appearance)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :custom
  ;; --no-gitconfig: the Nix delta wrapper injects --config with
  ;; side-by-side = true.  That adds structural layout text which
  ;; corrupts magit's section hierarchy ("--side-by-side" "false"
  ;; does NOT work — delta treats "false" as a filename, producing
  ;; zero output).  --no-gitconfig overrides --config and avoids the
  ;; problem entirely.
  (magit-delta-delta-args
   '("--no-gitconfig"
     "--max-line-distance" "0.6"
     "--true-color" "always"
     "--color-only"
     "--syntax-theme" "ansi"))
  :config
  (my/magit-delta-follow-appearance))

(use-package forge
  :after magit
  :straight (forge :host github :repo "magit/forge"))

(use-package magit-prime
  :after magit
  ;; Load with magit: nothing else would load it.
  :demand t
  :config (magit-prime-mode))

(use-package difftastic
  :after magit
  ;; Load with magit: nothing else would load it.
  :demand t
  :custom
  ;; Use the full frame width so side-by-side panels are as wide as possible.
  (difftastic-requested-window-width-function (lambda () (frame-width)))
  :config
  (require 'difftastic-bindings)
  ;; Adds M-d / M-c to the `d' (magit-diff) transient,
  ;; M-RET to magit-blame, and M-d to magit-file-dispatch.
  (difftastic-bindings-mode +1))

(use-package eldoc-box
  :bind* (("C-h C-d" . eldoc-box-help-at-point)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind-keymap ("C-c l" . lsp-command-map)
  :custom
  (lsp-keymap-prefix "C-c l")
  ;; Let corfu drive completion-at-point.
  (lsp-completion-provider :none)
  ;; Completion items can carry a second edit which inserts an import.
  (lsp-completion-enable-additional-text-edit t)
  ;; Kind is represented by the Corfu margin icon, not a repeated word.
  (lsp-completion-show-kind nil)
  ;; Let the major mode's indenter own indent-region.  When this knob
  ;; is non-nil and the server advertises textDocument/rangeFormatting
  ;; (clojure-lsp does), lsp-mode `add-function :override's
  ;; indent-region-function with lsp-format-region.  paredit's slurp
  ;; and barf both call indent-region after moving a delimiter, which
  ;; then round-trips synchronously to the server against a buffer
  ;; view the server has not been told about yet -- the resulting
  ;; TextEdits corrupt the buffer (stray closing parens, swallowed
  ;; slurps).  clojure-mode's indenter is already paredit-friendly;
  ;; explicit M-x lsp-format-buffer / lsp-format-region still call
  ;; into the server.  See plans/052-lsp-paredit-indent-region.
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  ;; Keep a discoverable code-action indicator beside doom-modeline.
  ;; Its diagnostic counts and workspace status do not need duplicating.
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-workspace-status-enable nil)
  ;; Code lenses (e.g. clojure-lsp's reference counts above each
  ;; defn/def) on globally.  Computed lazily per visible buffer.
  (lsp-lens-enable t)
  (lsp-enable-snippet nil)
  ;; Cap initial scan on large monorepos.
  (lsp-file-watch-threshold 5000)
  (lsp-session-file (no-littering-expand-var-file-name "lsp-mode/session-v1"))
  (lsp-before-save-edits nil)
  ;; Hover doc lives in eldoc-box; keep signature popups text only.
  (lsp-signature-render-documentation nil)
  (lsp-headerline-breadcrumb-enable t)
  ;; Buffer log per workspace; cap so it does not pile up forever.
  (lsp-log-max 1000)
  :hook ((lsp-mode . lsp-headerline-breadcrumb-mode)
         (lsp-mode . lsp-diagnostics-mode))
  :config
  (define-key lsp-command-map (kbd "T H") #'lsp-inlay-hints-mode)
  ;; Register standard prefix descriptions even when which-key loads later.
  (with-eval-after-load 'which-key
    (lsp-enable-which-key-integration t)
    (which-key-add-key-based-replacements
      "C-c l s" "symbols" "C-c l T H" "toggle inlay hints"))
  ;; Subtle symbol-at-point highlight: italic + underline on the
  ;; default background so the active occurrence reads without
  ;; competing with hl-line-mode.
  (dolist (face '(lsp-face-highlight-textual
                  lsp-face-highlight-read
                  lsp-face-highlight-write))
    (set-face-attribute face nil
                        :background 'unspecified
                        :slant 'italic
                        :underline t)))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t))

(use-package lsp-treemacs
  :after lsp-mode
  :commands (lsp-treemacs-symbols
             lsp-treemacs-references
             lsp-treemacs-errors-list)
  :custom
  ;; Auto-expand the incoming/outgoing call hierarchy two levels deep
  ;; on render.  Each level is a separate LSP request via the
  ;; tree's :children-async machinery, so depth 2 trades a brief
  ;; pause for a much more useful initial view.  Drop to 1 if
  ;; clojure-lsp feels sluggish on heavily-called symbols.
  (lsp-treemacs-call-hierarchy-expand-depth 2)
  :config
  (setq lsp-treemacs-symbols-position-params
        '((side . left) (slot . 2) (window-width . 40))))

(use-package consult-lsp
  :after lsp-mode
  :bind (:map lsp-mode-map
              ("C-c l s s" . consult-lsp-symbols)
              ("C-c l s f" . consult-lsp-file-symbols)))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c l" "lsp"))

(when (fboundp 'treesit-parser-create)
  (require 'treesit)

  ;; Keep grammars in the standard Emacs location:
  ;;   ~/.emacs.d/tree-sitter/
  ;; This makes `treesit-install-language-grammar' and external tools happy.
  (let ((grammar-dir (expand-file-name "tree-sitter/" user-emacs-directory)))
    (make-directory grammar-dir t)
    (add-to-list 'treesit-extra-load-path grammar-dir))

  ;; treesit-auto is kept for on-demand grammar installation only
  ;; (M-x treesit-auto-install-all).  It does NOT need to be loaded at
  ;; startup: major-mode-remap-alist is populated directly below via dolist,
  ;; and auto-install prompting is handled lazily when the package first loads.
  ;; treesit-auto is used for on-demand grammar installation.
  ;; With `use-package-always-defer', its interactive commands are otherwise
  ;; not discoverable via M-x unless the library is required manually.
  (use-package treesit-auto
    :commands (treesit-auto-install-all)
    :config
    (setq treesit-auto-install 'prompt))

  (defun my/treesit-remap-when-ready (lang from to &optional fallback)
    "Remap FROM to TO when LANG grammar is installed.
Otherwise FROM runs: add FALLBACK, when non-nil, to its hook."
    (if (and (treesit-ready-p lang t)
             (fboundp to))
        (add-to-list 'major-mode-remap-alist (cons from to))
      (when fallback
        (add-hook (intern (format "%s-hook" from)) fallback))))

  (defun my/treesit-auto-mode-when-ready (lang regexp mode)
    "Use MODE for REGEXP only when LANG grammar is installed."
    (when (and (treesit-ready-p lang t)
               (fboundp mode))
      (add-to-list 'auto-mode-alist (cons regexp mode))))

  ;; Only remap modes when the corresponding grammar exists.
  ;; This prevents "missing grammar" errors on fresh machines.
  (my/treesit-remap-when-ready 'python     'python-mode     'python-ts-mode)
  (my/treesit-remap-when-ready 'java       'java-mode       'java-ts-mode #'lsp-deferred)
  (my/treesit-remap-when-ready 'json       'js-json-mode    'json-ts-mode)
  (my/treesit-remap-when-ready 'css        'css-mode        'css-ts-mode)
  (my/treesit-remap-when-ready 'html       'html-mode       'html-ts-mode)
  (my/treesit-remap-when-ready 'html       'mhtml-mode      'html-ts-mode)
  (my/treesit-remap-when-ready 'dockerfile 'dockerfile-mode 'dockerfile-ts-mode #'lsp-deferred)

  ;; Extensions that should switch to ts-modes only when ready.
  (my/treesit-auto-mode-when-ready 'yaml       "\\.ya?ml\\'"                                              'yaml-ts-mode)
  (my/treesit-auto-mode-when-ready 'dockerfile "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" 'dockerfile-ts-mode)
  (my/treesit-auto-mode-when-ready 'bash       "\\.sh\\'"                                                'bash-ts-mode)
  (my/treesit-auto-mode-when-ready 'bash       "\\.bash\\'"                                              'bash-ts-mode)
  (my/treesit-auto-mode-when-ready 'bash       "\\.zsh\\'"                                               'bash-ts-mode))

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
  ;; The configuration is globally deferred. Demand this tiny formatter once
  ;; Corfu loads so the margin hook is registered before the first popup.
  :demand t
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
  :hook (emacs-startup . my/treesit-fold-bootstrap)
  :bind (;; IDE-style quick toggle, kept for muscle memory.
         ("C-s-<tab>" . treesit-fold-toggle)
         ;; C-c z = "zip" — the dedicated fold prefix.
         ("C-c z z"   . treesit-fold-toggle)
         ("C-c z c"   . treesit-fold-close)
         ("C-c z o"   . treesit-fold-open)
         ("C-c z O"   . treesit-fold-open-recursively)
         ("C-c z a"   . treesit-fold-open-all)
         ("C-c z A"   . treesit-fold-close-all))
  :init
  ;; Map non-`-ts-mode' major modes to the tree-sitter language whose
  ;; grammar should drive folding.  `treesit-fold-range-alist' itself
  ;; maps mode → (node-type . fn) pairs — it does *not* record which
  ;; grammar to load.  For `*-ts-mode' modes Emacs creates the parser
  ;; automatically; this alist plugs the gap for the legacy modes the
  ;; user actually edits.  Extend it in `local.el' if needed.
  (defvar my/treesit-fold-mode-language-alist
    '((emacs-lisp-mode       . elisp)
      (lisp-interaction-mode . elisp)
      (clojure-mode          . clojure)
      (clojurec-mode         . clojure)
      (clojurescript-mode    . clojure)
      (nix-mode              . nix)
      (sh-mode               . bash)
      (lisp-mode             . commonlisp)
      (json-mode             . json)
      (yaml-mode             . yaml)
      (dockerfile-mode       . dockerfile)
      (markdown-mode         . markdown))
    "Major mode → tree-sitter language for `treesit-fold' fallback.
Used by `my/treesit-fold-ensure-parser' to create a parser in
non-`-ts-mode' buffers whose grammar is installed.  Modes derived
from `*-ts-mode' do not need an entry: the major mode itself creates
the parser.  Languages whose grammar is missing are skipped silently.")

  (defun my/treesit-fold-ensure-parser ()
    "Create a tree-sitter parser so `treesit-fold-mode' can activate.
`treesit-fold-mode' refuses to engage when `(treesit-parser-list)' is
empty, which is the case in non-`-ts-mode' major modes like
`clojure-mode' or `nix-mode'.  When the major mode is in
`my/treesit-fold-mode-language-alist' and the corresponding grammar
is installed, create a parser so folding can engage."
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p)
               (null (treesit-parser-list))
               (alist-get major-mode treesit-fold-range-alist))
      (when-let* ((lang (alist-get major-mode
                                   my/treesit-fold-mode-language-alist)))
        (when (treesit-ready-p lang t)
          (ignore-errors (treesit-parser-create lang))))))

  (defun my/treesit-fold-bootstrap ()
    "Enable global folding and create parsers for supported legacy modes."
    (require 'treesit-fold)
    (add-hook 'after-change-major-mode-hook
              #'my/treesit-fold-ensure-parser -90)
    (global-treesit-fold-mode 1)))

(defun my/indent-bars-sync-spacing ()
  "Set `indent-bars-spacing-override' from the buffer's indent variable.
Bridges per-buffer indent width detected by `editorconfig' or
`dtrt-indent' (or the major mode itself) into the spacing
`indent-bars' uses to draw bars.  When no relevant variable is set
buffer-locally, leave the override `nil' so `indent-bars'' built-in
`indent-bars--guess-spacing' is in charge."
  (let ((width (cond
                ((or (derived-mode-p 'clojure-mode)
                     (derived-mode-p 'clojure-ts-mode)
                     (derived-mode-p 'clojurec-mode)
                     (derived-mode-p 'clojurescript-mode)
                     (derived-mode-p 'lisp-data-mode)
                     (derived-mode-p 'emacs-lisp-mode))
                 (and (boundp 'lisp-body-indent) lisp-body-indent))
                ((or (derived-mode-p 'python-mode)
                     (derived-mode-p 'python-base-mode))
                 (and (boundp 'python-indent-offset)
                      (local-variable-p 'python-indent-offset)
                      python-indent-offset))
                ((derived-mode-p 'sh-base-mode)
                 (and (boundp 'sh-basic-offset)
                      (local-variable-p 'sh-basic-offset)
                      sh-basic-offset))
                ((or (derived-mode-p 'c-mode)
                     (derived-mode-p 'c-ts-base-mode))
                 (and (boundp 'c-basic-offset)
                      (local-variable-p 'c-basic-offset)
                      (numberp c-basic-offset)
                      c-basic-offset)))))
    (when (and (numberp width) (> width 0))
      (setq-local indent-bars-spacing-override width)
      ;; `indent-bars-mode' caches `indent-bars-spacing' in `:setup'
      ;; before this hook runs, so refresh once we have the right value.
      (when (fboundp 'indent-bars-reset)
        (indent-bars-reset)))))

(use-package indent-bars
  :hook ((prog-mode . indent-bars-mode)
         (LaTeX-mode . indent-bars-mode)
         (indent-bars-mode . my/indent-bars-sync-spacing)))

(use-package apheleia
  :hook ((prog-mode . apheleia-mode)
         (html-mode . apheleia-mode)
         (html-ts-mode . apheleia-mode))
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'sh-mode apheleia-mode-alist) '(shfmt))
  (setf (alist-get 'alejandra apheleia-formatters) '("alejandra"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) '(alejandra))
  ;; Java is intentionally excluded: JDTLS drives formatting via
  ;; textDocument/willSaveWaitUntil; adding google-java-format as a second
  ;; pass re-indents the entire file on every save regardless of project
  ;; style.  Use C-c l = = (lsp-format-buffer) for explicit on-demand
  ;; formatting.
  (setf apheleia-mode-alist
        (assoc-delete-all 'java-mode apheleia-mode-alist))
  (setf apheleia-mode-alist
        (assoc-delete-all 'java-ts-mode apheleia-mode-alist)))

(keymap-set prog-mode-map "C-c r" #'apheleia-format-buffer)

(use-package ws-butler
  ;; The MELPA recipe points to a nongnu monorepo without an explicit :files
  ;; spec, which causes straight to build an empty directory.  The recipe
  ;; below fixes that by pinning to the same local clone and naming the one
  ;; file that needs to be linked into the build directory.
  :straight (ws-butler
             :local-repo "nongnu"
             :branch "elpa/ws-butler"
             :files ("ws-butler.el"))
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

;; .bb (babashka) scripts open in clojure-mode.
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

(use-package clojure-mode
  :hook ((clojure-mode       . paredit-mode)
         (clojure-mode       . lsp-deferred)
         (clojurec-mode      . paredit-mode)
         (clojurec-mode      . lsp-deferred)
         (clojurescript-mode . paredit-mode)
         (clojurescript-mode . lsp-deferred)))

;; lsp-clojure ships inside lsp-mode and loads on first Clojure attach.
(with-eval-after-load 'lsp-clojure
  (setq lsp-clojure-trace-enable nil))

;; CFR-decompiled .java files under ~/.cache/clojure-lsp/java/decompiled/
;; carry a "Decompiled with CFR" header; java-mode font-locks them
;; correctly out of the box.
(add-to-list 'auto-mode-alist
             '("/clojure-lsp/java/decompiled/.*\\.java\\'" . java-mode))

(use-package javap-mode
  :commands javap-mode
  :mode "\\.class\\'")

(use-package cider
  :hook ((cider-repl-mode . paredit-mode)
         (cider-repl-mode . rainbow-delimiters-mode))
  :bind (:map clojure-mode-map
              ("M-g j n" . consult-imenu)
              ("M-g j p" . consult-imenu-multi))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file (no-littering-expand-var-file-name "cider-repl-history"))
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-use-pretty-printing t)
  (cider-eval-result-position 'at-eol)
  (cider-eval-result-display 'overlay)
  (cider-show-error-buffer 'except-in-repl)
  (cider-eldoc-display-for-symbol-at-point t)
  (cider-test-show-report-on-success nil))

(defun my/clojure-mode-hook ()
  (clj-refactor-mode 1)
  ;; cljr-add-keybindings-with-prefix installs refactoring commands under C-c C-m
  ;; (this leaves cider-macroexpand-1 unbound, use M-x cider-macroexpand-1 instead)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :hook (clojure-mode . my/clojure-mode-hook))

;; kaocha test runner integration: run tests without leaving Emacs.
;; The C-c k keys mean what they mean in Python buffers: t the test at
;; point, f the namespace, a all; s and h show the warnings and hide
;; kaocha's windows.
(use-package kaocha-runner
  :after cider
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k f" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k s" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

;; Place CIDER REPL in a bottom side-window so it doesn't steal the
;; main editing area.  The REPL is shown on connect (cider-repl-pop-to-
;; buffer-on-connect is 'display-only above) but stays out of the way.
(add-to-list 'display-buffer-alist
             '("\\*cider-repl"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.3)))

(use-package python
  :straight nil
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project))

(defun my/python-package-entrypoint-module ()
  "Return the current package module when it has a __main__.py entry point."
  (when-let* ((file buffer-file-name)
              (directory (file-name-directory file)))
    (let (package-root parts)
      (while (file-exists-p (expand-file-name "__init__.py" directory))
        (push (file-name-nondirectory (directory-file-name directory)) parts)
        (setq package-root directory
              directory (file-name-directory
                         (directory-file-name directory))))
      (when (and package-root
                 (file-exists-p (expand-file-name "__main__.py" package-root)))
        (string-join parts ".")))))

(defun my/python-dape-command ()
  "Return a Dape command using the PET-selected Python executable."
  (when (and buffer-file-name (stringp python-shell-interpreter))
    (if-let ((module (my/python-package-entrypoint-module)))
        (list 'debugpy-module
              'command python-shell-interpreter
              :module module)
      (list 'debugpy
            'command python-shell-interpreter
            :program buffer-file-name))))

(defun my/python-dape-ensure (config)
  "Check CONFIG for debugpy without passing its interpreter through a shell."
  (let ((python (dape-config-get config 'command)))
    (unless (and (stringp python)
                 (equal 0 (process-file python nil nil nil
                                        "-c" "import debugpy.adapter")))
      (user-error "%s module debugpy is not installed" python))))

(defun my/python-language-server-command ()
  "Return the best installed Pyright-compatible language server."
  (cond
   ((executable-find "basedpyright-langserver") "basedpyright")
   ((executable-find "pyright-langserver") "pyright")
   ;; The Nix configuration installs BasedPyright. Keep that dependency name
   ;; when the executable is temporarily absent from a graphical Emacs PATH so
   ;; lsp-mode reports the missing prerequisite instead of downloading npm.
   (t "basedpyright")))

(defun my/python-use-host-ruff ()
  "Use the host-built Ruff for editor integrations in this buffer."
  (when-let ((ruff (executable-find "ruff")))
    (setq-local lsp-ruff-server-command (list ruff "server")
                flycheck-python-ruff-executable ruff)))

(defun my/python-after-pet ()
  "Configure editor tools after PET selects the project environment."
  ;; PET owns interpreter and virtualenv discovery. The language server itself
  ;; comes from the user profile so it is shared reliably between projects.
  (setq-local lsp-pyright-langserver-command
              (my/python-language-server-command)
              dape-command (my/python-dape-command))
  ;; PyPI's Ruff wheel is a generic dynamically linked executable. It exits
  ;; with code 127 from graphical Emacs on NixOS, so use the Nix-built Ruff for
  ;; editor services while keeping PET's project interpreter and Python path.
  (my/python-use-host-ruff)
  ;; Flycheck can activate after PET and ask PET to select executables again.
  ;; Append locally so this correction runs after PET's global mode hook.
  (add-hook 'flycheck-mode-hook #'my/python-use-host-ruff t t)
  ;; python-pytest builds a shell command instead of passing an argv vector.
  ;; Quote PET's absolute path because canonical project paths can contain
  ;; spaces or shell metacharacters.
  (when (and (boundp 'python-pytest-executable)
             (stringp python-pytest-executable))
    (setq-local python-pytest-executable
                (shell-quote-argument python-pytest-executable))))

(defvar-local my/python-importable-modules-cache nil
  "Importable top-level modules for the current project interpreter.")

(defun my/python-importable-modules ()
  "Return importable modules from the current PET-selected interpreter."
  (or my/python-importable-modules-cache
      (setq my/python-importable-modules-cache
            (process-lines
             python-shell-interpreter
             "-c"
             (concat
              "import pkgutil, sys; "
              "names = set(getattr(sys, 'stdlib_module_names', ())); "
              "names.update(module.name for module in pkgutil.iter_modules()); "
              "print(*sorted(names), sep='\\n')")))))

(defun my/python-sibling-modules ()
  "Return importable Python module names beside the current file."
  (when-let ((directory (and buffer-file-name
                             (file-name-directory buffer-file-name))))
    (when (file-exists-p (expand-file-name "__init__.py" directory))
      (mapcar #'file-name-base
              (seq-remove
               (lambda (file)
                 (member (file-name-base file)
                         (list "__init__" "__main__"
                               (file-name-base buffer-file-name))))
               (directory-files directory t
                                "\\`[[:alpha:]_][[:alnum:]_]*\\.py\\'"))))))

(defun my/python-add-import (statement)
  "Add import STATEMENT with the PET-selected isort executable."
  (let* ((isort (or (and (boundp 'python-isort-command)
                         python-isort-command)
                    (executable-find "isort")
                    (user-error "Install isort for automatic imports")))
         (root (if-let ((project (project-current)))
                   (expand-file-name (project-root project))
                 (expand-file-name default-directory)))
         (temporary-file (make-temp-file "emacs-python-import-" nil ".py")))
    (unwind-protect
        (progn
          (write-region nil nil temporary-file nil 'silent)
          (unless (zerop (call-process isort nil nil nil
                                       "--settings-path" root
                                       "--add-import" statement
                                       "--force-adds" temporary-file))
            (user-error "isort could not add %s" statement))
          (let ((source (current-buffer)))
            (with-temp-buffer
              (let ((formatted (current-buffer)))
                (insert-file-contents temporary-file)
                (with-current-buffer source
                  (replace-buffer-contents formatted))))))
      (delete-file temporary-file))))

(defun my/python-module-capf ()
  "Complete fuzzy module names and add the selected import."
  (unless (nth 8 (syntax-ppss))
    (let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                       (cons (point) (point))))
           (beginning (car bounds))
           (end (cdr bounds))
           (line-prefix (buffer-substring-no-properties
                         (line-beginning-position) beginning))
           (bare-import (string-match-p
                         "\\`[ \\t]*import[ \\t]+\\'" line-prefix))
           (relative-import (string-match-p
                             "\\`[ \\t]*from[ \\t]+\\.[ \\t]+import[ \\t]+\\'"
                             line-prefix))
           (other-from-import (and (string-match-p
                                    "\\`[ \\t]*from\\_>" line-prefix)
                                   (not relative-import)))
           (member-access (eq (char-before beginning) ?.)))
      ;; BasedPyright owns imported-module members and `from package import'
      ;; symbols. Offering top-level module names there only adds noise.
      (unless (or other-from-import member-access)
        (let* ((siblings (my/python-sibling-modules))
               (candidates (if relative-import
                               siblings
                             (delete-dups
                              (append siblings
                                      (my/python-importable-modules)))))
               (beginning-marker (copy-marker beginning)))
          (list beginning end candidates
                :exclusive 'no
                :company-kind (lambda (_) 'module)
                :exit-function
                (lambda (candidate status)
                  (when (eq status 'finished)
                    (cond
                     ((and bare-import (member candidate siblings))
                      (save-excursion
                        (goto-char beginning-marker)
                        (beginning-of-line)
                        (when (re-search-forward
                               (format "\\_<import[ \\t]+%s\\_>"
                                       (regexp-quote candidate))
                               (line-end-position) t)
                          (replace-match (format "from . import %s" candidate)
                                         t t))))
                     ((not (or bare-import relative-import))
                      (my/python-add-import
                       (if (member candidate siblings)
                           (format "from . import %s" candidate)
                         (format "import %s" candidate)))))))))))))

(defun my/python-completion-at-point ()
  "Merge language-server completion with fuzzy importable modules."
  (cape-wrap-super #'lsp-completion-at-point #'my/python-module-capf))

(defun my/python-completion-setup ()
  "Install fuzzy Python completion, with LSP results when available."
  (when (derived-mode-p 'python-base-mode)
    (let ((capfs (copy-sequence completion-at-point-functions))
          (python-capf (if (bound-and-true-p lsp-completion-mode)
                           #'my/python-completion-at-point
                         #'my/python-module-capf)))
      (dolist (capf '(lsp-completion-at-point
                      my/python-completion-at-point
                      my/python-module-capf))
        (setq capfs (delq capf capfs)))
      (setq-local completion-at-point-functions
                  (cons python-capf capfs)))))

(defun my/python-complete-after-import ()
  "Open completion immediately after typing an import-space."
  (when (and (eq last-command-event ?\s)
             (looking-back "\\_<import[ \\t]+" (line-beginning-position)))
    (completion-at-point)))

(defun my/python-editing-setup ()
  "Enable project-isolated IDE features in the current Python buffer."
  (setq-local lsp-auto-guess-root t
              lsp-guess-root-without-session t
              lsp-inlay-hint-enable t
              lsp-enable-snippet t
              corfu-auto-delay 0.0
              corfu-auto-trigger "."
              ;; Completing an exact module name applies its auto-import edit
              ;; before a following dot requests member completion.
              corfu-on-exact-match 'insert)
  (electric-pair-local-mode 1)
  (add-hook 'post-self-insert-hook #'my/python-complete-after-import nil t)
  ;; Module completion remains useful if a language server is unavailable.
  ;; `lsp-completion-mode-hook' upgrades it to the merged CAPF on attachment.
  (my/python-completion-setup))

(use-package pet
  :init
  ;; Register the autoloaded mode despite global use-package deferral.
  ;; PET must resolve project executables before lsp-deferred starts.
  (add-hook 'python-base-mode-hook #'pet-mode -10)
  (add-hook 'pet-after-buffer-local-vars-setup
            #'my/python-after-pet))

(add-hook 'python-base-mode-hook #'my/python-editing-setup -5)
(add-hook 'lsp-completion-mode-hook #'my/python-completion-setup)
(add-hook 'python-base-mode-hook #'lsp-deferred)

(use-package lsp-ruff
  :straight nil
  :after lsp-mode
  ;; Register Ruff before the deferred Python attachment selects clients.
  :demand t
  :custom
  (lsp-ruff-advertize-organize-imports t)
  (lsp-ruff-multi-root nil))

(use-package lsp-pyright
  :after (lsp-mode lsp-ruff)
  ;; Register BasedPyright alongside Ruff before lsp-deferred attaches either.
  :demand t
  :init
  ;; lsp-pyright captures this value while registering its dependency, so it
  ;; must be selected before the package loads rather than changed per buffer.
  (setq lsp-pyright-langserver-command
        (my/python-language-server-command))
  :custom
  ;; Editor defaults; a project typeCheckingMode takes precedence.
  (lsp-pyright-type-checking-mode
   (if (equal lsp-pyright-langserver-command "basedpyright") "all" "strict"))
  (lsp-pyright-disable-organize-imports t)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-diagnostic-mode "workspace")
  (lsp-pyright-basedpyright-inlay-hints-variable-types t)
  (lsp-pyright-basedpyright-inlay-hints-call-argument-names t)
  (lsp-pyright-basedpyright-inlay-hints-function-return-types t)
  (lsp-pyright-basedpyright-inlay-hints-generic-types t)
  ;; One process per project prevents interpreters and import paths from
  ;; leaking between unrelated Python workspaces.
  (lsp-pyright-multi-root nil)
  :config
  ;; lsp-pyright still registers most analysis options under python.analysis.
  ;; BasedPyright only accepts basedpyright.analysis; retain Python paths.
  ;; Copy the value definitions so PET's buffer-local settings stay dynamic.
  (when (equal lsp-pyright-langserver-command "basedpyright")
    (let (settings)
      (maphash (lambda (path value)
                 (when (string-prefix-p "python.analysis." path)
                   (push (cons path value) settings)))
               lsp-client-settings)
      (dolist (setting settings)
        (let ((path (concat "basedpyright.analysis."
                            (string-remove-prefix "python.analysis." (car setting)))))
          (unless (gethash path lsp-client-settings)
            (lsp-register-custom-settings (list (cons path (cdr setting)))))
          (remhash (car setting) lsp-client-settings))))))

(defun my/python-pytest-watch ()
  "Run pytest-watcher for the current PET-managed Python project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (ptw (or (pet-executable-find "ptw")
                  (user-error "Install pytest-watcher in this project")))
         ;; Keep this path raw because `mapconcat' quotes every argv element.
         (pytest (or (pet-executable-find "pytest")
                     (user-error "Install pytest in this project")))
         (default-directory root)
         (command (mapconcat #'shell-quote-argument
                             (list ptw "." "--now" "--runner" pytest)
                             " "))
         (project-name (file-name-nondirectory
                        (directory-file-name root))))
    (compilation-start command 'compilation-mode
                       (lambda (_) (format "*pytest-watch:%s*" project-name)))))

(use-package python-pytest
  :after python
  ;; C-c k is the test runner of Python and Clojure buffers: t the test
  ;; at point, f the file, a all, l the last run, w watch, k the menu.
  :bind (:map python-base-mode-map
              ("C-c k t" . python-pytest-run-def-or-class-at-point-dwim)
              ("C-c k f" . python-pytest-file-dwim)
              ("C-c k a" . python-pytest)
              ("C-c k l" . python-pytest-repeat)
              ("C-c k w" . my/python-pytest-watch)
              ("C-c k k" . python-pytest-dispatch))
  :custom
  (python-pytest-unsaved-buffers-behavior 'save-all))

(defun my/web-template-file-p ()
  "Return non-nil when the current buffer is a server-rendered template."
  (when-let* ((file buffer-file-name))
    (or (string-match-p "/templates/.*\\.html?\\'" file)
        (string-match-p "\\.\\(?:jinja\\|jinja2\\|j2\\|djhtml\\)\\'" file))))

(defun my/web-template-mode-setup ()
  "Configure Jinja templates without applying a destructive formatter."
  (when (my/web-template-file-p)
    (web-mode-set-engine "jinja")
    (when (bound-and-true-p apheleia-mode)
      (apheleia-mode -1))))

(defconst my/web-template-auto-mode-rules
  '(("/templates/.*\\.html?\\'" . web-mode)
    ("\\.\\(?:jinja\\|jinja2\\|j2\\|djhtml\\)\\'" . web-mode))
  "Auto-mode rules that must take precedence over generic HTML rules.")

(defun my/web-template-prioritize-auto-mode-rules ()
  "Move template rules ahead of generic HTML associations."
  (dolist (rule (reverse my/web-template-auto-mode-rules))
    (setq auto-mode-alist (cons rule (delete rule auto-mode-alist)))))

(my/web-template-prioritize-auto-mode-rules)

;; Loading html-ts-mode prepends its generic \\.html rule, so restore the
;; narrower template rules immediately afterwards.
(with-eval-after-load 'html-ts-mode
  (my/web-template-prioritize-auto-mode-rules))

(use-package web-mode
  :commands web-mode
  :hook (web-mode . lsp-deferred)
  :init
  (add-hook 'web-mode-hook #'my/web-template-mode-setup -10))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package datastar
  :straight (:type git
             :host github
             :repo "benjamin-asdf/datastar-el"
             :commit "434391920f2125ca27aa59892fd2d8d0a6087aa1")
  :commands (datastar-mode
             datastar-turn-on-mode
             datastar-attribute-help))

(dolist (hook '(typescript-ts-mode-hook
                tsx-ts-mode-hook
                js-mode-hook
                js-ts-mode-hook))
  (add-hook hook #'lsp-deferred))

(use-package auctex
  :hook ((LaTeX-mode . lsp-deferred)
         (LaTeX-mode . turn-on-auto-fill))
  :custom ((TeX-engine 'xetex)
           (TeX-PDF-mode t)))

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :custom ((cdlatex-math-modify-prefix [f7])))

(defun my/rust-inlay-hints ()
  "Show rust-analyzer's inlay hints in this buffer."
  (setq-local lsp-inlay-hint-enable t))

(use-package rust-mode
  :hook ((rust-mode    . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (rust-mode    . my/rust-inlay-hints)
         (rust-ts-mode . my/rust-inlay-hints)))

;; lsp-rust ships inside lsp-mode; tune rust-analyzer through the
;; matching lsp-rust-* defcustoms.
(with-eval-after-load 'lsp-rust
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-all-features t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-run-build-scripts t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-binding-mode-hints t
        lsp-rust-analyzer-closing-brace-hints t
        lsp-rust-analyzer-max-inlay-hint-length 25
        lsp-rust-analyzer-lens-enable t
        lsp-rust-analyzer-lens-implementations-enable t
        lsp-rust-analyzer-lens-references-adt-enable t
        lsp-rust-analyzer-lens-references-enum-variant-enable t
        lsp-rust-analyzer-lens-references-method-enable t
        lsp-rust-analyzer-lens-references-trait-enable t
        lsp-rust-analyzer-lens-run-enable t
        lsp-rust-analyzer-lens-debug-enable t))

(use-package cargo-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  (keymap-set cargo-minor-mode-map "C-c a" nil)
  (keymap-set cargo-minor-mode-map "C-c C" 'cargo-mode-command-map))

(use-package dockerfile-mode
  :mode (("\\Dockerfile\\'" . dockerfile-mode)
         ("\\.dockerignore\\'" . dockerfile-mode))
  ;; Without the dockerfile grammar, dockerfile-mode runs: see
  ;; `my/treesit-remap-when-ready'.
  :hook (dockerfile-ts-mode . lsp-deferred))

(use-package docker
  ;; C-c d is the debugger prefix of the language buffers.
  :bind ("C-c D" . docker)
  :config
  (let ((use-podman (and (executable-find "podman")
                         (or (not (executable-find "docker"))
                             (not my/macos-p)))))
    (if use-podman
        (setf docker-command "podman"
              docker-compose-command "podman-compose"
              docker-container-tramp-method "podman")
      (setf docker-command "docker"
            docker-compose-command (if (executable-find "docker-compose")
                                       "docker-compose"
                                     "docker")
            docker-container-tramp-method "docker"))))

(add-hook 'yaml-ts-mode-hook #'lsp-deferred)

(with-eval-after-load 'lsp-yaml
  (setq lsp-yaml-schemas
        '(("https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"
           "docker-compose*.yml" "docker-compose*.yaml" "compose*.yml" "compose*.yaml")
          ("https://json.schemastore.org/github-workflow"
           ".github/workflows/*.yml" ".github/workflows/*.yaml")
          ("https://json.schemastore.org/gitlab-ci"
           ".gitlab-ci.yml"))
        lsp-yaml-format-enable t
        lsp-yaml-completion t
        lsp-yaml-validate t))

;; Microsoft compose-language-service registered as a layered add-on
;; alongside yaml-language-server.
(with-eval-after-load 'lsp-mode
  (when-let* ((bin (executable-find "docker-compose-langserver")))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () `(,bin "--stdio")))
      :activation-fn (lambda (filename _mode)
                       (and (stringp filename)
                            (string-match-p
                             "/\\(?:docker-\\)?compose[^/]*\\.ya?ml\\'"
                             filename)))
      :priority -1
      :add-on? t
      :server-id 'docker-compose-langserver))))

(add-hook 'sh-mode-hook      #'lsp-deferred)
(add-hook 'bash-ts-mode-hook #'lsp-deferred)

(use-package lua-mode
  :hook (lua-mode . lsp-deferred))

(with-eval-after-load 'lsp-lua
  (when-let* ((lls (executable-find "lua-language-server")))
    (setq lsp-clients-lua-language-server-command (list lls)
          lsp-clients-lua-language-server-bin lls
          lsp-clients-lua-language-server-main-location lls)))

(defun my/terraform-flycheck-setup ()
  "Prefer LSP diagnostics and enable TFLint only when configured."
  (setq-local flycheck-disabled-checkers
              (cons 'terraform
                    (when (boundp 'flycheck-disabled-checkers)
                      flycheck-disabled-checkers)))
  (unless (locate-dominating-file default-directory ".tflint.hcl")
    (setq-local flycheck-disabled-checkers
                (cons 'terraform-tflint flycheck-disabled-checkers))))

(use-package terraform-mode
  :mode ("\\.tofutest\\.hcl\\'" . terraform-mode)
  :hook (terraform-mode . lsp-deferred)
  :init
  (add-hook 'terraform-mode-hook #'my/terraform-flycheck-setup -10)
  :custom (terraform-command "tofu"))

(with-eval-after-load 'flycheck
  (setq flycheck-terraform-executable "tofu")
  ;; Flycheck's project validator runs terraform, with no executable
  ;; option (added for flycheck#2360, commit 2a8f8d76), so replace it
  ;; through its public definition API.  A canary in
  ;; tests/diagnostics-test.el fails once upstream stops hard-coding
  ;; terraform; then set that option instead.
  (flycheck-define-project-checker 'terraform-validate
    "An OpenTofu project checker using `tofu validate'."
    :command '("tofu" "validate" "-json")
    :parser #'flycheck-parse-terraform-validate
    :enabled (lambda (directory)
               (directory-files directory nil "\\.tf\\'" t 1))))

(with-eval-after-load 'lsp-terraform
  (setq lsp-terraform-ls-server "tofu-ls"))

(with-eval-after-load 'lsp-diagnostics
  (require 'flycheck)
  (lsp-diagnostics-lsp-checker-if-needed)
  (flycheck-add-next-checker 'lsp '(warning . terraform-tflint) 'append))

(dolist (hook '(html-mode-hook
                html-ts-mode-hook
                css-mode-hook
                css-ts-mode-hook
                scss-mode-hook
                json-ts-mode-hook))
  (add-hook hook #'lsp-deferred))

(with-eval-after-load 'lsp-html
  (let ((alpine (expand-file-name "etc/lsp/alpine-data.json"
                                  user-emacs-directory)))
    (when (file-readable-p alpine)
      (setq lsp-html-custom-data (list alpine)))))

(use-package lsp-java
  ;; Without the java grammar, java-mode runs: see
  ;; `my/treesit-remap-when-ready'.
  :hook (java-ts-mode . lsp-deferred)
  :custom
  ;; Google Java Style: 2 space indent.
  (java-ts-indent-offset 2)
  (lsp-java-format-settings-url
   "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (lsp-java-format-settings-profile "GoogleStyle")
  (lsp-java-format-enabled t)
  (lsp-java-maven-download-sources t)
  (lsp-java-references-code-lens-enabled t)
  (lsp-java-implementations-code-lens-enabled t)
  (lsp-java-signature-help-enabled t)
  ;; Decompile jars without sources via Fernflower.
  (lsp-java-content-provider-preferred "fernflower")
  (lsp-java-import-gradle-enabled t)
  (lsp-java-autobuild-enabled t)
  (lsp-java-vmargs
   '("--add-modules=ALL-SYSTEM"
     "--add-opens" "java.base/java.util=ALL-UNNAMED"
     "--add-opens" "java.base/java.lang=ALL-UNNAMED"))
  :config
  ;; Run the jdtls launcher on PATH.  lsp-java tells that a server is
  ;; installed from the launcher jar under `lsp-java-server-install-dir',
  ;; a directory it deletes when it installs a server: give it one of its
  ;; own, whose plugins/ links to the installation's.
  (when-let* ((jdtls (executable-find "jdtls"))
              (home (file-name-directory
                     (directory-file-name
                      (file-name-directory (file-truename jdtls)))))
              (plugins (seq-find #'file-directory-p
                                 (mapcar (lambda (path) (expand-file-name path home))
                                         '("plugins" "share/java/jdtls/plugins"
                                           "libexec/plugins"))))
              (install (no-littering-expand-var-file-name "lsp-java/jdtls-on-path/"))
              (link (expand-file-name "plugins" install)))
    (make-directory install t)
    (setq lsp-java-server-install-dir install)
    ;; `lsp-update-servers' installs a server there, plugins/ included:
    ;; keep it until the user deletes it.
    (if (and (file-directory-p link) (not (file-symlink-p link)))
        (message "lsp-java runs the server installed in %s; delete it to run %s"
                 install jdtls)
      (make-symbolic-link plugins link t)
      (setq lsp-java-jdt-ls-prefer-native-command t
            lsp-java-jdt-ls-command jdtls))))

(use-package dape
  ;; Only load when first invoked — avoids the 0.5 s eager-require cost.
  :commands (dape
             dape-breakpoint-toggle
             dape-continue
             dape-next
             dape-step-in
             dape-step-out
             dape-evaluate-expression)
  :init
  ;; C-c d is the debugger prefix of Python and Java buffers, with the
  ;; same keys in each.  The keys exist as soon as the language's
  ;; mode loads, before their first use loads dape.
  (define-prefix-command 'my/debug-map)
  (define-key my/debug-map (kbd "d") #'dape)
  (define-key my/debug-map (kbd "b") #'dape-breakpoint-toggle)
  (define-key my/debug-map (kbd "c") #'dape-continue)
  (define-key my/debug-map (kbd "n") #'dape-next)
  (define-key my/debug-map (kbd "i") #'dape-step-in)
  (define-key my/debug-map (kbd "o") #'dape-step-out)
  (define-key my/debug-map (kbd "e") #'dape-evaluate-expression)
  (with-eval-after-load 'python
    (define-key python-base-mode-map (kbd "C-c d") 'my/debug-map))
  ;; Java debugs through JDTLS: C-c d d, then java-attach, or a named
  ;; configuration of `dape-configs' for the project.
  (with-eval-after-load 'java-ts-mode
    (define-key java-ts-mode-map (kbd "C-c d") 'my/debug-map))
  ;; Without the java grammar, java-mode runs: see
  ;; `my/treesit-remap-when-ready'.
  (with-eval-after-load 'cc-mode
    (define-key java-mode-map (kbd "C-c d") 'my/debug-map))
  :custom
  ;; Info buffers to the right
  (dape-buffer-window-arrangement 'right)

  :custom-face
  ;; Mark the selected stack frame independently of the editing cursor.
  (dape-source-line-face
   ((t (:background "#f0c674" :foreground "#1d1f21"
        :weight bold :extend t))))

  :config
  ;; Dape's built-in debugpy check uses a shell command, which breaks when the
  ;; PET-selected interpreter path contains spaces or parentheses. The adapter
  ;; launch itself already uses an argv vector; make its preflight equally safe.
  (dolist (name '(debugpy debugpy-module))
    (let ((config (alist-get name dape-configs)))
      (setf (plist-get config 'ensure) #'my/python-dape-ensure)))

  ;; Save buffers before starting a session.
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success.
  (add-hook 'dape-compile-hook #'kill-buffer))

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
  :hook (nix-mode . lsp-deferred)
  :bind (:map nix-mode-map
         ("C-c n r" . nix-repl)
         ("C-c n b" . nix-build)
         ("C-c n u" . nix-shell-unpack)
         ("C-c n s" . nix-shell))
  :config
  (nix-prettify-global-mode 1))

(defcustom my/nixd-flake nil
  "Directory of the NixOS flake whose options nixd completes, or nil.
Set it in `local.el'."
  :type '(choice (const :tag "None" nil) directory)
  :group 'my)

(defcustom my/nixd-flake-host (system-name)
  "The entry of the flake's nixosConfigurations that describes this machine."
  :type 'string
  :group 'my)

;; lsp-nix, inside lsp-mode, defines the nixd client and loads when the
;; first Nix buffer starts a server, after local.el: read its values then.
(with-eval-after-load 'lsp-nix
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> {}"
        lsp-nix-nixd-formatting-command ["alejandra"])
  (when my/nixd-flake
    (let ((system (format "(builtins.getFlake %S).nixosConfigurations.%s"
                          (expand-file-name my/nixd-flake) my/nixd-flake-host)))
      (setq lsp-nix-nixd-nixos-options-expr (concat system ".options")
            lsp-nix-nixd-home-manager-options-expr
            (format "%s.config.home-manager.users.%s.options"
                    system (user-login-name))))))

(use-package lsp-tailwindcss
  :after lsp-mode
  :custom
  (lsp-tailwindcss-add-on-mode t)
  ;; Reuse the self-contained Nix wrapper instead of lsp-mode's bundled
  ;; download. Its JavaScript runtime dependency is encapsulated by Nix.
  (lsp-tailwindcss-server-path
   (or (executable-find "tailwindcss-language-server") ""))
  (lsp-tailwindcss-major-modes
   '(rjsx-mode web-mode html-mode html-ts-mode mhtml-mode
     css-mode css-ts-mode scss-mode less-css-mode
     typescript-ts-mode tsx-ts-mode
     clojure-mode clojure-ts-mode clojurec-mode clojurescript-mode)))

;; htmx-lsp registered when its binary is present, scoped to buffers
;; that actually contain hx-* attributes.
(with-eval-after-load 'lsp-mode
  (when-let* ((bin (executable-find "htmx-lsp")))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () bin))
      :activation-fn (lambda (_filename major-mode)
                       (and (memq major-mode '(html-mode html-ts-mode
                                               mhtml-mode web-mode
                                               clojure-mode clojure-ts-mode
                                               clojurec-mode clojurescript-mode))
                            (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "hx-" 5000 t))))
      :priority -1
      :add-on? t
      :server-id 'htmx-lsp))))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$"   . jinx-correct)    ; correct word at point
         ("C-M-$" . jinx-languages)) ; change active languages
  :custom
  ;; Space-separated enchant locale codes checked simultaneously.
  ;; Latin uses aspell-la; Chinese has no hunspell dict (handled below).
  (jinx-languages "en_GB fr_FR pl_PL es_ES la"))

(defun my/flycheck-chain-prose-checkers ()
  "Chain the defined prose checkers after the checker a buffer selects.
LTeX+ and lsp-mode each select their checker for a buffer, and
Flycheck runs only the selected checker, then the chain of its next
checkers.  Link LTeX+ and Grammalecte after lsp-mode, then org-lint
and Grammalecte after LTeX+, then Grammalecte after org-lint, so each
reports whichever checker the buffer selects.  A checker is defined
when its package first needs it, so run again each time one is;
linking again changes nothing."
  (when (flycheck-valid-checker-p 'lsp-ltex-plus)
    ;; Flycheck runs a next checker only if it is registered.
    (add-to-list 'flycheck-checkers 'lsp-ltex-plus t))
  (pcase-dolist (`(,checker . ,next)
                 '((lsp . lsp-ltex-plus) (lsp . grammalecte)
                   (lsp-ltex-plus . org-lint) (lsp-ltex-plus . grammalecte)
                   (org-lint . grammalecte)))
    (when (and (flycheck-valid-checker-p checker)
               (flycheck-valid-checker-p next))
      (flycheck-add-next-checker checker (cons t next) t))))

(defun my/flycheck-check-again ()
  "Check the buffer once more, now that its check has finished.
One-shot, on the buffer's `flycheck-after-syntax-check-hook'."
  (remove-hook 'flycheck-after-syntax-check-hook #'my/flycheck-check-again t)
  (flycheck-buffer))

(defun my/flycheck-check-again-after-lsp-publish ()
  "Check lsp-mode's buffers again once their running check has finished.
On `lsp-diagnostics-updated-hook'.  lsp-mode asks Flycheck to check
its buffers when a server publishes, and Flycheck ignores the request
while a check runs.  A check that chains Grammalecte after lsp-mode
runs for a while; a publish meanwhile, such as marksman's broken
link, would otherwise show only at the next check.  Only the shown
buffers of the publishing server, which lsp-mode's own request is
limited to: another server's publishes must not check them again."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (get-buffer-window buffer)
                 (bound-and-true-p lsp-diagnostics-mode)
                 (memq lsp--cur-workspace lsp--buffer-workspaces)
                 (bound-and-true-p flycheck-mode)
                 (flycheck-running-p))
        (add-hook 'flycheck-after-syntax-check-hook
                  #'my/flycheck-check-again nil t)))))

(defun my/flycheck-select-lsp-over-ltex-plus ()
  "Select lsp-mode's checker again where LTeX+ has just taken its place.
On `lsp-ltex-plus-mode-hook'.  Turning LTeX+ on selects its checker,
as after `lsp-ltex-plus-restart-server'.  Where lsp-mode serves the
buffer, its checker must stay selected: it chains LTeX+'s, and nothing
chains it back."
  (when (and (bound-and-true-p lsp-diagnostics-mode)
             (eq flycheck-checker 'lsp-ltex-plus))
    (setq flycheck-checker 'lsp)))

;; lsp-mode defines its checker in the first buffer it serves; link it
;; at once, before that buffer's first check.
(with-eval-after-load 'lsp-diagnostics
  (require 'flycheck)
  (lsp-diagnostics-lsp-checker-if-needed)
  (my/flycheck-chain-prose-checkers)
  (add-hook 'lsp-diagnostics-updated-hook
            #'my/flycheck-check-again-after-lsp-publish))

(use-package lsp-ltex-plus
  :straight (lsp-ltex-plus :type git :host github
                          :repo "ltex-plus/emacs-ltex-plus")
  ;; Its checker is defined when the first buffer attaches to it.
  :hook ((lsp-ltex-plus-mode . my/flycheck-chain-prose-checkers)
         (lsp-ltex-plus-mode . my/flycheck-select-lsp-over-ltex-plus))
  :init
  ;; This client owns its JSON-RPC connection; keep legacy LTeX clients off.
  (with-eval-after-load 'lsp-mode
    (setq lsp-client-packages
          (remq 'lsp-ltex-plus (remq 'lsp-ltex lsp-client-packages)))
    (add-to-list 'lsp-disabled-clients 'ltex-ls))
  (lsp-ltex-plus-enable-for-modes
   :restrict-to '(text-mode bibtex-mode context-mode latex-mode LaTeX-mode
                 markdown-mode gfm-mode org-mode rst-mode))
  ;; Keep marksman when removing the old broad text-mode hook.  LaTeX
  ;; starts Texlab from its own section; no server serves BibTeX.
  (add-hook 'markdown-mode-hook #'lsp-deferred)
  :custom
  (lsp-ltex-plus-language "auto")
  (lsp-ltex-plus-additional-rules-mother-tongue "fr")
  (lsp-ltex-plus-diagnostic-severity "information")
  (lsp-ltex-plus-diagnostics-provider 'flycheck)
  (lsp-ltex-plus-check-fileless-buffers nil)
  (lsp-ltex-plus-check-comint-input nil))

;; global-flycheck-mode activates flycheck in every buffer that has a
;; suitable checker.  In LSP-managed buffers lsp-diagnostics-mode
;; pipes server diagnostics into flycheck (see the LSP section).
;; flycheck-grammalecte adds French grammar checks in French prose.
(use-package flycheck
  ;; At the end of startup, before the first file: an idle timer would
  ;; leave a file opened first unchecked until Emacs is idle.
  :hook (after-init . global-flycheck-mode)
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

(defun my/grammalecte-french-buffer-p ()
  "Return non-nil when the current buffer declares French as its language.
Org's #+LANGUAGE: fr keyword, or a buffer-local `jinx-languages' that
starts with French, as a file-local variable sets it."
  (or (and (derived-mode-p 'org-mode)
           (equal (cadar (org-collect-keywords '("LANGUAGE"))) "fr"))
      (and (local-variable-p 'jinx-languages)
           (string-prefix-p "fr" jinx-languages))))

(defun my/grammalecte-use-system-package ()
  "Use the Python package of the Grammalecte on `exec-path', if any.
The package lies beside grammalecte-cli.py, rather than in a copy
downloaded into flycheck-grammalecte.  flycheck-grammalecte puts the
parent of `grammalecte-python-package-directory' on PYTHONPATH for
every Python process Emacs starts, so that directory is a private link
to the package: its own site-packages would put a virtual
environment's or pip's other packages ahead of a project's.  Run when
the package loads, and again once exec-path-from-shell has set
`exec-path': a GUI Emacs started from the macOS Dock lacks the
profile's directory before.  On a change, Flycheck asks again whether
Grammalecte is installed in the buffers Grammalecte could check."
  (when-let* ((cli (executable-find "grammalecte-cli.py"))
              (package (car (file-expand-wildcards
                             (expand-file-name
                              "../lib/python*/site-packages/grammalecte"
                              (file-name-directory (file-truename cli))))))
              (link (no-littering-expand-var-file-name "grammalecte/grammalecte")))
    (unless (and (equal grammalecte-python-package-directory link)
                 (equal (file-symlink-p link) package))
      (make-directory (file-name-directory link) t)
      (make-symbolic-link package link t)
      (setq grammalecte-python-package-directory link)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (bound-and-true-p flycheck-mode)
                     (flycheck-checker-supports-major-mode-p 'grammalecte))
            (flycheck-reset-enabled-checker 'grammalecte)))))))

(defvar my/grammalecte-version nil
  "The package `grammalecte--version' probed last, and what it returned.")

(defun my/grammalecte-version-once (probe)
  "Return what PROBE, `grammalecte--version', finds, probing once per package.
Flycheck asks it in each prose buffer whether Grammalecte is
installed, before the predicate that limits Grammalecte to French;
each probe runs python3 and blocks Emacs for about 0.1 s."
  (let ((package (file-truename grammalecte-python-package-directory)))
    (unless (equal (car my/grammalecte-version) package)
      (setq my/grammalecte-version (cons package (funcall probe))))
    (cdr my/grammalecte-version)))

(use-package flycheck-grammalecte
  ;; Its checker exists only once its setup has run (CF-05).
  :after flycheck
  :demand t
  :custom
  ;; Spelling is handled by jinx; disable grammalecte's spell pass.
  (flycheck-grammalecte-report-spellcheck nil)
  (flycheck-grammalecte-report-grammar    t)
  (flycheck-grammalecte-report-apos       t)
  (flycheck-grammalecte-predicate #'my/grammalecte-french-buffer-p)
  ;; The package comes from outside Emacs: never offer to replace it.
  (grammalecte-check-upstream-version-delay 0)
  :config
  (advice-add 'grammalecte--version :around #'my/grammalecte-version-once)
  (my/grammalecte-use-system-package)
  (with-eval-after-load 'exec-path-from-shell
    (advice-add 'exec-path-from-shell-initialize :after
                #'my/grammalecte-use-system-package))
  (flycheck-grammalecte-setup)
  ;; The setup binds its correction menu to <mouse-3> in every Flycheck
  ;; buffer; nil there keeps right-click's global `mouse-save-then-kill'.
  (keymap-set flycheck-mode-map "<mouse-3>" nil)
  ;; The setup registers Grammalecte first, so a buffer LTeX+ does not
  ;; check would select it instead of its own checker, such as org-lint.
  ;; Registered last, it runs after that checker, which the next form
  ;; chains it to.
  (setq flycheck-checkers
        (append (remq 'grammalecte flycheck-checkers) '(grammalecte)))
  (my/flycheck-chain-prose-checkers))

;; Pinyin input method for Simplified and Traditional Chinese.
;; Activate/deactivate with C-\: it is `default-input-method', set
;; with the other global settings in the User experience section.
(use-package pyim
  :custom
  (pyim-default-scheme 'quanpin)  ; full pinyin; alternatives: 'wubi, 'cangjie
  :config
  ;; pyim-basedict: a minimal Pinyin dictionary (~100 k entries).
  ;; For a richer experience install pyim-greatdict separately.
  ;; Declared here, it is installed and loaded the first time pyim loads.
  (use-package pyim-basedict
    :demand t
    :config (pyim-basedict-enable)))

;; Insert correct whitespace between CJK and Latin/ASCII characters.
;; "Hello世界" becomes "Hello 世界" on save.
;;
;; pangu-spacing-mode unconditionally registers a jit-lock callback that
;; runs a CJK/Latin boundary regex on every fontification pass.  For
;; pure-ASCII buffers (the vast majority) this is wasted work.  The
;; guard below activates the mode only when >= 5% of the buffer is CJK,
;; and installs a cheap after-change-functions hook to lazily activate
;; it if CJK text is later inserted.

(defun my/buffer-cjk-fraction ()
  "Return the fraction of buffer characters that are CJK.
Uses Emacs character categories: \\cC (two-byte Chinese), \\cj (Japanese),
\\ck (Korean)."
  (if (= (point-min) (point-max))
      0.0
    (let ((cjk-count (how-many "\\cC\\|\\cj\\|\\ck" (point-min) (point-max))))
      (/ (float cjk-count) (- (point-max) (point-min))))))

(defun my/maybe-enable-pangu-spacing ()
  "Enable `pangu-spacing-mode' only when >= 5% of buffer content is CJK.
Also installs a self-removing `after-change-functions' hook so that if CJK
text is later inserted into a buffer that was initially below the threshold,
the mode activates lazily."
  (if (>= (my/buffer-cjk-fraction) 0.05)
      (pangu-spacing-mode 1)
    (add-hook 'after-change-functions
              #'my/pangu-spacing-lazy-activate nil t)))

(defun my/pangu-spacing-lazy-activate (beg end _len)
  "Activate pangu-spacing if CJK text was inserted into this buffer."
  (when (save-excursion
          (goto-char beg)
          (re-search-forward "\\cC\\|\\cj\\|\\ck" end t))
    (pangu-spacing-mode 1)
    (remove-hook 'after-change-functions
                 #'my/pangu-spacing-lazy-activate t)))

(use-package pangu-spacing
  :hook (text-mode . my/maybe-enable-pangu-spacing)
  :custom
  ;; Write real spaces to the file rather than display-only overlays.
  ;; The actual insertion happens on save (write-file-hooks), not on
  ;; every keystroke; jit-lock only maintains display overlays.
  (pangu-spacing-real-insert-separtor t))

;; CJK Han fallback: Noto Sans CJK, appended after all other fontset entries.
;; Traditional Chinese (zh-TW) first; the first matching font per codepoint wins.
;; Nerd Font PUA and Braille fallbacks are configured in the **** Font fallbacks
;; sub-section of *** Fonts.
(defun my/add-cjk-font-fallbacks ()
  "Draw Han characters with Noto Sans CJK, Traditional then Simplified."
  (set-fontset-font t 'han (font-spec :family "Noto Sans CJK TC") nil 'append)
  (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC") nil 'append))

(add-hook 'emacs-startup-hook #'my/add-cjk-font-fallbacks)

;; Defer server-start to an idle timer so it doesn't add latency before
;; the first frame is shown.  1 s is enough for all startup hooks to
;; complete; the server is ready before the user can type a command.
(run-with-idle-timer 1 nil
                     (lambda ()
                       (require 'server)
                       (unless (server-running-p)
                         (server-start))))

(use-package devdocs)

(when my/linux-p
  ;; Linux only, inherited from the old Linux-only email block.
  (setq shr-use-colors nil))

(use-package restclient)

(defun my/btop ()
  "Open a gterm buffer running btop."
  (interactive)
  (let ((buf-name "*gterm* btop"))
    (if-let* ((buf (get-buffer buf-name)))
        (switch-to-buffer buf)
      (gterm)
      (rename-buffer buf-name)
      (gterm-send-string "btop; exit 0\n"))))

(defun my/terminal-send (string)
  "Send STRING to the shell of the current eat, mistty or gterm buffer."
  (cond ((derived-mode-p 'eat-mode) (eat-term-send-string eat-terminal string))
        ((derived-mode-p 'mistty-mode) (mistty-send-string string))
        ((derived-mode-p 'gterm-mode) (gterm-send-string string))
        (t (user-error "Not an eat, mistty or gterm buffer"))))

(defun my/terminal-backward-word ()
  "Move the shell's cursor back one word: send ESC b."
  (interactive)
  (my/terminal-send "\eb"))

(defun my/terminal-forward-word ()
  "Move the shell's cursor forward one word: send ESC f."
  (interactive)
  (my/terminal-send "\ef"))

(defun my/terminal-backward-kill-word ()
  "Delete the word before the shell's cursor: send ESC DEL."
  (interactive)
  (my/terminal-send "\e\d"))

(defun my/terminal-edit-command-line ()
  "Edit the shell's command line in Emacs: send C-x C-e.
zsh's `edit-command-line' widget then opens it with $VISUAL, an
emacsclient, and reads the edited line back once the buffer is done."
  (interactive)
  (my/terminal-send "\C-x\C-e"))

(defun my/terminal-send-meta-key ()
  "Send the last key's character to the shell with Meta: ESC, then it.
Bound to Meta and a letter, such as M-c for fzf's directory widget."
  (interactive)
  (my/terminal-send (string ?\e (event-basic-type last-command-event))))

(defun my/terminal-send-backtab ()
  "Send Shift-Tab to the shell: CSI Z, as fzf's multi-select expects."
  (interactive)
  (my/terminal-send "\e[Z"))

(use-package eat
  :straight (eat :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :custom ((eat-term-scrollback-size (* 100 1024 1024))
           (eat-enable-directory-tracking t)
           (eat-enable-shell-prompt-annotation nil))
  :bind (:map eat-mode-map
         ("C-k" . eat-reset)
         ("M-p" . eat-previous-shell-prompt)
         ("M-n" . eat-next-shell-prompt)
         ;; Semi-char mode leaves C-x and the Meta arrows to Emacs, and
         ;; sends ^H for C-<backspace>; send what zsh binds instead, as
         ;; mistty and gterm do.
         :map eat-semi-char-mode-map
         ("M-<left>" . my/terminal-backward-word)
         ("M-<right>" . my/terminal-forward-word)
         ("C-<backspace>" . my/terminal-backward-kill-word)
         ("C-x C-e" . my/terminal-edit-command-line)))

(use-package mistty
  :straight (:host github :repo "szermatt/mistty")
  :bind (("C-c t" . mistty)
         ("C-c T" . mistty-other-window)
         :map mistty-prompt-map
         ;; RET runs the command; C-j inserts a newline for multi-line editing.
         ("RET"        . mistty-send-command)
         ("C-j"        . mistty-newline)
         ;; Forward fzf keys to the shell (C-r history, C-t file, M-c cd).
         ;; Shadows isearch-backward, transpose-chars, capitalize-word
         ;; in the prompt zone; acceptable trade-off at a shell prompt.
         ("C-r"       . mistty-send-key)
         ("C-t"       . mistty-send-key)
         ("M-c"       . mistty-send-key)
         ;; zsh's edit-command-line widget.  `mistty-send-key' fails on
         ;; a sequence of two keys.
         ("C-x C-e"   . my/terminal-edit-command-line)
         ;; Pass navigation keys to the shell rather than Emacs.
         ("M-r"       . mistty-send-key)
         ("M-s"       . mistty-send-key)
         ;; Word motion as zsh binds it, ESC b and ESC f.
         ;; `mistty-send-key' would send the xterm codes \e[1;3D and
         ;; \e[1;3C, which zsh leaves unbound.
         ("M-<left>"  . my/terminal-backward-word)
         ("M-<right>" . my/terminal-forward-word)
         ;; M-<up> / M-<down> are intentionally NOT overridden here.
         ;; The default mistty-mode-map already binds them to
         ;; mistty-send-C-p / mistty-send-C-n, which send C-p / C-n
         ;; (readline previous/next history).  Binding them to
         ;; mistty-send-key would send xterm escape codes (\e[1;3A)
         ;; that most shells don't understand, producing literal ";3A".
         ))

(use-package gterm
  :straight (:host github :repo "rwc9u/emacs-libgterm" :files ("*"))
  :init
  ;; Point source-dir at the repos checkout so zig build finds
  ;; vendor/ghostty and the existing zig-out artefacts.  The
  ;; straight/build/ copy lacks a working emacs-module.h detection on
  ;; NixOS because the Zig-side fallback is a macOS path.  The
  ;; checkout is under `straight-base-dir', which is not always
  ;; `user-emacs-directory': test sessions have their own.
  (setq gterm-source-dir (expand-file-name "straight/repos/emacs-libgterm/"
                                           straight-base-dir)
        gterm-module-path (expand-file-name
                           (concat "zig-out/lib/libgterm-module" module-file-suffix)
                           gterm-source-dir)
        ;; /bin/zsh does not exist on NixOS; find zsh via PATH.
        gterm-shell (or (executable-find "zsh") (getenv "SHELL") "/bin/sh"))

  ;; Loading gterm.el without its module builds it, after cloning
  ;; Ghostty, or asks to, before any advice on `gterm' could run: the
  ;; autoload of `gterm' loads gterm.el first.  So `gterm' stands in
  ;; for that autoload, and the module is built on request only.
  (defun my/gterm-compile-module ()
    "Build gterm's Zig module, cloning Ghostty when missing, then load gterm.
Run it once after installing gterm: `gterm' refuses to start until
the module exists.  It builds and loads a Debug module; rebuild it
with zig build -Doptimize=ReleaseSafe in `gterm-source-dir', then
restart Emacs."
    (interactive)
    ;; `dlet': gterm.el reads it while it loads; a lexical `let' fails.
    (dlet ((gterm-always-compile-module t))
      (require 'gterm)))

  (unless (featurep 'gterm)
    (defalias 'gterm
      (lambda ()
        "Create a new gterm terminal buffer, once gterm's module is built.
Without the module, signal a `user-error' that names
`my/gterm-compile-module', and leave gterm.el unloaded: loading it
would build the module.  gterm.el replaces this stand-in for its
autoload with the real `gterm' when it loads.  Completing a gterm-
name in C-h f or C-h v still loads gterm.el."
        (interactive)
        (unless (file-exists-p gterm-module-path)
          (user-error "gterm: no module at %s; build it with M-x my/gterm-compile-module"
                      gterm-module-path))
        (require 'gterm)
        (call-interactively #'gterm))))

  ;; macOS 26 SDK workaround: the default SDK (MacOSX.sdk) may lack
  ;; arm64-macos stubs (only arm64e-macos), breaking Zig's linker.
  ;; Detect this at compile time and shim xcrun to return a macOS 15
  ;; SDK that still ships arm64-macos stubs.  None of it is defined
  ;; elsewhere than on macOS.
  (when my/macos-p
    (defun my/gterm--macos15-sdk-path ()
      "Return the newest macOS 15.x SDK path, or nil if none found."
      (let ((sdk-dir "/Library/Developer/CommandLineTools/SDKs/"))
        (when (file-directory-p sdk-dir)
          (car (last (sort (file-expand-wildcards
                            (concat sdk-dir "MacOSX15*.sdk"))
                           #'string<))))))

    (defun my/gterm--sdk-needs-workaround-p ()
      "Return non-nil if the default macOS SDK lacks arm64-macos stubs."
      (let ((default-tbd
             (expand-file-name
              "usr/lib/libSystem.tbd"
              "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk")))
        (and (file-exists-p default-tbd)
             (with-temp-buffer
               (insert-file-contents default-tbd nil 0 500)
               (not (string-match-p "arm64-macos" (buffer-string)))))))

    (defun my/gterm-compile-with-sdk-workaround (orig-fn &rest args)
      "Advice around `gterm-module-compile' to shim xcrun on macOS 26+.
Creates a temporary xcrun wrapper that returns a macOS 15.x SDK path,
prepends it to PATH, and cleans up afterwards."
      (if-let* (((my/gterm--sdk-needs-workaround-p))
                (sdk (my/gterm--macos15-sdk-path))
                (shim-dir (make-temp-file "gterm-xcrun-" t)))
          (let ((shim-file (expand-file-name "xcrun" shim-dir)))
            (with-temp-file shim-file
              ;; Zig calls `xcrun --sdk macosx --show-sdk-path`.  Redirect
              ;; only macOS SDK queries; let other SDK types (iphoneos etc.)
              ;; fall through to the real xcrun.
              (insert "#!/bin/sh\n"
                      "case \"$*\" in\n"
                      "  *macosx*--show-sdk-path*|*--show-sdk-path*macosx*)\n"
                      "    echo '" sdk "' ;;\n"
                      "  *) exec /usr/bin/xcrun \"$@\" ;;\n"
                      "esac\n"))
            (set-file-modes shim-file #o755)
            (let ((process-environment
                   (cons (concat "PATH=" shim-dir ":" (getenv "PATH"))
                         process-environment)))
              (unwind-protect
                  (apply orig-fn args)
                (delete-directory shim-dir t))))
        (apply orig-fn args)))

    (advice-add 'gterm-module-compile :around
                #'my/gterm-compile-with-sdk-workaround))
  :config
  ;; Upstream bug: `gterm-mode-map' binds (vector 'control c), a
  ;; two-event sequence [control <char>], instead of the event C-<char>,
  ;; so every Ctrl+letter from its loop is dead.  It regressed in
  ;; https://github.com/rwc9u/emacs-libgterm/pull/5 (56c0c69), which
  ;; replaced the working (vector (list 'control c)).  Rebind them; the canary scenario of
  ;; tests/scenarios/l3-pty-gterm.el fails once upstream binds the
  ;; events correctly, and this loop can go.  Exclude letters with
  ;; dedicated bindings: C-c (prefix), C-g (quit), C-h (help), C-i/TAB,
  ;; C-m/RET, C-q (send-next-key), C-x (prefix), C-y (yank).
  (cl-loop for c from ?a to ?z
           unless (memq c '(?c ?g ?h ?i ?m ?q ?x ?y))
           do (define-key gterm-mode-map
                          (kbd (format "C-%c" c))
                          #'gterm-send-ctrl-key))
  ;; Meta+letter: send ESC + letter for word navigation and fzf cd.
  ;; M-f/M-b (word nav), M-d (kill-word), M-c (fzf cd widget).
  (dolist (c '(?b ?c ?d ?f))
    (define-key gterm-mode-map (kbd (format "M-%c" c)) #'my/terminal-send-meta-key))
  ;; Word keys as in eat and mistty; upstream sends the xterm codes
  ;; \e[1;3D and \e[1;3C for M-<left> and M-<right>, which zsh leaves
  ;; unbound, and ^H for C-<backspace>.
  (define-key gterm-mode-map (kbd "M-DEL") #'my/terminal-backward-kill-word)
  (define-key gterm-mode-map (kbd "C-<backspace>") #'my/terminal-backward-kill-word)
  (define-key gterm-mode-map (kbd "M-<left>") #'my/terminal-backward-word)
  (define-key gterm-mode-map (kbd "M-<right>") #'my/terminal-forward-word)
  (define-key gterm-mode-map (kbd "<backtab>") #'my/terminal-send-backtab)
  ;; zsh's edit-command-line: C-x C-e as in eat and mistty, and C-c C-e.
  (define-key gterm-mode-map (kbd "C-x C-e") #'my/terminal-edit-command-line)
  (define-key gterm-mode-map (kbd "C-c C-e") #'my/terminal-edit-command-line)

  ;; gterm's module reads the shell's output but never answers a
  ;; terminal query, and ignores OSC 7.  These run after
  ;; `gterm--filter' has fed OUTPUT to the module.  A sequence split
  ;; across two reads is missed; both are short and written at once.
  ;; Upstream issues, drafted in plan 091, not yet filed at
  ;; https://github.com/rwc9u/emacs-libgterm: "No reply to CSI 6 n"
  ;; and "OSC 7 is ignored".  Expiry: the CPR cell of
  ;; tests/scenarios/l3-pty-gterm.el fails on a second reply once
  ;; upstream answers; then drop `my/gterm-report-cursor'.  Once
  ;; upstream's filter sets `default-directory' on OSC 7, check with
  ;; this advice removed that the cd cell still passes, then drop it.
  (defun my/gterm-report-cursor (process output)
    "Answer a cursor position query, CSI 6 n, in OUTPUT of gterm's PROCESS.
fzf asks where the cursor is before it draws its list below the
prompt, and waits for the answer.  Advice after `gterm--filter'."
    (when (and (string-search "\e[6n" output)
               (buffer-live-p (process-buffer process)))
      (pcase-let ((`(,row . ,column) (gterm-cursor-pos
                                      (buffer-local-value 'gterm--term
                                                          (process-buffer process)))))
        (process-send-string process (format "\e[%d;%dR" (1+ row) (1+ column))))))

  (defun my/gterm-track-directory (process output)
    "Follow the directories the shell reports with OSC 7 in OUTPUT of PROCESS.
Set the `default-directory' of gterm's PROCESS buffer to the path of
the last report, if it is a local directory here.  Unlike eat and mistty,
it ignores the host and takes the path as sent, not percent-decoded,
as the shell hook of the Host ZSH setup sends it.  Advice after
`gterm--filter'."
    (let ((start 0)
          (directory nil))
      (while (string-match (rx "\e]7;file://" (* (not (any "/")))
                               (group "/" (* (not (any "\a\e"))))
                               (or "\a" "\e\\"))
                           output start)
        (setq directory (decode-coding-string (match-string 1 output) 'utf-8)
              start (match-end 0)))
      ;; Any program can print a report: a remote path would make
      ;; `file-directory-p' open a TRAMP connection from this filter.
      (when (and directory (not (file-remote-p directory))
                 (file-directory-p directory)
                 (buffer-live-p (process-buffer process)))
        (with-current-buffer (process-buffer process)
          (setq default-directory (file-name-as-directory directory))))))

  (advice-add 'gterm--filter :after #'my/gterm-report-cursor)
  (advice-add 'gterm--filter :after #'my/gterm-track-directory))

(use-package daemons)

(defun my/garamond-font ()
  "Set a Garamond font for nov-mode."
  (face-remap-add-relative 'variable-pitch :family "Garamond" :height 160))

;; centered-cursor-mode is used only here, to keep the reading line
;; vertically centred while paging through long-form prose.
(use-package centered-cursor-mode)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . centered-cursor-mode)
         (nov-mode . my/garamond-font)))

(use-package ready-player
  :defer 2
  :config (ready-player-mode +1)
  :custom ((ready-player-my-media-collection-location "~/Music/")))

(use-package time-zones)

(use-package mcp-server
  :straight (:type git
             :host github
             :repo "rhblind/emacs-mcp-server"
             :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :custom (mcp-server-socket-directory
           (no-littering-expand-var-file-name "mcp-server/"))
  :init
  ;; Not on `emacs-startup-hook': starting the server loads every tool
  ;; module, and with them Org and its agenda, clock and capture.  Idle
  ;; time counts from the end of startup, so this stays off the path to
  ;; the first frame, and runs after the Emacs server's one-second idle
  ;; timer.  A server already started, by hand or from local.el, is left
  ;; alone: starting it again signals an error.
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (unless (bound-and-true-p mcp-server-running)
                           (mcp-server-start-unix)))))

(use-package esup
  ;; Sometimes an error happens. It is fixed with:
  :custom (esup-depth 0))

;; Load machine-local overrides — gitignored, never committed.
(let ((f (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p f)
    (load f nil :nomessage)))

(provide 'init)
;;; init.el ends here
