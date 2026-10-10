;;; t1.el --- The session checks (T1) of plan 086 -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `tests/integrity/session/run' in a daemon session that 085's
;; runner starts as for a scenario file, with the fixture `local.el' of
;; `tests/integrity/fixtures/session/local/' in its init directory.
;;
;; Each spec runs one check and judges its subjects against the
;; known-defects ledger (`tests/integrity/known-defects/'): a listed
;; defect is tolerated, an unlisted one fails, and a listed one that is
;; no longer found fails as stale.  The specs "at startup" look at the
;; session as the runner leaves it; the others first load every
;; configured feature (`my/t1-force-load').  Actions run inside
;; `buttercup-suppress-warning-capture', so their warnings reach
;; *Warnings* as in a real session, instead of buttercup's own buffer.
;; The expected values come from the requirements: `fill-column' 120,
;; one sanityinc theme per appearance (CF-02), no Emacs-side pinentry
;; (D12), and agenda files that follow `local.el' (CF-13).  Each of
;; these value checks then runs again with the fix a user would make
;; applied, and must report nothing: a check that cannot go silent
;; would pin today's defect forever.  The italic comments of D10 are
;; checked in `t1-gui.el', which needs a graphical frame.

;;; Code:

(require 'buttercup)
(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path directory)
  (add-to-list 'load-path (expand-file-name ".." directory))
  (add-to-list 'load-path (expand-file-name "../static" directory)))
(require 't1-checks)
(require 't0-checks)

(defvar epg-pinentry-mode)
(defvar org-agenda-files)
(defvar auto-dark-themes)
(defvar my/gtd-file)

(defconst my/t1-lazy-features
  '(cider dape docker eat gterm lsp-mode magit mistty mu4e pyim treemacs treesit-auto)
  "Features that must load on first use, never at startup.
Each starts external tools or is large, and README.org defers it;
mu4e is gone since plan 087.")

(defvar my/test-session-startup-features)
(defconst my/t1-deferred-features
  '(mcp-server org org-agenda org-capture org-clock org-pcomplete sh-script)
  "Features startup must not load: an idle timer loads them after it.
Plan 098 starts the MCP server, whose tools load Org and through it
`sh-script', once Emacs is idle.  The runner fires that timer before
the specs, so these are judged against the features of the end of
startup.")

(defconst my/t1-document-keymaps
  '(("docs/EMACS-PYTHON-WORKBOOK.org" org-mode-map prog-mode-map python-base-mode-map)
    ("docs/gtd-getting-started.org" org-mode-map org-capture-mode-map)
    ("docs/org-tasks.org" org-mode-map))
  "The keymaps of the documents that plan 086 may not change: (FILE . MAPS).
A document names the keymaps of the modes it is about on a
#+KEYMAPS: line; until plan 095 adds those lines, these stand in.")

(let* ((forms (append (my/integrity-forms (expand-file-name "early-init.el" my/t1-root))
                      (my/integrity-forms (expand-file-name "init.el" my/t1-root))))
       (bindings (my/integrity-bindings forms))
       (maps (seq-uniq (mapcar (lambda (row) (plist-get row :map)) bindings)))
       (entries (my/integrity-ledger-entries
                 (expand-file-name "tests/integrity/known-defects/" my/t1-root)))
       (startup-remaps (list (copy-alist major-mode-remap-alist)
                             (copy-alist major-mode-remap-defaults)))
       (failures nil)
       (skipped nil)
       (load-noise nil)
       (variables nil))

  (describe "At startup"
    (it "shows no warning and logs no error"
      (my/t1-judge 'startup-noise
                   (append (my/t1-buffer-lines "*Warnings*")
                           (my/t1-error-lines (my/t1-buffer-lines (messages-buffer))))
                   entries))

    (it "has not loaded the features meant to load on first use"
      (my/t1-judge 'loaded-at-startup
                   (my/t1-loaded-features my/t1-lazy-features features) entries))

    (it "has left to idle time the features it defers past startup"
      ;; `init' is the last feature init.el provides: without it the
      ;; snapshot was not taken at the end of startup.
      (expect (memq 'init my/test-session-startup-features) :to-be-truthy)
      (my/t1-judge 'loaded-during-startup
                   (my/t1-loaded-features my/t1-deferred-features
                                          my/test-session-startup-features)
                   entries)))

  (describe "With every configured feature loaded"
    (before-all
      (buttercup-suppress-warning-capture
        (let ((warnings (length (my/t1-buffer-lines "*Warnings*")))
              (messages (with-current-buffer (messages-buffer) (point-max))))
          (setq failures (my/t1-force-load forms)
                skipped (append (mapcar #'car my/t1-deny-list) (mapcar #'car failures))
                load-noise (append (nthcdr warnings (my/t1-buffer-lines "*Warnings*"))
                                   (my/t1-error-lines
                                    (my/t1-buffer-lines (messages-buffer) messages))))
          ;; A variable may be declared by a library of its package that
          ;; nothing loaded: load the libraries of those to check, then
          ;; classify them.
          (let* ((set (my/integrity-set-variables forms))
                 (unrun (my/t1-unrun-variables forms))
                 (denied (my/t1-load-variable-libraries
                          (seq-keep (pcase-lambda (`(,variable . ,package))
                                      (and (not (memq package skipped))
                                           (not (memq variable unrun))
                                           variable))
                                    set))))
            (setq variables (my/t1-variable-defects set skipped (append unrun denied)))
            (message "T1: variables not checked: %S" (plist-get variables :not-checked))))))

    (it "loads every configured feature"
      (my/t1-judge 'cannot-load
                   (mapcar (lambda (failure) (symbol-name (car failure))) failures)
                   entries))

    (it "loads them without warnings or errors"
      (my/t1-judge 'load-noise load-noise entries))

    (it "judges every binding, unless a denied or failed package explains why not"
      (expect (my/t1-unjudged-bindings bindings skipped) :to-equal nil))

    (it "binds only commands"
      (my/t1-judge 'dangling-binding (my/t1-dangling-bindings maps) entries))

    (it "documents every command it binds"
      (my/t1-judge 'undocumented-command (my/t1-undocumented-commands bindings) entries))

    (it "binds no mu4e command"
      (my/t1-judge 'mu4e-binding (my/t1-mu4e-bindings bindings) entries))

    (it "sets only declared variables"
      (my/t1-judge 'undeclared-variable (plist-get variables :undeclared) entries))

    (it "sets no obsolete variable"
      (my/t1-judge 'obsolete-variable (plist-get variables :obsolete) entries))

    (it "names in :commands only commands its package defines"
      (my/t1-judge 'foreign-command (my/t1-foreign-commands forms skipped) entries))

    (it "keeps the global modes it turns on on, after the first command"
      (buttercup-suppress-warning-capture
        (expect (my/t1-press '((:keys "M-x") (:type "ignore") (:keys "<return>")))
                :to-equal nil)
        (my/t1-judge 'global-mode-off (my/t1-global-modes-off forms) entries)))

    (it "adds functions only to hooks that run"
      ;; With the remaps of startup: force-loading clojure-ts-mode, which
      ;; nothing else loads, adds remaps of its own.
      (let ((major-mode-remap-alist (car startup-remaps))
            (major-mode-remap-defaults (cadr startup-remaps)))
        (my/t1-judge 'dead-hook (my/t1-dead-hooks forms) entries)))

    (it "runs each Org hook function once"
      (my/t1-judge 'repeated-hook-function (my/t1-repeated-hook-functions 'org-mode) entries))

    (it "keeps svg-tag-mode off in Org and program buffers"
      (buttercup-suppress-warning-capture
        (my/t1-judge 'svg-tag-mode
                     (my/t1-buffers-with-mode
                      'svg-tag-mode
                      (mapcar (lambda (name) (expand-file-name name temporary-file-directory))
                              '("t1-svg-tag.org" "t1-svg-tag.el")))
                     entries)))

    (it "keeps the protected vanilla keys out of the global minor modes"
      (my/t1-judge 'minor-mode-key-shadow
                   (my/t1-minor-mode-key-shadows (my/t1-global-minor-mode-maps)
                                                 my/integrity-protected-keys)
                   entries))

    (it "keeps package keymaps off the reserved keys it binds globally"
      (let (mode-maps)
        (mapatoms (lambda (symbol)
                    (when (and (boundp symbol) (keymapp (symbol-value symbol))
                               (string-suffix-p "-mode-map" (symbol-name symbol)))
                      (push symbol mode-maps))))
        (my/t1-judge 'package-key-shadow
                     (my/t1-package-key-shadows bindings mode-maps my/t1-package-shadows-allowed)
                     entries)))

    (it "binds every key the documentation cites, in the keymaps it is about"
      (my/t1-judge 'unbound-cited-key
                   (my/t1-unbound-cited-keys my/t1-root bindings my/t1-document-keymaps)
                   entries))

    (it "fills at 120 columns by default"
      (my/t1-judge 'fill-column (my/t1-fill-column-subjects) entries)
      (let ((default (default-value 'fill-column)))
        (unwind-protect
            (progn
              (setq-default fill-column 120)
              (expect (my/t1-fill-column-subjects) :to-equal nil))
          (setq-default fill-column default))))

    (it "keeps Emacs-side GPG prompts in the gpg-agent dialog"
      (my/t1-judge 'gpg-prompt (my/t1-gpg-prompt-subjects) entries)
      (let ((epg-pinentry-mode nil))
        (expect (my/t1-gpg-prompt-subjects) :to-equal nil)))

    (it "shows the tasks of the GTD file local.el names in the agenda"
      (buttercup-suppress-warning-capture
        ;; The fixture local.el has run: it names its own GTD file.
        (expect my/gtd-file :to-equal (expand-file-name "gtd.org" user-emacs-directory))
        (my/t1-judge 'agenda-files
                     (my/t1-agenda-subjects "A task only this fixture file holds")
                     entries)
        (let ((org-agenda-files (list my/gtd-file)))
          (expect (my/t1-agenda-subjects "A task only this fixture file holds")
                  :to-equal nil))))

    (it "enables one theme, the one for the appearance"
      (buttercup-suppress-warning-capture
        ;; The same, whichever appearance the desktop is in when it starts.
        (dolist (start (mapcar #'car my/t1-themes))
          (let ((restore (my/scenario-system-apply (list :appearance start) nil)))
            (unwind-protect
                (my/t1-judge 'theme-per-appearance (my/t1-theme-subjects) entries)
              (funcall restore))))
        ;; With the themes auto-dark switches between set (084 CF-02).
        (let ((auto-dark-themes (mapcar (lambda (pair) (list (cdr pair))) my/t1-themes)))
          (expect (my/t1-theme-subjects) :to-equal nil))))))

;;; t1.el ends here
