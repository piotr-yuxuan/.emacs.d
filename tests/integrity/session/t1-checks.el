;;; t1-checks.el --- The session checks (T1) of plan 086 -*- lexical-binding: t -*-

;;; Commentary:
;; Each check compares what the configuration says, read as data by
;; `config-forms.el', with the live session, and returns its subjects:
;; strings naming what it found, which the known-defects ledger
;; (`ledger.el') lists or not.  `t1.el' runs the checks in a daemon
;; session started by `tests/integrity/session/run'; `t1-checks-test.el'
;; runs each against a fixture that holds exactly its defect, and
;; against a clean counterpart.  `cheatsheet.el' writes
;; `docs/reference/keys.org' with `my/t1-cheatsheet'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'loadhist)
(require 'config-forms)
(require 'ledger)
(require 'bind-key)
(require 'keystrokes)
(require 'scenario/system)

(declare-function buttercup-fail "buttercup")
(defvar epg-pinentry-mode)
(defvar org-agenda-buffer-name)

(defconst my/t1-root
  (expand-file-name "../../../" (file-name-directory
                                 (or load-file-name buffer-file-name)))
  "The root of the working tree whose configuration is checked.")

(defconst my/t1-deny-list
  '((gterm . "loading it compiles its Zig module")
    (pyim . "its :config declares pyim-basedict, which straight.el would clone")
    (pinentry . "the user-visible checks observe whether the configuration loads it (D12)"))
  "Features the force-load leaves alone, each with the reason.")

(defconst my/t1-translation-maps
  '(key-translation-map function-key-map local-function-key-map input-decode-map)
  "Keymaps whose bindings translate keys instead of naming commands.")

(defconst my/t1-startup-hooks '(after-init-hook emacs-startup-hook window-setup-hook)
  "The hooks every session runs once, at the end of its startup.")

(defconst my/t1-themes
  '((dark . sanityinc-tomorrow-night) (light . sanityinc-tomorrow-day))
  "The theme the user wants for each appearance of the desktop.")

(defconst my/t1-fill-column 120
  "The fill column the user wants by default: 084 CF-01, `my/line-width'.")

(defun my/t1-press (steps)
  "Press the keys of STEPS, as in a scenario's :when; return the errors shown."
  (plist-get (my/scenario-as-user
              (lambda () (my/scenario-press (my/scenario-compile-steps steps))))
             :errors))

(defun my/t1-judge (check subjects entries)
  "Fail the running spec unless the ledger ENTRIES account for SUBJECTS.
CHECK names the check.  The failure lists the subjects no entry for
CHECK lists, and the listed subjects CHECK no longer finds."
  (let* ((verdict (my/integrity-ledger-compare check subjects entries))
         (unexpected (plist-get verdict :unexpected))
         (stale (plist-get verdict :stale)))
    (when (or unexpected stale)
      (buttercup-fail "%s: not in the ledger: %S; stale ledger entries: %S"
                      check unexpected stale))))

(defun my/t1-error-lines (lines)
  "Return the LINES of *Messages* that report an error.
Such a line has the word error, as in \"Error running timer\" or
\"error in process sentinel:\", not as part of a longer name such
as `flycheck-error' or \"errors\"."
  (let ((case-fold-search nil))
    (seq-filter (lambda (line)
                  (string-match-p "\\(?:^\\|[^[:alnum:]_-]\\)\\(?:[Ee]rror\\|ERROR\\)\\(?:$\\|[^[:alnum:]_-]\\)"
                                  line))
                lines)))

(defun my/t1-buffer-lines (buffer &optional start)
  "Return the lines of BUFFER from START, or nil if BUFFER does not exist.
The lines lose what depends on the display: the icon that starts a
warning, and curved quotes, which become ASCII."
  (and (get-buffer buffer)
       (with-current-buffer buffer
         (mapcar (lambda (line)
                   (replace-regexp-in-string
                    "[‘’]" "'" (string-trim-left line "[^[:alnum:]]+")))
                 (split-string (buffer-substring-no-properties
                                (min (or start (point-min)) (point-max)) (point-max))
                               "\n" t)))))

(defun my/t1-loaded-features (wanted loaded)
  "Return the names of the WANTED features that LOADED holds, in order.
LOADED is a list of features: `features' for those loaded now, or
`my/test-session-startup-features' for those startup loaded."
  (mapcar #'symbol-name (seq-filter (lambda (feature) (memq feature loaded)) wanted)))

(defun my/t1-fill-column-subjects ()
  "Return \"default N\" when the default `fill-column' is not `my/t1-fill-column'."
  (let ((value (default-value 'fill-column)))
    (unless (eql value my/t1-fill-column)
      (list (format "default %S" value)))))

(defun my/t1-gpg-prompt-subjects ()
  "Return what takes Emacs-side GPG prompts out of the gpg-agent dialog (D12).
That is \"epg-pinentry-mode VALUE\" when the variable is set, and
\"pinentry loaded\" when the pinentry package is."
  (append (and (bound-and-true-p epg-pinentry-mode)
               (list (format "epg-pinentry-mode %S" epg-pinentry-mode)))
          (and (featurep 'pinentry) (list "pinentry loaded"))))

(defun my/t1-agenda-subjects (heading)
  "Return why the agenda the user opens lacks HEADING, a task of `my/gtd-file'.
Press C-c a t, the list of every TODO item, as the user does, and
look for HEADING there: the subject is \"local.el GTD file not read\"
when it is missing, and each error pressing showed.  The agenda
buffer is killed."
  (let ((errors (my/t1-press '((:keys "C-c a t")))))
    (unwind-protect
        (append (mapcar (lambda (error) (format "C-c a t: %S" error)) errors)
                (unless (and (get-buffer org-agenda-buffer-name)
                             (with-current-buffer org-agenda-buffer-name
                               (string-search heading (buffer-string))))
                  (list "local.el GTD file not read")))
      (when (get-buffer org-agenda-buffer-name)
        (kill-buffer org-agenda-buffer-name)))))

(defun my/t1-theme-subjects ()
  "Return the appearances whose enabled themes are not the wanted one.
Switch the desktop appearance through 085's :appearance boundary, as
when the user switches it: first to the last appearance of
`my/t1-themes', then to each of them in turn.  The first switch makes
the history the same whatever appearance the session is in, and the
boundaries nest, so the real appearance is read only once, when the
outermost one is restored at the end.  A subject is \"APPEARANCE
ENABLED-THEMES\"."
  (let ((restores (list (my/scenario-system-apply
                         (list :appearance (car (car (last my/t1-themes)))) nil)))
        subjects)
    (unwind-protect
        (pcase-dolist (`(,appearance . ,theme) my/t1-themes)
          (push (my/scenario-system-apply (list :appearance appearance) nil) restores)
          (unless (equal custom-enabled-themes (list theme))
            (push (format "%s %S" appearance custom-enabled-themes) subjects)))
      (mapc #'funcall restores))
    (nreverse subjects)))

(defun my/t1-italic-subjects (frame)
  "Return the theme changes after which FRAME shows comments not italic (D10).
Switch the appearance to dark, then light, through 085's :appearance
boundary, and in light load the night theme with M-x load-theme, as
the user would; then restore the appearance.  A subject names the
change and the slant FRAME shows, with the errors M-x showed."
  (let ((slant (lambda () (face-attribute 'font-lock-comment-face :slant frame t)))
        subjects)
    (pcase-dolist (`(,appearance . ,_) my/t1-themes)
      (let ((restore (my/scenario-system-apply (list :appearance appearance) nil)))
        (unwind-protect
            (progn
              (unless (eq (funcall slant) 'italic)
                (push (format "%s %S" appearance (funcall slant)) subjects))
              (when (eq appearance 'light)
                (let ((errors (my/t1-press '((:keys "M-x") (:type "load-theme")
                                             (:keys "<return>")
                                             (:type "sanityinc-tomorrow-night")
                                             (:keys "<return>")))))
                  (unless (and (null errors) (eq (funcall slant) 'italic))
                    (push (format "load-theme %S %S" (funcall slant) errors) subjects)))))
          (funcall restore))))
    (nreverse subjects)))

(defun my/t1-force-load (forms)
  "Load every feature FORMS configure, except those of `my/t1-deny-list'.
The features are the `use-package' names, then the features that
`with-eval-after-load' and :after wait for, then the libraries that
commands FORMS bind still autoload from, such as the menus of
dirvish's extensions: only loaded, a command shows its docstring and
whether it is defined.  So do the major modes whose hooks FORMS add
to: only loaded, css-mode.el defines the modes derived from css-mode,
whose buffers run its hook.  `require' leaves a library already loaded
alone; a command it does not define is the dangling-binding check's.
Return the features that could not be loaded, as (FEATURE . MESSAGE),
in order."
  (let* ((debug-on-error nil)
         (failures nil)
         (load (lambda (features)
                 (dolist (feature (seq-uniq features))
                   (unless (assq feature my/t1-deny-list)
                     (condition-case err
                         (require feature)
                       (error (push (cons feature (error-message-string err)) failures))))))))
    (funcall load (append (my/integrity-packages forms)
                          (my/integrity-after-load-features forms)))
    (funcall load (seq-keep (lambda (function)
                              (and (autoloadp function) (intern (nth 1 function))))
                            (append
                             (mapcar (lambda (row)
                                       (let ((command (plist-get row :command)))
                                         (and (symbolp command) (symbol-function command))))
                                     (my/integrity-bindings forms))
                             (mapcar (pcase-lambda (`(,hook . ,_))
                                       (symbol-function
                                        (intern (string-remove-suffix "-hook" (symbol-name hook)))))
                                     (my/t1-added-hooks forms)))))
    (nreverse failures)))

(defun my/t1-loaded-autoload-p (function)
  "Return non-nil when FUNCTION is an autoload into a file already loaded.
Loading that file did not define the function, so calling it fails."
  (and (autoloadp function)
       (load-history-filename-element (load-history-regexp (nth 1 function)))))

(defun my/t1-dangling-bindings (maps)
  "Return the key bindings of the keymaps MAPS that are not commands.
MAPS are keymap variables, or `global' for the global map; the
prefix keymaps they hold are followed, their parent keymaps are
not.  Unbound variables, whose package is not loaded, the key
translation maps of `my/t1-translation-maps', and menu and mouse
events are skipped.  A binding is dangling when it is not a
command, or when it is an autoload into a file that has been
loaded without defining it.  Each subject is \"MAP KEY BINDING\",
BINDING being a symbol or \"lambda\"."
  (let ((stack (mapcar (lambda (map)
                         (list (if (eq map 'global) global-map (symbol-value map)) [] map))
                       (seq-filter (lambda (map) (or (eq map 'global) (boundp map)))
                                   (seq-difference maps my/t1-translation-maps))))
        visited subjects)
    (while stack
      (pcase-let ((`(,keymap ,prefix ,map) (pop stack)))
        (unless (memq keymap visited)
          (push keymap visited)
          (map-keymap-internal
           (lambda (event binding)
             (let* ((keys (vconcat prefix (vector event)))
                    (definition (pcase binding
                                  (`(menu-item ,_ ,definition . ,_) definition)
                                  (`(,(pred stringp) . ,definition) definition)
                                  (_ binding)))
                    (function (indirect-function definition)))
               (cond ((or (memq event '(menu-bar tool-bar tab-bar))
                          (seq-intersection (event-modifiers event) '(click down drag))))
                     ((keymapp function) (push (list function keys map) stack))
                     ((or (my/t1-loaded-autoload-p function)
                          (and (not (autoloadp function)) definition
                               (not (commandp definition))))
                      (push (format "%s %s %s" map (key-description keys)
                                    (if (symbolp definition) definition "lambda"))
                            subjects)))))
           keymap))))
    (nreverse subjects)))

(defun my/t1-unjudged-bindings (bindings skipped)
  "Return the BINDINGS the binding checks cannot judge, unless SKIPPED explains it.
BINDINGS are rows of `my/integrity-bindings'.  A binding cannot be
judged when its keymap variable is unbound, or when its command is
still an autoload from a library not loaded.  That is explained when
the row comes from the `use-package' of one of the SKIPPED packages,
or the library is one: those of `my/t1-deny-list', and those the
force-load could not load, which cannot-load reports.  A subject is
\"keymap MAP\" or \"command COMMAND\"."
  (seq-uniq
   (seq-keep (lambda (row)
               (let* ((map (plist-get row :map))
                      (command (plist-get row :command))
                      (function (and (symbolp command) (symbol-function command))))
                 (cond ((memq (plist-get row :source) skipped) nil)
                       ((not (or (eq map 'global) (boundp map))) (format "keymap %s" map))
                       ((and (autoloadp function)
                             (not (my/t1-loaded-autoload-p function))
                             (not (memq (intern (nth 1 function)) skipped)))
                        (format "command %s" command)))))
             bindings)))

(defun my/t1-undocumented-commands (bindings)
  "Return the names of the commands BINDINGS bind that have no docstring.
That is, whose cheatsheet description is blank (`my/t1-binding-summary').
BINDINGS are rows of `my/integrity-bindings'.  Commands that are not
defined are the dangling-binding check's; the force-load loads the
library of every other one, unless its package is denied."
  (seq-uniq
   (seq-keep (lambda (row)
               (let ((command (plist-get row :command)))
                 (and (not (plist-get row :keymap))
                      (commandp command) (not (keymapp command))
                      (not (autoloadp (symbol-function command)))
                      (string-blank-p (cdr (my/t1-binding-summary command)))
                      (symbol-name command))))
             bindings)))

(defun my/t1-platform-guard (condition)
  "Return what the `when' or `unless' test CONDITION gives on this platform.
That is t or nil when its value follows from comparisons of
`system-type' with quoted symbols and from the constants `my/macos-p'
and `my/linux-p', combined with `not', `and' and `or'.  It is
:unknown when the value depends on anything else, which is never
evaluated: (and X (eq system-type \\='darwin)) is nil off macOS
whatever X is, (and X my/linux-p) is :unknown on Linux."
  (pcase condition
    ((or `(,(or 'eq 'eql 'equal) system-type (quote ,(and (pred symbolp) type)))
         `(,(or 'eq 'eql 'equal) (quote ,(and (pred symbolp) type)) system-type))
     (eq system-type type))
    (`(memq system-type (quote ,(and (pred proper-list-p) types)))
     (and (memq system-type types) t))
    ((and (or 'my/macos-p 'my/linux-p) (pred boundp))
     (and (symbol-value condition) t))
    (`(not ,operand)
     (let ((value (my/t1-platform-guard operand)))
       (if (eq value :unknown) value (not value))))
    (`(,(and (or 'and 'or) operator) . ,(and (pred proper-list-p) operands))
     ;; nil decides an `and', t an `or', whatever the other operands are.
     (let ((values (mapcar #'my/t1-platform-guard operands))
           (decisive (eq operator 'or)))
       (cond ((memq decisive values) decisive)
             ((memq :unknown values) :unknown)
             (t (not decisive)))))
    (_ :unknown)))

(defun my/t1-unrun-variables (forms)
  "Return the variables FORMS set only in code this platform skips.
FORMS are (FORM . LINE) pairs, as from `my/integrity-forms'.  That
code is the body of a `when' whose test `my/t1-platform-guard'
finds nil, or of an `unless' whose test it finds t.  A variable also
set anywhere else, under a test it cannot decide included, is run."
  (let ((run (mapcar (lambda (entry)
                       (list (named-let prune ((form (car entry)))
                               (pcase form
                                 ((and `(,(and (or 'when 'unless) macro) ,test . ,_)
                                       (guard (eq (my/t1-platform-guard test)
                                                  (eq macro 'unless))))
                                  nil)
                                 ((pred proper-list-p) (mapcar #'prune form))
                                 (`(,head . ,tail) (cons (prune head) (prune tail)))
                                 (_ form)))))
                     forms)))
    (seq-difference (seq-uniq (mapcar #'car (my/integrity-set-variables forms)))
                    (mapcar #'car (my/integrity-set-variables run)))))

(defun my/t1-mu4e-bindings (bindings)
  "Return \"MAP KEY COMMAND\" for each of BINDINGS whose command is of mu4e.
BINDINGS are rows of `my/integrity-bindings'.  The commands of mu4e
and of its companions start with mu4e; plan 087 removed them all,
whatever map binds them."
  (seq-keep (lambda (row)
              (let ((command (plist-get row :command)))
                (and (symbolp command)
                     (string-prefix-p "mu4e" (symbol-name command))
                     (format "%s %s %s" (plist-get row :map) (plist-get row :key) command))))
            bindings))

(defun my/t1-load-variable-libraries (variables)
  "Load the library each of the VARIABLES, declared by no loaded code, names.
Following the Emacs naming convention, that library is the longest
prefix of the variable's name, or of its downcased name, that names a
library: dirvish-emerge for `dirvish-emerge-groups', tex for
`TeX-engine'.  A library of `my/t1-deny-list' is not loaded.  Return
the variables whose library is denied."
  (let (denied)
    (dolist (variable variables)
      (let ((prefix (symbol-name variable))
            (library nil))
        (while (and (not (or library (special-variable-p variable)
                             (custom-variable-p variable)))
                    (string-match "-[^-]*\\'" prefix))
          (setq prefix (substring prefix 0 (match-beginning 0))
                library (seq-find #'locate-library (list prefix (downcase prefix)))))
        (cond ((null library))
              ((assq (intern library) my/t1-deny-list) (push variable denied))
              (t (ignore-errors (require (intern library)))))))
    (nreverse denied)))

(defun my/t1-variable-defects (variables skipped unchecked)
  "Classify the VARIABLES the configuration sets.
VARIABLES are (VARIABLE . PACKAGE) pairs, as from
`my/integrity-set-variables'.  The variables whose PACKAGE is in
SKIPPED, the packages not loaded, and the UNCHECKED ones are not
checked.  Return a plist of variable names: :undeclared, set but
neither special, customizable nor documented; :obsolete, declared
obsolete; :not-checked."
  (let (undeclared obsolete not-checked)
    (pcase-dolist (`(,variable . ,package) (seq-uniq variables))
      (let ((name (symbol-name variable)))
        (cond ((or (memq package skipped) (memq variable unchecked))
               (push name not-checked))
              ((get variable 'byte-obsolete-variable) (push name obsolete))
              ((not (or (special-variable-p variable) (custom-variable-p variable)
                        (get variable 'variable-documentation)))
               (push name undeclared)))))
    (list :undeclared (nreverse undeclared)
          :obsolete (nreverse obsolete)
          :not-checked (nreverse not-checked))))

(defun my/t1-subforms (forms)
  "Return every list in FORMS, depth first, except quoted data.
FORMS are (FORM . LINE) pairs, as from `my/integrity-forms'."
  (let ((stack (mapcar #'car forms))
        subforms)
    (while stack
      (let ((form (pop stack)))
        (when (and (consp form) (not (eq (car form) 'quote)))
          (push form subforms)
          (let (children)
            (while (consp form)
              (push (car form) children)
              (setq form (cdr form)))
            (setq stack (nconc (nreverse children) stack))))))
    (nreverse subforms)))

(defun my/t1-added-hooks (forms)
  "Return the (HOOK . FUNCTION) pairs FORMS add, in order.
They come from `add-hook' on a quoted hook, from `add-hook' on the
variable of a `dolist' over a quoted list of hooks, and from the
`use-package' :hook entries (HOOKS . FUNCTION), alone or in a list
of entries, HOOKS being a hook name without its -hook suffix or a
list of them.  FUNCTION is a symbol, or :lambda for anything else."
  (mapcar
   (pcase-lambda (`(,hook . ,function))
     (cons hook (pcase function
                  ((pred symbolp) function)
                  (`(,(or 'quote 'function) ,(and (pred symbolp) symbol)) symbol)
                  (_ :lambda))))
   (mapcan
    (lambda (form)
      (pcase form
        (`(add-hook (quote ,(and (pred symbolp) hook)) ,function . ,_)
         (list (cons hook function)))
        (`(dolist (,variable (quote ,(and (pred proper-list-p) hooks))) . ,body)
         (mapcan (lambda (inner)
                   (pcase inner
                     (`(add-hook ,(pred (eq variable)) ,function . ,_)
                      (mapcar (lambda (hook) (cons hook function)) hooks))))
                 (my/t1-subforms (mapcar #'list body))))
        (`(use-package ,_ . ,arguments)
         (mapcan (lambda (entry)
                   (pcase entry
                     ((and `(,(or (pred symbolp) (pred proper-list-p)) . ,function)
                           `(,hooks . ,_)
                           (guard (or (and function (symbolp function))
                                      (memq (car-safe function) '(lambda function quote)))))
                      (mapcar (lambda (hook) (cons (intern (format "%s-hook" hook)) function))
                              (ensure-list hooks)))))
                 (my/t1-subforms (mapcar #'list (seq-take-while
                                                 (lambda (argument) (not (keywordp argument)))
                                                 (cdr (memq :hook arguments)))))))))
    (my/t1-subforms forms))))

(defun my/t1-global-modes-off (forms)
  "Return the names of the global minor modes FORMS turn on that are off.
A mode is turned on by a call (MODE), (MODE 1), (MODE +1) or
\(MODE t), or by adding it to a hook of `my/t1-startup-hooks'."
  (let ((modes (mapcar #'cdr (seq-filter (lambda (pair) (memq (car pair) my/t1-startup-hooks))
                                         (my/t1-added-hooks forms)))))
    (dolist (form (my/t1-subforms forms))
      (pcase form
        (`(,(and (pred symbolp) mode) . ,(or 'nil '(1) '(+1) '(t)))
         (push mode modes))))
    (seq-uniq
     (seq-keep (lambda (mode)
                 (and (memq mode minor-mode-list) (boundp mode)
                      (not (local-variable-if-set-p mode))
                      (not (default-value mode))
                      (symbol-name mode)))
               modes))))

(defun my/t1-dead-hooks (forms)
  "Return the names of the mode hooks FORMS add to that never run.
MODE-hook runs when a mode whose chain of `derived-mode-parent'
holds MODE starts: MODE itself, or a mode derived from it that
`auto-mode-alist' opens files in, as scss-mode, derived from css-mode,
for .scss files; slitex-mode, derived from latex-mode, opens none.
Starting a major mode starts what `major-mode-remap' gives for it.
So MODE-hook never runs:
- when MODE is not a function and no loaded code declares the hook;
- when MODE is an alias, as AUCTeX makes context-mode of ConTeXt-mode:
  calling it runs the hooks of the mode it names, unless the hook is
  an alias too;
- when MODE, and each mode derived from it that opens files, are
  remapped to major modes that do not derive from MODE, as tree-sitter
  modes and AUCTeX do.  An extra parent of `derived-mode-add-parents',
  such as java-mode for java-ts-mode, makes `derived-mode-p' true but
  runs no hook.
Only the modes of loaded libraries have a parent: the force-load
loads the library of each MODE, which defines the modes derived from
it beside it, as css-mode.el does.  The modes of the packages of
`my/t1-deny-list', which are not loaded, are not checked."
  (let ((opened (seq-uniq (seq-filter #'symbolp
                                      (mapcar (lambda (entry)
                                                (if (consp (cdr entry)) (cadr entry) (cdr entry)))
                                              auto-mode-alist)))))
    (seq-uniq
     (seq-keep (pcase-lambda (`(,hook . ,_))
                 (let* ((name (symbol-name hook))
                        (mode (and (string-suffix-p "-mode-hook" name)
                                   (intern (string-remove-suffix "-hook" name)))))
                   (and mode
                        (not (seq-some (lambda (denied)
                                         (string-prefix-p (format "%s-" (car denied)) name))
                                       my/t1-deny-list))
                        (or (and (not (fboundp mode))
                                 (not (special-variable-p hook))
                                 (not (get hook 'variable-documentation)))
                            (and (function-alias-p mode) (eq (indirect-variable hook) hook))
                            (not (seq-some (lambda (start)
                                             (let ((parent (major-mode-remap start)))
                                               (while (and parent (not (eq parent mode)))
                                                 (setq parent (get parent 'derived-mode-parent)))
                                               parent))
                                           (cons mode opened))))
                        name)))
               (my/t1-added-hooks forms)))))

(defun my/t1-foreign-commands (forms skipped)
  "Return \"PACKAGE COMMAND\" for each :commands entry its package does not define.
FORMS are read as by `my/integrity-forms'.  The `use-package'
:commands of PACKAGE create autoloads into PACKAGE: once PACKAGE is
loaded, each COMMAND must be defined by a file of the directory
PACKAGE was loaded from.  Packages in SKIPPED, which were not
loaded, are not checked."
  (let (subjects)
    (dolist (form (my/t1-subforms forms))
      (when (and (eq (car form) 'use-package) (not (memq (cadr form) skipped)))
        (let ((package (cadr form))
              (section nil))
          (dolist (item (cddr form))
            (if (keywordp item)
                (setq section item)
              (when (eq section :commands)
                (dolist (command (ensure-list item))
                  (let ((file (symbol-file command 'defun))
                        (home (feature-file package)))
                    (unless (and file home (not (autoloadp (symbol-function command)))
                                 (file-equal-p (file-name-directory file)
                                               (file-name-directory home)))
                      (push (format "%s %s" package command) subjects))))))))))
    (nreverse subjects)))

(defun my/t1-repeated-hook-functions (mode)
  "Return \"MODE FUNCTION\" for each function the hooks of MODE run twice.
Starting MODE runs the hooks of MODE and of every mode it derives
from; a function on two of them runs twice in every MODE buffer."
  (let (seen subjects)
    (dolist (parent (derived-mode-all-parents mode))
      (let ((hook (intern (format "%s-hook" parent))))
        (dolist (function (and (boundp hook) (default-value hook)))
          (when (and (symbolp function) (not (eq function t)))
            (when (memq function seen)
              (push (format "%s %s" mode function) subjects))
            (push function seen)))))
    (nreverse subjects)))

(defun my/t1-buffers-with-mode (mode files)
  "Return the major modes of the FILES whose buffers have MODE on.
Each file is visited as when the user opens it, with
`find-file-noselect', which runs its major mode and the mode's
hooks, then its buffer is killed."
  (seq-keep (lambda (file)
              (let ((buffer (find-file-noselect file)))
                (prog1 (with-current-buffer buffer
                         (and (boundp mode) (symbol-value mode) (symbol-name major-mode)))
                  (kill-buffer buffer))))
            files))

(defun my/t1-key-binding (keymap key)
  "Return (PREFIX . BINDING): what KEY, a `key-parse' string, runs in KEYMAP.
When the first events of KEY already form a complete key, as C-c a
does in C-c a g when it runs the agenda dispatcher, PREFIX is that
key and the rest of KEY is input to its command; otherwise PREFIX is
KEY.  PREFIX is a `key-description'.  BINDING is nil when PREFIX
runs nothing there: unbound, or bound to `undefined'.  A menu item
whose :filter hides its definition, as those of `lsp-command-map' do
outside a buffer whose language server supports them, binds that
definition."
  (let* ((events (key-parse key))
         (found (lookup-key keymap events))
         (prefix (if (numberp found) (seq-take events found) events))
         (binding (if (numberp found) (lookup-key keymap prefix) found))
         (parent (and (null binding) (lookup-key keymap (seq-take prefix (1- (length prefix)))))))
    (when (keymapp parent)
      (map-keymap (lambda (event raw)
                    (when (and (null binding) (eql event (aref prefix (1- (length prefix)))))
                      (setq binding (keymap--get-keyelt raw nil))))
                  parent))
    (cons (key-description prefix) (and (not (eq binding 'undefined)) binding))))

(defun my/t1-global-minor-mode-maps ()
  "Return (MODE . KEYMAP) for each global minor mode that is on.
The keymaps come from `minor-mode-map-alist'; a mode is global when
its variable is not buffer-local, as for `which-key-mode'.  Such a
keymap comes before the global map in every buffer."
  (seq-filter (pcase-lambda (`(,mode . ,keymap))
                (and (boundp mode) (default-value mode)
                     (not (local-variable-if-set-p mode))
                     (keymapp keymap)))
              minor-mode-map-alist))

(defconst my/t1-key-reading-commands
  '((org-agenda . "the agenda dispatcher reads the key of a view, as t in C-c a t")
    (org-capture . "it reads the key of a capture template, as n in C-c c n")
    (describe-key . "it reads the key to describe, as C-c d n in C-h k C-c d n"))
  "Commands that read the keys typed after their own: (COMMAND . REASON).
A transient prefix reads them too, but only its layout says which; so
a key under a transient prefix, as C-c d n under docker's, must be
bound in a keymap of its own.")

(defun my/t1-unbound-cited-keys (root bindings contexts)
  "Return \"FILE KEY\" for each key a document under ROOT cites that runs nothing.
The documents are the Org files under ROOT's docs/, apart from those
of docs/work/, plans and reviews that cite keys to discuss them, and
the generated docs/reference/keys.org.  FILE is relative to ROOT.  A
citation is a verbatim or code object, as Org reads it, holding a
`key-valid-p' key sequence whose first key has a modifier or is a
function key; \" / \" separates alternatives, as in =C-c l / C-c C-l=.

The keymaps a document is read in are the global map,
`override-global-map', the keymaps of the global minor modes that are
on, and the keymap variables it names on #+KEYMAPS: lines, or that
the alist CONTEXTS (FILE . MAPS) names for it.  A :bind-keymap row of
BINDINGS, rows of `my/integrity-bindings', binds its key there to the
value of its keymap variable, as its first press does.  A key runs
something when one of those keymaps binds it to a prefix keymap, or to
a command that can run: not `undefined', a function that is not a
command, nor an autoload into a file loaded without defining it.  The
rest of a key whose first events run a command is input to it only
when the command is one of `my/t1-key-reading-commands'."
  (require 'org)
  (let ((overlay (make-sparse-keymap)))
    (dolist (row bindings)
      (let ((map (plist-get row :command)))
        (when (and (plist-get row :keymap) (memq (plist-get row :map) '(global override-global-map))
                   (boundp map) (keymapp (symbol-value map)))
          (keymap-set overlay (plist-get row :key) (symbol-value map)))))
    (mapcan
     (lambda (file)
       (let ((relative (file-relative-name file root)))
         (unless (string-match-p "\\`docs/\\(?:work/\\|reference/keys\\.org\\'\\)" relative)
           (with-temp-buffer
             (insert-file-contents file)
             (let ((org-inhibit-startup t))
               (delay-mode-hooks (org-mode)))
             (let* ((names (append (alist-get relative contexts nil nil #'equal)
                                   (mapcar #'intern (mapcan #'split-string
                                                            (cdar (org-collect-keywords '("KEYMAPS")))))))
                    (keymaps (append (list overlay global-map override-global-map)
                                     (mapcar #'cdr (my/t1-global-minor-mode-maps))
                                     (seq-keep (lambda (name)
                                                 (and (boundp name) (keymapp (symbol-value name))
                                                      (symbol-value name)))
                                               names)))
                    (keys (seq-filter (lambda (key)
                                        (and (key-valid-p key)
                                             (string-match-p "\\`\\(?:[ACHMSs]-\\|<[^>]+>\\)" key)))
                                      (mapcan (lambda (value) (split-string value " / "))
                                              (org-element-map (org-element-parse-buffer)
                                                  '(verbatim code)
                                                (lambda (object)
                                                  (org-element-property :value object)))))))
               (mapcar (lambda (key) (format "%s %s" relative key))
                       (seq-remove
                        (lambda (key)
                          (seq-some
                           (lambda (keymap)
                             (pcase-let* ((`(,prefix . ,binding) (my/t1-key-binding keymap key))
                                          (function (indirect-function binding)))
                               (and (or (keymapp function)
                                        (and (commandp function)
                                             (not (my/t1-loaded-autoload-p function))))
                                    (or (equal prefix (key-description (key-parse key)))
                                        (assq binding my/t1-key-reading-commands)))))
                           keymaps))
                        (seq-uniq keys))))))))
     (sort (directory-files-recursively (expand-file-name "docs" root) "\\.org\\'") #'string<))))

(defun my/t1-minor-mode-key-shadows (modes keys)
  "Return \"MODE KEY\" for each of KEYS that the keymap of one of MODES binds.
MODES is an alist (MODE . KEYMAP), as `my/t1-global-minor-mode-maps'
returns it: such a keymap takes KEYS from the global map everywhere."
  (mapcan (pcase-lambda (`(,mode . ,keymap))
            (mapcan (lambda (key)
                      (and (cdr (my/t1-key-binding keymap key))
                           (list (format "%s %s" mode key))))
                    keys))
          modes))

(defconst my/t1-reserved-key-regexp "\\`\\(?:C-c [A-Za-z]\\(?: \\|\\'\\)\\|<f[5-9]>\\)"
  "The keys Emacs reserves for users: C-c and a letter, and F5 to F9.
See (elisp) Key Binding Conventions: no package may bind them.")

(defconst my/t1-package-shadows-allowed
  '((eat-char-mode-map . "eat's char mode sends every key to the terminal")
    (eat-eshell-char-mode-map . "eat's char mode sends every key to the terminal")
    (xwidget-webkit-isearch-mode-map
     . "active only while a webkit search reads keys; any other key ends the search")
    (rxt--read-pcre-mode-map . "active only in the minibuffer, while pcre2el reads a regexp"))
  "Package keymaps that may take reserved keys: (MAP . REASON).")

(defun my/t1-package-key-shadows (rows maps allowed)
  "Return \"MAP KEY\" for each of MAPS that hides a reserved key ROWS bind globally.
ROWS are rows of `my/integrity-bindings'; MAPS are keymap variables,
such as every bound variable named *-mode-map.  The keys are those of
`my/t1-reserved-key-regexp' that ROWS bind in the global map: a
package keymap that binds one hides the configuration's key wherever
its mode is on, as cargo-minor-mode does with C-c a.  KEY is the key
of MAP that does it, a prefix of the global key when MAP binds that
prefix to a command, as cdlatex can with C-c.  Not returned: the
binding the global map has, a prefix keymap over a global prefix
keymap (their keys merge), a binding MAP inherits from its parent
keymap (the parent is the one to report), a key the configuration
itself binds in MAP or under it (the static mode-map-shadow check
reports those), and the maps of the alist ALLOWED (MAP . REASON)."
  (let ((reserved (seq-filter (lambda (row)
                                (and (eq (plist-get row :map) 'global) (plist-get row :command)
                                     (string-match-p my/t1-reserved-key-regexp
                                                     (plist-get row :key))))
                              rows))
        subjects)
    (dolist (map (seq-remove (lambda (map) (assq map allowed)) maps))
      (dolist (row reserved)
        (pcase-let* ((keymap (symbol-value map))
                     (`(,key . ,here) (my/t1-key-binding keymap (plist-get row :key)))
                     (global (cdr (my/t1-key-binding global-map key)))
                     (inherited (and (keymap-parent keymap)
                                     (cdr (my/t1-key-binding (keymap-parent keymap) key)))))
          (unless (or (null here) (equal here global) (equal here inherited)
                      (and (keymapp here) (or (keymapp global) (plist-get row :keymap)))
                      (seq-some (lambda (other)
                                  (and (eq (plist-get other :map) map)
                                       (string-prefix-p key (plist-get other :key))))
                                rows))
            (push (format "%s %s" map key) subjects)))))
    (sort (seq-uniq subjects) #'string<)))

(defun my/t1-binding-summary (binding)
  "Return (NAME . DESCRIPTION), how the cheatsheet shows the key BINDING.
BINDING is what a key runs: nil, a keymap, a command symbol, an
anonymous function, or :lambda for one read from the configuration.
For a command, DESCRIPTION is the first line of its docstring, with
its key references substituted and ASCII quotes, and empty when it
has none."
  (cond ((null binding) '("unbound" . ""))
        ((keymapp binding)
         (cons (if (symbolp binding) (symbol-name binding) "prefix") "prefix map"))
        ((or (eq binding :lambda) (not (symbolp binding))) '("anonymous command" . ""))
        ((or (not (fboundp binding)) (my/t1-loaded-autoload-p (symbol-function binding)))
         (cons (symbol-name binding) "undefined command"))
        (t (let ((text-quoting-style 'straight))
             (cons (symbol-name binding)
                   (car (split-string (or (ignore-errors (documentation binding)) "")
                                      "\n")))))))

(defun my/t1-cheatsheet (bindings unresolved)
  "Return the Org text of the key cheatsheet.
BINDINGS are rows of `my/integrity-bindings'; UNRESOLVED is a list
of (FILE . ROWS), ROWS from `my/integrity-unresolved'.  Each map
gets a section, `global' first, then by name, with a row per key,
sorted by key; when the configuration binds a key twice, the row is
the last binding, as in the live keymap.  A row shows the key, what
it runs in the live map (see `my/t1-binding-summary'), and the first
line of its docstring; the row of a :bind-keymap prefix names its
keymap, and the row of a map whose package is not loaded names the
command the configuration binds."
  (concat
   "#+TITLE: Key bindings\n\n"
   "Generated by =tests/integrity/session/run cheatsheet= from the live\n"
   "keymaps of a session in which every configured package is loaded.\n"
   "Do not edit; regenerate it.\n"
   (mapconcat
    (lambda (map)
      (let ((loaded (or (eq map 'global) (boundp map)))
            (rows nil))
        (dolist (row bindings)
          (when (eq (plist-get row :map) map)
            (setf (alist-get (plist-get row :key) rows nil nil #'equal) row)))
        (concat
         (format "\n* %s\n\n| Key | Command | Description |\n|-\n" map)
         (mapconcat
          (pcase-lambda (`(,key . ,row))
            (let* ((summary (my/t1-binding-summary
                             (if (and loaded (not (plist-get row :keymap)))
                                 (let ((binding (lookup-key (if (eq map 'global)
                                                                global-map
                                                              (symbol-value map))
                                                            (key-parse key))))
                                   (and (not (numberp binding)) binding))
                               (plist-get row :command))))
                   (cells (cond ((plist-get row :keymap)
                                 (cons (symbol-name (plist-get row :command)) "prefix map"))
                                ((not loaded) (cons (car summary) "package not loaded"))
                                (t summary))))
              (format "| %s | %s | %s |\n"
                      (format (if (string-search "=" key) "~%s~" "=%s=") key)
                      (string-replace "|" "\\vert{}" (car cells))
                      (string-replace "|" "\\vert{}" (cdr cells)))))
          (sort rows (lambda (a b) (string< (car a) (car b))))
          ""))))
    (sort (seq-uniq (mapcar (lambda (row) (plist-get row :map)) bindings))
          (lambda (a b) (or (eq a 'global) (and (not (eq b 'global)) (string< a b)))))
    "")
   "\n* Forms the readers could not resolve\n\n"
   (mapconcat (pcase-lambda (`(,file . ,rows))
                (mapconcat (pcase-lambda (`(,description . ,line))
                             (format "- %s:%d: %s\n" file line description))
                           rows ""))
              unresolved "")))

(provide 't1-checks)

;;; t1-checks.el ends here
