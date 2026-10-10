;;; t0-checks.el --- The static integrity checks (T0) -*- lexical-binding: t -*-

;;; Commentary:
;; The checks of tier T0 of plan 086: they read the configuration as
;; data and need no session.  Each check returns the subjects it
;; reports, as strings: a file name, a package name, or "MAP KEY" for
;; a binding.  `t0.el' compares them with the known-defects ledger.
;;
;; Two functions read the file system: `my/integrity-tangle-differences'
;; tangles README.org in a separate Emacs, and
;; `my/integrity-package-autoloads' reads the autoloads files of the
;; installed packages.  Every other check is a pure function of the
;; forms read by `my/integrity-forms' or of the bindings read by
;; `my/integrity-bindings'.

;;; Code:

(require 'seq)
(require 'config-forms)

(defconst my/integrity-live-directory
  (file-name-as-directory
   (expand-file-name (or (getenv "EMACS_LIVE_DIR") "~/.emacs.d")))
  "The live configuration whose straight.el builds the checks read.
The same default and variable as the runner of `tests/run'.")

(defconst my/integrity-tangle-seconds 60
  "The longest `my/integrity-tangle-differences' lets a tangle run.")

(defconst my/integrity-protected-keys
  '("C--" "C-x t 1" "C-x t t" "C-x t d" "M-0" "C-<left>" "C-<right>"
    "C-x f" "C-h C-d")
  "Vanilla keys the global map and `override-global-map' must keep.
The first seven are the keys decision D4 restores.  C-x f and
C-h C-d may be bound only as `my/integrity-protected-key-overrides'
says.")

(defconst my/integrity-protected-key-overrides
  '(("C-x f" consult-recent-file
     "recent files replace `set-fill-column' (docs/work/README.org, key allocation)")
    ("C-h C-d" eldoc-box-help-at-point
     "documentation at point replaces the debugging FAQ (docs/work/README.org, key allocation)"))
  "Deliberate overrides of protected keys: (KEY COMMAND REASON).")

(defconst my/integrity-mode-map-prefixes
  '(("C-c d" . "the debugger prefix of every language (D4)")
    ("C-c k" . "the test-runner prefix of every language (D4)"))
  "Prefixes under which a mode map may shadow a global binding: (KEY . REASON).")

(defconst my/integrity-loaded-by-packages
  '((lsp-tailwindcss
     . "lsp-mode requires every package of `lsp-client-packages' when it starts a server"))
  "Packages that other packages load, which no configuration form shows.
An alist of (PACKAGE . REASON).")

(defconst my/integrity--load-keywords
  '(:demand :defer :disabled :bind :bind* :bind-keymap :bind-keymap* :commands
            :autoload :hook :mode :interpreter :magic :magic-fallback)
  "The `use-package' keywords that load a deferred package, or skip its block.
Except with the values of `my/integrity--inert-load-sections'.")

(defconst my/integrity--inert-load-sections
  '((:defer) (:defer t) (:demand nil) (:disabled nil))
  "Sections with a keyword of `my/integrity--load-keywords' that load nothing.")

(defconst my/integrity--inert-autoloads
  '(autoload custom-autoload defvar defvar-local defconst defcustom put
             function-put defalias define-obsolete-function-alias
             define-obsolete-variable-alias make-obsolete
             register-definition-prefixes provide)
  "Forms of an autoloads file that change nothing until a package loads.")

(defun my/integrity--setting (forms variable)
  "Return the value form of the last top-level setting of VARIABLE in FORMS.
The settings are `setq', `setq-default', `setopt' and
`customize-set-variable'; nil when FORMS set VARIABLE nowhere."
  (let (value)
    (dolist (entry forms value)
      (pcase (car entry)
        (`(,(or 'setq 'setq-default 'setopt) . ,pairs)
         (while (consp pairs)
           (when (eq (car pairs) variable)
             (setq value (cadr pairs)))
           (setq pairs (cddr pairs))))
        (`(customize-set-variable (quote ,(pred (eq variable))) ,set . ,_)
         (setq value set))))))

(defun my/integrity--binding-subject (binding)
  "Return the subject naming BINDING in reports and in the ledger: \"MAP KEY\"."
  (format "%s %s" (plist-get binding :map) (plist-get binding :key)))

(defun my/integrity--keys-overlap-p (key other)
  "Return non-nil when the key descriptions KEY and OTHER are equal or one prefixes the other."
  (let* ((events (key-parse key))
         (others (key-parse other))
         (length (min (length events) (length others))))
    (equal (seq-take events length) (seq-take others length))))

(defun my/integrity-tangle-differences (directory files org-directory)
  "Return the FILES of DIRECTORY that a fresh tangle of its README.org changes.
The tangle runs in a separate `emacs --batch -Q' with only
ORG-DIRECTORY, a build of Org, added to `load-path', on a copy of
README.org in a temporary directory.  A file is returned when the
tangle does not produce it, when DIRECTORY lacks it, or when their
bytes differ.  Signal an error with the output of the tangle when it
fails or outlives `my/integrity-tangle-seconds'.  The process, its
buffer and the temporary directory are removed on every exit."
  (let* ((temporary (make-temp-file "integrity-tangle-" t))
         (copy (expand-file-name "README.org" temporary))
         (output (generate-new-buffer " *integrity-tangle*"))
         (deadline (+ (float-time) my/integrity-tangle-seconds))
         (bytes (lambda (file)
                  (and (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents-literally file)
                         (buffer-string)))))
         process)
    (unwind-protect
        (progn
          (copy-file (expand-file-name "README.org" directory) copy)
          (setq process
                (make-process
                 :name "integrity-tangle" :buffer output :noquery t :connection-type 'pipe
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "--batch" "-Q" "-L" org-directory
                                "--eval" "(require 'org)"
                                "--eval" (format "(org-babel-tangle-file %S)" copy))))
          ;; A prompt reads end of file and fails rather than waiting.
          (process-send-eof process)
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.1))
          (unless (and (eq (process-status process) 'exit) (eql (process-exit-status process) 0))
            (error "Tangling %s failed (%s %s): %s" copy (process-status process)
                   (process-exit-status process) (with-current-buffer output (buffer-string))))
          (seq-remove (lambda (file)
                        (let ((tangled (funcall bytes (expand-file-name file temporary))))
                          (and tangled
                               (equal tangled (funcall bytes (expand-file-name file directory))))))
                      files))
      (when process
        (delete-process process))
      (kill-buffer output)
      (delete-directory temporary t))))

(defun my/integrity-package-autoloads (packages build-directory)
  "Return the PACKAGES that load only on use, with the symbols that load them.
BUILD-DIRECTORY holds one directory per installed package, with its
NAME-autoloads.el, as straight.el builds them.  The result is an
alist of (PACKAGE . SYMBOLS), where SYMBOLS are the functions and
options the autoloads file declares, whose use loads PACKAGE.  It
leaves out the packages that may load by themselves:
- an installed package whose autoloads file does anything else,
  such as adding a hook, a mode association, an input method or a
  `with-eval-after-load';
- a package that Emacs provides at startup or ships in
  `lisp-directory', which Emacs's own autoloads reach;
- a package of `my/integrity-loaded-by-packages'.
A package found nowhere has no symbols: nothing loads it."
  (mapcan
   (lambda (package)
     (let* ((file (expand-file-name (format "%s/%s-autoloads.el" package package)
                                    build-directory))
            (library (locate-library (symbol-name package)))
            (forms (and (file-exists-p file) (mapcar #'car (my/integrity-forms file)))))
       (and (not (featurep package))
            (not (assq package my/integrity-loaded-by-packages))
            (not (and library (file-in-directory-p library lisp-directory)))
            (seq-every-p (lambda (form)
                           (or (memq (car-safe form) my/integrity--inert-autoloads)
                               (and (eq (car-safe form) 'add-to-list)
                                    (equal (nth 1 form) ''load-path))))
                         forms)
            (list (cons package
                        (seq-uniq
                         (mapcan (lambda (form)
                                   (pcase form
                                     (`(,(or 'autoload 'custom-autoload)
                                        (quote ,(and (pred symbolp) symbol)) . ,_)
                                      (list symbol))))
                                 forms)))))))
   (seq-uniq packages)))

(defun my/integrity-never-loading (forms autoloads)
  "Return the names of the `use-package' blocks in FORMS that nothing loads.
AUTOLOADS is an alist (PACKAGE . SYMBOLS) of the packages that load
only on use, as `my/integrity-package-autoloads' returns it.  A
block for such a PACKAGE is returned when `use-package-always-defer'
is set and the block has :after or :config, so it waits for PACKAGE
to load, while nothing in the configuration loads it:
- none of its sections is in `my/integrity--load-keywords', apart
  from `my/integrity--inert-load-sections';
- no form outside the block, and none of its :init, :preface or
  :custom, mentions PACKAGE (as `require' would) or one of SYMBOLS,
  other than as the feature a `with-eval-after-load',
  `eval-after-load' or :after waits for: waiting for a package does
  not load it.
Running a command with \\[execute-extended-command] can still load
such a package; its block then runs late or never, which is the
defect."
  (let ((mentions (make-hash-table :test #'eq))
        (count (lambda (symbol entries)
                 ;; The mentions of SYMBOL in the forms ENTRIES that do
                 ;; not just wait for it.
                 (- (seq-count (lambda (x) (eq x symbol)) (flatten-tree entries))
                    (seq-count (lambda (x) (eq x symbol))
                               (my/integrity--feature-rows (mapcar #'list entries)))))))
    (dolist (symbol (flatten-tree (mapcar #'car forms)))
      (puthash symbol (1+ (gethash symbol mentions 0)) mentions))
    (dolist (feature (seq-filter #'symbolp (my/integrity--feature-rows forms)))
      (puthash feature (1- (gethash feature mentions 0)) mentions))
    (and
     (my/integrity--setting forms 'use-package-always-defer)
     (mapcan
      (lambda (site)
        (let* ((form (car site))
               (sections (my/integrity--sections form))
               (startup (mapcan (lambda (section)
                                  (and (memq (car section) '(:init :preface :custom))
                                       (copy-sequence (cdr section))))
                                sections))
               (loads (lambda (symbol)
                        ;; Mentioned outside the block, or in a section
                        ;; of it that runs at startup.
                        (< 0 (+ (- (gethash symbol mentions 0) (funcall count symbol (list form)))
                                (funcall count symbol startup))))))
          (and (assq (cadr form) autoloads)
               (or (assq :after sections) (assq :config sections))
               (not (seq-some (lambda (section)
                                (and (memq (car section) my/integrity--load-keywords)
                                     (not (member section my/integrity--inert-load-sections))))
                              sections))
               (not (seq-some loads (assq (cadr form) autoloads)))
               (list (symbol-name (cadr form))))))
      (seq-filter (lambda (site) (eq (car (car site)) 'use-package))
                  (my/integrity--sites forms))))))

(defun my/integrity-duplicate-packages (forms)
  "Return the names of the packages FORMS declare with `use-package' more than once."
  (let ((names (my/integrity-packages forms)))
    (mapcar #'symbol-name
            (seq-uniq (seq-filter (lambda (name) (memq name (cdr (memq name names))))
                                  names)))))

(defun my/integrity-redundant-defaults (forms)
  "Return the `use-package' keywords in FORMS that repeat a default.
Each is reported as \"PACKAGE :KEYWORD t\":
- :defer t, when `use-package-always-defer' is set;
- :ensure t, when `use-package-always-ensure' is set, or when
  `straight-use-package-by-default' is, since straight.el then adds
  :straight t and ignores :ensure."
  (let ((defaults
         `((:defer . ,(my/integrity--setting forms 'use-package-always-defer))
           (:ensure . ,(or (my/integrity--setting forms 'use-package-always-ensure)
                           (my/integrity--setting forms 'straight-use-package-by-default))))))
    (mapcan
     (lambda (site)
       (and (eq (car (car site)) 'use-package)
            (mapcan (lambda (section)
                      (and (equal (cdr section) '(t))
                           (alist-get (car section) defaults)
                           (list (format "%s %s t" (cadr (car site)) (car section)))))
                    (my/integrity--sections (car site)))))
     (my/integrity--sites forms))))

(defun my/integrity-duplicate-bindings (bindings)
  "Return the subjects of the keys BINDINGS bind more than once in one map.
A binding to nil removes a key and is not counted."
  (let (seen duplicates)
    (dolist (binding bindings)
      (when (plist-get binding :command)
        (let ((subject (my/integrity--binding-subject binding)))
          (if (member subject seen)
              (push subject duplicates)
            (push subject seen)))))
    (seq-uniq (nreverse duplicates))))

(defun my/integrity-untypable-keys (bindings)
  "Return the subjects of BINDINGS whose key no keyboard produces.
Such a key holds an event with the shift modifier on a lowercase
letter, without control: Shift and a letter arrive as the uppercase
letter, so S-f never fires.  Control keeps the shift modifier, so
C-S-f is typable and not returned."
  (mapcan (lambda (binding)
            (and (seq-some (lambda (event)
                             (and (integerp event)
                                  (memq 'shift (event-modifiers event))
                                  (<= ?a (logand event #x3fffff) ?z)))
                           (key-parse (plist-get binding :key)))
                 (list (my/integrity--binding-subject binding))))
          bindings))

(defun my/integrity-lambda-bindings (bindings)
  "Return the subjects of BINDINGS whose command is an anonymous function.
Such a command has no name to call and no docstring for the cheatsheet."
  (mapcan (lambda (binding)
            (and (eq (plist-get binding :command) :lambda)
                 (list (my/integrity--binding-subject binding))))
          bindings))

(defun my/integrity-protected-key-shadows (bindings)
  "Return the subjects of BINDINGS that shadow a protected vanilla key.
Only bindings in the global map and in `override-global-map' are
considered; which other maps belong to global minor modes is known
only once their packages load.  A binding shadows a protected key
of `my/integrity-protected-keys' when the keys are equal or one is
a prefix of the other, unless the binding is listed in
`my/integrity-protected-key-overrides'."
  (mapcan
   (lambda (binding)
     (let ((key (plist-get binding :key)))
       (and (memq (plist-get binding :map) '(global override-global-map))
            (plist-get binding :command)
            (seq-some (lambda (protected) (my/integrity--keys-overlap-p key protected))
                      my/integrity-protected-keys)
            (not (eq (car (alist-get key my/integrity-protected-key-overrides nil nil #'equal))
                     (plist-get binding :command)))
            (list (my/integrity--binding-subject binding)))))
   bindings))

(defun my/integrity-mode-map-shadows (bindings)
  "Return the subjects of the mode-map BINDINGS that shadow a global one.
A mode map is a map whose name ends in -mode-map.  Its binding
shadows a binding of the global map, both made by BINDINGS, when
the keys are equal or one is a prefix of the other; a key under a
global prefix keymap (a :bind-keymap entry) adds to it rather than
shadowing it.  Keys under the prefixes of
`my/integrity-mode-map-prefixes' are deliberate and not returned."
  (let ((global (seq-filter (lambda (binding)
                              (and (eq (plist-get binding :map) 'global)
                                   (plist-get binding :command)))
                            bindings)))
    (mapcan
     (lambda (binding)
       (let ((key (plist-get binding :key)))
         (and (string-suffix-p "-mode-map" (symbol-name (plist-get binding :map)))
              (plist-get binding :command)
              (not (seq-some (lambda (prefix)
                               (or (equal key (car prefix))
                                   (string-prefix-p (concat (car prefix) " ") key)))
                             my/integrity-mode-map-prefixes))
              (seq-some (lambda (other)
                          (let ((prefix (plist-get other :key)))
                            (and (my/integrity--keys-overlap-p key prefix)
                                 (not (and (plist-get other :keymap)
                                           (< (length (key-parse prefix))
                                              (length (key-parse key))))))))
                        global)
              (list (my/integrity--binding-subject binding)))))
     bindings)))

(provide 't0-checks)
;;; t0-checks.el ends here
