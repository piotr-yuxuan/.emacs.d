;;; config-forms.el --- Read the configuration as data -*- lexical-binding: t -*-

;;; Commentary:
;; Pure readers over the forms of `init.el' and `early-init.el', shared
;; by the static checks (T0) and the session checks (T1) of plan 086.
;; Nothing here evaluates, expands or loads configuration code: each
;; reader walks the forms returned by `my/integrity-forms'.  A form the
;; readers cannot resolve is reported by `my/integrity-unresolved'
;; rather than guessed.

;;; Code:

(require 'seq)

(defconst my/integrity--binders
  '((let . 1) (let* . 1) (when-let . 1) (when-let* . 1) (if-let . 1)
    (if-let* . 1) (and-let* . 1) (dolist . 1) (dotimes . 1) (seq-doseq . 1)
    (cl-dolist . 1) (condition-case . 1) (lambda . 1) (defun . 2) (defmacro . 2)
    (defsubst . 2) (cl-defun . 2) (cl-defmacro . 2))
  "Forms that bind local variables, and the position of their bindings.
The element at that position is a variable, or a list whose
elements are variables or lists that start with one.")

(defconst my/integrity--binding-functions
  ;; FUNCTION         MAP KEY COMMAND KEY-STYLE  DEFAULT-MAP
  '((define-key        1   2   3       events     nil)
    (keymap-set        1   2   3       key-parse  nil)
    (global-set-key    nil 1   2       events     global-map)
    (keymap-global-set nil 1   2       key-parse  global-map)
    (bind-key          3   1   2       kbd        global-map)
    (bind-key*         3   1   2       kbd        override-global-map))
  "Functions that make one binding, and where their arguments are.
MAP, KEY and COMMAND are argument positions; MAP nil means the
function always binds in DEFAULT-MAP.  KEY-STYLE says how a string
key is read, see `my/integrity--row'.")

(defconst my/integrity--bind-sections
  '((:bind . global) (:bind* . override-global-map)
    (:bind-keymap . global) (:bind-keymap* . override-global-map))
  "The `use-package' sections that bind keys, and their default map.")

(defun my/integrity-forms (file)
  "Return the top-level forms of FILE, each as (FORM . LINE).
FORM is read with `read', without evaluation or macro expansion.
LINE is the 1-based line on which FORM starts.  Signal an error
naming FILE when a form cannot be read."
  (with-temp-buffer
    (insert-file-contents file)
    (with-syntax-table emacs-lisp-mode-syntax-table
      (let (forms)
        (condition-case err
            (while (progn (forward-comment (buffer-size)) (not (eobp)))
              (let ((line (line-number-at-pos)))
                (push (cons (read (current-buffer)) line) forms)))
          (error (error "%s:%d: %s" file (line-number-at-pos)
                        (error-message-string err))))
        (nreverse forms)))))

(defun my/integrity--sites (forms)
  "Return the call sites of FORMS, in reading order.
Each site is (FORM PACKAGE BOUND LINE): FORM is a list that is not
quoted data; PACKAGE is the name of the innermost `use-package'
around FORM, or nil; BOUND lists the names bound locally around
FORM; LINE is the line of its top-level form."
  (let ((stack (mapcar (lambda (entry) (list (car entry) nil nil (cdr entry)))
                       forms))
        sites)
    (while stack
      (pcase-let ((`(,form ,package ,bound ,line) (pop stack)))
        (when (and (consp form) (not (eq (car form) 'quote)))
          (push (list form package bound line) sites)
          (let* ((position (alist-get (car form) my/integrity--binders))
                 (names (and position (proper-list-p form) (nth position form)))
                 (package (if (eq (car form) 'use-package) (cadr form) package))
                 (bound (append (mapcar (lambda (name) (if (consp name) (car name) name))
                                        (ensure-list names))
                                bound))
                 (children nil))
            (while (consp form)
              (push (list (car form) package bound line) children)
              (setq form (cdr form)))
            (setq stack (nconc (nreverse children) stack))))))
    (nreverse sites)))

(defun my/integrity--sections (form)
  "Return the keyword sections of the `use-package' FORM, in order.
Each section is (KEYWORD . VALUES); a keyword given twice gives two
sections."
  (let (sections)
    (dolist (item (cddr form))
      (if (keywordp item)
          (push (list item) sections)
        (when sections
          (push item (cdar sections)))))
    (nreverse (mapcar (lambda (section)
                        (cons (car section) (reverse (cdr section))))
                      sections))))

(defun my/integrity--command (form evaluated)
  "Return the command FORM denotes: a symbol, nil, :lambda or :unresolved.
EVALUATED is non-nil when the binding function evaluates FORM, as
`define-key' does; :bind and `bind-keys' take a bare symbol as the
command itself."
  (pcase form
    ('nil nil)
    (`(,(or 'quote 'function) ,(and (pred symbolp) command)) command)
    ((or `(lambda . ,_) `(function (lambda . ,_))) :lambda)
    ((and (pred symbolp) (guard (not evaluated))) form)
    (_ :unresolved)))

(defun my/integrity--row (site map key style command)
  "Return the row of a binding made at SITE.
MAP and KEY are the forms naming the keymap and the key, COMMAND the
command already resolved.  STYLE says how a string KEY is read:
`kbd' for `bind-key' and :bind, `key-parse' for `keymap-set', and
`events' for `define-key' and `global-set-key', which take the
characters of a string as events and also accept a `kbd' or
`key-parse' call.  The row is a binding plist, or (:unresolved
DESCRIPTION :line LINE) when the map or the key cannot be resolved,
as for a map held in a variable bound around SITE."
  (let ((keymap (pcase map
                  ((or 'global 'global-map '(current-global-map) ''global-map) 'global)
                  ((and (pred symbolp) (pred identity) (guard (not (memq map (nth 2 site)))))
                   map)
                  (`(quote ,(and (pred symbolp) (pred identity) name)) name)))
        (description
         (ignore-errors
           (pcase (list style key)
             (`(,_ ,(pred vectorp)) (key-description key))
             (`(kbd ,(pred stringp)) (key-description (kbd key)))
             (`(key-parse ,(and (pred stringp) (pred key-valid-p)))
              (key-description (key-parse key)))
             (`(events ,(pred stringp)) (key-description key))
             (`(events (,(and (or 'kbd 'key-parse) parse) ,(and (pred stringp) string)))
              (key-description (funcall parse string)))))))
    (if (and keymap description)
        (list :map keymap :key description :command command
              :source (nth 1 site) :line (nth 3 site))
      (list :unresolved (format "%S in %S: map %S, key %S not resolved"
                                (car-safe (car site)) (nth 1 site) map key)
            :line (nth 3 site)))))

(defun my/integrity--bind-rows (entries map keymap site)
  "Return the rows of the `bind-keys' style ENTRIES made at SITE.
ENTRIES are bindings (KEY . COMMAND), `:map MAP' switches and lists
of these, as in `bind-keys' or a `use-package' :bind section.  They
bind in MAP until a switch.  KEYMAP non-nil marks :bind-keymap
entries, whose command is a keymap variable.  Reading stops at any
other keyword, which is reported as unresolved."
  (let (rows)
    (while entries
      (let ((entry (pop entries)))
        (cond
         ((eq entry :map) (setq map (pop entries)))
         ((and (consp entry) (or (stringp (car entry)) (vectorp (car entry))))
          (let ((row (my/integrity--row site map (car entry) 'kbd
                                        (my/integrity--command (cdr entry) nil))))
            (push (if (and keymap (plist-member row :map)) (append row '(:keymap t)) row)
                  rows)))
         ((and (proper-list-p entry) (or (consp (car entry)) (keywordp (car entry))))
          (setq entries (append entry entries)))
         (t
          (push (list :unresolved (format "%S in %S: %S not read"
                                          (car-safe (car site)) (nth 1 site) entry)
                      :line (nth 3 site))
                rows)
          (setq entries nil)))))
    (nreverse rows)))

(defun my/integrity--binding-rows (forms)
  "Return the binding rows of FORMS: binding plists and unresolved rows."
  (mapcan
   (lambda (site)
     (let* ((form (car site))
            (spec (alist-get (car form) my/integrity--binding-functions)))
       (cond
        (spec
         (pcase-let ((`(,map ,key ,command ,style ,default) spec))
           (list (my/integrity--row site (or (and map (nth map form)) default)
                                    (nth key form) style
                                    (my/integrity--command (nth command form) t)))))
        ((memq (car form) '(bind-keys bind-keys*))
         (my/integrity--bind-rows (cdr form)
                                  (if (eq (car form) 'bind-keys) 'global 'override-global-map)
                                  nil site))
        ((eq (car form) 'use-package)
         (mapcan (lambda (section)
                   (let ((default (alist-get (car section) my/integrity--bind-sections)))
                     (and default
                          (my/integrity--bind-rows
                           (cdr section) default
                           (memq (car section) '(:bind-keymap :bind-keymap*))
                           (list form (cadr form) (nth 2 site) (nth 3 site))))))
                 (my/integrity--sections form)))
        ((memq (car form) '(local-set-key keymap-local-set local-unset-key keymap-local-unset
                            global-unset-key keymap-global-unset keymap-unset unbind-key
                            define-keymap defvar-keymap define-key-after keymap-set-after
                            substitute-key-definition keymap-substitute))
         (list (list :unresolved (format "%S in %S binds keys the readers do not read"
                                         (car form) (nth 1 site))
                     :line (nth 3 site)))))))
   (my/integrity--sites forms)))

(defun my/integrity--feature-rows (forms)
  "Return the features FORMS wait for, and unresolved rows, in order."
  (mapcan
   (lambda (site)
     (pcase (car site)
       (`(,(or 'with-eval-after-load 'eval-after-load)
          (quote ,(and (pred symbolp) feature)) . ,_)
        (list feature))
       (`(,(and (or 'with-eval-after-load 'eval-after-load) waiting) ,what . ,_)
        (list (list :unresolved (format "%S in %S waits for %S"
                                        waiting (nth 1 site) what)
                    :line (nth 3 site))))
       (`(use-package . ,_)
        (mapcan (lambda (section)
                  (and (eq (car section) :after)
                       (seq-remove #'keywordp (flatten-tree (cdr section)))))
                (my/integrity--sections (car site))))))
   (my/integrity--sites forms)))

(defun my/integrity-bindings (forms)
  "Return the key bindings made by FORMS, in reading order.
FORMS is a list as returned by `my/integrity-forms'.  Bindings come
from `use-package' :bind, :bind*, :bind-keymap and :bind-keymap*,
and from `bind-key', `bind-key*', `bind-keys', `bind-keys*',
`define-key', `keymap-set', `keymap-global-set' and
`global-set-key', wherever they appear.

Each binding is a plist (:map MAP :key KEY :command COMMAND
:source SOURCE :line LINE), with :keymap t added when the binding
is a :bind-keymap entry:
- MAP is a keymap variable symbol, or the symbol `global' for the
  global map (`global-map', `current-global-map', the default of
  `global-set-key', `bind-key' and :bind).  :bind* and `bind-key*'
  bind in `override-global-map'.
- KEY is a `key-description' string: of `key-parse' for
  `keymap-set' and `keymap-global-set', of `kbd' for strings given
  to `bind-key' and :bind, and of the literal string or vector, or
  of the `kbd' or `key-parse' call, given to `define-key' and
  `global-set-key'.
- COMMAND is a symbol, nil when the form removes a binding,
  :lambda for a lambda, or :unresolved for anything else (for
  example a variable).  For a :bind-keymap entry it is the keymap
  variable.
- SOURCE is the name of the innermost enclosing `use-package', or
  nil.
- LINE is the line of the enclosing top-level form.
A binding whose map or key cannot be resolved is not returned; it
is reported by `my/integrity-unresolved'."
  (seq-remove (lambda (row) (eq (car-safe row) :unresolved))
              (my/integrity--binding-rows forms)))

(defun my/integrity-packages (forms)
  "Return the names of the `use-package' forms in FORMS, in order.
Nested declarations count, and duplicates are kept."
  (mapcan (lambda (site)
            (and (eq (car (car site)) 'use-package)
                 (list (cadr (car site)))))
          (my/integrity--sites forms)))

(defun my/integrity-after-load-features (forms)
  "Return the features FORMS wait for, in order, without duplicates.
They are the quoted features of `with-eval-after-load' and
`eval-after-load', and the features named by `use-package' :after
\(the keywords :all, :any, :and and :or are not features)."
  (seq-uniq (seq-filter #'symbolp (my/integrity--feature-rows forms))))

(defun my/integrity-set-variables (forms)
  "Return the variables FORMS set, in order, as (VARIABLE . PACKAGE).
The setters are `setq', `setq-default', `setq-local', `setopt',
`customize-set-variable' with a quoted variable, and `use-package'
:custom.  A `setq' of a name bound locally by an enclosing `let',
`let*', `lambda', `defun', `dolist', `dotimes', `condition-case'
or similar form is not a configuration variable and is left out.
PACKAGE is the name of the innermost enclosing `use-package', or
nil.  Duplicates are kept."
  (mapcan
   (lambda (site)
     (pcase-let ((`(,form ,package ,bound ,_line) site))
       (pcase form
         (`(,(or 'setq 'setq-default 'setq-local 'setopt) . ,pairs)
          (let (variables)
            (while (consp pairs)
              (unless (memq (car pairs) bound)
                (push (cons (car pairs) package) variables))
              (setq pairs (cddr pairs)))
            (nreverse variables)))
         (`(customize-set-variable (quote ,(and (pred symbolp) variable)) . ,_)
          (list (cons variable package)))
         (`(use-package ,name . ,_)
          (mapcan (lambda (section)
                    (and (eq (car section) :custom)
                         (mapcan (lambda (entry)
                                   (and (consp entry) (symbolp (car entry))
                                        (list (cons (car entry) name))))
                                 (mapcan (lambda (value)
                                           (if (consp (car-safe value))
                                               (copy-sequence value)
                                             (list value)))
                                         (cdr section)))))
                  (my/integrity--sections form))))))
   (my/integrity--sites forms)))

(defun my/integrity-unresolved (forms)
  "Return what the readers could not resolve in FORMS, in order.
Each element is (DESCRIPTION . LINE): DESCRIPTION says which form
and which part of it, LINE is the line of the enclosing top-level
form."
  (sort (mapcan (lambda (row)
                  (and (eq (car-safe row) :unresolved)
                       (list (cons (plist-get row :unresolved) (plist-get row :line)))))
                (append (my/integrity--binding-rows forms)
                        (my/integrity--feature-rows forms)))
        (lambda (a b) (< (cdr a) (cdr b)))))

(provide 'config-forms)
;;; config-forms.el ends here
