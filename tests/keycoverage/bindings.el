;;; bindings.el --- Print the binding table of plan 086 as JSON -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `tests/coverage' in `emacs --batch -Q':
;;
;;   emacs --batch -Q -l tests/keycoverage/bindings.el ROOT
;;
;; It reads ROOT/early-init.el and ROOT/init.el as data with the
;; readers of plan 086 (`tests/integrity/config-forms.el'), and prints
;; their bindings, in reading order, as one JSON array of objects
;; {"map", "key", "command", "package", "prefix"}:
;; - map and key are as `my/integrity-bindings' gives them;
;; - command is the name of the bound symbol (":lambda" or
;;   ":unresolved" when the readers name none), or null when the form
;;   removes the binding;
;; - package is the enclosing `use-package', or null;
;; - prefix is true for a :bind-keymap entry, whose command is a keymap.
;; Nothing is evaluated: the configuration is only read.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "../integrity" (file-name-directory load-file-name)))
(require 'config-forms)

(let* ((root (pop command-line-args-left))
       (forms (apply #'append
                     (mapcar (lambda (name)
                               (my/integrity-forms (expand-file-name name root)))
                             '("early-init.el" "init.el")))))
  (princ (json-serialize
          (vconcat
           (mapcar (lambda (binding)
                     (let ((command (plist-get binding :command))
                           (package (plist-get binding :source)))
                       `((map . ,(symbol-name (plist-get binding :map)))
                         (key . ,(plist-get binding :key))
                         (command . ,(if command (symbol-name command) :null))
                         (package . ,(if package (format "%s" package) :null))
                         (prefix . ,(if (plist-get binding :keymap) t :false)))))
                   (my/integrity-bindings forms)))))
  (terpri))

;;; bindings.el ends here
