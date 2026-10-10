;;; scenario/outcome.el --- The outcome registry of the scenario language -*- lexical-binding: t -*-

;;; Commentary:
;; An outcome is a fact the user can observe after a scenario's keys,
;; written as a clause (KEYWORD ARG...) in `:then'.  Clause arguments
;; are data: they are never evaluated.
;;
;; Each keyword is defined once, with `my/scenario-define-outcome', in
;; a file under `tests/helpers/outcomes/'.  Every file there is loaded
;; by `outcomes.el', so a plan adds outcomes by adding its own file.
;;
;; A definition is a function of a context plist and the clause
;; arguments.  It runs in the buffer the user looks at after the keys,
;; and returns nil when the fact holds, otherwise a string that says
;; what the user would see instead.  The context holds:
;;
;;   :entry-buffer   the buffer the row opened;
;;   :before-text    its text just before the keys;
;;   :before-window  the window selected just before the keys;
;;   :errors         the errors the user saw, as condition data;
;;   :samples        an alist (NAME . SNAPSHOT) of (:sample NAME) steps.

;;; Code:

(defvar my/scenario-outcomes (make-hash-table :test #'eq)
  "Map each outcome keyword to (FUNCTION . FILE).
FILE is the file that defined it.")

(defun my/scenario-register-outcome (keyword function)
  "Register FUNCTION as the definition of the outcome KEYWORD.
Signal an error when another file already defined KEYWORD.  Loading
the same file again replaces its definition."
  (let ((file (or load-file-name buffer-file-name))
        (existing (gethash keyword my/scenario-outcomes)))
    (when (and existing (not (equal (cdr existing) file)))
      (error "Outcome %S is already defined in %s" keyword (cdr existing)))
    (puthash keyword (cons function file) my/scenario-outcomes)))

(defmacro my/scenario-define-outcome (keyword arglist docstring &rest body)
  "Define the outcome KEYWORD with ARGLIST, DOCSTRING and BODY.
ARGLIST names the context plist first, then the clause arguments.
BODY returns nil when the outcome holds, otherwise a string that
describes what the user would see instead."
  (declare (indent 2) (doc-string 3))
  (unless (and (keywordp keyword) (stringp docstring) (consp arglist))
    (error "Outcome definition needs a keyword, an arglist and a docstring: %S"
           keyword))
  (let ((name (intern (format "my/scenario-outcome%s" keyword))))
    `(progn
       (defun ,name ,arglist ,docstring ,@body)
       (my/scenario-register-outcome ,keyword #',name))))

(defun my/scenario-check-clause (clause)
  "Signal an error unless CLAUSE names a defined outcome with valid arguments."
  (let* ((entry (and (consp clause) (gethash (car clause) my/scenario-outcomes)))
         (arity (and entry (func-arity (car entry))))
         (given (1+ (length (cdr-safe clause)))))
    (cond ((not (and (consp clause) (proper-list-p clause)))
           (error "Scenario :then clause must be a list (KEYWORD ARG...): %S" clause))
          ((not entry)
           (error "Scenario :then: unknown outcome %S; see `my/scenario-vocabulary'"
                  (car clause)))
          ((or (< given (car arity))
               (and (numberp (cdr arity)) (> given (cdr arity))))
           (error "Scenario :then: wrong number of arguments in %S" clause)))))

(defun my/scenario-outcome-verdict (clause context)
  "Return nil when CLAUSE holds in the current buffer, else a description.
CONTEXT is the plist described in this file's commentary."
  (apply (car (gethash (car clause) my/scenario-outcomes)) context (cdr clause)))

(defun my/scenario-outcome-failures (clauses context)
  "Return a description of each failure of a scenario, or nil.
A failure is a clause of CLAUSES that does not hold with CONTEXT, or
an error the user saw while CLAUSES do not expect (:user-error
REGEXP)."
  (let ((errors (plist-get context :errors))
        (failures (delq nil (mapcar (lambda (clause)
                                      (when-let* ((verdict (my/scenario-outcome-verdict
                                                            clause context)))
                                        (format "%S: %s" clause verdict)))
                                    clauses))))
    (if (and errors (not (assq :user-error clauses)))
        (cons (format "unexpected error: %s"
                      (mapconcat #'error-message-string errors "; "))
              failures)
      failures)))

(provide 'scenario/outcome)

;;; scenario/outcome.el ends here
