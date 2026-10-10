;;; scenario.el --- The scenario language: starts, keys and outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; A scenario says what the user does and what the user then sees:
;;
;;   (scenario
;;    :describe "C-<right> slurps the next form, point stays"
;;    :given ((:name "clj" :file "src/example.clj" :text "(+ 1-!-) 2")
;;            (:name "el" :file "example.el" :text "(+ 1-!-) 2"))
;;    :when ((:keys "C-<right>"))
;;    :then ((:buffer-text "(+ 1-!- 2)")
;;           (:balanced)))
;;
;; Every slot is data, written unquoted, and validated when the form
;; expands; nothing in it is evaluated.
;;
;;   :describe  the name of the buttercup suite;
;;   :given     rows, each one start of the same story and one spec
;;              named "[NAME]" (see `scenario/row.el');
;;   :wait-for  optional conditions to wait for, as an idle user, after
;;              the entry buffer opens (see `scenario/wait.el');
;;   :when      steps, pressed as one key sequence (see `keystrokes.el');
;;   :then      outcomes (see `scenario/outcome.el' and
;;              `tests/helpers/outcomes/').
;;
;; Each spec runs `my/scenario-run-row'.  The rows of one file share
;; one Emacs session: a scenario that needs the first file of a fresh
;; session belongs alone in its own file.  `my/scenario-vocabulary'
;; generates the reference of the steps and outcomes.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'subr-x)
(require 'keystrokes)
(require 'scenario/outcome)
(require 'scenario/row)
(require 'scenario/system)
(require 'scenario/wait)
(require 'outcomes)

(defmacro scenario (&rest spec)
  "Define a buttercup suite from the scenario SPEC, one spec per row.
SPEC is a plist with :describe, :given, :when and :then, and
optionally :wait-for; see the commentary of `scenario.el'."
  (let ((slots (cl-loop for (slot _) on spec by #'cddr collect slot))
        (given (plist-get spec :given))
        (then (plist-get spec :then))
        (wait-for (plist-get spec :wait-for)))
    (unless (and (cl-evenp (length spec))
                 (cl-subsetp '(:describe :given :when :then) slots)
                 (cl-subsetp slots '(:describe :given :wait-for :when :then))
                 (stringp (plist-get spec :describe)))
      (error "Scenario needs :describe (a string), :given, :when and :then, \
and may have :wait-for: %S" slots))
    (dolist (list (list given (plist-get spec :when) then))
      (unless (and (consp list) (proper-list-p list) (not (eq (car list) 'quote)))
        (error "Scenario :given, :when and :then must be non-empty unquoted \
lists: %S" list)))
    (mapc #'my/scenario-row-check given)
    (mapc #'my/scenario-check-clause then)
    (my/scenario-wait-check wait-for)
    (let ((names (mapcar (lambda (row) (plist-get row :name)) given))
          (keys (my/scenario-compile-steps (plist-get spec :when))))
      (unless (equal names (delete-dups (copy-sequence names)))
        (error "Scenario rows need distinct names: %S" names))
      `(describe ,(plist-get spec :describe)
         ,@(mapcar (lambda (row)
                     `(it ,(format "[%s]" (plist-get row :name))
                        (my/scenario-run-row ',row ',keys ',then ',wait-for)))
                   given)))))

(defun my/scenario-run-row (row keys clauses wait-for)
  "Run ROW of a scenario as the body of a buttercup spec.
KEYS is the compiled `:when', CLAUSES the `:then' and WAIT-FOR the
`:wait-for' of the scenario.  Skip when WAIT-FOR needs a missing
executable.  Otherwise, in a fresh sandbox and with the row's system
boundaries stubbed, open the entry buffer, wait and press the keys as
the user, then check every outcome in the buffer of the selected
window.  An error the user saw fails the spec unless CLAUSES expect
\(:user-error REGEXP).  Clean up even on failure, with
`my/scenario-row-clean-up', and restore the boundaries even when that
fails; an error in either fails a spec that would otherwise pass."
  (my/scenario-wait-skip-unless-available wait-for)
  (let ((sandbox (my/scenario-sandbox-create))
        (window (selected-window))
        (restore #'ignore)
        (before nil)
        (cleaned nil))
    (unwind-protect
        (progn
          (setq restore (my/scenario-system-apply
                         (plist-get row :system)
                         (my/scenario-row-root row sandbox)))
          (let ((seen (my/scenario-as-user
                       (lambda ()
                         (let ((entry (my/scenario-row-open row sandbox)))
                           ;; After each command, the command loop shows
                           ;; the selected window's buffer and redisplays,
                           ;; which runs the window change hooks.
                           (redisplay t)
                           (my/scenario-wait wait-for)
                           (setq before
                                 (list :entry-buffer entry
                                       :before-text (with-current-buffer entry
                                                      (buffer-substring-no-properties
                                                       (point-min) (point-max)))
                                       :before-window (selected-window)))
                           (my/scenario-press keys)
                           (set-buffer (window-buffer (selected-window)))
                           (redisplay t))))))
            (unless before
              (buttercup-fail "The row did not start: %s"
                              (mapconcat #'error-message-string
                                         (plist-get seen :errors) "; ")))
            (set-buffer (window-buffer (selected-window)))
            (when-let* ((failures (my/scenario-outcome-failures
                                   clauses (append before seen))))
              (buttercup-fail "%s" (string-join failures "\n")))))
      ;; Killing buffers can run hooks and timers, such as a language
      ;; server's shutdown, whose errors are shown as in a real session.
      (setq cleaned (my/scenario-as-user
                     (lambda ()
                       (unwind-protect (my/scenario-row-clean-up sandbox window)
                         (funcall restore))))))
    (when-let* ((errors (plist-get cleaned :errors)))
      (buttercup-fail "Cleaning up the row failed: %s"
                      (mapconcat #'error-message-string errors "; ")))))

(defun my/scenario-vocabulary ()
  "Return the reference of the scenario steps and outcomes as Org text.
It is generated from `my/scenario-steps' and `my/scenario-outcomes'."
  (let ((entry (lambda (keyword function arguments)
                 (format "- =%S= :: %s\n"
                         (cons keyword
                               (mapcar (lambda (argument)
                                         (if (memq argument '(&optional &rest))
                                             argument
                                           (intern (upcase (symbol-name argument)))))
                                       arguments))
                         (car (split-string (documentation function t) "\n"))))))
    (concat
     "* Steps of :when\n\n"
     (mapconcat (lambda (step)
                  (funcall entry (car step) (cdr step)
                           (help-function-arglist (cdr step) t)))
                my/scenario-steps "")
     "\n* Outcomes of :then\n\n"
     (mapconcat (lambda (keyword)
                  (let ((function (car (gethash keyword my/scenario-outcomes))))
                    (funcall entry keyword function
                             (cdr (help-function-arglist function t)))))
                (sort (hash-table-keys my/scenario-outcomes) #'string<) ""))))

(provide 'scenario)

;;; scenario.el ends here
