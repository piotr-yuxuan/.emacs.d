;;; scenario/wait.el --- What a scenario waits for before its keys -*- lexical-binding: t -*-

;;; Commentary:
;; A scenario's optional `:wait-for' lists conditions that must hold
;; after the entry buffer opens and before the keys are pressed.  The
;; wait is user idle time (`my/scenario-idle'), so idle timers run as
;; in a real session; that is how `lsp-deferred' starts a language
;; server.
;;
;;   (:lsp-attached SERVER)   a workspace of SERVER, a symbol of
;;                            `my/scenario-lsp-servers', serves the
;;                            entry buffer;
;;   (:lsp-indexed)           every workspace of the entry buffer is
;;                            initialized and reports no work in
;;                            progress.
;;
;; A spec waiting for a server whose executable is missing is skipped
;; with the message "missing executable NAME"; the runner accepts that
;; skip only when an expected-skip entry with the reason
;; (:missing-executable "NAME") holds.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'keystrokes)

(declare-function lsp-workspaces "lsp-mode")
(declare-function lsp--workspace-server-id "lsp-mode")
(declare-function lsp--workspace-status "lsp-mode")
(declare-function lsp--workspace-work-done-tokens "lsp-mode")

(defconst my/scenario-lsp-servers
  '((clojure-lsp "clojure-lsp")
    (pyright "basedpyright-langserver")
    (ruff "ruff"))
  "Language servers a scenario may wait for, with the executables they need.")

(defvar my/scenario-wait-seconds 120
  "Seconds a scenario waits, in all, for its `:wait-for' conditions.
Cold language servers can take a minute to index a project.")

(defun my/scenario-wait-check (conditions)
  "Signal an error unless CONDITIONS is a valid `:wait-for' list."
  (unless (proper-list-p conditions)
    (error "Scenario :wait-for must be a list of conditions: %S" conditions))
  (dolist (condition conditions)
    (unless (pcase condition
              (`(:lsp-attached ,server) (assq server my/scenario-lsp-servers))
              ('(:lsp-indexed) t))
      (error "Scenario :wait-for: unknown condition %S" condition))))

(defun my/scenario-wait-skip-unless-available (conditions)
  "Skip the running spec when CONDITIONS need a missing executable."
  (pcase-dolist (`(,kind ,server) conditions)
    (when (eq kind :lsp-attached)
      (dolist (executable (cdr (assq server my/scenario-lsp-servers)))
        (unless (executable-find executable)
          (buttercup-skip "missing executable %s" executable))))))

(defun my/scenario-wait (conditions)
  "Wait, as an idle user, until each of CONDITIONS holds in turn.
Signal an error when they do not all hold within
`my/scenario-wait-seconds'."
  (let ((deadline (+ (float-time) my/scenario-wait-seconds)))
    (dolist (condition conditions)
      (unless (my/scenario-idle
               (max 1 (round (* 1000 (- deadline (float-time)))))
               (lambda ()
                 (and (fboundp 'lsp-workspaces)
                      (pcase condition
                        (`(:lsp-attached ,server)
                         (cl-some (lambda (workspace)
                                    (eq (lsp--workspace-server-id workspace) server))
                                  (lsp-workspaces)))
                        ('(:lsp-indexed)
                         (and (lsp-workspaces)
                              (cl-every
                               (lambda (workspace)
                                 (and (eq (lsp--workspace-status workspace)
                                          'initialized)
                                      (zerop (hash-table-count
                                              (lsp--workspace-work-done-tokens
                                               workspace)))))
                               (lsp-workspaces))))))))
        (error "Scenario :wait-for %S did not hold within %s seconds"
               condition my/scenario-wait-seconds)))))

(provide 'scenario/wait)

;;; scenario/wait.el ends here
