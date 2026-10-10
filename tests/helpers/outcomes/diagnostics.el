;;; outcomes/diagnostics.el --- What checkers report, whatever shows it -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 090.  A diagnostic is a finding the user reads in a buffer: a
;; Flycheck error or a Flymake diagnostic.  These outcomes count both
;; front ends together, so a scenario holds whichever one shows a
;; finding, and a finding shown by both counts twice, as the user sees
;; it twice.  The text of a diagnostic is what its front end shows: the
;; message of a Flycheck error followed by its id in brackets, or the
;; text of a Flymake diagnostic.  Lines count from 1.
;;
;; Checkers report asynchronously.  Before counting, the outcomes wait,
;; as an idle user and for at most `my/diagnostics-wait-seconds', until
;; the diagnostics of the buffer have settled:
;; - `my/diagnostics-quiet-seconds' of idle time have passed, so the
;;   idle timers that start a deferred language server or a deferred
;;   check have run, and no language server has published diagnostics
;;   during the last such period, so the checks a publish triggers have
;;   started;
;; - no source of `my/diagnostics-sources' is still pending; an
;;   lsp-mode workspace is pending until it has published, or for
;;   `my/diagnostics-lsp-grace-seconds' after its initialization.
;; Publishes are observed through the hooks lsp-mode and LTeX+ run for
;; them; nothing in the configuration is replaced.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'keystrokes)
(require 'scenario/outcome)

(defvar lsp--cur-workspace)
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(declare-function flycheck-running-p "flycheck")
(declare-function flycheck-deferred-check-p "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-checker "flycheck")
(declare-function flycheck-error-format-message-and-id "flycheck")
(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-backend "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function lsp-workspaces "lsp-mode")

(defvar my/diagnostics-wait-seconds 90
  "Seconds a diagnostic outcome waits, in all, for the diagnostics to settle.
A cold LTeX+ server loads its language models before it publishes.")

(defvar my/diagnostics-quiet-seconds 1.5
  "Idle seconds, and seconds since the last publish, before diagnostics settle.
Longer than the idle delays of Flycheck, lsp-mode and LTeX+, 0.5 s each.")

(defvar my/diagnostics-lsp-grace-seconds 30
  "Seconds an initialized lsp-mode workspace may take to publish, at most.
Some servers, such as marksman, publish nothing for a file without
findings, so a workspace that has not published by then counts as
finished.  A loaded machine can take ten seconds to publish.")

(defvar my/diagnostics--published (make-hash-table :test #'eq :weakness 'key)
  "The lsp-mode workspaces and LTeX+ buffers that have published diagnostics.")

(defvar my/diagnostics--initialized (make-hash-table :test #'eq :weakness 'key)
  "When each lsp-mode workspace was initialized, as a `float-time'.")

(defvar my/diagnostics--last-publish 0.0
  "When a language server last published diagnostics, as a `float-time'.")

(defun my/diagnostics--note-publish (&optional buffer)
  "Record that diagnostics were published for BUFFER or by a workspace.
LTeX+ runs `lsp-ltex-plus--diagnostics-functions' with the BUFFER;
lsp-mode runs `lsp-diagnostics-updated-hook' with no argument while
`lsp--cur-workspace' is the publishing workspace."
  (when-let* ((publisher (or buffer (bound-and-true-p lsp--cur-workspace))))
    (puthash publisher t my/diagnostics--published))
  (setq my/diagnostics--last-publish (float-time)))

(defun my/diagnostics--note-initialized ()
  "Record when `lsp--cur-workspace' was initialized.
On `lsp-after-initialize-hook', which runs once the workspace has
opened its buffers."
  (puthash lsp--cur-workspace (float-time) my/diagnostics--initialized))

;; Only once the packages define their hooks: lsp-mode makes an obsolete
;; alias of its hook, and warns when the hook already has a value.
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-diagnostics-updated-hook #'my/diagnostics--note-publish)
  (add-hook 'lsp-after-initialize-hook #'my/diagnostics--note-initialized))
(with-eval-after-load 'lsp-ltex-plus-conn
  (add-hook 'lsp-ltex-plus--diagnostics-functions #'my/diagnostics--note-publish))
;; Turning LTeX+ on again, as `lsp-ltex-plus-restart-server' does, opens
;; the buffer on a new server: wait for that server's publish.
(add-hook 'lsp-ltex-plus-mode-hook
          (lambda ()
            (when lsp-ltex-plus-mode
              (remhash (current-buffer) my/diagnostics--published))))

(defconst my/diagnostics-sources
  (list
   (cons "an lsp-mode workspace is starting or has not published"
         (lambda ()
           (and (bound-and-true-p lsp-mode)
                (cl-some (lambda (workspace)
                           (let ((initialized (gethash workspace my/diagnostics--initialized)))
                             (not (or (gethash workspace my/diagnostics--published)
                                      (and initialized
                                           (> (- (float-time) initialized)
                                              my/diagnostics-lsp-grace-seconds))))))
                         (lsp-workspaces)))))
   (cons "LTeX+ has not published"
         (lambda ()
           (and (bound-and-true-p lsp-ltex-plus-mode)
                (not (gethash (current-buffer) my/diagnostics--published)))))
   (cons "Flycheck has a check running or pending"
         (lambda ()
           (and (bound-and-true-p flycheck-mode)
                (or (flycheck-running-p)
                    (flycheck-deferred-check-p)
                    (not (memq flycheck-last-status-change
                               '(finished errored suspicious no-checker)))))))
   (cons "a Flymake backend has not reported"
         (lambda ()
           ;; A disabled backend counts as reported; one that reports
           ;; repeatedly, such as a language server's, stays running.
           (and (bound-and-true-p flymake-mode)
                (let ((reported (flymake-reporting-backends)))
                  (run-hook-wrapped 'flymake-diagnostic-functions
                                    (lambda (backend)
                                      (not (memq backend reported)))))))))
  "The sources of diagnostics a buffer may wait for: (DESCRIPTION . PENDING).
PENDING is called in the buffer and returns non-nil while the source
has not finished.")

(defun my/diagnostics-count-verdict (line regexp expected)
  "Return nil when EXPECTED diagnostics on LINE have a text matching REGEXP.
Otherwise return what the user sees instead.  Count once the
diagnostics of the current buffer have settled; when they have not
settled within `my/diagnostics-wait-seconds', fail whatever the count.
See the commentary."
  (let ((buffer (current-buffer))
        (start (float-time))
        (pending nil)
        (case-fold-search nil))
    (my/scenario-as-user
     (lambda ()
       (my/scenario-idle
        (* 1000 my/diagnostics-wait-seconds)
        (lambda ()
          (with-current-buffer buffer
            (setq pending
                  (cond ((< (- (float-time) start) my/diagnostics-quiet-seconds)
                         "the first quiet period")
                        ((< (- (float-time) my/diagnostics--last-publish)
                            my/diagnostics-quiet-seconds)
                         "a language server published less than a quiet period ago")
                        (t (car (cl-find-if (lambda (source) (funcall (cdr source)))
                                            my/diagnostics-sources)))))
            (not pending))))))
    (let* ((shown
            (append
             (and (bound-and-true-p flycheck-mode)
                  (mapcar (lambda (err)
                            (list (flycheck-error-line err) (flycheck-error-checker err)
                                  (flycheck-error-format-message-and-id err)))
                          flycheck-current-errors))
             (and (bound-and-true-p flymake-mode)
                  (mapcar (lambda (diagnostic)
                            (list (save-restriction
                                    (widen)
                                    (line-number-at-pos (flymake-diagnostic-beg diagnostic)))
                                  (flymake-diagnostic-backend diagnostic)
                                  (flymake-diagnostic-text diagnostic)))
                          (flymake-diagnostics)))))
           (count (cl-count-if (lambda (diagnostic)
                                 (and (eql (car diagnostic) line)
                                      (string-match-p regexp (nth 2 diagnostic))))
                               shown)))
      ;; A count taken before the checkers finished proves nothing.
      (unless (and (not pending) (eql count expected))
        (format "%d diagnostics on line %s match %S, not %s%s; after %.1f s, \
the buffer shows (LINE SOURCE TEXT): %S"
                count line regexp expected
                (if pending (format ", and the wait ended while %s" pending) "")
                (- (float-time) start) shown)))))

(my/scenario-define-outcome :diagnostic-at (_context line regexp)
  "Exactly one diagnostic on LINE has a text matching REGEXP.
Flycheck errors and Flymake diagnostics count together, once the
checkers have settled; see `outcomes/diagnostics.el'."
  (my/diagnostics-count-verdict line regexp 1))

(my/scenario-define-outcome :diagnostic-count (_context line regexp count)
  "COUNT diagnostics on LINE have a text matching REGEXP.
As `:diagnostic-at', for a finding the user is expected to see COUNT
times, or never with a COUNT of 0."
  (my/diagnostics-count-verdict line regexp count))

(my/scenario-define-outcome :key-binding (_context key command)
  "KEY, in `key-parse' syntax, runs COMMAND in the current buffer.
For a mouse key, which keystrokes cannot dispatch without a position
(the documented mouse edge of plan 053a): read the binding that the
dispatch resolves with `key-binding'."
  (let ((actual (key-binding (key-parse key))))
    (unless (eq actual command)
      (format "%s runs %S" key actual))))

(my/scenario-define-outcome :executables-found (_context &rest names)
  "The session finds every program of NAMES on `exec-path'.
Otherwise skip the spec, before any other outcome waits when this one
comes first in :then: the scenario needs those programs.  The runner
accepts the skip only with an expected-skip entry whose reason holds,
such as (:missing-executable NAME)."
  (when-let* ((missing (seq-remove #'executable-find names)))
    (buttercup-skip "missing executable %s" (string-join missing ", "))))

(provide 'outcomes/diagnostics)

;;; outcomes/diagnostics.el ends here
