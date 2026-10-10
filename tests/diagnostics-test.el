;;; diagnostics-test.el --- Self-tests of plan 090's diagnostic outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; The diagnostic outcomes must count what the user sees: a finding
;; shown by Flycheck and by Flymake counts twice (DD-07), a finding on
;; another line or with another text does not count, and nothing is
;; counted before the checkers have finished.  The plausible wrong
;; implementations these specs reject read one front end only, ignore
;; the line, match the message without the id, or count at once.
;;
;; The buffers here have no checker program: Flycheck's state is set as
;; a finished check leaves it, and a Flymake backend of this file
;; reports synchronously.  The scenarios of tests/scenarios/l3-diagnostics*.el
;; run the real checkers.

;;; Code:

(require 'buttercup)
(require 'flycheck)
(require 'flymake)
(require 'scenario/outcome)
(require 'outcomes)

(defun diagnostics-test--backend (report-fn &rest _)
  "Report one shellcheck-like note on line 2 through REPORT-FN."
  (funcall report-fn
           (list (flymake-make-diagnostic (current-buffer)
                                          (save-excursion (goto-char (point-min))
                                                          (line-beginning-position 2))
                                          (save-excursion (goto-char (point-min))
                                                          (line-end-position 2))
                                          :warning "SC2086: Double quote to prevent globbing"))))
(function-put #'diagnostics-test--backend 'flymake-always-safe t)

(defun diagnostics-test--silent-backend (_report-fn &rest _)
  "Never report, as a backend whose program has not answered yet."
  nil)
(function-put #'diagnostics-test--silent-backend 'flymake-always-safe t)

(defmacro diagnostics-test--with-buffer (spec &rest body)
  "Run BODY in a buffer of three lines whose checkers SPEC describes.
SPEC is a plist: :flycheck, a list of (LINE MESSAGE ID) for a
finished Flycheck check, or the symbol `running' for one still
running; :flymake, a list of Flymake backends of this file."
  (declare (indent 1))
  `(with-temp-buffer
     (insert "#!/bin/sh\necho $name\necho done\n")
     (let ((flycheck (plist-get ',spec :flycheck))
           (backends (plist-get ',spec :flymake))
           (my/diagnostics-quiet-seconds 0.05)
           (my/diagnostics-wait-seconds 0.5))
       (when flycheck
         ;; The state a check leaves, without a checker program.
         (setq-local flycheck-mode t
                     flycheck-last-status-change (if (eq flycheck 'running)
                                                     'running
                                                   'finished)
                     flycheck-current-errors
                     (unless (eq flycheck 'running)
                       (mapcar (pcase-lambda (`(,line ,message ,id))
                                 (flycheck-error-new-at line 1 'warning message
                                                        :id id :checker 'sh-shellcheck))
                               flycheck))))
       (when backends
         (setq-local flymake-diagnostic-functions backends)
         (flymake-mode 1)
         (flymake-start))
       (unwind-protect (progn ,@body)
         (when backends (flymake-mode -1))))))

(defun diagnostics-test--verdict (clause)
  "Return the verdict of CLAUSE in the current buffer."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause nil))

(describe "The diagnostic outcomes"

  (it "count a finding shown by Flycheck and by Flymake twice"
    (diagnostics-test--with-buffer (:flycheck ((2 "Double quote" "SC2086"))
                                    :flymake (diagnostics-test--backend))
      (expect (diagnostics-test--verdict '(:diagnostic-at 2 "SC2086"))
              :to-match "\\`2 diagnostics on line 2")
      (expect (diagnostics-test--verdict '(:diagnostic-count 2 "SC2086" 2)) :to-be nil)))

  (it "count a Flymake diagnostic alone"
    (diagnostics-test--with-buffer (:flymake (diagnostics-test--backend))
      (expect (diagnostics-test--verdict '(:diagnostic-at 2 "Double quote")) :to-be nil)))

  (it "match the id Flycheck shows after the message"
    (diagnostics-test--with-buffer (:flycheck ((2 "Double quote" "SC2086")))
      (expect (diagnostics-test--verdict '(:diagnostic-at 2 "Double quote \\[SC2086\\]"))
              :to-be nil)))

  (it "count only the diagnostics of the line"
    (diagnostics-test--with-buffer (:flycheck ((3 "Double quote" "SC2086")))
      (expect (diagnostics-test--verdict '(:diagnostic-at 2 "SC2086"))
              :to-match "\\`0 diagnostics on line 2")
      (expect (diagnostics-test--verdict '(:diagnostic-count 3 "SC2086" 1)) :to-be nil)))

  (it "match the text case-sensitively"
    (diagnostics-test--with-buffer (:flycheck ((2 "Double quote" "SC2086")))
      (let ((case-fold-search t))
        (expect (diagnostics-test--verdict '(:diagnostic-at 2 "double quote"))
                :to-match "\\`0 diagnostics"))))

  (it "do not count while a Flycheck check runs, and say so"
    (diagnostics-test--with-buffer (:flycheck running)
      (expect (diagnostics-test--verdict '(:diagnostic-count 2 "SC2086" 0))
              :to-match "wait ended while Flycheck has a check running")))

  (it "do not count while a Flymake backend has not reported"
    (diagnostics-test--with-buffer (:flymake (diagnostics-test--silent-backend))
      (expect (diagnostics-test--verdict '(:diagnostic-count 2 "SC2086" 0))
              :to-match "wait ended while a Flymake backend has not reported")))

  (it "count once the checkers have finished, without waiting longer"
    (diagnostics-test--with-buffer (:flycheck ((2 "Double quote" "SC2086")))
      (let ((my/diagnostics-wait-seconds 30)
            (start (float-time)))
        (expect (diagnostics-test--verdict '(:diagnostic-at 2 "SC2086")) :to-be nil)
        (expect (- (float-time) start) :to-be-less-than 5)
        (expect (- (float-time) start) :not :to-be-less-than 0.05))))

  (it "skip the spec when a program of :executables-found is missing"
    (expect (diagnostics-test--verdict '(:executables-found "emacs")) :to-be nil)
    (expect (diagnostics-test--verdict
             '(:executables-found "emacs" "no-such-program-of-plan-090"))
            :to-throw 'buttercup-pending)))

(describe "The Flycheck checkers the configuration redefines (DD-09)"

  (it "still need terraform-validate for OpenTofu: upstream runs terraform"
    ;; Canary.  README.org redefines Flycheck's terraform-validate
    ;; project checker because upstream hard-codes the terraform
    ;; executable, which an OpenTofu setup lacks.  Once upstream reads
    ;; the executable from an option, this fails: drop the redefinition
    ;; and set that option instead.
    (let ((form (with-temp-buffer
                  (insert-file-contents (find-library-name "flycheck"))
                  (search-forward "(flycheck-define-project-checker 'terraform-validate")
                  (goto-char (match-beginning 0))
                  (read (current-buffer)))))
      (expect (plist-get (nthcdr 3 form) :command)
              :to-equal '(quote ("terraform" "validate" "-json")))
      ;; Upstream's usual fix keeps the name in :command and adds an
      ;; executable option.
      (expect (boundp 'flycheck-terraform-validate-executable) :to-be nil))))

(describe "The publish hooks the diagnostic outcomes observe"

  ;; Canary.  outcomes/diagnostics.el waits for publishes through these
  ;; private names; a renamed hook would make every LTeX+ or lsp-mode
  ;; scenario wait its full time and blame the server.  README.org reads
  ;; lsp--buffer-workspaces to find the publishing server's buffers.
  (it "still exist in lsp-mode and LTeX+"
    (require 'lsp-mode)
    (require 'lsp-ltex-plus-conn)
    ;; Defined, not merely bound: add-hook, as the outcomes and
    ;; README.org call it on these names, binds a void hook to nil;
    ;; only defvar and defcustom make a variable special.
    (expect (special-variable-p 'lsp-diagnostics-updated-hook) :to-be t)
    (expect (special-variable-p 'lsp-after-initialize-hook) :to-be t)
    (expect (special-variable-p 'lsp--cur-workspace) :to-be t)
    (expect (special-variable-p 'lsp--buffer-workspaces) :to-be t)
    (expect (special-variable-p 'lsp-ltex-plus--diagnostics-functions) :to-be t)))

(defvar diagnostics-test--starts 0
  "How many checks the checker `diagnostics-test-slow' has started.")

(defvar diagnostics-test--callback nil
  "The callback of the check `diagnostics-test-slow' runs last.")

(flycheck-define-generic-checker 'diagnostics-test-slow
  "A checker whose check runs until the spec finishes it."
  :start (lambda (_checker callback)
           (setq diagnostics-test--starts (1+ diagnostics-test--starts)
                 diagnostics-test--callback callback))
  :modes '(fundamental-mode))

(defun diagnostics-test--checks (publish-during publisher)
  "Return how many checks start around one publish of lsp-mode.
The buffer is shown, and the workspace `served' serves it.  Its first
check runs; PUBLISHER publishes during it when PUBLISH-DURING, else
after it; then the user checks once more.  Each check is finished as
soon as it runs, as a slow chained checker would finish it."
  (require 'lsp-diagnostics)
  (let ((served (make-lsp--workspace))
        (others (make-lsp--workspace))
        (diagnostics-test--starts 0))
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (setq-local flycheck-check-syntax-automatically nil
                  flycheck-checker 'diagnostics-test-slow
                  lsp-diagnostics-mode t
                  lsp--buffer-workspaces (list served))
      (flycheck-mode 1)
      (dolist (step (if publish-during
                        '(check publish finish check finish)
                      '(check finish publish check finish)))
        (pcase step
          ('check (flycheck-buffer))
          ('publish (let ((lsp--cur-workspace (if (eq publisher 'served) served others)))
                      (run-hooks 'lsp-diagnostics-updated-hook)))
          ('finish (while (and (flycheck-running-p) (< diagnostics-test--starts 10))
                     (funcall diagnostics-test--callback 'finished nil)))))
      diagnostics-test--starts)))

(describe "A publish during a check"

  ;; Flycheck ignores lsp-mode's request to check while a check runs,
  ;; as when Grammalecte runs after lsp-mode in French prose: the
  ;; configuration checks the buffer once more when it ends.
  (it "checks the publishing server's buffer again once the check ends"
    (expect (diagnostics-test--checks t 'served) :to-be 3))

  (it "does not when the publish arrives after the check"
    (expect (diagnostics-test--checks nil 'served) :to-be 2))

  (it "does not when another server publishes"
    (expect (diagnostics-test--checks t 'others) :to-be 2)))

;; The Grammalecte of a pip prefix, as on macOS: its site-packages also
;; holds the user's other packages, here requests.
(defmacro diagnostics-test--with-pip-prefix (&rest body)
  "Run BODY with a pip prefix of Grammalecte first on `exec-path'.
The prefix's gc_engine appends a line to its file `probes' each time
Python imports it.  BODY sees the prefix directory as `prefix'."
  (declare (indent 0))
  `(let* ((prefix (file-name-as-directory (make-temp-file "pip-prefix" t)))
          (site (expand-file-name "lib/python3.12/site-packages/" prefix))
          (exec-path (list (expand-file-name "bin" prefix)))
          (no-littering-var-directory (expand-file-name "var/" prefix))
          ;; PYTHONPATH unset, and set again only in this binding.
          (process-environment (cons "PYTHONPATH" process-environment))
          (grammalecte-python-package-directory grammalecte-python-package-directory)
          (my/grammalecte-version nil))
     (unwind-protect
         (progn
           (dolist (dir '("bin" "requests" "grammalecte/fr"))
             (make-directory (expand-file-name dir (if (equal dir "bin") prefix site)) t))
           (write-region "" nil (expand-file-name "bin/grammalecte-cli.py" prefix))
           (set-file-modes (expand-file-name "bin/grammalecte-cli.py" prefix) #o755)
           (dolist (file '("requests/__init__.py" "grammalecte/__init__.py"
                           "grammalecte/fr/__init__.py"))
             (write-region "" nil (expand-file-name file site)))
           (write-region (format "__version__ = '9.9.9'\nopen(%S, 'a').write('probe\\n')\n"
                                 (expand-file-name "probes" prefix))
                         nil (expand-file-name "grammalecte/fr/gc_engine.py" site))
           ,@body)
       (delete-directory prefix t))))

(describe "Grammalecte's Python package"

  (it "reaches PYTHONPATH without the other packages beside it"
    (diagnostics-test--with-pip-prefix
      (my/grammalecte-use-system-package)
      (expect (car (grammalecte--version)) :to-equal "9.9.9")
      ;; Every Python process Emacs starts inherits PYTHONPATH.
      (expect (mapcar (lambda (dir) (directory-files dir nil "\\`[^.]"))
                      (split-string (getenv "PYTHONPATH") ":" t))
              :to-equal '(("grammalecte")))))

  ;; Flycheck runs the probe, its :enabled test, once in each prose
  ;; buffer, French or not.
  (it "is probed once, not once per buffer"
    (diagnostics-test--with-pip-prefix
      (my/grammalecte-use-system-package)
      (dotimes (_ 2)
        (with-temp-buffer
          (expect (car (grammalecte--version)) :to-equal "9.9.9")))
      (expect (with-temp-buffer
                (insert-file-contents (expand-file-name "probes" prefix))
                (count-lines (point-min) (point-max)))
              :to-be 1))))

(provide 'diagnostics-test)

;;; diagnostics-test.el ends here
