;;; test-session.el --- Session side of the tests/run harness -*- lexical-binding: t -*-

;;; Commentary:
;; Every session started by `tests/run' loads this file from its
;; hermetic early-init, before the configuration.  (Its feature is not
;; `session': Org configures a package of that name once it loads.)
;; It provides:
;;
;; - the startup protocol, which the runner drives through the
;;   session's private server socket: `my/test-session-started' tells
;;   when startup has finished; after creating the frame and
;;   opening any first file, the runner calls
;;   `my/test-session-note-idle-timers' and waits until
;;   `my/test-session-settled-p' tells that those timers have run;
;; - `my/test-session-frame', the client frame the runner created;
;; - `my/test-session-run-files', which loads test files, runs their
;;   specs with a JSON-lines reporter and a command recorder, and
;;   evaluates the reasons of the expected-skip entries.
;;
;; The lifeline timer that ends a session once its runner is gone is
;; not here: it is in the early-init that `tests/runner/sessions.py'
;; writes (EARLY_INIT), which knows the runner's PID.
;;
;; The result file holds one JSON object per line, appended as soon as
;; it is known:
;;
;;   {"type": "load-error", "file": F, "error": TEXT}
;;   {"type": "spec", "file": F, "name": FULL-NAME,
;;    "status": "passed" | "failed" | "pending", "disabled": BOOL,
;;    "failure": TEXT | null, "commands": [{"command": C, "keys": K}],
;;    "skip_entries": [ID]}
;;   {"type": "skip-entries", "entries": [{"id": ID, "regexp": R,
;;    "reason": TEXT, "holds": BOOL | null, "error": TEXT | null}]}
;;   {"type": "done", "server_socket": PATH | null, "editor": E,
;;    "visual": V, "emacs_socket_name": S, "display": D,
;;    "wayland_display": W, "frame_displays": [D]}
;;
;; Expected-skip entries live in `tests/expected-skips/*.el', one file
;; per plan.  Each top-level form is (SPEC-REGEXP REASON), where REASON
;; is one of (:missing-executable "NAME"), (:no-gui) or
;; (:platform SYSTEM-TYPE).  The runner decides from these lines; this
;; file only reports.  Nothing here advises or replaces configuration
;; code.

;;; Code:

(require 'seq)

(defvar straight-base-dir)
(defvar server-process)
;; Buttercup loads only when the specs run, after this file.  Its
;; variables are bound dynamically here even when this file is compiled.
(defvar buttercup-suites)
(defvar buttercup-color)
(defvar buttercup-reporter)
(declare-function buttercup-run "buttercup")
(declare-function buttercup-mark-skipped "buttercup")
(declare-function buttercup-spec-full-name "buttercup")
(declare-function buttercup-spec-status "buttercup")
(declare-function buttercup-spec-failure-description "buttercup")
(declare-function buttercup-suite-or-spec-parents "buttercup")

(defconst my/test-session--tests-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "The `tests/' directory, captured at load time.")

(defvar my/test-session-expected-skips-directory
  (expand-file-name "expected-skips/" my/test-session--tests-directory)
  "Directory holding the expected-skip entry files.")

(defvar my/test-session-frame nil
  "The client frame tests run in, or nil when the session has none.")

(defvar my/test-session-started nil
  "Non-nil once `window-setup-hook' has run.")

(defvar my/test-session-startup-features nil
  "The value of `features' once `window-setup-hook' has run.
What startup loaded: the idle timers the runner waits for before the
specs load more after it.")

(defvar my/test-session--idle-timers nil
  "The one-shot idle timers noted by `my/test-session-note-idle-timers'.")

(defvar my/test-session--commands nil
  "Commands recorded since the running spec started, most recent first.")

;; Depth 100: run after every other function of the hook.  One function
;; sets both, so the runner never sees the end of startup without them.
(add-hook 'window-setup-hook
          (lambda ()
            (setq my/test-session-startup-features (copy-sequence features)
                  my/test-session-started t))
          100)

(add-hook 'server-after-make-frame-hook
          (lambda () (setq my/test-session-frame (selected-frame))))

(defun my/test-session-note-idle-timers ()
  "Note the one-shot idle timers pending now, for `my/test-session-settled-p'.
Each is noted with its idle time, as (TIMER . TIME)."
  (setq my/test-session--idle-timers
        (mapcar (lambda (timer) (cons timer (timer--time timer)))
                (seq-remove #'timer--repeat-delay timer-idle-list)))
  t)

(defun my/test-session-settled-p ()
  "Return non-nil once every noted idle timer has run or been cancelled.
A timer that ran and armed itself again has another idle time, so it
counts as run: jit-lock's stealth timer does that for as long as the
load average is high, as it is while the tests run."
  (not (seq-some (pcase-lambda (`(,timer . ,time))
                   (and (memq timer timer-idle-list)
                        (equal (timer--time timer) time)))
                 my/test-session--idle-timers)))

(defun my/test-session--write (path object)
  "Append OBJECT to PATH as one line of JSON."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (json-serialize object) "\n") nil path t 'silent)))

(defun my/test-session--record-command ()
  "Record the command about to run, and its keys, for the running spec.
It runs from `pre-command-hook', at dispatch: `post-command-hook' runs
late for a command that signals inside a keyboard macro, when the next
command loop starts, and by then its keys are gone.  A command that is
not a symbol, such as a lambda, is recorded as null."
  (when this-command
    (push `((command . ,(if (symbolp this-command)
                            (symbol-name this-command)
                          :null))
            (keys . ,(key-description (this-command-keys-vector))))
          my/test-session--commands)))

;; Installed once: each run binds its own `my/test-session--commands',
;; so a run nested in a spec, as in the self-tests, records apart.
(add-hook 'pre-command-hook #'my/test-session--record-command)

(defun my/test-session-skip-entries (directory)
  "Read the expected-skip entries of the `*.el' files in DIRECTORY.
Return a list of plists (:id ID :regexp REGEXP :reason REASON).  An
entry that is not (SPEC-REGEXP REASON) with a valid regexp, or a file
that cannot be read, gives a plist (:id ID :error TEXT) instead.  The
files are read, never evaluated."
  (let (entries)
    (dolist (file (and (file-directory-p directory)
                       (directory-files directory t "\\`[^.].*\\.el\\'")))
      (let ((forms (condition-case err
                       (with-temp-buffer
                         (insert-file-contents file)
                         (read (concat "(" (buffer-string) "\n)")))
                     (error (list (list :unreadable (error-message-string err))))))
            (index 0))
        (dolist (form forms)
          (setq index (1+ index))
          (let ((id (format "%s:%d" (file-name-nondirectory file) index)))
            (push (pcase form
                    (`(:unreadable ,text) (list :id id :error text))
                    (`(,(and regexp (pred stringp)) ,reason)
                     (condition-case err
                         (progn (string-match-p regexp "")
                                (list :id id :regexp regexp :reason reason))
                       (invalid-regexp
                        (list :id id :error (error-message-string err)))))
                    (_ (list :id id
                             :error (format "Not (SPEC-REGEXP REASON): %S"
                                            form))))
                  entries)))))
    (nreverse entries)))

(defun my/test-session-reason-holds-p (reason)
  "Return non-nil when the expected-skip REASON is true in this session.
Signal an error for a REASON this harness does not know."
  (pcase reason
    (`(:missing-executable ,(and name (pred stringp)))
     (not (executable-find name)))
    ('(:no-gui)
     (not (and (frame-live-p my/test-session-frame)
               (display-graphic-p my/test-session-frame))))
    (`(:platform ,(and platform (pred symbolp)))
     (eq system-type platform))
    (_ (error "Unknown expected-skip reason %S" reason))))

(defun my/test-session-spec-record (spec file entries commands)
  "Return the result line of the finished buttercup SPEC, as an alist.
FILE is the test file that defined SPEC, or nil; ENTRIES are the
expected-skip entries of `my/test-session-skip-entries'; COMMANDS are
the commands SPEC ran, oldest first.  See the commentary for the line
format."
  (let ((name (buttercup-spec-full-name spec))
        (status (buttercup-spec-status spec))
        (failure (buttercup-spec-failure-description spec))
        (case-fold-search nil))
    `((type . "spec")
      (file . ,(or file :null))
      (name . ,name)
      (status . ,(symbol-name status))
      ;; `xit' and `xdescribe' leave this description.
      (disabled . ,(if (and (eq status 'pending) (member failure '(nil "" "PENDING")))
                       t
                     :false))
      (failure . ,(cond ((eq status 'passed) :null)
                        ((stringp failure) failure)
                        (t (format "%S" failure))))
      (commands . ,(vconcat commands))
      (skip_entries
       . ,(vconcat (seq-keep (lambda (entry)
                               (let ((regexp (plist-get entry :regexp)))
                                 (and regexp (string-match-p regexp name)
                                      (plist-get entry :id))))
                             entries))))))

(defun my/test-session-entry-record (entry)
  "Return the expected-skip ENTRY evaluated in this session, as an alist.
ENTRY is a value of `my/test-session-skip-entries'.  The alist is one
entry of the skip-entries line: holds is t or false, or null with an
error when ENTRY could not be read or its reason is unknown."
  (let ((holds :null)
        (problem (plist-get entry :error)))
    (unless problem
      (condition-case err
          (setq holds (if (my/test-session-reason-holds-p (plist-get entry :reason))
                          t
                        :false))
        (error (setq problem (error-message-string err)))))
    `((id . ,(plist-get entry :id))
      (regexp . ,(or (plist-get entry :regexp) :null))
      (reason . ,(format "%S" (plist-get entry :reason)))
      (holds . ,holds)
      (error . ,(or problem :null)))))

(defun my/test-session-done-record ()
  "Return the done line of this session, as an alist.
It holds the facts by which the runner judges that the session kept
away from the user's Emacs and screen: its server socket, EDITOR,
VISUAL, EMACS_SOCKET_NAME, DISPLAY, WAYLAND_DISPLAY, and the display
of each graphical frame."
  `((type . "done")
    (server_socket . ,(if (and (boundp 'server-process)
                               (process-live-p server-process))
                          (process-contact server-process :local)
                        :null))
    (editor . ,(or (getenv "EDITOR") :null))
    (visual . ,(or (getenv "VISUAL") :null))
    (emacs_socket_name . ,(or (getenv "EMACS_SOCKET_NAME") :null))
    (display . ,(or (getenv "DISPLAY") :null))
    (wayland_display . ,(or (getenv "WAYLAND_DISPLAY") :null))
    (frame_displays
     . ,(vconcat (seq-uniq (seq-keep (lambda (frame)
                                       (and (display-graphic-p frame)
                                            (frame-parameter frame 'display)))
                                     (frame-list)))))))

(defun my/test-session-run-files (files result-path &optional only)
  "Load FILES, run their specs, and append the results to RESULT-PATH.
Each file is loaded inside `condition-case'; a file that fails to
load contributes a load-error line and none of its specs.  With
ONLY, the full name of one spec, buttercup skips every other spec
and only that one is reported.  See the commentary for the line
format."
  (add-to-list 'load-path
               (expand-file-name "helpers" my/test-session--tests-directory))
  (add-to-list 'load-path
               (expand-file-name "straight/build/buttercup" straight-base-dir))
  (require 'buttercup)
  (when (frame-live-p my/test-session-frame)
    (select-frame my/test-session-frame t))
  (let ((entries (my/test-session-skip-entries
                  my/test-session-expected-skips-directory))
        (buttercup-suites nil)
        (buttercup-color nil)
        (suite-files nil))
    (dolist (file files)
      (let ((before buttercup-suites))
        (condition-case err
            (progn
              (load file nil t t)
              (dolist (suite (nthcdr (length before) buttercup-suites))
                (push (cons suite file) suite-files)))
          (error
           (setq buttercup-suites before)
           (my/test-session--write
            result-path `((type . "load-error") (file . ,file)
                          (error . ,(error-message-string err))))))))
    (when only
      (buttercup-mark-skipped
       (lambda (spec) (equal (buttercup-spec-full-name spec) only))
       t))
    (let ((buttercup-reporter
           (lambda (event spec)
             (pcase event
               ('spec-started (setq my/test-session--commands nil))
               ((and 'spec-done
                     (guard (or (not only)
                                (equal (buttercup-spec-full-name spec) only))))
                (my/test-session--write
                 result-path
                 (my/test-session-spec-record
                  spec
                  (cdr (assq (car (last (buttercup-suite-or-spec-parents spec)))
                             suite-files))
                  entries (reverse my/test-session--commands))))))))
      (let ((my/test-session--commands nil))
        (buttercup-run t)))
    (my/test-session--write
     result-path
     `((type . "skip-entries")
       (entries . ,(vconcat (mapcar #'my/test-session-entry-record entries)))))
    (my/test-session--write result-path (my/test-session-done-record))))

(provide 'test-session)

;;; test-session.el ends here
