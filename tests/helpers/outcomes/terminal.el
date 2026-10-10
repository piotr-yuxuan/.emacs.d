;;; outcomes/terminal.el --- Outcomes about terminal buffers -*- lexical-binding: t -*-

;;; Commentary:
;; What the user sees in a terminal buffer (eat, mistty, gterm) after
;; the keys of a PTY scenario of plan 091.  See `scenario/outcome.el'
;; for the contract.
;;
;; A terminal answers in its own time.  Each outcome about the terminal
;; waits, as an idle user does, up to `my/terminal-outcome-seconds' for
;; its fact, and then fails with what the terminal shows.  The screen
;; is the last `my/terminal-screen-lines' lines of the buffer.
;;
;; The two :skip-unless- outcomes are preconditions, not facts: put
;; first in `:then', they skip the spec when a program the scenario
;; needs is missing.  The scenario language has no earlier place for a
;; precondition other than a language server's.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'keystrokes)
(require 'scenario/outcome)

(defvar my/terminal-outcome-seconds 10
  "Seconds an outcome waits, by default, for the terminal to show its fact.")

(defconst my/terminal-screen-lines 100
  "Lines at the end of a terminal buffer that its outcomes read.")

(defun my/terminal-screen ()
  "Return the last `my/terminal-screen-lines' lines of the current buffer."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- my/terminal-screen-lines))
    (buffer-substring-no-properties (point) (point-max))))

(defun my/terminal-await (fact &optional seconds)
  "Wait as an idle user until the function FACT returns nil; return its last value.
FACT returns nil once the outcome holds, else what the user sees.  It
is called in the buffer current when the wait starts.  The wait lasts
SECONDS at most, `my/terminal-outcome-seconds' by default.  It runs
as the command loop would run it, see `my/scenario-as-user': an error
in a process filter or timer is then shown, as to the user, instead
of entering the test framework's debugger."
  (let ((buffer (current-buffer))
        (verdict nil))
    (my/scenario-as-user
     (lambda ()
       (my/scenario-idle (* 1000 (or seconds my/terminal-outcome-seconds))
                         (lambda ()
                           (not (setq verdict (with-current-buffer buffer
                                                (funcall fact))))))))
    verdict))

(my/scenario-define-outcome :terminal-shows (_context regexp &optional seconds)
  "The terminal screen shows text matching REGEXP, within the wait.
The screen is the end of the current buffer; ^ and $ match at its
line boundaries.  SECONDS, if given, replaces the default wait."
  (my/terminal-await
   (lambda ()
     (let ((case-fold-search nil)
           (screen (my/terminal-screen)))
       (unless (string-match-p regexp screen)
         (format "the screen ends with %S" (substring screen (max 0 (- (length screen)
                                                                         600)))))))
   seconds))

(my/scenario-define-outcome :terminal-directory (context relative)
  "The current buffer's directory becomes RELATIVE to the entry's, within the wait.
RELATIVE is a directory name, such as \".\" or \"sub/\", taken
relative to the `default-directory' of the row's entry buffer: the
project root for an entry at the root."
  (let ((expected (file-name-as-directory
                   (expand-file-name relative (buffer-local-value
                                               'default-directory
                                               (plist-get context :entry-buffer))))))
    (my/terminal-await
     (lambda ()
       (unless (equal (file-truename default-directory) (file-truename expected))
         (format "the directory is %S, not %S" default-directory expected))))))

(my/scenario-define-outcome :terminal-size-fits-window (_context)
  "The last size `stty size' printed fits the selected window, within the wait.
stty prints the ROWS and COLUMNS the shell's programs lay their
output out in.  Each must equal the window's body height and width,
or be one less: a terminal may keep the last column for its cursor."
  (my/terminal-await
   (lambda ()
     (let ((screen (my/terminal-screen))
           (start 0)
           (size nil))
       (while (string-match "^\\([0-9]+\\) \\([0-9]+\\)$" screen start)
         (setq size (cons (string-to-number (match-string 1 screen))
                          (string-to-number (match-string 2 screen)))
               start (match-end 0)))
       (let ((rows (window-body-height))
             (columns (window-body-width)))
         (unless (and size
                      (<= (1- rows) (car size) rows)
                      (<= (1- columns) (cdr size) columns))
           (format "stty size printed %S in a window of %d rows and %d columns"
                   size rows columns)))))))

(defvar gterm-module-path)

(my/scenario-define-outcome :skip-unless-gterm-module (_context)
  "Skip the spec unless gterm's module is built; the first clause of `:then'.
Without its module, `gterm' refuses to start with a `user-error'
\(see tests/scenarios/l1-gterm-no-module.el): the keys stop there,
and the harness records the error and the keys left unpressed.  This
clause, evaluated first, skips the spec with the message \"missing
gterm module PATH\" before 085's harness counts that error.  The
runner accepts the skip only with an expected-skip entry whose reason
holds."
  (unless (and (boundp 'gterm-module-path) (file-exists-p gterm-module-path))
    (buttercup-skip "missing gterm module %s" (bound-and-true-p gterm-module-path))))

(my/scenario-define-outcome :skip-unless-executables (_context &rest names)
  "Skip the spec unless each executable of NAMES is found; the first clause.
The keys of a scenario that runs a missing program, such as btop,
only make the shell say so; the spec then skips with the message
\"missing executable NAME\", which the expected-skip reason
\(:missing-executable \"NAME\") allows."
  (dolist (name names)
    (unless (executable-find name)
      (buttercup-skip "missing executable %s" name))))

(my/scenario-define-outcome :key-sequence-bound (_context map keys command)
  "In the keymap MAP, the key sequence KEYS, a vector, runs COMMAND.
Written for canaries of upstream bugs that the configuration works
around, such as a binding of two events where one was meant."
  (let ((actual (and (boundp map) (keymapp (symbol-value map))
                     (lookup-key (symbol-value map) keys))))
    (unless (eq actual command)
      (format "%S in %S runs %S" keys map actual))))

(my/scenario-define-outcome :function-unadvised (_context function)
  "FUNCTION, a symbol, carries no advice, also while it is not defined yet."
  (let ((advice nil))
    (advice-mapc (lambda (piece _properties) (push piece advice)) function)
    (when advice
      (format "%S is advised by %S" function advice))))

(my/scenario-define-outcome :messages-exclude (_context regexp)
  "No line of the *Messages* buffer, over the whole session, matches REGEXP.
For what Emacs reports from a process filter, where an error is only
shown, such as TRAMP's failure to connect."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (when (re-search-forward regexp nil t)
          (format "*Messages* shows %S"
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))))))

(my/scenario-define-outcome :kill-ring-head (_context regexp)
  "The most recent kill, as `yank' would insert it, matches REGEXP."
  (let ((case-fold-search nil)
        (head (and kill-ring (substring-no-properties (current-kill 0 t)))))
    (unless (and head (string-match-p regexp head))
      (format "the most recent kill is %S" head))))

(my/scenario-define-outcome :round-trips-below (_context milliseconds)
  "Nine in ten round trips that `pty-throughput' timed took under MILLISECONDS.
None took four times as long or more.  A terminal that blocks Emacs
while it renders makes most round trips slow; a busy machine delays a
few, which the rule allows.  The fixture shell function writes the
round trips, one number of milliseconds per line, to the file named
by PTY_LATENCY_FILE.  The outcome fails on a failed round trip, and
with fewer than 5 round trips, as the sampling then says nothing."
  (let* ((file (getenv "PTY_LATENCY_FILE"))
         (lines (and file (file-readable-p file)
                     (with-temp-buffer
                       (insert-file-contents file)
                       (split-string (buffer-string) "\n" t))))
         (times (mapcar #'string-to-number
                        (cl-remove-if-not (lambda (line)
                                            (string-match-p "\\`[0-9]+\\'" line))
                                          lines)))
         (slow (cl-count-if (lambda (time) (>= time milliseconds)) times)))
    (cond ((< (length times) 5)
           (format "%d round trips were timed: %S" (length times) lines))
          ((/= (length times) (length lines))
           (format "some round trips failed: %S" lines))
          ((or (> (* 10 slow) (length times))
               (>= (apply #'max times) (* 4 milliseconds)))
           (format "%d of %d round trips took %d ms or more, up to %d ms: %S"
                   slow (length times) milliseconds (apply #'max times) times)))))

(provide 'outcomes/terminal)

;;; outcomes/terminal.el ends here
