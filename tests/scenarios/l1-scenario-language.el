;;; l1-scenario-language.el --- Self-tests of the scenario language in a session -*- lexical-binding: t -*-

;;; Commentary:
;; The scenario language promises that keys behave as when the user
;; types them in this session: a prompt receives the keys that follow
;; it, waiting runs idle timers, and errors are caught as the command
;; loop catches them.  This file checks those promises in a real
;; session.  The passing cases are scenarios; the failing cases run a
;; row with `my/scenario-run-row' and expect the spec to fail.
;;
;; The test commands below are bound under the free prefix C-c 9 of
;; this session's global map.

;;; Code:

(require 'scenario)

(defvar scenario-language-test--late-timer nil
  "The idle timer of `scenario-language-test-read' that must not fire.")

(defun scenario-language-test-user-error ()
  "Signal a `user-error'."
  (interactive)
  (user-error "Nothing to do here"))

(defun scenario-language-test-guarded ()
  "Insert \"handled\" after an error caught by `condition-case-unless-debug'."
  (interactive)
  (condition-case-unless-debug nil
      (error "Guarded failure")
    (error (insert "handled"))))

(defun scenario-language-test-read ()
  "Insert a string read from the minibuffer.
Typing in the prompt starts a timer at 0.1 s, an idle timer at 0.2 s
and an idle timer at 5 s; each marks the prompt when it fires."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-self-insert-hook
                  (lambda ()
                    (let ((prompt (current-buffer)))
                      (run-with-timer 0.1 nil (lambda ()
                                                (with-current-buffer prompt
                                                  (insert "[timer]"))))
                      (run-with-idle-timer 0.2 nil (lambda ()
                                                     (with-current-buffer prompt
                                                       (insert "[idle]"))))
                      (setq scenario-language-test--late-timer
                            (run-with-idle-timer 5 nil (lambda ()
                                                         (with-current-buffer prompt
                                                           (insert "[late]")))))))
                  nil t))
    (unwind-protect
        (insert (read-string "Probe: "))
      (when (timerp scenario-language-test--late-timer)
        (cancel-timer scenario-language-test--late-timer)))))

(defun scenario-language-test-quit-allowed ()
  "Insert the value of `inhibit-quit', which the command loop binds to nil."
  (interactive)
  (insert (format "%S" inhibit-quit)))

(defvar scenario-language-test--refusing nil
  "The buffer `scenario-language-test-refuse-to-die' made unkillable.")

(defun scenario-language-test-refuse-to-die ()
  "Make the current buffer signal an error when it is killed."
  (interactive)
  (setq scenario-language-test--refusing (current-buffer))
  (add-hook 'kill-buffer-hook (lambda () (error "Refusing to die")) nil t))

(defun scenario-language-test-repeat ()
  "Insert \"again\" when the previous command was this one, else \"first\"."
  (interactive)
  (insert (if (eq last-command this-command) "again" "first")))

(keymap-set global-map "C-c 9 u" #'scenario-language-test-user-error)
(keymap-set global-map "C-c 9 l" #'scenario-language-test-repeat)
(keymap-set global-map "C-c 9 g" #'scenario-language-test-guarded)
(keymap-set global-map "C-c 9 r" #'scenario-language-test-read)
(keymap-set global-map "C-c 9 q" #'scenario-language-test-quit-allowed)
(keymap-set global-map "C-c 9 m" "a b C-c 9 u")  ; a keyboard macro that errors
(keymap-set global-map "C-c 9 k" #'scenario-language-test-refuse-to-die)
(get-buffer-create "scenario-language-alpha")
(get-buffer-create "scenario-language-beta")

(scenario
 ;; First in the file: the timer that starts the specs binds
 ;; `inhibit-quit' to t, and the first error the command loop handles
 ;; sets that binding to nil for the rest of the run.
 :describe "SL-QUIT-ALLOWED: a key runs with quitting allowed, as in the command loop"
 :given ((:name "quit" :file "notes.txt"))
 :when ((:keys "C-c 9 q"))
 :then ((:buffer-text "nil")))

(scenario
 :describe "SL-PROMPT: a prompt receives the keys that follow it"
 :given ((:name "two other buffers" :file "notes.txt" :text "first line\n"))
 :when ((:keys "C-x b") (:type "language-alp") (:sample "prompt") (:keys "<return>"))
 :then ((:buffer-name "\\`scenario-language-alpha\\'")
        (:sampled "prompt" :minibuffer "language-alp")
        (:buffer-text-unchanged)))

(scenario
 :describe "SL-IDLE: waiting runs due timers and idle timers inside a prompt"
 :given ((:name "prompt" :file "notes.txt"))
 :when ((:keys "C-c 9 r") (:type "x") (:idle 400) (:sample "waited")
        (:keys "<return>"))
 :then ((:sampled "waited" :minibuffer "\\`Probe: x\\[timer\\]\\[idle\\]\\'")
        (:buffer-text "x[timer][idle]")))

(scenario
 :describe "SL-NO-COMMAND: a probe between two keys runs no command"
 :given ((:name "repeat" :file "notes.txt"))
 :when ((:keys "C-c 9 l") (:idle 10) (:sample "between") (:keys "C-c 9 l"))
 :then ((:buffer-text "firstagain")))

(scenario
 :describe "SL-GUARDED: condition-case-unless-debug catches as in a real session"
 :given ((:name "guarded" :file "notes.txt"))
 :when ((:keys "C-c 9 g"))
 :then ((:buffer-text "handled")))

(scenario
 :describe "SL-USER-ERROR: an expected user-error passes"
 :given ((:name "at top level" :file "notes.txt"))
 :when ((:keys "C-c 9 u"))
 :then ((:user-error "\\`Nothing to do here\\'")
        (:buffer-text "")))

(scenario
 :describe "SL-PROMPT-ERROR: a user-error inside a prompt is caught"
 :given ((:name "in a prompt" :file "notes.txt"))
 :when ((:keys "C-c 9 r") (:type "x") (:sample "prompt") (:keys "C-c 9 u"))
 :then ((:user-error "\\`Nothing to do here\\'")
        (:sampled "prompt" :minibuffer "\\`Probe: x\\'")
        (:buffer-text "")))

(scenario
 :describe "SL-PROMPT-QUIT: after C-g closes a prompt, the next keys reach the buffer"
 :given ((:name "in a prompt" :file "notes.txt"))
 :when ((:keys "C-c 9 r") (:type "x") (:keys "C-g") (:type "abc"))
 :then ((:buffer-text "abc")))

(defun scenario-language-test--failure (row steps clauses)
  "Run ROW with STEPS and CLAUSES; return its failure message, or nil."
  (condition-case failure
      (progn (my/scenario-run-row row (my/scenario-compile-steps steps) clauses nil)
             nil)
    (buttercup-failed (cdr failure))))

(describe "SL-FAILURES: the scenario fails"

  (it "on an unexpected user-error"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 u")) '((:buffer-text "")))
            :to-equal "unexpected error: Nothing to do here"))

  (it "on an unbound key, even when a user-error is expected"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 x"))
             '((:user-error "\\`Nothing to do here\\'")))
            :to-match "(:user-error"))

  (it "when keys follow an error, even an expected one"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 u") (:type "abc"))
             '((:user-error "\\`Nothing to do here\\'") (:buffer-text "")))
            :to-match "were not pressed: a b c"))

  (it "when keys follow an error inside a prompt"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt")
             '((:keys "C-c 9 r") (:type "x") (:keys "C-c 9 u") (:keys "<return>"))
             '((:user-error "\\`Nothing to do here\\'") (:buffer-text "")))
            :to-match "were not pressed: <return>"))

  (it "when a probe follows an error"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 u") (:idle 10) (:type "a"))
             '((:user-error "\\`Nothing to do here\\'") (:buffer-text "")))
            :to-match "were not pressed: (:idle 10) a\""))

  (it "when the keyboard macro of a key ends in an error"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 m") (:type "x"))
             '((:user-error "\\`Nothing to do here\\'") (:buffer-text "ab")))
            :to-match "keyboard macro of a key"))

  (it "when keys follow a quit"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-g") (:type "abc"))
             '((:buffer-text "")))
            :to-equal "unexpected error: The keys after an error or quit were not \
pressed: a b c"))

  (it "when the keys end inside a prompt"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt") '((:keys "C-c 9 r") (:type "x"))
             '((:buffer-text "")))
            :to-equal "unexpected error: The keys ended inside a prompt; \
end them with <return> or C-g"))

  (it "when a buffer refuses to die, after restoring the boundaries"
    (let ((browser browse-url-browser-function))
      (unwind-protect
          (progn
            (expect (scenario-language-test--failure
                     '(:name "r" :file "notes.txt" :system (:record-browse-url t))
                     '((:keys "C-c 9 k")) '((:buffer-text "")))
                    :to-equal "Cleaning up the row failed: Killing the buffers of \
the row failed: Refusing to die")
            (expect browse-url-browser-function :to-be browser))
        (when (buffer-live-p scenario-language-test--refusing)
          (with-current-buffer scenario-language-test--refusing
            (kill-local-variable 'kill-buffer-hook))
          (kill-buffer scenario-language-test--refusing)))))

  (it "on an outcome that does not hold, naming the outcome"
    (expect (scenario-language-test--failure
             '(:name "r" :file "notes.txt" :text "a-!-b") '((:open))
             '((:line-at-point "\\`b\\'")))
            :to-equal "(:line-at-point \"\\\\`b\\\\'\"): line 1 at point is \"ab\"")))

(provide 'l1-scenario-language)

;;; l1-scenario-language.el ends here
