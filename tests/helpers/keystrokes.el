;;; keystrokes.el --- The :when grammar: what the user presses -*- lexical-binding: t -*-

;;; Commentary:
;; A scenario's `:when' is a list of steps.  The whole list compiles to
;; one key vector, pressed by one `execute-kbd-macro', so a prompt
;; opened by one step receives the keys of the next steps, as when the
;; user types them.  The steps are the productions of
;; `my/scenario-steps':
;;
;;   (:keys "C-x b")   keys in `key-parse' syntax, named as the keys of
;;                     a graphical keyboard: <tab>, <return>,
;;                     <backspace>, <escape>; TAB, RET, DEL and ESC are
;;                     rejected;
;;   (:type "text")    printable characters, one event each;
;;   (:open)           nothing: the row only opens its entry buffer;
;;   (:idle MS)        the user waits MS milliseconds;
;;   (:redisplay-while-typing)
;;                     Emacs redisplays while the user's next key is
;;                     already pending, as when the user types fast:
;;                     jit-lock then defers fontification.  It needs a
;;                     graphical frame;
;;   (:sample NAME)    the outcomes may later look at what the user saw
;;                     at this point, under NAME.
;;
;; Steps are data and are validated when the scenario expands.  There
;; is no evaluation escape.
;;
;; `:idle', `:redisplay-while-typing' and `:sample' are probes: each
;; appends the private event `scenario-probe', which
;; `key-translation-map' translates to no key at all by calling
;; `my/scenario-probe'.  A probe therefore runs while
;; Emacs reads the next key, where a pausing user is, also inside a
;; minibuffer or a prefix key, and no command runs for it: modes that
;; react to commands, such as completion popups or multiple cursors,
;; do not see it.  A final probe checks that the keys did not leave a
;; prompt open: when a keyboard macro runs out of keys inside a
;; minibuffer, Emacs accepts the input as if <return> had been pressed.
;;
;; `my/scenario-as-user' runs the opening of a row and its keys as the
;; command loop runs a command: `debug-on-error' and `inhibit-quit' are
;; nil, so code guarded by `condition-case-unless-debug' behaves as in a
;; real session, and an error is shown in the echo area and recorded
;; for the outcomes instead of entering the debugger.
;;
;; An error, or a quit outside a prompt, ends a keyboard macro, where a
;; user who goes on typing has every later key run.  Keys and probes
;; left after such a step therefore never run, and the row fails,
;; naming them: the erroring key must be the last step.  Inside the
;; keyboard macro of a key bound to one, what is left cannot be told,
;; so an error there always fails the row.  C-g inside a prompt only
;; closes the prompt, and the keys go on.

;;; Code:

(require 'cl-lib)
(require 'scenario/display)

(defconst my/scenario-probe-event 'scenario-probe
  "The event that runs the next pending probe of the keys being pressed.")

(defconst my/scenario-steps
  `((:keys
     . ,(lambda (keys)
          "Press KEYS, written as for `key-parse', with graphical key names."
          (unless (and (stringp keys) (key-valid-p keys))
            (error "Scenario step (:keys %S): not valid `key-parse' syntax" keys))
          (when (cl-some (lambda (key)
                           (member (replace-regexp-in-string
                                    "\\`\\(?:[ACHMSs]-\\)+" "" key)
                                   '("TAB" "RET" "DEL" "ESC")))
                         (split-string keys " "))
            (error "Scenario step (:keys %S): name the GUI key, such as <tab>, \
<return>, <backspace> or <escape>" keys))
          (cons (key-parse keys) nil)))
    (:type
     . ,(lambda (text)
          "Type TEXT, one character event at a time."
          (unless (and (stringp text) (string-match-p "\\`[^[:cntrl:]]+\\'" text))
            (error "Scenario step (:type %S): printable text only; \
press other keys with :keys" text))
          (cons (string-to-vector text) nil)))
    (:open
     . ,(lambda ()
          "Press nothing: the scenario checks the entry buffer as opened."
          (cons [] nil)))
    (:idle
     . ,(lambda (ms)
          "Wait MS milliseconds without input; see `my/scenario-idle'."
          (unless (and (natnump ms) (<= 1 ms 60000))
            (error "Scenario step (:idle %S): milliseconds from 1 to 60000" ms))
          (cons (vector my/scenario-probe-event) (list :idle ms))))
    (:redisplay-while-typing
     . ,(lambda ()
          "Redisplay while the next key is pending, as when the user types fast."
          (cons (vector my/scenario-probe-event) (list :redisplay-while-typing))))
    (:sample
     . ,(lambda (name)
          "Snapshot what the user sees, for the outcome (:sampled NAME ...)."
          (unless (and (stringp name) (not (string-empty-p name)))
            (error "Scenario step (:sample %S): NAME must be a string" name))
          (cons (vector my/scenario-probe-event) (list :sample name)))))
  "The productions of the `:when' grammar.
Each maps a step keyword to a function of the step arguments that
signals an error for invalid arguments and otherwise returns
\(EVENTS . PROBE), where PROBE is nil or the probe the events run.")

(defun my/scenario-compile-steps (steps)
  "Validate STEPS, the `:when' of a scenario, and return (KEYS . PROBES).
STEPS is a non-empty list, as the `scenario' macro checks.  KEYS is
the event vector to press, ending with a probe event that checks no
prompt is left open; PROBES lists the probes its probe events run, in
order.  Signal an error for an invalid step."
  (when (and (member '(:open) steps) (cdr steps))
    (error "Scenario :when: (:open) must be the only step"))
  (let ((keys []) (probes nil))
    (dolist (step steps)
      (let* ((production (alist-get (car-safe step) my/scenario-steps))
             (arity (and production (proper-list-p step) (func-arity production))))
        (unless (and arity (<= (car arity) (length (cdr step)) (cdr arity)))
          (error "Scenario :when: unknown step %S; steps are %S"
                 step (mapcar #'car my/scenario-steps)))
        (let ((compiled (apply production (cdr step))))
          (setq keys (vconcat keys (car compiled)))
          (when (cdr compiled) (push (cdr compiled) probes)))))
    (let ((names (mapcar #'cadr (seq-filter (lambda (probe) (eq (car probe) :sample))
                                            probes))))
      (when (/= (length names) (length (delete-dups (copy-sequence names))))
        (error "Scenario :when: sample names must be unique: %S" names)))
    (cons (vconcat keys (vector my/scenario-probe-event))
          (reverse (cons '(:end) probes)))))

;;;; Running steps

(defvar my/scenario--probes nil
  "Probes still to run for the keys being pressed.")

(defvar my/scenario--samples nil
  "Snapshots taken by the keys being pressed, newest first.")

(defvar my/scenario--errors nil
  "Errors the user saw while `my/scenario-as-user' ran, newest first.")

(defvar my/scenario--depth 0
  "The recursion depth at which `my/scenario-as-user' started.")

(defun my/scenario-idle (ms &optional until)
  "Let MS milliseconds pass without input, as when the user waits.
Timers and process output run during that real time, and each idle
timer runs once when the idle time reaches its delay, as when the
command loop waits for input.  `read-event' and `sit-for' would run
no idle timer here.  Return non-nil, early, as soon as the function
UNTIL returns non-nil.  Call it inside `my/scenario-as-user', which
ends any idle period Emacs itself counts, so that only this function
runs idle timers."
  (let ((start (float-time))
        (ran nil)
        (satisfied nil))
    (redisplay)
    (while (and (not (setq satisfied (and until (funcall until))))
                (< (- (float-time) start) (/ ms 1000.0)))
      (let ((due (cl-find-if (lambda (timer)
                               (and (not (memq timer ran))
                                    (<= (float-time (timer--time timer))
                                        (- (float-time) start))))
                             timer-idle-list)))
        (if (not due)
            (accept-process-output nil 0.01)
          (push due ran)
          (timer-event-handler due))))
    satisfied))

(defun my/scenario--record-error (data context caller)
  "Record DATA as an error the user saw, and display it.
CONTEXT and CALLER are as for `command-error-function'.  When the
error happens in a prompt the keys opened, Emacs has stopped the
keyboard macro, so leave the prompt, as the user would with C-g."
  (ignore context caller)
  (push data my/scenario--errors)
  ;; Not `command-error-default-function': it rings the bell, which
  ;; signals one more error while a keyboard macro runs.
  (message "%s" (error-message-string data))
  (when (> (recursion-depth) my/scenario--depth)
    (abort-recursive-edit)))

(defun my/scenario--watch-prompt ()
  "Record the errors of the minibuffer being set up.
The minibuffer reports errors through its own buffer-local
`command-error-function'."
  (setq-local command-error-function #'my/scenario--record-error))

(defun my/scenario-probe (_prompt)
  "Run the next probe of the keys being pressed; translate it to no key.
Bound in `key-translation-map', which passes the PROMPT of the key
being read."
  (pcase (pop my/scenario--probes)
    (`(:idle ,ms) (my/scenario-idle ms))
    ('(:redisplay-while-typing)
     (unless (display-graphic-p)
       (error "(:redisplay-while-typing) needs a graphical frame: declare \
`scenario-display: graphic'"))
     ;; Inside a keyboard macro `redisplay' does nothing.  A pending
     ;; event makes `input-pending-p' true for the code redisplay runs,
     ;; such as jit-lock, as for a key that arrives during redisplay.
     (let ((executing-kbd-macro nil)
           (unread-command-events (list 'scenario-pending-key)))
       (redisplay)))
    (`(:sample ,name)
     (let ((window (or (minibuffer-selected-window) (selected-window))))
       (push (cons name
                   (list :minibuffer (and (minibufferp)
                                          (my/scenario-displayed-text
                                           (point-min) (point-max)))
                         :buffer (buffer-name (window-buffer window))
                         :line (with-current-buffer (window-buffer window)
                                 (save-excursion
                                   (goto-char (window-point window))
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                         :echo (current-message)))
             my/scenario--samples)))
    ('(:end)
     (when (> (recursion-depth) my/scenario--depth)
       (my/scenario--record-error
        '(error "The keys ended inside a prompt; end them with <return> or C-g")
        "" nil)))
    (_ (user-error "No scenario probe is pending")))
  [])

(keymap-set key-translation-map (format "<%s>" my/scenario-probe-event)
            #'my/scenario-probe)

(defun my/scenario-press (compiled)
  "Press the keys of COMPILED, a value of `my/scenario-compile-steps'.
Call it inside `my/scenario-as-user', which collects the samples and
the errors.  When an error or a quit ends the keyboard macro before
its last step, record an error that names the keys and probes left,
so the row fails.  When it ends inside the keyboard macro of a key,
record an error, as what is left cannot be told."
  (let ((my/scenario--probes (cdr compiled))
        (keys (car compiled)))
    ;; The handler runs before the macro unwinds, while
    ;; `executing-kbd-macro' and its index tell how far the keys went,
    ;; and `my/scenario--probes' holds the probes not run.  An error
    ;; inside a prompt reaches it as the quit that leaves the prompt
    ;; (see `my/scenario--record-error'), after the command loop of the
    ;; prompt has set `executing-kbd-macro' to nil; the index is valid.
    (handler-bind
        (((error quit)
          (lambda (_)
            (if (not (memq executing-kbd-macro (list nil keys)))
                (push '(error "An error or quit inside the keyboard macro of a \
key ended the keys; what was left cannot be told")
                      my/scenario--errors)
              (let* ((probes my/scenario--probes)
                     (left (delq nil
                                 (mapcar
                                  (lambda (event)
                                    (if (not (eq event my/scenario-probe-event))
                                        (key-description (vector event))
                                      (let ((probe (pop probes)))
                                        (and probe (not (equal probe '(:end)))
                                             (format "%S" probe)))))
                                  (seq-drop keys executing-kbd-macro-index)))))
                (when left
                  (push `(error ,(format "The keys after an error or quit were not \
pressed: %s" (string-join left " ")))
                        my/scenario--errors)))))))
      (execute-kbd-macro keys))))

(defun my/scenario-as-user (function)
  "Call FUNCTION as the command loop calls a command.
Bind `debug-on-error' and `inhibit-quit' to nil, and
`debug-ignored-errors' to its standard value; a timer that starts the
specs binds `inhibit-quit' to t.  Show and record each error the user
would see, including errors inside a prompt, and return (:errors
ERRORS :samples SAMPLES), oldest first.  A quit ends FUNCTION silently.

A user who acts is not idle.  When Emacs counts itself idle, as when
a timer started the specs, end that idle period first: otherwise each
`input-pending-p' or `sit-for' in a command would run every pending
idle timer, whatever its delay."
  (when (current-idle-time)
    ;; Emacs ends an idle period when it reads an input event, and a
    ;; user signal that Emacs sends itself is one.
    (let ((special-event-map
           (define-keymap "<sigusr1>"
             (lambda () (interactive) (throw 'my/scenario--acting nil)))))
      (catch 'my/scenario--acting
        (signal-process (emacs-pid) 'sigusr1)
        (read-event))))
  (let ((my/scenario--errors nil)
        (my/scenario--samples nil)
        (my/scenario--depth (recursion-depth))
        (inhibit-quit nil)
        (debug-on-error nil)
        (debug-ignored-errors
         (eval (car (get 'debug-ignored-errors 'standard-value)) t)))
    (add-hook 'minibuffer-setup-hook #'my/scenario--watch-prompt 90)
    (unwind-protect
        (condition-case err
            (funcall function)
          (quit nil)
          (error (my/scenario--record-error err "" nil)))
      (remove-hook 'minibuffer-setup-hook #'my/scenario--watch-prompt))
    (list :errors (reverse my/scenario--errors)
          :samples (reverse my/scenario--samples))))

(provide 'keystrokes)

;;; keystrokes.el ends here
