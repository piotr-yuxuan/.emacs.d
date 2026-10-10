;;; test-session-test.el --- Self-tests of the session side of tests/run -*- lexical-binding: t -*-

;;; Commentary:
;; Harness self-tests for `test-session.el'.  They run in the logic-test
;; session, where `test-session.el' is already loaded by the hermetic
;; early-init.  The reporter tests run a small suite of their own
;; through `my/test-session-run-files' and read back the JSON lines.

;;; Code:

(require 'buttercup)
(require 'seq)
(require 'test-session)

(defun test-session-test--write (directory name content)
  "Write CONTENT to the file NAME in DIRECTORY and return its path."
  (let ((path (expand-file-name name directory)))
    (with-temp-file path (insert content))
    path))

(defun test-session-test--lines (path)
  "Return the JSON lines of PATH as a list of alists."
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar (lambda (line) (json-parse-string line :object-type 'alist
                                              :array-type 'list
                                              :null-object nil
                                              :false-object nil))
            (split-string (buffer-string) "\n" t))))

(defun test-session-test--spec (lines name)
  "Return the spec line of LINES whose full name is NAME."
  (seq-find (lambda (line) (and (equal (alist-get 'type line) "spec")
                                (equal (alist-get 'name line) name)))
            lines))

(describe "The session reporter"
  :var (directory lines specs-file broken-file)
  (before-all
    (setq directory (make-temp-file "session-test-" t)
          specs-file (test-session-test--write directory "specs.el" ";; -*- lexical-binding: t -*-
(defun test-session-test-refuse ()
  \"Signal a `user-error'.\"
  (interactive)
  (user-error \"Refused\"))
(describe \"a\"
  (it \"passes\" (expect t :to-be t))
  (it \"presses a key that errors\"
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (use-local-map (define-keymap \"C-c 7 e\" #'test-session-test-refuse))
      (execute-kbd-macro (kbd \"C-c 7 e\"))))
  (it \"types\"
    (let ((buffer (generate-new-buffer \"typing\")))
      (switch-to-buffer buffer)
      (insert \"xy\")
      (goto-char (point-min))
      (execute-kbd-macro (kbd \"C-f C-f\"))
      (kill-buffer buffer)))
  (it \"fails\" (expect 1 :to-equal 2))
  (it \"errors\" (error \"Boom in spec\"))
  (it \"skips\" (buttercup-skip \"No tool here\"))
  (xit \"is disabled\"))
(xdescribe \"b\"
  (it \"is disabled too\" (expect t :to-be t)))
")
          broken-file (test-session-test--write directory "broken.el" ";; -*- lexical-binding: t -*-
(describe \"c\" (it \"never runs\" (expect t :to-be t)))
(error \"Boom at load\")
"))
    (let ((skips (expand-file-name "skips/" directory)))
      (make-directory skips)
      (test-session-test--write skips "plan.el" (format "
;; Comments are allowed.
(\"\\\\`a skips\\\\'\" (:platform %s))
(not-a-regexp (:no-gui))
(\"[\" (:no-gui))
(\"\\\\`a passes\\\\'\" (:unknown-reason))
" system-type))
      (let ((my/test-session-expected-skips-directory skips)
            (result (expand-file-name "result.jsonl" directory)))
        (my/test-session-run-files (list specs-file broken-file) result)
        (setq lines (test-session-test--lines result)))))
  (after-all
    (delete-directory directory t))

  (it "writes one line per finished spec, in order, with its file"
    (expect (mapcar (lambda (line) (alist-get 'name line))
                    (seq-filter (lambda (line) (equal (alist-get 'type line) "spec"))
                                lines))
            :to-equal '("a passes" "a presses a key that errors" "a types" "a fails"
                        "a errors" "a skips" "a is disabled" "b is disabled too"))
    (expect (alist-get 'file (test-session-test--spec lines "a fails"))
            :to-equal specs-file))

  (it "reports each status, and the failure text of what did not pass"
    (expect (alist-get 'status (test-session-test--spec lines "a passes")) :to-equal "passed")
    (expect (alist-get 'failure (test-session-test--spec lines "a passes")) :to-be nil)
    (expect (alist-get 'status (test-session-test--spec lines "a fails")) :to-equal "failed")
    (expect (alist-get 'failure (test-session-test--spec lines "a fails"))
            :to-match "Expected `1' to be `equal' to `2'")
    (expect (alist-get 'status (test-session-test--spec lines "a errors")) :to-equal "failed")
    (expect (alist-get 'failure (test-session-test--spec lines "a errors"))
            :to-match "Boom in spec")
    (expect (alist-get 'failure (test-session-test--spec lines "a skips"))
            :to-equal "No tool here"))

  (it "tells a run-time skip from a disabled spec"
    (expect (mapcar (lambda (name)
                      (let ((spec (test-session-test--spec lines name)))
                        (list (alist-get 'status spec) (alist-get 'disabled spec))))
                    '("a skips" "a is disabled" "b is disabled too"))
            :to-equal '(("pending" nil) ("pending" t) ("pending" t))))

  (it "records the commands of a spec and attributes them to that spec only"
    (expect (alist-get 'commands (test-session-test--spec lines "a types"))
            :to-equal '(((command . "forward-char") (keys . "C-f"))
                        ((command . "forward-char") (keys . "C-f"))))
    (expect (alist-get 'commands (test-session-test--spec lines "a fails")) :to-be nil))

  (it "records a command that signals, with its keys, in its own spec"
    (expect (alist-get 'commands (test-session-test--spec lines "a presses a key that errors"))
            :to-equal '(((command . "test-session-test-refuse") (keys . "C-c 7 e")))))

  (it "leaves the recorder of the enclosing run working"
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "xy")
      (goto-char (point-min))
      (execute-kbd-macro (kbd "C-f"))
      (expect (car my/test-session--commands)
              :to-equal '((command . "forward-char") (keys . "C-f")))))

  (it "reports a file that fails to load, and none of its specs"
    (expect (seq-filter (lambda (line) (equal (alist-get 'type line) "load-error")) lines)
            :to-equal `(((type . "load-error") (file . ,broken-file)
                         (error . "Boom at load"))))
    (expect (test-session-test--spec lines "c never runs") :to-be nil))

  (it "lists the expected-skip entries each spec matches"
    (expect (alist-get 'skip_entries (test-session-test--spec lines "a skips"))
            :to-equal '("plan.el:1"))
    (expect (alist-get 'skip_entries (test-session-test--spec lines "a passes"))
            :to-equal '("plan.el:4")))

  (it "evaluates every entry, and reports the entries it cannot use"
    (expect (mapcar (lambda (entry)
                      (list (alist-get 'id entry) (alist-get 'holds entry)
                            (and (alist-get 'error entry) t)))
                    (alist-get 'entries
                               (seq-find (lambda (line)
                                           (equal (alist-get 'type line) "skip-entries"))
                                         lines)))
            :to-equal '(("plan.el:1" t nil) ("plan.el:2" nil t)
                        ("plan.el:3" nil t) ("plan.el:4" nil t))))

  (it "ends with a done line"
    (expect (alist-get 'type (car (last lines))) :to-equal "done")))

(describe "A spec run alone"
  :var (directory lines marker)
  (before-all
    (setq directory (make-temp-file "test-session-alone-" t)
          marker (expand-file-name "other-spec-ran" directory))
    (let ((file (test-session-test--write directory "alone.el" (format ";; -*- lexical-binding: t -*-
(describe \"d\"
  (it \"writes a marker\" (write-region \"\" nil %S))
  (it \"is chosen\" (expect 1 :to-equal 2)))
" marker)))
          (result (expand-file-name "result.jsonl" directory))
          (my/test-session-expected-skips-directory directory))
      (my/test-session-run-files (list file) result "d is chosen")
      (setq lines (test-session-test--lines result))))
  (after-all
    (delete-directory directory t))

  (it "reports that spec only, with its own status"
    (expect (mapcar (lambda (line) (list (alist-get 'name line) (alist-get 'status line)))
                    (seq-filter (lambda (line) (equal (alist-get 'type line) "spec"))
                                lines))
            :to-equal '(("d is chosen" "failed"))))

  (it "does not run the body of any other spec"
    (expect (file-exists-p marker) :to-be nil)))

(describe "The expected-skip entry files"
  (it "are read in order, and a file that cannot be read is one bad entry"
    (let ((directory (make-temp-file "test-session-skips-" t)))
      (unwind-protect
          (progn
            (test-session-test--write directory "a.el" "(\"x\" (:no-gui))\n(\"y\" (:no-gui))")
            (test-session-test--write directory "b.el" "(\"z\" (:no-gui)")
            (expect (mapcar (lambda (entry)
                              (list (plist-get entry :id) (plist-get entry :regexp)
                                    (and (plist-get entry :error) t)))
                            (my/test-session-skip-entries directory))
                    :to-equal '(("a.el:1" "x" nil) ("a.el:2" "y" nil) ("b.el:1" nil t))))
        (delete-directory directory t)))))

(describe "An expected-skip reason"
  (it "is a missing executable only when exec-path has no such program"
    (let* ((directory (make-temp-file "session-test-path-" t))
           (program (test-session-test--write directory "session-test-tool" "#!/bin/sh\n")))
      (unwind-protect
          (progn
            (set-file-modes program #o755)
            (let ((exec-path (list directory)))
              (expect (my/test-session-reason-holds-p
                       '(:missing-executable "session-test-tool"))
                      :to-be nil))
            (let ((exec-path nil))
              (expect (my/test-session-reason-holds-p
                       '(:missing-executable "session-test-tool"))
                      :to-be-truthy)))
        (delete-directory directory t))))

  (it "is a platform only on that platform"
    (expect (my/test-session-reason-holds-p (list :platform system-type)) :to-be-truthy)
    (expect (my/test-session-reason-holds-p '(:platform no-such-system)) :to-be nil))

  (it "is the absence of a graphical test frame"
    (let ((my/test-session-frame nil))
      (expect (my/test-session-reason-holds-p '(:no-gui)) :to-be-truthy)))

  (it "is an error when unknown"
    (expect (my/test-session-reason-holds-p '(:no-network)) :to-throw 'error)))

(describe "The settle protocol"
  (it "waits for the noted one-shot idle timers only"
    (let* ((timer-idle-list nil)
           (once (run-with-idle-timer 3600 nil #'ignore))
           (repeating (run-with-idle-timer 3600 t #'ignore)))
      (unwind-protect
          (progn
            (my/test-session-note-idle-timers)
            (expect (my/test-session-settled-p) :to-be nil)
            (cancel-timer once)
            (expect (my/test-session-settled-p) :to-be-truthy)
            (let ((later (run-with-idle-timer 3600 nil #'ignore)))
              (expect (my/test-session-settled-p) :to-be-truthy)
              (cancel-timer later)))
        (cancel-timer once)
        (cancel-timer repeating))))

  (it "counts a noted timer that ran and armed itself again as run"
    (let* ((timer-idle-list nil)
           (rearming (run-with-idle-timer 3600 nil #'ignore)))
      (unwind-protect
          (progn
            (my/test-session-note-idle-timers)
            ;; What `timer-event-handler' and jit-lock's stealth timer
            ;; do: the timer leaves the list, runs, and comes back later.
            (cancel-timer rearming)
            (timer-set-idle-time rearming 7200)
            (timer-activate-when-idle rearming t)
            (expect (my/test-session-settled-p) :to-be-truthy))
        (cancel-timer rearming)))))

(provide 'test-session-test)

;;; test-session-test.el ends here
