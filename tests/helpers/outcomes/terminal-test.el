;;; outcomes/terminal-test.el --- Self-tests of the terminal outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; A terminal changes its buffer after the keys, from its process
;; output; here a timer plays that part.  The waiting outcomes must
;; hold once the change comes within the wait, and fail with what the
;; buffer shows when it never comes.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defvar gterm-module-path)
(defvar outcomes-terminal-test--map nil
  "A keymap for the self-test of `:key-sequence-bound'.")

(defun outcomes-terminal-test--verdict (clause &optional context)
  "Return the verdict of CLAUSE with CONTEXT, after checking it as `scenario' does."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause context))

(defmacro outcomes-terminal-test--in-buffer (text &rest body)
  "Run BODY in a temporary buffer holding TEXT, with a short wait.
The selected window shows the buffer, as it shows a terminal's, so
the window's size is the buffer's own.  The buffer the session shows
there, *scratch*, gains a margin column when the configuration's idle
timer turns flycheck on, which can happen during a wait."
  (declare (indent 1))
  `(let ((my/terminal-outcome-seconds 0.5))
     (with-temp-buffer
       (save-window-excursion
         (set-window-buffer nil (current-buffer))
         (insert ,text)
         ,@body))))

(describe "Terminal outcomes"

  (it ":terminal-shows waits for the output to come"
    (outcomes-terminal-test--in-buffer "prompt> echo late\n"
      (let ((buffer (current-buffer)))
        (run-at-time 0.1 nil (lambda ()
                               (with-current-buffer buffer
                                 (goto-char (point-max))
                                 (insert "late\nprompt> ")))))
      (expect (outcomes-terminal-test--verdict '(:terminal-shows "^late$")) :to-be nil)))

  (it ":terminal-shows fails with the screen when the output never comes"
    (outcomes-terminal-test--in-buffer "prompt> echo late\n"
      (expect (outcomes-terminal-test--verdict '(:terminal-shows "^late$"))
              :to-match "prompt> echo late")))

  (it ":terminal-shows waits SECONDS instead of the default when given"
    (outcomes-terminal-test--in-buffer "prompt> echo slow\n"
      (let ((buffer (current-buffer)))
        (run-at-time 0.8 nil (lambda ()
                               (with-current-buffer buffer
                                 (goto-char (point-max))
                                 (insert "slow\n")))))
      (expect (outcomes-terminal-test--verdict '(:terminal-shows "^slow$"))
              :to-be-truthy)
      (expect (outcomes-terminal-test--verdict '(:terminal-shows "^slow$" 2))
              :to-be nil)))

  (it ":terminal-shows reads only the end of a long buffer"
    (outcomes-terminal-test--in-buffer (concat "early\n" (make-string 200 ?\n))
      (expect (outcomes-terminal-test--verdict '(:terminal-shows "^early$"))
              :to-be-truthy)))

  (it ":terminal-directory compares with the entry's directory, once it changes"
    (let* ((root (file-name-as-directory (make-temp-file "terminal-test-" t)))
           (sub (expand-file-name "sub/" root))
           (entry (generate-new-buffer " entry")))
      (unwind-protect
          (progn
            (make-directory sub)
            (with-current-buffer entry (setq default-directory root))
            (outcomes-terminal-test--in-buffer ""
              (setq default-directory root)
              (expect (outcomes-terminal-test--verdict
                       '(:terminal-directory "sub/") (list :entry-buffer entry))
                      :to-match "not")
              (let ((buffer (current-buffer)))
                (run-at-time 0.1 nil (lambda ()
                                       (with-current-buffer buffer
                                         (setq default-directory sub)))))
              (expect (outcomes-terminal-test--verdict
                       '(:terminal-directory "sub/") (list :entry-buffer entry))
                      :to-be nil)))
        (kill-buffer entry)
        (delete-directory root t))))

  (it ":terminal-size-fits-window reads the last size printed"
    (outcomes-terminal-test--in-buffer ""
      (insert (format "%d %d\n%d %d\n" (window-body-height) (window-body-width) 3 10))
      (expect (outcomes-terminal-test--verdict '(:terminal-size-fits-window))
              :to-match "(3 . 10)"))
    (outcomes-terminal-test--in-buffer ""
      (insert (format "%d %d\n%d %d\n" 3 10 (window-body-height) (window-body-width)))
      (expect (outcomes-terminal-test--verdict '(:terminal-size-fits-window))
              :to-be nil)))

  (it ":terminal-size-fits-window allows one column and one row less, not more"
    (outcomes-terminal-test--in-buffer ""
      (insert (format "%d %d\n" (1- (window-body-height)) (1- (window-body-width))))
      (expect (outcomes-terminal-test--verdict '(:terminal-size-fits-window))
              :to-be nil))
    (outcomes-terminal-test--in-buffer ""
      (insert (format "%d %d\n" (window-body-height) (1+ (window-body-width))))
      (expect (outcomes-terminal-test--verdict '(:terminal-size-fits-window))
              :to-be-truthy))
    (outcomes-terminal-test--in-buffer ""
      (insert (format "%d %d\n" (window-body-height) (- (window-body-width) 2)))
      (expect (outcomes-terminal-test--verdict '(:terminal-size-fits-window))
              :to-be-truthy)))

  (it ":kill-ring-head reads the most recent kill"
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil)
          (interprogram-cut-function nil)
          (interprogram-paste-function nil))
      (expect (outcomes-terminal-test--verdict '(:kill-ring-head "x")) :to-match "nil")
      (kill-new "older")
      (kill-new "scroll-42")
      (expect (outcomes-terminal-test--verdict '(:kill-ring-head "\\`scroll-42\\'"))
              :to-be nil)
      (expect (outcomes-terminal-test--verdict '(:kill-ring-head "older"))
              :to-be-truthy)))

  (it ":skip-unless-executables skips the spec only for a missing executable"
    (expect (outcomes-terminal-test--verdict '(:skip-unless-executables "sh"))
            :to-be nil)
    (expect (outcomes-terminal-test--verdict
             '(:skip-unless-executables "sh" "outcomes-terminal-test-missing"))
            :to-throw 'buttercup-pending))

  (it ":skip-unless-gterm-module skips the spec only without the module file"
    (let ((gterm-module-path (make-temp-file "terminal-test-module")))
      (unwind-protect
          (expect (outcomes-terminal-test--verdict '(:skip-unless-gterm-module))
                  :to-be nil)
        (delete-file gterm-module-path))
      (expect (outcomes-terminal-test--verdict '(:skip-unless-gterm-module))
              :to-throw 'buttercup-pending)))

  (it ":key-sequence-bound tells a two-event binding from a one-event one"
    (let ((outcomes-terminal-test--map (make-sparse-keymap)))
      (define-key outcomes-terminal-test--map (vector 'control ?a) #'ignore)
      (define-key outcomes-terminal-test--map (kbd "C-b") #'ignore)
      (expect (outcomes-terminal-test--verdict
               '(:key-sequence-bound outcomes-terminal-test--map [control 97] ignore))
              :to-be nil)
      (expect (outcomes-terminal-test--verdict
               '(:key-sequence-bound outcomes-terminal-test--map [control 98] ignore))
              :to-match "runs nil")))

  (it ":round-trips-below allows one slow round trip in ten, none four times as slow"
    (let* ((file (make-temp-file "terminal-test-latency"))
           (process-environment (cons (concat "PTY_LATENCY_FILE=" file)
                                      process-environment))
           (verdict (lambda (times)
                      (with-temp-file file
                        (insert (mapconcat #'number-to-string times "\n") "\n"))
                      (outcomes-terminal-test--verdict '(:round-trips-below 250)))))
      (unwind-protect
          (progn
            (expect (funcall verdict '(3 90 120 249 10)) :to-be nil)
            (expect (funcall verdict '(3 90 120 999 10 20 30 40 50 60)) :to-be nil)
            (expect (funcall verdict '(3 90 120 250 10 20 30 40 50))
                    :to-match "1 of 9 round trips took 250 ms")
            (expect (funcall verdict '(3 90 120 1000 10 20 30 40 50 60))
                    :to-match "up to 1000 ms")
            (expect (funcall verdict '(3 90 120 10)) :to-match "4 round trips")
            (with-temp-file file (insert "3\n90\nfailed\n120\n10\n11\n"))
            (expect (outcomes-terminal-test--verdict '(:round-trips-below 250))
                    :to-match "failed"))
        (delete-file file)))))

(provide 'outcomes/terminal-test)

;;; outcomes/terminal-test.el ends here
