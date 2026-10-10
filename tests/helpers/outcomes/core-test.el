;;; outcomes/core-test.el --- Self-tests of the core outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; Each outcome must hold when the user would see the fact and fail
;; when the user would not.  The failing cases are the plausible wrong
;; implementations: a point check that only searches the buffer, a
;; display check that reads buffer text, an error check that accepts
;; any error, and so on.  A verdict is nil when the outcome holds.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defvar outcomes-core-test--on nil)
(defvar outcomes-core-test--off nil)
(defvar outcomes-core-test--list nil)

(defun outcomes-core-test--verdict (clause &optional context)
  "Return the verdict of CLAUSE in the current buffer, with CONTEXT."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause context))

(describe "The outcome registry"

  (it "rejects an unknown outcome and a wrong number of arguments"
    (expect (my/scenario-check-clause '(:no-such-outcome)) :to-throw 'error)
    (expect (my/scenario-check-clause '(:buffer-text)) :to-throw 'error)
    (expect (my/scenario-check-clause '(:buffer-text "a" "b")) :to-throw 'error)
    (expect (my/scenario-check-clause ":buffer-text") :to-throw 'error))

  (it "accepts any number of arguments for a &rest outcome"
    (expect (my/scenario-check-clause '(:minor-modes-on a b c)) :not :to-throw))

  (it "refuses a second definition of a keyword from another file"
    (let ((load-file-name "/elsewhere/outcomes.el"))
      (expect (my/scenario-register-outcome :buffer-text #'ignore)
              :to-throw 'error)))

  (it "documents every outcome"
    (maphash (lambda (_keyword entry)
               (expect (documentation (car entry))
                       :to-match "\\`[A-Z]"))
             my/scenario-outcomes)))

(describe "Core outcomes on point and display"

  (it ":line-at-point checks the line point is on, not the buffer"
    (with-temp-buffer
      (insert "target above\nmiddle\ntarget below\n")
      (goto-char (point-min))
      (forward-line 1)
      (expect (outcomes-core-test--verdict '(:line-at-point "target")) :to-match "middle")
      (forward-line 1)
      (expect (outcomes-core-test--verdict '(:line-at-point "target below")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:line-at-point "target above"))
              :to-match "line 3")
      (goto-char (point-min))
      (expect (outcomes-core-test--verdict '(:line-at-point "target below"))
              :to-match "line 1")))

  (it ":line-display-matches sees an overlay's after-string"
    (with-temp-buffer
      (insert "(defn f [x] x)\n(defn g [y] y)\n")
      (let ((overlay (make-overlay 11 11)))
        (overlay-put overlay 'after-string ": Long"))
      (expect (outcomes-core-test--verdict
               '(:line-display-matches "defn f" "\\[x: Long\\]"))
              :to-be nil)
      (expect (outcomes-core-test--verdict
               '(:line-display-matches "defn g" ": Long"))
              :to-match "defn g")))

  (it ":line-display-matches ignores invisible text and shows display strings"
    (with-temp-buffer
      (insert "visible secret tail\n")
      (put-text-property 9 16 'invisible t)
      (put-text-property 16 20 'display "END")
      (expect (outcomes-core-test--verdict
               '(:line-display-matches "secret" "\\`visible END\\'"))
              :to-be nil)
      (expect (outcomes-core-test--verdict
               '(:line-display-matches "visible" "secret"))
              :to-match "visible END")))

  (it ":text-hidden and :text-shown read the displayed text"
    (with-temp-buffer
      (insert "(defn f [x]\n  (* x x))\n")
      (let ((overlay (make-overlay 12 23)))
        (overlay-put overlay 'invisible t))
      (expect (outcomes-core-test--verdict '(:text-hidden "* x x")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:text-shown "* x x")) :to-match "displays")
      (expect (outcomes-core-test--verdict '(:text-shown "defn f")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:text-hidden "defn f")) :to-match "displays")
      (expect (outcomes-core-test--verdict '(:text-hidden "absent"))
              :to-match "does not contain")))

  (it ":face-computed accepts a face among several and rejects another"
    (with-temp-buffer
      (insert "word")
      (put-text-property 1 5 'face '(bold font-lock-string-face))
      (goto-char 2)
      (expect (outcomes-core-test--verdict '(:face-computed font-lock-string-face))
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:face-computed font-lock-doc-face))
              :to-match "bold")))

  (it ":face-shown fails where no graphical frame redisplays the buffer"
    ;; The logic session is a batch Emacs; l1-scenario-language-gui.el
    ;; checks this outcome in a graphical frame.
    (with-temp-buffer
      (insert "word")
      (put-text-property 1 5 'face 'font-lock-string-face)
      (put-text-property 1 5 'fontified t)
      (goto-char 2)
      (expect (outcomes-core-test--verdict '(:face-shown font-lock-string-face))
              :to-equal "no graphical frame shows the buffer"))))

(describe "Core outcomes on text"

  (it ":buffer-text compares the whole text"
    (with-temp-buffer
      (insert "(+ 1 2)")
      (expect (outcomes-core-test--verdict '(:buffer-text "(+ 1 2)")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:buffer-text "(+ 1 2")) :to-match "(\\+ 1 2)")))

  (it ":buffer-text with the point marker also checks point"
    (with-temp-buffer
      (insert "(+ 1 2)")
      (goto-char 5)
      (expect (outcomes-core-test--verdict '(:buffer-text "(+ 1-!- 2)")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:buffer-text "(+ -!-1 2)"))
              :to-match "1-!- 2")))

  (it ":region-text needs an active region holding the text"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "(* x x)")
      (expect (outcomes-core-test--verdict '(:region-text "")) :to-match "nil")
      (set-mark 4)
      (goto-char 5)
      (expect (outcomes-core-test--verdict '(:region-text "x")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:region-text "")) :to-match "\"x\"")
      (goto-char 4)
      (expect (outcomes-core-test--verdict '(:region-text "")) :to-be nil)))

  (it ":buffer-matches is case-sensitive"
    (with-temp-buffer
      (insert "Welcome")
      (expect (outcomes-core-test--verdict '(:buffer-matches "Welcome")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:buffer-matches "welcome")) :to-match "Welcome")))

  (it ":buffer-text-changed and -unchanged compare the entry buffer"
    (let ((entry (generate-new-buffer " entry")))
      (unwind-protect
          (with-temp-buffer
            (with-current-buffer entry (insert "after"))
            (let ((changed (list :entry-buffer entry :before-text "before"))
                  (same (list :entry-buffer entry :before-text "after")))
              (expect (outcomes-core-test--verdict '(:buffer-text-changed) changed) :to-be nil)
              (expect (outcomes-core-test--verdict '(:buffer-text-changed) same) :to-be-truthy)
              (expect (outcomes-core-test--verdict '(:buffer-text-unchanged) same) :to-be nil)
              (expect (outcomes-core-test--verdict '(:buffer-text-unchanged) changed)
                      :to-match "after")))
        (kill-buffer entry))))

  (it ":no-trailing-whitespace finds a trailing tab"
    (with-temp-buffer
      (insert "clean\nclean\n")
      (expect (outcomes-core-test--verdict '(:no-trailing-whitespace)) :to-be nil)
      (insert "dirty\t\n")
      (expect (outcomes-core-test--verdict '(:no-trailing-whitespace)) :to-match "line 3")))

  (it ":balanced rejects a missing and an extra closing paren"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(a (b))\n;; (comment\n")
      (expect (outcomes-core-test--verdict '(:balanced)) :to-be nil)
      (insert "(c")
      (expect (outcomes-core-test--verdict '(:balanced)) :to-match "unbalanced")
      (erase-buffer)
      (insert "(a))")
      (expect (outcomes-core-test--verdict '(:balanced)) :to-match "unbalanced"))))

(describe "Core outcomes on buffers, modes and variables"

  (it ":mode accepts a parent mode and rejects an unrelated one"
    (with-temp-buffer
      (emacs-lisp-mode)
      (expect (outcomes-core-test--verdict '(:mode prog-mode)) :to-be nil)
      (expect (outcomes-core-test--verdict '(:mode text-mode)) :to-match "emacs-lisp-mode")))

  (it ":minor-modes-on and -off read the buffer's mode variables"
    (with-temp-buffer
      (setq-local outcomes-core-test--on t)
      (expect (outcomes-core-test--verdict
               '(:minor-modes-on outcomes-core-test--on))
              :to-be nil)
      (expect (outcomes-core-test--verdict
               '(:minor-modes-on outcomes-core-test--on outcomes-core-test--off))
              :to-match "outcomes-core-test--off")
      (expect (outcomes-core-test--verdict '(:minor-modes-off outcomes-core-test--off))
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:minor-modes-off outcomes-core-test--on))
              :to-match "are on")
      (expect (outcomes-core-test--verdict '(:minor-modes-off outcomes-core-test--typo))
              :to-match "do not exist")))

  (it ":variable-equals, -contains and -excludes take data, not forms"
    (with-temp-buffer
      (setq-local outcomes-core-test--list '(a b))
      (expect (outcomes-core-test--verdict '(:variable-equals outcomes-core-test--list (a b)))
              :to-be nil)
      (expect (outcomes-core-test--verdict
               '(:variable-equals outcomes-core-test--list '(a b)))
              :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:variable-contains outcomes-core-test--list b))
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:variable-contains outcomes-core-test--list c))
              :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:variable-excludes outcomes-core-test--list c))
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:variable-excludes outcomes-core-test--list a))
              :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:variable-excludes outcomes-core-test--unbound a))
              :to-be-truthy)))

  (it ":buffer-name, :file-suffix and :buffer-absent"
    (with-temp-buffer
      (rename-buffer "outcomes-core-test.txt" t)
      (setq buffer-file-name "/tmp/dir/outcomes-core-test.txt")
      (unwind-protect
          (progn
            (expect (outcomes-core-test--verdict '(:buffer-name "\\`outcomes-core-test"))
                    :to-be nil)
            (expect (outcomes-core-test--verdict '(:buffer-name "other")) :to-be-truthy)
            (expect (outcomes-core-test--verdict '(:file-suffix "dir/outcomes-core-test.txt"))
                    :to-be nil)
            (expect (outcomes-core-test--verdict '(:file-suffix "other.txt")) :to-be-truthy)
            (expect (outcomes-core-test--verdict '(:buffer-absent "\\`outcomes-core-test"))
                    :to-match "outcomes-core-test.txt")
            (expect (outcomes-core-test--verdict '(:buffer-absent "\\`no such buffer\\'"))
                    :to-be nil))
        (setq buffer-file-name nil))))

  (it ":window-shows looks at every window of the frame"
    (save-window-excursion
      (delete-other-windows)
      (let ((shown (get-buffer-create " outcomes-core-test-shown")))
        (unwind-protect
            (progn
              (set-window-buffer (split-window) shown)
              (expect (outcomes-core-test--verdict '(:window-shows "core-test-shown"))
                      :to-be nil)
              (expect (outcomes-core-test--verdict '(:window-shows "\\`absent\\'"))
                      :to-be-truthy))
          (kill-buffer shown))))))

(describe "Core outcomes on errors, samples and the browser"

  (it ":user-error needs a user-error with a matching message"
    (let ((user-error-only (list :errors '((user-error "Match required"))))
          (other (list :errors '((error "Boom"))))
          (mixed (list :errors '((user-error "Match required") (error "Boom")))))
      (expect (outcomes-core-test--verdict '(:user-error "Match") user-error-only)
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:user-error "Other") user-error-only)
              :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:user-error "Boom") other) :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:user-error "Match") mixed) :to-be-truthy)
      (expect (outcomes-core-test--verdict '(:user-error "") (list :errors nil))
              :to-be-truthy)))

  (it ":sampled reads one field of one named snapshot"
    (let ((context (list :samples '(("open" :minibuffer "Switch to: alp" :buffer "start"
                                     :line "first" :echo nil)))))
      (expect (outcomes-core-test--verdict '(:sampled "open" :minibuffer "alp") context)
              :to-be nil)
      (expect (outcomes-core-test--verdict '(:sampled "open" :buffer "alp") context)
              :to-match "start")
      (expect (outcomes-core-test--verdict '(:sampled "open" :echo ".") context)
              :to-match "nil")
      (expect (outcomes-core-test--verdict '(:sampled "never" :buffer ".") context)
              :to-match "never reached")
      (expect (outcomes-core-test--verdict '(:sampled "open" :point ".") context)
              :to-match "unknown")))

  (it ":browsed-url reads what the browser boundary recorded"
    (let ((my/scenario-browsed-urls '("codex://threads/1")))
      (expect (outcomes-core-test--verdict '(:browsed-url "codex://threads/1")) :to-be nil)
      (expect (outcomes-core-test--verdict '(:browsed-url "codex://threads/2"))
              :to-match "threads/1"))))

(provide 'outcomes/core-test)

;;; outcomes/core-test.el ends here
