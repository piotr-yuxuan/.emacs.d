;;; scenario-test.el --- Self-tests of the scenario macro, rows and boundaries -*- lexical-binding: t -*-

;;; Commentary:
;; The macro validates every slot when it expands, so an authoring
;; mistake fails before any spec runs.  Rows, system boundaries and
;; waits are checked here through their functions.  What happens when
;; keys are pressed in a live session is covered by
;; `tests/scenarios/l1-scenario-language.el'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'scenario)

(defvar lsp-session-file)
(defvar lsp--session)
(defvar auto-dark--last-dark-mode-state)
(declare-function make-lsp--workspace "lsp-mode")
(declare-function auto-dark--current-system-mode "auto-dark")
(declare-function lsp-session "lsp-mode")
(declare-function lsp-session-folders "lsp-mode")

(defun scenario-test--expand (&rest overrides)
  "Expand a valid scenario after applying the plist OVERRIDES to it."
  (let ((spec (list :describe "d"
                    :given '((:name "row" :file "a.txt" :text "x"))
                    :when '((:keys "C-e"))
                    :then '((:buffer-text "x")))))
    (cl-loop for (slot value) on overrides by #'cddr
             do (setq spec (plist-put spec slot value)))
    (macroexpand-1 (cons 'scenario spec))))

(describe "The scenario macro"

  (it "expands to one spec per row that runs the row with data"
    (expect (scenario-test--expand
             :given '((:name "one" :file "a.txt") (:name "two" :file "b.txt")))
            :to-equal
            '(describe "d"
               (it "[one]" (my/scenario-run-row '(:name "one" :file "a.txt")
                                                '([?\C-e scenario-probe] (:end))
                                                '((:buffer-text "x")) 'nil))
               (it "[two]" (my/scenario-run-row '(:name "two" :file "b.txt")
                                                '([?\C-e scenario-probe] (:end))
                                                '((:buffer-text "x")) 'nil)))))

  (it "rejects missing, unknown and malformed slots"
    (expect (macroexpand-1 '(scenario :describe "d" :given ((:name "r" :file "a"))
                                      :then ((:buffer-text ""))))
            :to-throw 'error)
    (expect (scenario-test--expand :describe 'symbol) :to-throw 'error)
    (expect (scenario-test--expand :wait :forever) :to-throw 'error))

  (it "rejects quoted and empty :given, :when and :then"
    (expect (scenario-test--expand :given ''((:name "r" :file "a"))) :to-throw 'error)
    (expect (scenario-test--expand :when ''((:keys "C-e"))) :to-throw 'error)
    (expect (scenario-test--expand :then ''((:buffer-text ""))) :to-throw 'error)
    (expect (scenario-test--expand :given nil) :to-throw 'error)
    (expect (scenario-test--expand :when nil) :to-throw 'error)
    (expect (scenario-test--expand :when "C-e") :to-throw 'error)
    (expect (scenario-test--expand :then nil) :to-throw 'error))

  (it "rejects an unknown outcome, a bad step and a bad wait"
    (expect (scenario-test--expand :then '((:point-near "x"))) :to-throw 'error)
    (expect (scenario-test--expand :when '((kbd "C-e"))) :to-throw 'error)
    (expect (scenario-test--expand :wait-for '((:lsp-attached no-such-server)))
            :to-throw 'error))

  (it "rejects two rows with one name"
    (expect (scenario-test--expand
             :given '((:name "same" :file "a.txt") (:name "same" :file "b.txt")))
            :to-throw 'error)))

(describe "Rows"

  (it "reject an unknown key, no shape, two shapes and stray keys"
    (dolist (row '((:name "r" :file "a" :command (ignore))
                   (:name "r")
                   (:name "r" :file "a" :project "p" :entry "e")
                   (:name "r" :project "p")
                   (:name "r" :existing-buffer "x" :text "t")
                   (:file "a")
                   (:name "r" :file "a" :system (:network down))
                   (:name "r" :file "a" :system (:appearance dim))
                   (:name "r" :file "a" :text "a-!-b-!-c")
                   (:name "r" :file "a" :text "a-!-b" :point-on "b")))
      (expect (my/scenario-row-check row) :to-throw 'error)))

  (it "accept the three shapes"
    (dolist (row '((:name "r" :file "a" :text "t" :system (:appearance dark))
                   (:name "r" :project "p" :entry "e" :point-on "x"
                          :system (:lsp-root "."))
                   (:name "r" :existing-buffer "x" :system (:record-browse-url t))))
      (expect (my/scenario-row-check row) :not :to-throw)))

  (it "place point at the marker and remove it from the file"
    (let ((sandbox (my/scenario-sandbox-create)))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer
                (my/scenario-row-open
                 '(:name "r" :file "src/a.el" :text "(a |b) -!-(c)") sandbox)
              (unwind-protect
                  (progn
                    (expect (buffer-string) :to-equal "(a |b) (c)")
                    (expect (point) :to-equal 8)
                    (expect (file-truename buffer-file-name)
                            :to-equal (file-truename
                                       (expand-file-name "src/a.el" sandbox))))
                (kill-buffer))))
        (delete-directory sandbox t))))

  (it "place point on the first occurrence of :point-on"
    (let ((sandbox (my/scenario-sandbox-create)))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer
                (my/scenario-row-open
                 '(:name "r" :project "clojure-nav" :entry "src/acme/core.clj"
                         :point-on "another/quux")
                 sandbox)
              (unwind-protect
                  (progn
                    (expect (looking-at-p "another/quux x") :to-be-truthy)
                    (expect (file-truename (locate-dominating-file buffer-file-name ".git"))
                            :to-equal (file-truename
                                       (my/scenario-row-root
                                        '(:project "clojure-nav") sandbox))))
                (kill-buffer))))
        (delete-directory sandbox t))))

  (it "refuse a :point-on that the text does not hold"
    (let ((sandbox (my/scenario-sandbox-create)))
      (unwind-protect
          (save-window-excursion
            (expect (my/scenario-row-open
                     '(:name "r" :file "b.txt" :text "ab" :point-on "z") sandbox)
                    :to-throw 'error)
            (dolist (buffer (buffer-list))
              (when (file-in-directory-p (buffer-local-value 'default-directory buffer)
                                         sandbox)
                (kill-buffer buffer))))
        (delete-directory sandbox t))))

  (it "use an existing buffer only when the selected window shows it"
    (save-window-excursion
      (let ((shown (get-buffer-create " scenario-test-shown")))
        (unwind-protect
            (progn
              (switch-to-buffer shown)
              (expect (my/scenario-row-open
                       '(:name "r" :existing-buffer "scenario-test-shown") "/tmp/")
                      :to-be shown)
              (expect (my/scenario-row-open
                       '(:name "r" :existing-buffer "\\`\\*dashboard\\*\\'") "/tmp/")
                      :to-throw 'error))
          (kill-buffer shown))))))

(describe "System boundaries"

  (it "record browsed URLs, then restore the browser and forget them"
    (let ((browse-url-browser-function #'ignore))
      (let ((restore (my/scenario-system-apply '(:record-browse-url t) "/tmp/")))
        (browse-url "https://example.org/")
        (expect my/scenario-browsed-urls :to-equal '("https://example.org/"))
        (funcall restore))
      (expect my/scenario-browsed-urls :to-be nil)
      (expect browse-url-browser-function :to-be #'ignore)))

  (it "restore the stubbed boundaries when a later one fails"
    (require 'lsp-mode)
    (let ((browse-url-browser-function #'ignore))
      (cl-letf (((symbol-function 'lsp-workspace-folders-add)
                 (lambda (_folder) (error "No session")))
                ((symbol-function 'lsp-workspace-folders-remove) #'ignore))
        (expect (my/scenario-system-apply
                 '(:record-browse-url t :lsp-root "project") "/tmp/")
                :to-throw 'error '("No session")))
      (expect browse-url-browser-function :to-be #'ignore)))

  (it "write the lsp root into the lsp session, then remove it"
    (require 'lsp-mode)
    (let* ((directory (make-temp-file "scenario-test-lsp-" t))
           (lsp-session-file (expand-file-name "session" directory))
           (lsp--session nil))
      (unwind-protect
          (let ((restore (my/scenario-system-apply '(:lsp-root "project") directory))
                (folder (file-name-as-directory (expand-file-name "project" directory))))
            (expect (mapcar #'file-name-as-directory (lsp-session-folders (lsp-session)))
                    :to-contain folder)
            (expect (file-exists-p lsp-session-file) :to-be-truthy)
            (funcall restore)
            (expect (mapcar #'file-name-as-directory (lsp-session-folders (lsp-session)))
                    :not :to-contain folder))
        (delete-directory directory t))))

  (it "make auto-dark see the stubbed OS appearance, then the real one"
    (require 'auto-dark)
    (let ((real (auto-dark--current-system-mode))
          (wanted (if (eq (auto-dark--current-system-mode) 'dark) 'light 'dark)))
      (let ((restore (my/scenario-system-apply `(:appearance ,wanted) "/tmp/")))
        (unwind-protect
            (progn
              (expect (auto-dark--current-system-mode) :to-be wanted)
              (expect auto-dark--last-dark-mode-state :to-be wanted))
          (funcall restore)))
      (expect (auto-dark--current-system-mode) :to-equal real))))

(describe "Waiting"

  (it "skips a spec whose language server executable is missing"
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (not (equal name "clojure-lsp")) "/bin/x"))))
      (expect (condition-case skip
                  (my/scenario-wait-skip-unless-available '((:lsp-attached clojure-lsp)))
                (buttercup-pending (cdr skip)))
              :to-equal "missing executable clojure-lsp")
      (expect (my/scenario-wait-skip-unless-available '((:lsp-attached ruff)))
              :not :to-throw)))

  (it "waits until every workspace is initialized and idle"
    (require 'lsp-mode)
    (let* ((busy (make-hash-table :test #'equal))
           (ready (make-lsp--workspace :status 'initialized
                                       :work-done-tokens (make-hash-table)))
           (indexing (make-lsp--workspace :status 'initialized :work-done-tokens busy))
           (starting (make-lsp--workspace :status 'starting
                                          :work-done-tokens (make-hash-table)))
           (workspaces nil)
           (my/scenario-wait-seconds 0.3))
      (puthash "token" t busy)
      (cl-letf (((symbol-function 'lsp-workspaces) (lambda () workspaces)))
        (expect (my/scenario-wait '((:lsp-indexed))) :to-throw 'error)
        (setq workspaces (list ready indexing))
        (expect (my/scenario-wait '((:lsp-indexed))) :to-throw 'error)
        (setq workspaces (list ready starting))
        (expect (my/scenario-wait '((:lsp-indexed))) :to-throw 'error)
        (setq workspaces (list ready))
        (expect (my/scenario-wait '((:lsp-indexed))) :not :to-throw)))))

(describe "The vocabulary"

  (it "lists every step and outcome"
    (let ((text (my/scenario-vocabulary)))
      (dolist (step my/scenario-steps)
        (expect text :to-match (regexp-quote (format "=(%S" (car step)))))
      (maphash (lambda (keyword _)
                 (expect text :to-match (regexp-quote (format "=(%S" keyword))))
               my/scenario-outcomes))))

(provide 'scenario-test)

;;; scenario-test.el ends here
