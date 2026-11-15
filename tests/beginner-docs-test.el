;;; beginner-docs-test.el --- Getting started stays a beginner's path -*- lexical-binding: t -*-

;;; Commentary:
;; Getting started is written for a beginner on NixOS and on macOS with
;; Homebrew.  Its commands sit in source blocks marked `:beginner
;; macos', `:beginner nixos' or `:beginner both'; the Homebrew beginner
;; workflow of the public repository runs the macOS ones, as printed by
;; tests/tools/doc-block, on a clean runner.  These specs keep that
;; contract: the blocks the workflow runs are shell, each step with a
;; macOS part has a NixOS part, the tool prints what the document holds,
;; and the expert material waits until "Going further".

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-element)

(defconst beginner-docs-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "The checkout under test, which holds tests/.")

(defconst beginner-docs-test--tutorial
  (expand-file-name "docs/tutorials/getting-started.org" beginner-docs-test--root)
  "The tutorial of the beginner path.")

(defconst beginner-docs-test--check
  (expand-file-name "docs/how-to/check-with-the-tests.org" beginner-docs-test--root)
  "The how-to whose macOS commands the workflow runs after the tutorial's.")

(defun beginner-docs-test--blocks (file)
  "Return FILE's beginner blocks, as (SECTION TARGET LANGUAGE BODY).
SECTION is the title of the top-level heading that holds the block."
  (with-temp-buffer
    (insert-file-contents file)
    (delay-mode-hooks (org-mode))
    (apply #'append
           (org-element-map (org-element-parse-buffer) 'headline
             (lambda (section)
               (when (= (org-element-property :level section) 1)
                 (org-element-map (org-element-contents section) 'src-block
                   (lambda (block)
                     (let ((parameters (org-element-property :parameters block)))
                       (when (and parameters
                                  (string-match ":beginner\\(?: +\\([^ ]*\\)\\)?"
                                                parameters))
                         (list (org-element-property :raw-value section)
                               (or (match-string 1 parameters) "")
                               (org-element-property :language block)
                               (string-trim-right
                                (org-element-property :value block)))))))))))))

(defun beginner-docs-test--doc-block (platform file)
  "Return what tests/tools/doc-block prints for PLATFORM and FILE."
  (with-temp-buffer
    (let ((status (call-process
                   "python3" nil t nil
                   (expand-file-name "tests/tools/doc-block" beginner-docs-test--root)
                   platform file)))
      (unless (eql status 0)
        (error "doc-block exited with %S: %s" status (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun beginner-docs-test--text-before (file heading)
  "Return the text of FILE before the top-level HEADING, or all of it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
        (buffer-substring-no-properties (point-min) (match-beginning 0))
      (buffer-string))))

(describe "tests/tools/doc-block"
  (it "prints a platform's blocks and the shared ones, in document order"
    (let ((file (make-temp-file "doc-block-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "* One\n"
                      "#+BEGIN_SRC sh :beginner both\nshared\n#+END_SRC\n"
                      "- An item:\n"
                      "  #+begin_src sh :beginner macos\n  indented\n  ,* starred\n  #+end_src\n"
                      "#+BEGIN_SRC nix :beginner nixos\nother platform\n#+END_SRC\n"
                      "#+BEGIN_SRC sh\nunmarked\n#+END_SRC\n"
                      "#+BEGIN_SRC sh :beginner macos :tangle no\nlast\n#+END_SRC\n"))
            (expect (beginner-docs-test--doc-block "macos" file)
                    :to-equal "shared\n\nindented\n* starred\n\nlast")
            (expect (beginner-docs-test--doc-block "nixos" file)
                    :to-equal "shared\n\nother platform"))
        (delete-file file)))))

(describe "Check the configuration with its tests"
  (it "installs every package, then runs the fast suite, on macOS"
    (let ((commands (beginner-docs-test--doc-block "macos" beginner-docs-test--check)))
      (expect commands :to-match "^nix/setup\\.sh packages$")
      (expect commands :to-match "tests/run fast$")
      (expect (string-match "nix/setup" commands) :to-be-less-than
              (string-match "tests/run fast" commands)))))

(describe "Getting started"
  (it "marks its beginner blocks for macos, nixos or both"
    (let ((blocks (beginner-docs-test--blocks beginner-docs-test--tutorial)))
      (expect (seq-filter (lambda (b) (equal (nth 1 b) "macos")) blocks) :not :to-be nil)
      (expect (seq-remove (lambda (b) (member (nth 1 b) '("macos" "nixos" "both"))) blocks)
              :to-equal nil)))

  (it "runs only shell blocks on macOS"
    (dolist (file (list beginner-docs-test--tutorial beginner-docs-test--check))
      (expect (seq-filter (lambda (b) (and (member (nth 1 b) '("macos" "both"))
                                           (not (equal (nth 2 b) "sh"))))
                          (beginner-docs-test--blocks file))
              :to-equal nil)))

  (it "gives a NixOS part to each step that has a macOS part"
    (let* ((blocks (beginner-docs-test--blocks beginner-docs-test--tutorial))
           (sections (lambda (target)
                       (seq-uniq (mapcar #'car (seq-filter (lambda (b) (equal (nth 1 b) target))
                                                           blocks))))))
      (expect (seq-difference (funcall sections "macos") (funcall sections "nixos"))
              :to-equal nil)))

  (it "is what tests/tools/doc-block prints for macOS"
    (expect (beginner-docs-test--doc-block "macos" beginner-docs-test--tutorial)
            :to-equal
            (mapconcat (lambda (b) (nth 3 b))
                       (seq-filter (lambda (b) (member (nth 1 b) '("macos" "both")))
                                   (beginner-docs-test--blocks beginner-docs-test--tutorial))
                       "\n\n")))

  (it "starts Emacs by its third step"
    (with-temp-buffer
      (insert-file-contents beginner-docs-test--tutorial)
      (goto-char (point-min))
      (let ((steps 0) start)
        (while (and (not start) (re-search-forward "^\\* [0-9]+\\. \\(.*\\)$" nil t))
          (setq steps (1+ steps))
          (when (string-match-p "\\`Start Emacs" (match-string 1))
            (setq start (match-string 1))))
        (expect start :not :to-be nil)
        (expect steps :not :to-be-greater-than 3))))

  (it "names no expert tool before Going further"
    (let ((case-fold-search t)
          (before (beginner-docs-test--text-before beginner-docs-test--tutorial
                                                   "Going further")))
      (expect (seq-filter (lambda (word) (string-match-p (regexp-quote word) before))
                          '("flake" "Home Manager" "nix run" "setup.sh" "tests/run"))
              :to-equal nil))))

(provide 'beginner-docs-test)

;;; beginner-docs-test.el ends here
