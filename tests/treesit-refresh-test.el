;;; treesit-refresh-test.el --- Logic tests of plan 111 -*- lexical-binding: t -*-

;;; Commentary:
;; The background rebuild of the tree-sitter grammars: what it installs,
;; keeps and deletes, from the rules of plan 111's D3, and one real run
;; of its builder that reaches no network, in a scratch
;; `user-emacs-directory'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar treesit-auto-recipe-list)
(defvar treesit-auto-langs)
(defvar my/treesit-refresh-process)
(declare-function my/treesit-refresh-actions "init")
(declare-function my/treesit-refresh-grammars "init")
(declare-function make-treesit-auto-recipe "treesit-auto")

(describe "my/treesit-refresh-actions"
  (it "installs each grammar the builder kept"
    (expect (my/treesit-refresh-actions nil '("libtree-sitter-yaml.so"))
            :to-equal '((install "libtree-sitter-yaml.so"))))

  (it "keeps the installed grammar of a language that failed to build"
    (expect (my/treesit-refresh-actions
             '(("libtree-sitter-json.so" . nil)) '("libtree-sitter-yaml.so"))
            :to-equal '((install "libtree-sitter-yaml.so"))))

  (it "deletes the copy a build left, and no grammar for its language"
    (expect (my/treesit-refresh-actions
             '(("libtree-sitter-yaml.dylib.old" . nil)
               ("libtree-sitter-markdown-inline.dylib" . nil))
             nil)
            :to-equal '((delete "libtree-sitter-yaml.dylib.old"))))

  (it "neither replaces nor deletes a symbolic link"
    (expect (my/treesit-refresh-actions
             '(("libtree-sitter-yaml.so" . t) ("libtree-sitter-gone.so.old" . t))
             '("libtree-sitter-yaml.so"))
            :to-equal nil)))

(describe "my/treesit-refresh-grammars"
  (it "keeps what it cannot rebuild and cleans the rest, in a batch Emacs"
    (let* ((home (make-temp-file "treesit-refresh-test-" t))
           (user-emacs-directory (file-name-as-directory home))
           (dir (expand-file-name "tree-sitter/" home))
           (target (expand-file-name "elsewhere.so" home))
           (my/treesit-refresh-process nil)
           ;; The builder's git reads no configuration of the user's,
           ;; and ~ no longer names the checkout.
           (default-directory (file-name-as-directory home))
           (process-environment
            (append (list (concat "HOME=" home) "GIT_CONFIG_GLOBAL=/dev/null"
                          "GIT_CONFIG_NOSYSTEM=1")
                    process-environment)))
      (unwind-protect
          (progn
            (require 'treesit-auto)
            (make-directory dir t)
            (dolist (file '("libtree-sitter-foo.so" "libtree-sitter-foo.so.old"
                            "libtree-sitter-other.so"))
              (write-region "" nil (expand-file-name file dir)))
            (write-region "" nil target)
            (make-symbolic-link target (expand-file-name "libtree-sitter-bar.so" dir))
            ;; foo's source is a path that does not exist: its build fails
            ;; at the clone, offline.
            (let ((treesit-auto-recipe-list
                   (list (make-treesit-auto-recipe
                          :lang 'foo :ts-mode 'foo-ts-mode
                          :url (expand-file-name "no-such-repository" home))
                         (make-treesit-auto-recipe
                          :lang 'bar :ts-mode 'bar-ts-mode :url "unused")))
                  (treesit-auto-langs '(foo bar)))
              (my/treesit-refresh-grammars))
            (let ((deadline (+ (float-time) 60)))
              (while (and (process-live-p my/treesit-refresh-process)
                          (< (float-time) deadline))
                (accept-process-output my/treesit-refresh-process 1)))
            ;; The sentinel runs once the process is gone.
            (accept-process-output nil 0.5)
            (expect (process-live-p my/treesit-refresh-process) :to-be nil)
            (expect (sort (directory-files dir nil "\\`[^.]") #'string<)
                    :to-equal '("libtree-sitter-bar.so" "libtree-sitter-foo.so"
                                "libtree-sitter-other.so"))
            (expect (file-symlink-p (expand-file-name "libtree-sitter-bar.so" dir))
                    :to-be-truthy)
            (expect (directory-files dir nil "\\`\\.refresh-") :to-equal nil))
        (when (process-live-p my/treesit-refresh-process)
          (kill-process my/treesit-refresh-process))
        (delete-directory home t)))))

;;; treesit-refresh-test.el ends here
