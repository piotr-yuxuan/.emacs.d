;;; outcomes/golden-test.el --- Self-tests of the golden outcome -*- lexical-binding: t -*-

;;; Commentary:
;; `:golden' must record every token with its line, column, face and
;; text; hold on what it approved; and fail when a token gets another
;; face, when a face loses an attribute, when the keys changed the
;; text, and when nothing is approved yet.  The plausible wrong
;; implementations compare only the text, only the face names, or the
;; record with itself, or approve only the faces.  These specs run in
;; batch, without a frame: the attributes they record are a terminal
;; frame's, which is enough to tell a change.

;;; Code:

(require 'buttercup)
(require 'scenario/outcome)
(require 'outcomes)

(defvar my/golden-directory)

(defconst outcomes-golden-test--fixture ";; A comment.\n(defun f ()\n 1)\n"
  "The text of the fixture f.el, as approved before the specs.")

(defconst outcomes-golden-test--reindented ";; A comment.\n(defun f ()\n  1)\n"
  "The text of the copy of f.el, as the keys left it.")

(describe "The :golden outcome"
  :var (directory buffer verdict)
  (before-each
    (setq directory (file-name-as-directory (make-temp-file "golden-test-" t))
          buffer (generate-new-buffer "golden-test"))
    (write-region outcomes-golden-test--fixture nil (expand-file-name "f.el" directory)
                  nil 'silent)
    (with-current-buffer buffer
      (insert outcomes-golden-test--reindented)
      (emacs-lisp-mode)
      ;; A copy of the fixture, as the row's sandbox holds it.
      (setq buffer-file-name (expand-file-name "copy/f.el" directory))
      (set-buffer-modified-p nil))
    (setq verdict
          (lambda (before &optional update)
            (let ((my/golden-directory directory)
                  (process-environment (cons (if update "UPDATE_GOLDEN=1" "UPDATE_GOLDEN")
                                             process-environment)))
              (my/scenario-check-clause '(:golden))
              (my/scenario-outcome-verdict
               '(:golden) (list :entry-buffer buffer :before-text before))))))
  (after-each
    (kill-buffer buffer)
    (delete-directory directory t))

  (it "approves the text and every token with UPDATE_GOLDEN=1, then holds"
    (expect (funcall verdict outcomes-golden-test--fixture t) :to-be nil)
    (expect (with-temp-buffer
              (insert-file-contents (expand-file-name "f.el" directory))
              (buffer-string))
            :to-equal outcomes-golden-test--reindented)
    (let ((record (with-temp-buffer
                    (insert-file-contents (expand-file-name "f.el.faces" directory))
                    (buffer-string))))
      (expect record :to-match "\\`mode emacs-lisp-mode\n")
      (expect record :to-match "^face font-lock-keyword-face")
      (expect record :to-match "^1:0 font-lock-comment-delimiter-face \";; \"$")
      (expect record :to-match "^1:3 font-lock-comment-face \"A comment.\\\\n\"$")
      (expect record :to-match "^2:1 font-lock-keyword-face \"defun\"$")
      (expect record :to-match "^2:7 font-lock-function-name-face \"f\"$"))
    (expect (funcall verdict outcomes-golden-test--reindented) :to-be nil))

  (it "fails when nothing is approved"
    (expect (funcall verdict outcomes-golden-test--reindented)
            :to-match "faces differ from .*f\\.el\\.faces at line 1: .*UPDATE_GOLDEN=1"))

  (it "fails when the keys changed the text"
    (funcall verdict outcomes-golden-test--reindented t)
    (expect (funcall verdict outcomes-golden-test--fixture)
            :to-match "changed the text at line 3: \" 1)\" became \"  1)\""))

  (it "fails when a token gets another face"
    (funcall verdict outcomes-golden-test--reindented t)
    (with-current-buffer buffer
      ;; As after an update of the mode: `defun' is a builtin now.
      (setq-local font-lock-keywords '(("\\_<defun\\_>" . font-lock-builtin-face))))
    (expect (funcall verdict outcomes-golden-test--reindented)
            :to-match "faces differ .*font-lock-builtin-face"))

  (it "fails when a face loses an attribute"
    (funcall verdict outcomes-golden-test--reindented t)
    (let ((slant (face-attribute 'font-lock-comment-face :slant)))
      (unwind-protect
          (progn
            (set-face-attribute 'font-lock-comment-face nil :slant 'oblique)
            (expect (funcall verdict outcomes-golden-test--reindented)
                    :to-match "\"face font-lock-comment-face[^\"]*:slant oblique\""))
        (set-face-attribute 'font-lock-comment-face nil :slant slant))))

  (it "records that an anonymous face has a colour, not which one"
    ;; As org-modern's table rules, coloured by the theme, which the
    ;; desktop appearance picks: the record must not change with it,
    ;; but must still change when the rule goes.
    (with-current-buffer buffer
      (setq-local font-lock-keywords '(("\\_<defun\\_>" 0 '(:overline "#b294bb")))))
    (funcall verdict outcomes-golden-test--reindented t)
    (with-current-buffer buffer
      (setq-local font-lock-keywords '(("\\_<defun\\_>" 0 '(:overline "#8959a8")))))
    (expect (funcall verdict outcomes-golden-test--reindented) :to-be nil)
    (with-current-buffer buffer
      (setq-local font-lock-keywords '(("\\_<defun\\_>" 0 '(:underline t)))))
    (expect (funcall verdict outcomes-golden-test--reindented)
            :to-match "faces differ .*:overline.* became .*:underline t")))

(provide 'outcomes/golden-test)

;;; outcomes/golden-test.el ends here
