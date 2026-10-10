;;; sample.el --- A golden fixture of Emacs Lisp -*- lexical-binding: t -*-

;;; Commentary:
;; Typical configuration code: a custom variable, a function with a
;; docstring, a macro, a hook and a key binding.

;;; Code:

(require 'cl-lib)

(defcustom sample-width 120
  "The width of a line, in columns."
  :type 'natnum
  :group 'convenience)

(defvar sample--cache (make-hash-table :test #'equal)
  "Results already computed, keyed by file name.")

(defun sample-words (text &optional limit)
  "Return the words of TEXT, at most LIMIT of them.
Signal `user-error' when TEXT is empty."
  (when (string-empty-p text)
    (user-error "No text in %S" (current-buffer)))
  (let ((words (split-string text "[ \t\n]+" t)))
    (if (and limit (> (length words) limit))
        (cl-subseq words 0 limit)
      words)))

(defmacro sample-with-cache (key &rest body)
  "Return the cached value of KEY, or compute it with BODY."
  (declare (indent 1))
  `(or (gethash ,key sample--cache)
       (puthash ,key (progn ,@body) sample--cache)))

(defun sample-count ()
  "Show how many words the current buffer holds."
  (interactive)
  (let* ((file (or buffer-file-name "*scratch*"))
         (count (sample-with-cache file
                                   (length (sample-words (buffer-string))))))
    (message "%s: %d words, ?a is %c" file count ?a)))

(add-hook 'text-mode-hook #'visual-line-mode)
(keymap-set global-map "C-c w" #'sample-count)

(provide 'sample)

;;; sample.el ends here
