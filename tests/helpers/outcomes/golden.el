;;; outcomes/golden.el --- Golden approval of faces and indentation -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 094.  A golden fixture is a file NAME of `tests/golden/'.  Next
;; to it, NAME.faces records how the configuration shows it: the major
;; mode; each face used, with those of its attributes in
;; `my/golden-attributes' it specifies itself; then every token with a
;; face, as its line, its column (in characters), its face and its
;; text.  A package update that changes fontification, or a theme
;; change that drops an attribute such as the italic of comments,
;; changes that record.
;;
;; `:golden' runs in a row opened on a copy of the fixture, after keys
;; that reindent the whole buffer.  It holds when reindenting changed
;; nothing and the record is the content of NAME.faces.  With
;; UPDATE_GOLDEN=1 in the environment of `tests/run', it approves what
;; it sees instead: it writes the reindented text over the fixture and
;; the record into NAME.faces, then holds.  Review that change with
;; git before committing it.

;;; Code:

(require 'cl-lib)
(require 'scenario/outcome)

(defvar my/golden-directory
  (expand-file-name "../../golden/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the golden fixtures and their records, `tests/golden/'.")

(defconst my/golden-attributes '(:inherit :height :weight :slant)
  "The face attributes a record holds for each face it uses.
Colours are left out, here and in the anonymous faces of tokens:
auto-dark picks the theme from the desktop appearance, and a terminal
frame has other colours than a graphical one.")

(my/scenario-define-outcome :golden (context)
  "The entry buffer keeps the text and faces approved in `tests/golden/'.
The entry buffer visits a copy of the fixture NAME of `tests/golden/'.
Its text must be the text before the keys, and the record of its
faces, as font-lock computes them over the whole buffer, the content
of NAME.faces.  With UPDATE_GOLDEN=1 in the environment, write the
text over the fixture and the record into NAME.faces instead.  Face
attributes are read in the selected frame, and themes give a terminal
frame other ones, so a file using this outcome declares
`scenario-display: graphic'."
  (let* ((buffer (plist-get context :entry-buffer))
         (fixture (expand-file-name (file-name-nondirectory (buffer-file-name buffer))
                                    my/golden-directory))
         (approved (concat fixture ".faces"))
         (before (plist-get context :before-text))
         (print-escape-newlines t)
         (faces nil)
         (tokens nil)
         (difference (lambda (old new)
                       (let* ((old (split-string old "\n"))
                              (new (split-string new "\n"))
                              (line (cl-mismatch old new :test #'equal)))
                         (format "line %d: %S became %S"
                                 (1+ line) (nth line old) (nth line new))))))
    (with-current-buffer buffer
      ;; Fontify the whole buffer in one pass, whatever redisplay
      ;; fontified before: the tokens then do not depend on which
      ;; chunks of the buffer the window showed.
      (font-lock-flush)
      (font-lock-ensure)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((face (get-text-property (point) 'face))
                (end (next-single-property-change (point) 'face nil (point-max))))
            (when face
              ;; An anonymous face, such as org-modern's table rule,
              ;; can hold a colour of the theme: record that it has
              ;; one, not which.
              (push (format "%d:%d %s %S" (line-number-at-pos) (- (point) (pos-bol))
                            (replace-regexp-in-string "\"#[[:xdigit:]]+\"" "COLOUR"
                                                      (prin1-to-string face) t t)
                            (buffer-substring-no-properties (point) end))
                    tokens)
              (dolist (one (ensure-list face))
                (when (facep one)
                  (cl-pushnew one faces))))
            (goto-char end)))))
    (let ((text (with-current-buffer buffer
                  (buffer-substring-no-properties (point-min) (point-max))))
          (record (concat
                   (format "mode %s\n" (buffer-local-value 'major-mode buffer))
                   (mapconcat (lambda (face)
                                (format "face %s%s\n" face
                                        (mapconcat (lambda (attribute)
                                                     (let ((value (face-attribute face attribute)))
                                                       (if (eq value 'unspecified)
                                                           ""
                                                         (format " %s %S" attribute value))))
                                                   my/golden-attributes "")))
                              (sort faces #'string<) "")
                   (mapconcat (lambda (token) (concat token "\n")) (nreverse tokens) "")))
          (recorded (if (file-exists-p approved)
                        (with-temp-buffer
                          (insert-file-contents approved)
                          (buffer-string))
                      ""))
          (coding-system-for-write 'utf-8-unix))
      (cond ((equal (getenv "UPDATE_GOLDEN") "1")
             (write-region text nil fixture nil 'silent)
             (write-region record nil approved nil 'silent)
             nil)
            ((not (equal text before))
             (format "reindenting changed the text at %s" (funcall difference before text)))
            ((not (equal record recorded))
             (format "the faces differ from %s at %s; review the difference, then \
approve it with UPDATE_GOLDEN=1"
                     approved (funcall difference recorded record)))))))

(provide 'outcomes/golden)

;;; outcomes/golden.el ends here
