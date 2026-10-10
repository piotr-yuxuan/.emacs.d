;;; outcomes/core.el --- The core outcomes of the scenario language -*- lexical-binding: t -*-

;;; Commentary:
;; The outcomes every scenario may use.  See `scenario/outcome.el' for
;; the contract: each definition returns nil when the user would see
;; the fact, otherwise what the user would see instead.  Regular
;; expressions are matched case-sensitively.

;;; Code:

(require 'cl-lib)
(require 'scenario/outcome)
(require 'scenario/display)
(require 'scenario/row)
(require 'scenario/system)

;;;; Buffer text

(my/scenario-define-outcome :buffer-text (_context text)
  "The whole text of the current buffer is TEXT.
When TEXT holds the point marker -!-, point is also where it is."
  (let ((actual (concat (buffer-substring-no-properties (point-min) (point))
                        (if (string-search my/scenario-point-marker text)
                            my/scenario-point-marker
                          "")
                        (buffer-substring-no-properties (point) (point-max)))))
    (unless (equal actual text)
      (format "the buffer text is %S" actual))))

(my/scenario-define-outcome :buffer-matches (_context regexp)
  "Some text of the current buffer matches REGEXP."
  (let ((case-fold-search nil)
        (actual (buffer-substring-no-properties (point-min) (point-max))))
    (unless (string-match-p regexp actual)
      (format "no match in the buffer text %S" actual))))

(my/scenario-define-outcome :buffer-text-changed (context)
  "The keys changed the text of the entry buffer."
  (with-current-buffer (plist-get context :entry-buffer)
    (when (equal (buffer-substring-no-properties (point-min) (point-max))
                 (plist-get context :before-text))
      "the entry buffer text did not change")))

(my/scenario-define-outcome :buffer-text-unchanged (context)
  "The keys left the text of the entry buffer as it was."
  (with-current-buffer (plist-get context :entry-buffer)
    (let ((actual (buffer-substring-no-properties (point-min) (point-max))))
      (unless (equal actual (plist-get context :before-text))
        (format "the entry buffer text became %S" actual)))))

(my/scenario-define-outcome :no-trailing-whitespace (_context)
  "No line of the current buffer ends with spaces or tabs."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "[ \t]+$" nil t)
      (format "line %d ends with whitespace" (line-number-at-pos)))))

(my/scenario-define-outcome :balanced (_context)
  "The delimiters of the current buffer are balanced."
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn
          (while (progn (skip-chars-forward " \t\n") (not (eobp)))
            (goto-char (or (scan-sexps (point) 1) (point-max))))
          nil)
      (scan-error (format "unbalanced: %s" (cadr err))))))

;;;; Point and display

(my/scenario-define-outcome :line-at-point (_context regexp)
  "The text of the line at point matches REGEXP."
  (let ((case-fold-search nil)
        (line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (unless (string-match-p regexp line)
      (format "line %d at point is %S" (line-number-at-pos) line))))

(my/scenario-define-outcome :line-display-matches (_context line-regexp display-regexp)
  "The first line whose text matches LINE-REGEXP displays as DISPLAY-REGEXP.
The displayed line is the buffer text without invisible text, with
string `display' properties and overlay strings in place.  See
`my/scenario-displayed-text'."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (if (not (re-search-forward line-regexp nil t))
          (format "no line matches %S" line-regexp)
        (let ((shown (my/scenario-displayed-text (line-beginning-position)
                                                 (line-end-position))))
          (unless (string-match-p display-regexp shown)
            (format "the line displays as %S" shown)))))))

(my/scenario-define-outcome :text-shown (_context text)
  "TEXT appears in the current buffer as displayed.
See `my/scenario-displayed-text' for what displayed means."
  (let ((shown (my/scenario-displayed-text (point-min) (point-max))))
    (unless (string-search text shown)
      (format "the buffer displays as %S" shown))))

(my/scenario-define-outcome :text-hidden (_context text)
  "TEXT is in the current buffer but does not appear as displayed.
Folding, for example, hides text this way."
  (let ((shown (my/scenario-displayed-text (point-min) (point-max))))
    (cond ((not (string-search text (buffer-substring-no-properties
                                     (point-min) (point-max))))
           (format "the buffer text does not contain %S" text))
          ((string-search text shown)
           (format "the buffer displays as %S" shown)))))

(my/scenario-define-outcome :face-shown (_context face)
  "The text at point is shown with FACE, possibly among other faces.
The faces are read as redisplay and the idle timers left them, so
text that jit-lock has not fontified fails.  Only a graphical frame
redisplays the buffer, so a file using this outcome declares
`scenario-display: graphic'."
  (let ((faces (ensure-list (get-text-property (point) 'face))))
    (cond ((not (display-graphic-p))
           "no graphical frame shows the buffer")
          ((and jit-lock-mode (not (eq (get-text-property (point) 'fontified) t)))
           "the text at point is not fontified")
          ((not (memq face faces))
           (format "the faces at point are %S" faces)))))

(my/scenario-define-outcome :face-computed (_context face)
  "Font-lock gives the text at point FACE, possibly among other faces.
The text is fontified here first, whatever redisplay did, so this
checks the faces the mode computes, not that they are shown: use
`:face-shown' for that."
  (font-lock-ensure)
  (let ((faces (ensure-list (get-text-property (point) 'face))))
    (unless (memq face faces)
      (format "the faces at point are %S" faces))))

(my/scenario-define-outcome :region-text (_context text)
  "The region is active and holds TEXT, which may be empty."
  (let ((actual (and (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end)))))
    (unless (equal actual text)
      (format "the region holds %S" actual))))

(my/scenario-define-outcome :header-line-matches (_context regexp)
  "The header line of the selected window shows text matching REGEXP.
Emacs formats no mode line in batch or in the initial frame of a
daemon, so a file using this outcome declares `scenario-display:
graphic'."
  (let ((case-fold-search nil)
        (shown (and header-line-format
                    (substring-no-properties
                     (format-mode-line header-line-format nil (selected-window)
                                       (current-buffer))))))
    (unless (and shown (string-match-p regexp shown))
      (format "the header line is %S" shown))))

;;;; Buffers, files and modes

(my/scenario-define-outcome :buffer-name (_context regexp)
  "The name of the current buffer matches REGEXP."
  (let ((case-fold-search nil))
    (unless (string-match-p regexp (buffer-name))
      (format "the current buffer is %S" (buffer-name)))))

(my/scenario-define-outcome :buffer-absent (_context regexp)
  "No live buffer has a name matching REGEXP."
  (let* ((case-fold-search nil)
         (found (cl-remove-if-not (lambda (buffer)
                                    (string-match-p regexp (buffer-name buffer)))
                                  (buffer-list))))
    (when found
      (format "these buffers exist: %S" (mapcar #'buffer-name found)))))

(my/scenario-define-outcome :file-suffix (_context suffix)
  "The current buffer visits a file whose name ends with SUFFIX."
  (unless (and buffer-file-name (string-suffix-p suffix buffer-file-name))
    (format "the current buffer %S visits %S" (buffer-name) buffer-file-name)))

(my/scenario-define-outcome :window-shows (_context regexp)
  "A window of the selected frame shows a buffer whose name matches REGEXP."
  (let ((case-fold-search nil)
        (names (mapcar (lambda (window) (buffer-name (window-buffer window)))
                       (window-list nil 'no-minibuffer))))
    (unless (cl-some (lambda (name) (string-match-p regexp name)) names)
      (format "the windows show %S" names))))

(my/scenario-define-outcome :mode (_context mode)
  "The major mode of the current buffer is MODE or derives from it."
  (unless (provided-mode-derived-p major-mode mode)
    (format "the major mode is %S" major-mode)))

(my/scenario-define-outcome :minor-modes-on (_context &rest modes)
  "Each minor mode of MODES is on in the current buffer."
  (let ((off (cl-remove-if (lambda (mode) (and (boundp mode) (symbol-value mode)))
                           modes)))
    (when off
      (format "these modes are off: %S" off))))

(my/scenario-define-outcome :minor-modes-off (_context &rest modes)
  "Each minor mode of MODES is off in the current buffer.
A mode whose package is not loaded is off; a name that is neither a
variable nor a function fails, as a likely typo."
  (let ((unknown (cl-remove-if (lambda (mode) (or (boundp mode) (fboundp mode)))
                               modes))
        (on (cl-remove-if-not (lambda (mode) (and (boundp mode) (symbol-value mode)))
                              modes)))
    (cond (unknown (format "these modes do not exist: %S" unknown))
          (on (format "these modes are on: %S" on)))))

(my/scenario-define-outcome :variable-equals (_context variable value)
  "VARIABLE has the value VALUE in the current buffer."
  (let ((actual (and (boundp variable) (symbol-value variable))))
    (unless (and (boundp variable) (equal actual value))
      (format "%S is %S" variable actual))))

(my/scenario-define-outcome :variable-contains (_context variable element)
  "The list value of VARIABLE in the current buffer contains ELEMENT."
  (let ((actual (and (boundp variable) (symbol-value variable))))
    (unless (member element actual)
      (format "%S is %S" variable actual))))

(my/scenario-define-outcome :variable-excludes (_context variable element)
  "VARIABLE is bound and its list value in the current buffer lacks ELEMENT."
  (let ((actual (and (boundp variable) (symbol-value variable))))
    (when (or (not (boundp variable)) (member element actual))
      (format "%S is %S" variable actual))))

;;;; What the keys raised, sampled or sent out

(my/scenario-define-outcome :user-error (context regexp)
  "The keys raised a `user-error' whose message matches REGEXP.
Any error the keys raise fails the scenario, unless it expects this
outcome; then every error must be such a `user-error'.  An unbound
key also raises a `user-error', so REGEXP should name the message
the command gives."
  (let* ((case-fold-search nil)
         (messages (mapcar #'error-message-string (plist-get context :errors))))
    (unless (and messages
                 (cl-every (lambda (data) (eq (car data) 'user-error))
                           (plist-get context :errors))
                 (cl-every (lambda (message) (string-match-p regexp message))
                           messages))
      (format "the errors were %S" messages))))

(my/scenario-define-outcome :sampled (context name field regexp)
  "FIELD of the snapshot taken by the step (:sample NAME) matches REGEXP.
FIELD is one of:
  :minibuffer  the active minibuffer as displayed (prompt, input and
               the candidates a completion UI shows), or nil;
  :buffer      the name of the buffer the user works in;
  :line        the line at point in that buffer;
  :echo        the message in the echo area, or nil."
  (let* ((case-fold-search nil)
         (sample (assoc name (plist-get context :samples)))
         (value (plist-get (cdr sample) field)))
    (cond ((not sample) (format "the keys never reached (:sample %S)" name))
          ((not (memq field '(:minibuffer :buffer :line :echo)))
           (format "unknown sample field %S" field))
          ((not (and value (string-match-p regexp value)))
           (format "%S was %S" field value)))))

(my/scenario-define-outcome :browsed-url (_context url)
  "The keys sent URL to the web browser.
Needs the row boundary (:system (:record-browse-url t))."
  (unless (member url my/scenario-browsed-urls)
    (format "the browser received %S" my/scenario-browsed-urls)))

(provide 'outcomes/core)

;;; outcomes/core.el ends here
