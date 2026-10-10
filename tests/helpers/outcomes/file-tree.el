;;; outcomes/file-tree.el --- Outcomes about file managers, trees and tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089: what the user sees of dirvish, Treemacs, the trash and the
;; tab bar.  See `scenario/outcome.el' for the contract.  Regular
;; expressions are matched case-sensitively.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require 'scenario/outcome)
(require 'scenario/display)

(declare-function dired-move-to-filename "dired")
(declare-function dirvish-curr "dirvish")
(declare-function dv-curr-layout "dirvish")

(defconst my/scenario-icon-glyph
  (rx (any (#xE000 . #xF8FF) (#xF0000 . #x10FFFD)))
  "A character of a Unicode private use area, where icon fonts draw icons.")

(my/scenario-define-outcome :file-icons (_context count)
  "Each file line of the current Dired buffer shows COUNT icons before its name.
An icon is an image, from a `display' property of the visible text
of the line or of an overlay string, or a character of a Unicode
private use area, where icon fonts such as Nerd Fonts draw their
glyphs.  dirvish and treemacs-icons-dired both draw their icons
before the file name, so a line showing both has two."
  (let ((case-fold-search nil)
        (images (lambda (object beg end)
                  ;; The images shown by the `display' properties of the
                  ;; text of OBJECT from BEG to END.
                  (cl-loop for pos = beg
                           then (next-single-property-change pos 'display object end)
                           while (< pos end)
                           count (and (eq (car-safe (get-text-property pos 'display object))
                                          'image)
                                      (not (and (bufferp object) (invisible-p pos)))))))
        (wrong nil))
    (if (not (derived-mode-p 'dired-mode))
        (format "the current buffer is in %S, not Dired" major-mode)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((name (dired-move-to-filename)))
            (let* ((beg (line-beginning-position))
                   (strings (seq-filter #'stringp
                                        (mapcan (lambda (overlay)
                                                  (list (overlay-get overlay 'before-string)
                                                        (overlay-get overlay 'after-string)))
                                                (overlays-in beg name))))
                   (found (+ (cl-count-if
                              (lambda (char)
                                (string-match-p my/scenario-icon-glyph (string char)))
                              (my/scenario-displayed-text beg name))
                             (funcall images (current-buffer) beg name)
                             (apply #'+ (mapcar (lambda (string)
                                                  (funcall images string 0 (length string)))
                                                strings)))))
              (unless (= found count)
                (push (format "%s shows %d" (buffer-substring-no-properties
                                             name (line-end-position))
                              found)
                      wrong))))
          (forward-line 1)))
      (when wrong
        (string-join (nreverse wrong) "; ")))))

(my/scenario-define-outcome :dirvish-session (_context)
  "dirvish manages the current buffer: `dirvish-curr' finds its session.
A plain Dired buffer, without dirvish's overlay, fails."
  (unless (and (fboundp 'dirvish-curr) (dirvish-curr))
    (format "no dirvish session manages %S, in %S" (buffer-name) major-mode)))

(my/scenario-define-outcome :dirvish-layout (_context recipe)
  "The dirvish session of the current buffer shows the full-frame layout RECIPE.
RECIPE is (DEPTH MAX-PARENT-WIDTH PREVIEW-WIDTH), as an element of
`dirvish-layout-recipes': the session's layout is RECIPE, and the
selected frame shows DEPTH windows of parent directories."
  (let* ((session (and (fboundp 'dirvish-curr) (dirvish-curr)))
         (layout (and session (dv-curr-layout session)))
         (parents (cl-count-if (lambda (window)
                                 (with-current-buffer (window-buffer window)
                                   (derived-mode-p 'dirvish-directory-view-mode)))
                               (window-list nil 'no-minibuffer))))
    (unless (and (equal layout recipe) (= parents (car recipe)))
      (format "the layout is %S, with %d parent windows" layout parents))))

(my/scenario-define-outcome :line-overlay-face (_context line-regexp face)
  "An overlay gives FACE to the first line whose text matches LINE-REGEXP.
dirvish draws some attributes, such as the face of an empty directory
of `collapse', as an overlay on the file name rather than as text."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (if (not (re-search-forward line-regexp nil t))
          (format "no line matches %S" line-regexp)
        (let ((faces (mapcar (lambda (overlay) (overlay-get overlay 'face))
                             (overlays-in (line-beginning-position) (line-end-position)))))
          (unless (memq face faces)
            (format "the overlays of the line give the faces %S" faces)))))))

(my/scenario-define-outcome :trashed (_context name)
  "The file NAME of `default-directory' is gone, and the trash holds it.
The trash is the freedesktop one under XDG_DATA_HOME, which Emacs
uses when `trash-directory' is nil.  A scenario file using this
outcome sets XDG_DATA_HOME to a directory of its own; when it is
unset, this outcome fails, so a test never looks at the user's
trash."
  (let* ((data (getenv "XDG_DATA_HOME"))
         (trash (and data (expand-file-name "Trash/files/" data))))
    (cond ((not data) "XDG_DATA_HOME is not set")
          ((file-exists-p (expand-file-name name))
           (format "%s is still in %s" name default-directory))
          ((not (file-exists-p (expand-file-name name trash)))
           (format "the trash holds %S"
                   (and (file-directory-p trash)
                        (directory-files trash nil directory-files-no-dot-files-regexp)))))))

(my/scenario-define-outcome :window-line-at-point (_context buffer-regexp line-regexp)
  "A window showing a buffer named BUFFER-REGEXP has point on a LINE-REGEXP line.
The window's own point counts, where the user sees its cursor or
highlighted line, not the point of its buffer.  The window is the
first of the selected frame whose buffer name matches BUFFER-REGEXP."
  (let* ((case-fold-search nil)
         (window (seq-find (lambda (window)
                             (string-match-p buffer-regexp
                                             (buffer-name (window-buffer window))))
                           (window-list nil 'no-minibuffer)))
         (line (and window
                    (with-current-buffer (window-buffer window)
                      (save-excursion
                        (goto-char (window-point window))
                        (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position)))))))
    (cond ((not window)
           (format "no window shows a buffer matching %S" buffer-regexp))
          ((not (string-match-p line-regexp line))
           (format "point is on the line %S" line)))))

(my/scenario-define-outcome :tabs-count (_context count)
  "The selected frame has COUNT tabs."
  (let ((actual (length (funcall tab-bar-tabs-function))))
    (unless (= actual count)
      (format "the frame has %d tabs" actual))))

(my/scenario-define-outcome :other-tab-shows (_context regexp)
  "A tab other than the current one has a name matching REGEXP.
`tab-bar-tab-name-current', the default, names a tab after the
buffer its selected window showed when the user left the tab."
  (let* ((case-fold-search nil)
         (names (mapcar (lambda (tab) (alist-get 'name tab))
                        (seq-remove (lambda (tab) (eq (car tab) 'current-tab))
                                    (funcall tab-bar-tabs-function)))))
    (unless (seq-some (lambda (name) (string-match-p regexp name)) names)
      (format "the other tabs are %S" names))))

(provide 'outcomes/file-tree)

;;; outcomes/file-tree.el ends here
