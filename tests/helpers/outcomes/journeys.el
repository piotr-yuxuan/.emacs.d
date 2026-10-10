;;; outcomes/journeys.el --- Outcomes of the plan 094 journeys -*- lexical-binding: t -*-

;;; Commentary:
;; Outcomes the daily journeys need and the other outcome files lack.
;; See `scenario/outcome.el' for the contract.

;;; Code:

(require 'seq)
(require 'scenario/outcome)

(my/scenario-define-outcome :saved-file-matches (context path regexp)
  "The file PATH holds text matching REGEXP on disk.
PATH is relative to the root of the row: the git repository around
the entry buffer, that is the copy of a `:project' fixture or the
sandbox of a `:file' row."
  (let* ((case-fold-search nil)
         (entry (plist-get context :entry-buffer))
         (root (and (buffer-live-p entry)
                    (locate-dominating-file
                     (buffer-local-value 'default-directory entry) ".git")))
         (file (and root (expand-file-name path root)))
         (text (and file (file-readable-p file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))
    (cond ((not root) "the entry buffer is gone or outside a repository")
          ((not text) (format "%s is not readable" file))
          ((not (string-match-p regexp text))
           (format "%s holds %S" path text)))))

(my/scenario-define-outcome :buffer-named-matches (_context name-regexp regexp)
  "A live buffer whose name matches NAME-REGEXP holds text matching REGEXP.
For a buffer the user sees beside the current one, such as Magit's
status buffer next to a commit message, or the Org file a capture
went into.  The first buffer of `buffer-list' whose name matches is
checked."
  (let* ((case-fold-search nil)
         (buffer (seq-find (lambda (buffer)
                             (string-match-p name-regexp (buffer-name buffer)))
                           (buffer-list)))
         (text (and buffer (with-current-buffer buffer
                             (buffer-substring-no-properties (point-min) (point-max))))))
    (cond ((not buffer) (format "no buffer name matches %S" name-regexp))
          ((not (string-match-p regexp text))
           (format "the buffer %S holds %S" (buffer-name buffer) text)))))

(my/scenario-define-outcome :completion-popup-shows (_context regexp)
  "Corfu's completion popup is visible and lists a candidate matching REGEXP.
The popup is a visible child frame of the selected frame showing the
buffer \" *corfu*\".  Only a graphical frame has child frames, so a
file using this outcome declares `scenario-display: graphic'."
  (let* ((case-fold-search nil)
         (popup (seq-find (lambda (frame)
                            (and (eq (frame-parent frame) (selected-frame))
                                 (frame-visible-p frame)
                                 (equal (buffer-name (window-buffer (frame-root-window frame)))
                                        " *corfu*")))
                          (frame-list)))
         (text (and popup
                    (with-current-buffer (window-buffer (frame-root-window popup))
                      (buffer-substring-no-properties (point-min) (point-max))))))
    (cond ((not popup) "no completion popup is visible")
          ((not (string-match-p regexp text))
           (format "the popup lists %S" text)))))

(provide 'outcomes/journeys)

;;; outcomes/journeys.el ends here
