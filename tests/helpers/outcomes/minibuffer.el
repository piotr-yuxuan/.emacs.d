;;; outcomes/minibuffer.el --- What the minibuffer shows, during the keys -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 105.  An outcome a scenario may wait for in the middle of its
;; keys, with (:idle-until (:minibuffer-matches REGEXP) MS): for
;; example, until the matches of an asynchronous search appear.

;;; Code:

(require 'scenario/outcome)
(require 'scenario/display)

(my/scenario-define-outcome :minibuffer-matches (_context regexp)
  "The active minibuffer shows text that matches REGEXP.
The text is as displayed, with the overlay strings in which completion
interfaces list their candidates; see `my/scenario-displayed-text'."
  (let ((window (active-minibuffer-window))
        (case-fold-search nil))
    (if (not window)
        "no minibuffer is active"
      (let ((shown (with-current-buffer (window-buffer window)
                     (my/scenario-displayed-text (point-min) (point-max)))))
        (unless (string-match-p regexp shown)
          (format "the minibuffer shows %S" shown))))))

(put :minibuffer-matches 'my/scenario-during-keys t)

(provide 'outcomes/minibuffer)

;;; outcomes/minibuffer.el ends here
