;;; outcomes/file-tree-test.el --- Self-tests of the file-tree outcomes -*- lexical-binding: t -*-

;;; Commentary:
;; Each outcome of `outcomes/file-tree.el' must hold on the state it
;; names and fail on a plausible other one.

;;; Code:

(require 'buttercup)
(require 'dired)
(require 'scenario/outcome)
(require 'outcomes)

(defun outcomes-file-tree-test--verdict (clause &optional context)
  "Return the verdict of CLAUSE with CONTEXT."
  (my/scenario-check-clause clause)
  (my/scenario-outcome-verdict clause context))

(defmacro outcomes-file-tree-test--with-listing (&rest body)
  "Run BODY in a Dired buffer listing a.txt and b.txt, with no icons."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "  /fixture:\n"
             "  -rw-r--r-- 1 user users 1 Oct  8 22:49 a.txt\n"
             "  -rw-r--r-- 1 user users 1 Oct  8 22:49 b.txt\n")
     (let ((dired-mode-hook nil))
       (dired-mode "/fixture/"))
     ,@body))

(defun outcomes-file-tree-test--name-position (name)
  "Return the position where the file NAME starts in the listing."
  (save-excursion
    (goto-char (point-min))
    (search-forward name)
    (match-beginning 0)))

(defun outcomes-file-tree-test--glyph-before (name)
  "Draw a Nerd Font glyph before the file NAME, as dirvish does."
  (let* ((start (outcomes-file-tree-test--name-position name))
         (overlay (make-overlay (1- start) start)))
    (overlay-put overlay 'after-string (string #xF07C ?\s))))

(defun outcomes-file-tree-test--image-before (name &optional invisible)
  "Show an image before the file NAME, as treemacs-icons-dired does.
With INVISIBLE, the image's text is invisible."
  (let ((start (outcomes-file-tree-test--name-position name))
        (inhibit-read-only t))
    (put-text-property (- start 2) (1- start) 'display '(image :type png :file "x.png"))
    (when invisible
      (put-text-property (- start 2) (1- start) 'invisible t))))

(describe "File-tree outcomes"

  (it ":file-icons counts glyphs and images before each name, exactly"
    (outcomes-file-tree-test--with-listing
      (expect (outcomes-file-tree-test--verdict '(:file-icons 0)) :to-be nil)
      (expect (outcomes-file-tree-test--verdict '(:file-icons 1))
              :to-equal "a.txt shows 0; b.txt shows 0")
      (outcomes-file-tree-test--glyph-before "a.txt")
      (outcomes-file-tree-test--glyph-before "b.txt")
      (expect (outcomes-file-tree-test--verdict '(:file-icons 1)) :to-be nil)
      (outcomes-file-tree-test--image-before "b.txt")
      (expect (outcomes-file-tree-test--verdict '(:file-icons 1)) :to-equal "b.txt shows 2")
      (outcomes-file-tree-test--image-before "a.txt" 'invisible)
      (expect (outcomes-file-tree-test--verdict '(:file-icons 1)) :to-equal "b.txt shows 2")))

  (it ":file-icons fails outside Dired"
    (with-temp-buffer
      (expect (outcomes-file-tree-test--verdict '(:file-icons 1)) :to-match "not Dired")))

  (it ":dirvish-session and :dirvish-layout fail without a dirvish session"
    (outcomes-file-tree-test--with-listing
      (expect (outcomes-file-tree-test--verdict '(:dirvish-session))
              :to-match "no dirvish session")
      (expect (outcomes-file-tree-test--verdict '(:dirvish-layout (1 0.11 0.55)))
              :to-match "the layout is nil")))

  (it ":line-overlay-face reads the faces of overlays on the first matching line"
    (outcomes-file-tree-test--with-listing
      (expect (outcomes-file-tree-test--verdict '(:line-overlay-face " a\\.txt$" bold))
              :to-equal "the overlays of the line give the faces nil")
      (let ((start (outcomes-file-tree-test--name-position "b.txt")))
        (overlay-put (make-overlay start (+ start 5)) 'face 'bold))
      (expect (outcomes-file-tree-test--verdict '(:line-overlay-face " a\\.txt$" bold))
              :to-equal "the overlays of the line give the faces nil")
      (expect (outcomes-file-tree-test--verdict '(:line-overlay-face " b\\.txt$" bold))
              :to-be nil)
      (expect (outcomes-file-tree-test--verdict '(:line-overlay-face " c\\.txt$" bold))
              :to-match "no line matches")))

  (it ":trashed holds only when the file moved into the XDG trash"
    (let* ((root (make-temp-file "outcomes-trash-" t))
           (default-directory (file-name-as-directory (expand-file-name "files" root)))
           (data (expand-file-name "data" root))
           (process-environment (cons (concat "XDG_DATA_HOME=" data) process-environment)))
      (unwind-protect
          (progn
            (make-directory default-directory)
            (write-region "x" nil "kept.txt")
            (expect (outcomes-file-tree-test--verdict '(:trashed "kept.txt"))
                    :to-match "still in")
            (expect (outcomes-file-tree-test--verdict '(:trashed "gone.txt"))
                    :to-match "the trash holds nil")
            (make-directory (expand-file-name "Trash/files" data) t)
            (write-region "x" nil (expand-file-name "Trash/files/gone.txt" data))
            (expect (outcomes-file-tree-test--verdict '(:trashed "gone.txt")) :to-be nil)
            (let ((process-environment (cons "XDG_DATA_HOME" process-environment)))
              (expect (outcomes-file-tree-test--verdict '(:trashed "gone.txt"))
                      :to-equal "XDG_DATA_HOME is not set")))
        (delete-directory root t))))

  (it ":window-line-at-point reads the point of the window, not of its buffer"
    (save-window-excursion
      (delete-other-windows)
      (let ((tree (get-buffer-create " outcomes-file-tree-test")))
        (unwind-protect
            (let ((window (split-window-right)))
              (with-current-buffer tree (insert "root\n  App.java\n"))
              (set-window-buffer window tree)
              (set-window-point window (with-current-buffer tree (point-max)))
              (with-current-buffer tree (goto-char (point-min)))
              (expect (outcomes-file-tree-test--verdict
                       '(:window-line-at-point "outcomes-file-tree" "\\`\\'"))
                      :to-be nil)
              (set-window-point window 7)
              (expect (outcomes-file-tree-test--verdict
                       '(:window-line-at-point "outcomes-file-tree" "App\\.java"))
                      :to-be nil)
              (expect (outcomes-file-tree-test--verdict
                       '(:window-line-at-point "outcomes-file-tree" "root"))
                      :to-match "App.java")
              (expect (outcomes-file-tree-test--verdict
                       '(:window-line-at-point "absent" "root"))
                      :to-match "no window"))
          (kill-buffer tree)))))

  (it ":tabs-count and :other-tab-shows see the tabs of the selected frame"
    (let ((buffer (get-buffer-create "outcomes-tab-left")))
      (unwind-protect
          (save-window-excursion
            (switch-to-buffer buffer)
            (tab-bar-close-other-tabs)
            (expect (outcomes-file-tree-test--verdict '(:tabs-count 1)) :to-be nil)
            (expect (outcomes-file-tree-test--verdict '(:other-tab-shows "tab-left"))
                    :to-equal "the other tabs are nil")
            (tab-bar-new-tab)
            (switch-to-buffer "*scratch*")
            (expect (outcomes-file-tree-test--verdict '(:tabs-count 2)) :to-be nil)
            (expect (outcomes-file-tree-test--verdict '(:tabs-count 1))
                    :to-equal "the frame has 2 tabs")
            (expect (outcomes-file-tree-test--verdict '(:other-tab-shows "\\`outcomes-tab-left\\'"))
                    :to-be nil)
            (expect (outcomes-file-tree-test--verdict '(:other-tab-shows "scratch"))
                    :to-match "outcomes-tab-left"))
        (tab-bar-close-other-tabs)
        (kill-buffer buffer)))))

(provide 'outcomes/file-tree-test)

;;; outcomes/file-tree-test.el ends here
