;;; scenario/row.el --- Rows of :given: where a scenario starts -*- lexical-binding: t -*-

;;; Commentary:
;; Each row of a scenario's `:given' is a plist naming one start.  It
;; has a `:name' and exactly one shape:
;;
;;   (:file PATH :text TEXT)       a new file PATH, relative to a fresh
;;                                 sandbox, holding TEXT; the marker
;;                                 `-!-' in TEXT places point;
;;   (:project FIXTURE :entry PATH)  a copy of tests/fixtures/FIXTURE,
;;                                 as its own git repository, opening
;;                                 PATH inside it;
;;   (:existing-buffer REGEXP)     the buffer the selected window already
;;                                 shows, whose name must match REGEXP:
;;                                 the dashboard of a new client frame,
;;                                 or the file the runner opened at
;;                                 startup for a file declaring
;;                                 `scenario-startup: first-file'.  The
;;                                 row fails to start when another
;;                                 buffer is shown.
;;
;; Optional keys: `:point-on SUBSTRING' places point at the start of
;; the first occurrence of SUBSTRING, and `:system' stubs system
;; boundaries (see `scenario/system.el').
;;
;; Files are opened with `find-file', so the whole mode-hook chain of
;; the configuration runs.  The sandbox is a fresh git repository
;; under `temporary-file-directory', so project discovery never
;; escapes into the checkout; the runner gives each session its own
;; temporary directory and removes it with the session.

;;; Code:

(require 'cl-lib)
(require 'scenario/system)

(defconst my/scenario-point-marker "-!-"
  "The marker that places point in the `:text' of a `:file' row.")

(defconst my/scenario-fixtures-directory
  (expand-file-name "../../fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of project fixtures, `tests/fixtures/'.")

(defun my/scenario-row-check (row)
  "Signal an error unless ROW is a valid row of `:given'."
  (let* ((keys (and (proper-list-p row) (cl-evenp (length row))
                    (cl-loop for (key _) on row by #'cddr collect key)))
         (unknown (cl-set-difference keys '(:name :file :text :project :entry
                                                  :existing-buffer :point-on
                                                  :system)))
         ;; The point markers in :text, and :point-on: one at most.
         (placements (+ (1- (length (split-string
                                     (format "%s" (or (plist-get row :text) ""))
                                     (regexp-quote my/scenario-point-marker))))
                        (if (plist-get row :point-on) 1 0))))
    (cond
     ((not (stringp (plist-get row :name)))
      (error "Scenario row needs a :name string: %S" row))
     (unknown
      (error "Scenario row %S: unknown keys %S" (plist-get row :name) unknown))
     ((/= 1 (cl-count-if (lambda (key) (memq key keys))
                         '(:file :project :existing-buffer)))
      (error "Scenario row %S needs exactly one of :file, :project and \
:existing-buffer" (plist-get row :name)))
     ((and (memq :text keys) (not (memq :file keys)))
      (error "Scenario row %S: :text belongs to a :file row" (plist-get row :name)))
     ((and (memq :project keys) (not (stringp (plist-get row :entry))))
      (error "Scenario row %S: a :project row needs an :entry path"
             (plist-get row :name)))
     ((> placements 1)
      (error "Scenario row %S: use one point marker or :point-on"
             (plist-get row :name)))
     (t (my/scenario-system-check (plist-get row :system))))))

(defun my/scenario-row-root (row sandbox)
  "Return the directory ROW works in, under SANDBOX.
That is the copy of the fixture for a `:project' row, else SANDBOX."
  (file-name-as-directory
   (if (plist-get row :project)
       (expand-file-name (file-name-nondirectory
                          (directory-file-name (plist-get row :project)))
                         sandbox)
     sandbox)))

(defun my/scenario-row-open (row sandbox)
  "Create the files of ROW under SANDBOX and show its entry buffer.
Show it in the selected window, place point, and return it.  ROW is
valid, as `my/scenario-row-check' tells when the scenario expands."
  (let ((root (my/scenario-row-root row sandbox))
        (parts (split-string (or (plist-get row :text) "")
                             (regexp-quote my/scenario-point-marker))))
    (cond
     ((plist-get row :file)
      (let ((file (expand-file-name (plist-get row :file) root)))
        (make-directory (file-name-directory file) t)
        (write-region (string-join parts) nil file nil 'silent)
        (find-file file)
        (goto-char (1+ (length (car parts))))))
     ((plist-get row :project)
      (copy-directory (expand-file-name (plist-get row :project)
                                        my/scenario-fixtures-directory)
                      root nil t t)
      (unless (zerop (call-process "git" nil nil nil "init" "-q" root))
        (error "Cannot make %s a git repository" root))
      (find-file (expand-file-name (plist-get row :entry) root)))
     ((not (string-match-p (plist-get row :existing-buffer)
                           (buffer-name (window-buffer (selected-window)))))
      (error "Scenario row %S: the selected window shows %S, not %S"
             (plist-get row :name) (buffer-name (window-buffer (selected-window)))
             (plist-get row :existing-buffer)))
     (t (set-buffer (window-buffer (selected-window)))))
    (when-let* ((needle (plist-get row :point-on)))
      (goto-char (point-min))
      (unless (search-forward needle nil t)
        (error "Scenario row %S: :point-on %S not found" (plist-get row :name) needle))
      (goto-char (match-beginning 0)))
    (current-buffer)))

(defun my/scenario-row-clean-up (sandbox window)
  "Undo what a row did to the session, except its files under SANDBOX.
Select WINDOW, the window selected when the row started, if it is
still live; leave the selected window alone in its frame; and kill
the visible buffers whose directory is in SANDBOX.  Hidden buffers
belong to packages, which may still use them, so they stay, as a user
leaves them.  The files stay too: a package may still read them, and
the runner removes the session's temporary directory.  A buffer whose
kill hooks signal an error stays; once the others are killed, signal
the errors."
  (when (window-live-p window)
    (select-window window))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (let ((refusals nil))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (not (string-prefix-p " " (buffer-name buffer)))
                 (file-in-directory-p (buffer-local-value 'default-directory buffer)
                                      sandbox))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (condition-case err
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer))
          (error (push (error-message-string err) refusals)))))
    (when refusals
      (error "Killing the buffers of the row failed: %s"
             (string-join (nreverse refusals) "; ")))))

(defun my/scenario-sandbox-create ()
  "Create a fresh sandbox directory, a git repository, and return its name."
  (let ((sandbox (file-name-as-directory (make-temp-file "scenario-" t))))
    (unless (zerop (call-process "git" nil nil nil "init" "-q" sandbox))
      (error "Cannot make %s a git repository" sandbox))
    sandbox))

(provide 'scenario/row)

;;; scenario/row.el ends here
