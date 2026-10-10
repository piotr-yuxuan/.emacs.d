;;; org-tasks-test.el --- Org task workflow tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Logic tests for source labelling, journal placement, and the
;; read-only dashboard projection introduced by plan 079.

;;; Code:

(require 'buttercup)
(require 'json)
(require 'org)
(require 'org-capture)

(defun org-tasks-test--temporary-directory ()
  "Create a temporary test directory under `temporary-file-directory'."
  (make-temp-file "org-tasks-" t))

(defun org-tasks-test--write (path content)
  "Write CONTENT to PATH and return PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content))
  path)

(defun org-tasks-test--parse-json-file (path)
  "Parse JSON at PATH into an alist containing lists."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))

(describe "Org task sources"
  (it "derives a friendly label from the repository"
    (let* ((root (org-tasks-test--temporary-directory))
           (repository (expand-file-name "widgets/" root))
           (file (expand-file-name "todo.org" repository))
           (my/org-source-label-alist '(("widgets" . "Workshop"))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git/" repository) t)
            (org-tasks-test--write file "* TODO Order parts\n")
            (expect (my/org-source-label file) :to-equal "Workshop"))
        (delete-directory root t))))

  (it "keeps a new journal day inside the Journal subtree"
    (let* ((root (org-tasks-test--temporary-directory))
           (my/gtd-file
            (org-tasks-test--write
             (expand-file-name "todo.org" root)
             "* Journal\n\n** <2026-08-01 Sat>\n\n* After journal\n\n** Reference\n"))
           buffer)
      (unwind-protect
          (progn
            (my/gtd-today-heading)
            (setq buffer (current-buffer))
            (let* ((text (buffer-string))
                   (today (format-time-string "** <%Y-%m-%d %a>")))
              (expect (string-match-p (regexp-quote today) text) :to-be-truthy)
              (expect (string-match (regexp-quote today) text)
                      :to-be-less-than
                      (string-match (regexp-quote "* After journal") text))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory root t))))

  (it "skips dated journal headings from the task-only daily agenda"
    (with-temp-buffer
      (org-mode)
      (insert "* Journal\n** <2026-08-28 Fri>\nEntered on <2026-08-28 Fri>\n")
      (goto-char (point-min))
      (forward-line 1)
      (expect (my/org-agenda-skip-non-task) :to-be-truthy)
      (erase-buffer)
      (insert "* TODO Attend the review\nSCHEDULED: <2026-08-28 Fri>\n")
      (goto-char (point-min))
      (expect (my/org-agenda-skip-non-task) :to-be nil)))

  (it "captures a quick task in Inbox without inventing a date"
    (let* ((root (org-tasks-test--temporary-directory))
           (file (org-tasks-test--write
                  (expand-file-name "todo.org" root)
                  "* Inbox\n"))
           (template (copy-tree (assoc "t" org-capture-templates)))
           (org-capture-templates (list template))
           target-buffer)
      (unwind-protect
          (progn
            (setf (nth 3 template) `(file+headline ,file "Inbox"))
            (org-capture nil "t")
            (insert "Captured task")
            (org-capture-finalize)
            (setq target-buffer (get-file-buffer file))
            (with-temp-buffer
              (insert-file-contents file)
              (expect (buffer-string)
                      :to-equal "* Inbox\n\n** TODO Captured task\n")
              (expect (buffer-string) :not :to-match "SCHEDULED:")))
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer (set-buffer-modified-p nil))
          (kill-buffer target-buffer))
        (delete-directory root t)))))

(describe "Org dashboard projection"
  (it "exports only active task headings with context and JSON nulls"
    (let* ((root (org-tasks-test--temporary-directory))
           (repository (expand-file-name "widgets/" root))
           (file (expand-file-name "todo.org" repository))
           (data-file (expand-file-name "tmp/tasks.json" root))
           (org-agenda-files (list file))
           (my/org-dashboard-data-file data-file)
           (my/org-source-label-alist '(("widgets" . "Workshop")))
           buffer)
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git/" repository) t)
            (org-tasks-test--write
             file
             (concat
              "* Release\n"
              "** NEXT Prepare the notes\n"
              "SCHEDULED: <2026-08-29 Sat>\n"
              "** TODO Complete the checklist\n"
              "*** WAIT Receive the approval\n"
              "Waiting since <2026-08-28 Fri>\n"
              "** DONE Superseded\n"
              "* Journal\n"
              "** <2026-08-28 Fri>\n"
              "Entered on <2026-08-28 Fri>\n"
              "* Archive :ARCHIVE:\n"
              "** TODO Hidden\n"))
            (my/org-dashboard-export)
            (setq buffer (get-file-buffer file))
            (let* ((json-text
                    (with-temp-buffer
                      (insert-file-contents data-file)
                      (buffer-string)))
                   (payload
                    (json-parse-string json-text
                                       :object-type 'alist
                                       :array-type 'list
                                       :null-object :null))
                   (tasks (alist-get 'tasks payload)))
              (expect (length tasks) :to-equal 3)
              (expect (mapcar (lambda (task) (alist-get 'state task)) tasks)
                      :to-equal '("NEXT" "TODO" "WAIT"))
              (expect (alist-get 'source (car tasks)) :to-equal "Workshop")
              (expect (alist-get 'breadcrumbs (car tasks)) :to-equal '("Release"))
              (expect (alist-get 'scheduled (car tasks))
                      :to-equal "<2026-08-29 Sat>")
              (expect (alist-get 'deadline (car tasks)) :to-equal :null)
              (expect (alist-get 'timestamp (cadr tasks)) :to-equal :null)
              (expect (alist-get 'timestamp (caddr tasks))
                      :to-equal "<2026-08-28 Fri>")
              (dolist (task tasks)
                (expect (alist-get 'id task)
                        :to-match "\\`[0-9a-f]\\{16\\}-[1-9][0-9]*\\'"))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory root t))))

  (it "changes generation-scoped identifiers on every export"
    (let* ((root (org-tasks-test--temporary-directory))
           (repository (expand-file-name "widgets/" root))
           (file (expand-file-name "todo.org" repository))
           (data-file (expand-file-name "tmp/tasks.json" root))
           (org-agenda-files (list file))
           (my/org-dashboard-data-file data-file)
           first-id
           buffer)
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git/" repository) t)
            (org-tasks-test--write file "* NEXT Order parts\n")
            (my/org-dashboard-export)
            (setq first-id
                  (alist-get
                   'id
                   (car
                    (alist-get
                     'tasks
                     (org-tasks-test--parse-json-file data-file)))))
            (my/org-dashboard-export)
            (expect
             (alist-get
              'id
              (car
               (alist-get
                'tasks
                (org-tasks-test--parse-json-file data-file))))
             :not :to-equal first-id)
            (setq buffer (get-file-buffer file)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory root t)))))

(provide 'org-tasks-test)

;;; org-tasks-test.el ends here
