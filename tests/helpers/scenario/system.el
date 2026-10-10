;;; scenario/system.el --- System boundaries a scenario row may stub -*- lexical-binding: t -*-

;;; Commentary:
;; A row's `:system' plist replaces a real system boundary for the
;; duration of one spec.  Each boundary is something outside Emacs
;; that a test cannot or must not drive for real:
;;
;;   (:appearance light|dark)   the OS appearance, as auto-dark reads it;
;;   (:record-browse-url t)     the desktop web browser;
;;   (:lsp-root DIRECTORY)      the user's answer to lsp-mode's "import
;;                              project root?" prompt, kept in the
;;                              private lsp session file.
;;
;; `my/scenario-system-apply' returns a function that restores the
;; boundary.  Nothing here changes configuration code.

;;; Code:

(require 'cl-lib)

(declare-function auto-dark--check-and-set-dark-mode "auto-dark")
(declare-function lsp-workspace-folders-add "lsp-mode")
(declare-function lsp-workspace-folders-remove "lsp-mode")
(defvar lsp-restart)

(defvar my/scenario-browsed-urls nil
  "URLs sent to the web browser since a row recorded browsing, newest first.")

(defun my/scenario-system-check (system)
  "Signal an error unless SYSTEM is a valid `:system' plist of a row."
  (unless (and (proper-list-p system) (cl-evenp (length system)))
    (error "Scenario row :system must be a plist: %S" system))
  (cl-loop for (key value) on system by #'cddr
           do (unless (pcase key
                        (:appearance (memq value '(light dark)))
                        (:record-browse-url (eq value t))
                        (:lsp-root (stringp value)))
                (error "Scenario row :system: invalid %S %S" key value))))

(defun my/scenario-system-apply (system root)
  "Stub the boundaries of SYSTEM; return a function that restores them.
ROOT is the directory the row works in; a relative `:lsp-root' is
expanded against it.  When a boundary cannot be stubbed, restore the
ones already stubbed and signal the error."
  (let ((restores nil))
    (condition-case err
        (cl-loop
         for (key value) on system by #'cddr
         do (pcase-exhaustive key
              (:appearance
               ;; The OS query is the boundary; auto-dark's own change
               ;; handling then switches the themes, as when the user
               ;; switches the desktop appearance.
               (let ((query (lambda () value)))
                 (advice-add 'auto-dark--current-system-mode :override query)
                 (push (lambda ()
                         (advice-remove 'auto-dark--current-system-mode query)
                         (auto-dark--check-and-set-dark-mode))
                       restores)
                 (auto-dark--check-and-set-dark-mode)))
              (:record-browse-url
               (let ((browser browse-url-browser-function))
                 (push (lambda () (setq browse-url-browser-function browser
                                        my/scenario-browsed-urls nil))
                       restores)
                 (setq my/scenario-browsed-urls nil
                       browse-url-browser-function
                       (lambda (url &rest _) (push url my/scenario-browsed-urls)))))
              (:lsp-root
               (let ((folder (expand-file-name value root)))
                 (require 'lsp-mode)
                 ;; Removing the folder also shuts its language servers
                 ;; down.  Stub: lsp-mode marks a server as shut down only
                 ;; after its shutdown request returns, so a server exiting
                 ;; during that wait makes lsp-mode ask whether to restart
                 ;; it, and the prompt would block the spec.
                 (push (lambda ()
                         (let ((lsp-restart 'ignore))
                           (lsp-workspace-folders-remove folder)))
                       restores)
                 (lsp-workspace-folders-add folder)))))
      (error (mapc #'funcall restores)
             (signal (car err) (cdr err))))
    (lambda () (mapc #'funcall restores))))

(provide 'scenario/system)

;;; scenario/system.el ends here
