;;; outcomes/sockets.el --- Outcomes about local sockets -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 098.  A client of a local server, such as the MCP bridge
;; mcp-wrapper.py, needs its Unix socket to accept a connection.  See
;; `scenario/outcome.el' for the contract.

;;; Code:

(require 'scenario/outcome)

(my/scenario-define-outcome :socket-accepts (_context directory-variable name)
  "The Unix socket NAME accepts a connection, as a client sees it.
The socket is in the directory that DIRECTORY-VARIABLE holds."
  (let* ((directory (and (boundp directory-variable) (symbol-value directory-variable)))
         (path (and (stringp directory) (expand-file-name name directory))))
    (cond ((not path) (format "%S holds no directory" directory-variable))
          ((not (file-exists-p path)) (format "%s does not exist" path))
          (t (condition-case err
                 (ignore (delete-process
                          (make-network-process :name "scenario-socket-accepts"
                                                :family 'local :service path
                                                :noquery t)))
               (error (format "connecting to %s fails: %s"
                              path (error-message-string err))))))))

(provide 'outcomes/sockets)

;;; outcomes/sockets.el ends here
