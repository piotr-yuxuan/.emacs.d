;;; l1-mcp-server-idle-start.el --- The MCP server starts once Emacs is idle -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 098.  The MCP server no longer starts during startup: its tools
;; load Org.  It starts once Emacs has been idle for two seconds, and
;; the bridge (mcp-wrapper.py) needs its socket to accept a connection.
;; The runner starts the specs once the idle timers pending after
;; startup have run, so this file shows that the server still starts,
;; not when: T1's "left to idle time" spec shows that it left startup.
;; In a test session the socket is in the session's sock/ directory.

;;; Code:

(require 'scenario)

(scenario
 :describe "The MCP socket accepts a connection once the session has been idle"
 :given ((:name "from a file" :file "notes.txt" :text "Some notes.\n"))
 :when ((:idle 1))
 :then ((:socket-accepts mcp-server-socket-directory "emacs-mcp-server.sock")))

(provide 'l1-mcp-server-idle-start)

;;; l1-mcp-server-idle-start.el ends here
