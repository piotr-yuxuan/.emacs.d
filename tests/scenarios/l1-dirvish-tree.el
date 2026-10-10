;;; l1-dirvish-tree.el --- dirvish walks collapsed chains with <tab> and <return> -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 089.  The fixture file-tree lists lib/ (two files), src/, test/
;; and notes.txt.  src/ heads the chain src/main/java/com/example/App.java,
;; in which main/ also holds a hidden .gitkeep: the chain must ignore
;; it, as IntelliJ does.  These rows press keys and read the buffer
;; text, which needs no frame; l1-dirvish-tree-gui.el checks what
;; dirvish draws only on a graphical frame (icons, chains, faces).
;;
;; lib/ also holds: omit/only/{junk.fasl,real.txt}; pkg/a/b/{x,y}.txt,
;; a chain ending at a directory; loop/back, a symlink to its own
;; directory; loopy/{me -> me, solo/x -> x, notes.txt}, links that do
;; not resolve.

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-DIRVISH-CHAIN-TAB: <tab> on a collapsed chain lists its last directory"
 :given ((:name "src chain" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "<tab>"))
 :then ((:buffer-matches " App\\.java$")
        (:line-at-point " src$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-TAB-FOLD: <tab> twice on a collapsed chain folds it again"
 :given ((:name "src chain" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "<tab> <tab>"))
 :then ((:buffer-matches " src\n.* test$")
        (:line-at-point " src$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-TAB-FOLD-DIRECTORY: <tab> twice on a chain ending at a directory folds it"
 :given ((:name "pkg chain" :project "file-tree" :entry "lib" :point-on " pkg"))
 :when ((:keys "<tab> <tab>"))
 :then ((:buffer-matches " pkg\n.* one\.txt$")
        (:line-at-point " pkg$")))

(scenario
 :describe "BL1-DIRVISH-SELF-LINK-TAB: <tab> on a directory holding only a link to itself expands it"
 :given ((:name "solo" :project "file-tree" :entry "lib/loopy" :point-on " solo"))
 :when ((:keys "<tab>"))
 :then ((:buffer-matches " solo\n.* x -> x$")
        (:line-at-point " solo$")))

(scenario
 :describe "BL1-DIRVISH-SELF-LINK-RET: <return> on a directory holding only a link to itself enters it"
 :given ((:name "solo" :project "file-tree" :entry "lib/loopy" :point-on " solo"))
 :when ((:keys "<return>"))
 :then ((:buffer-name "\\`solo\\'")
        (:buffer-matches " x -> x$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-TAB-FULL-FRAME: <tab> lists a chain's last directory in the full-frame layout"
 ;; The user's path: C-x C-j from notes.txt, the last line, then p p up
 ;; to src.  When the row opens the directory before the keys, M-t
 ;; then <tab> with no redisplay between them puts the subtree at the
 ;; end of the buffer, and upstream's dirvish-subtree-toggle does the
 ;; same on a line drawing a chain: an upstream defect that a keyboard
 ;; macro also reaches.  Opening with C-x C-j inside the keys avoids it.
 :given ((:name "from notes.txt" :project "file-tree" :entry "notes.txt"))
 :when ((:keys "C-x C-j p p M-t <tab>"))
 :then ((:buffer-matches " src\n.* main\n.* java\n.* com\n.* example\n.* App\\.java$")
        (:line-at-point " src$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-TAB-DIRECTORY: <tab> on a chain ending at a directory lists that directory"
 :given ((:name "pkg chain" :project "file-tree" :entry "lib" :point-on " pkg"))
 :when ((:keys "<tab>"))
 :then ((:buffer-matches " x\\.txt$")
        (:buffer-matches " y\\.txt$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-DOTDOT: <return> on .. goes up, even from a directory's only child"
 ;; C-u C-x d asks for the listing switches; -la shows . and ..  The
 ;; listed directory lib/omit/only is the only entry of lib/omit.
 :given ((:name "only" :project "file-tree" :entry "lib/omit/only/real.txt"))
 :when ((:keys "C-u C-x d C-a C-k") (:type "-la") (:keys "<return> <return>")
        (:sample "listing")
        (:keys "M-< n") (:sample "on dotdot") (:keys "<return>"))
 :then ((:sampled "listing" :buffer "only")
        (:sampled "on dotdot" :line " \\.\\.$")
        (:buffer-matches " only$")
        (:mode dired-mode)))

(scenario
 :describe "BL1-DIRVISH-CHAIN-OFF-TAB: with collapse off in M-s, <tab> expands one level"
 ;; Upstream's one level: main/, then the next entry of the root, test/.
 :given ((:name "src" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "M-s c C-g <tab>"))
 :then ((:buffer-matches " main\n.* test$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-OFF-RET: with collapse off in M-s, <return> enters the directory"
 :given ((:name "src" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "M-s c C-g <return>"))
 :then ((:buffer-matches " main$")
        (:mode dired-mode)))

(scenario
 :describe "BL1-DIRVISH-TAB: <tab> on a directory without a chain expands it one level"
 :given ((:name "lib" :project "file-tree" :entry "." :point-on " lib"))
 :when ((:keys "<tab>"))
 :then ((:buffer-matches " one\\.txt$")
        (:buffer-matches " two\\.txt$")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-RET: <return> on a collapsed chain visits the file at its end"
 :given ((:name "src chain" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "<return>"))
 :then ((:file-suffix "file-tree/src/main/java/com/example/App.java")))

(scenario
 :describe "BL1-DIRVISH-CHAIN-RET-FULL-FRAME: <return> on a chain leaves the full-frame layout"
 :given ((:name "src chain" :project "file-tree" :entry "." :point-on " src"))
 :when ((:keys "M-t <return>"))
 :then ((:file-suffix "file-tree/src/main/java/com/example/App.java")
        (:windows-count 1)))

(provide 'l1-dirvish-tree)

;;; l1-dirvish-tree.el ends here
