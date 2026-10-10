;;; l1-fontlock-treesit.el --- Tree-sitter fontification across modes -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 072 / FL-1 to FL-5 (carry-forward index).  Asserts that
;; opening a file in one of the live config's tree-sitter remapped
;; modes produces tree-sitter-driven font-lock faces at well-known
;; positions.
;;
;; The live config's `my/treesit-remap-when-ready' hooks remap
;; `python-mode' -> `python-ts-mode' and `js-json-mode' ->
;; `json-ts-mode' when the corresponding grammar is installed.
;; `my/treesit-auto-mode-when-ready' adds a `\\.sh\\'' entry to
;; `auto-mode-alist' for `bash-ts-mode' so freshly opened shell files
;; (without a shebang line that would route through
;; `interpreter-mode-alist') open in the tree-sitter mode.  Each
;; scenario asserts the major mode first; on a runner missing the
;; relevant grammar the remap silently no-ops and the file opens in
;; the legacy mode, which `:mode' catches.
;;
;; The bash fixture deliberately omits a shebang because
;; `set-auto-mode' consults `interpreter-mode-alist' before
;; `auto-mode-alist' when a shebang is present.
;;
;; FL-2 uses an ordinary string literal: the first string of a
;; function body is a docstring, which python-ts-mode shows with
;; `font-lock-doc-face' (TS-22).

;;; Code:

(require 'scenario)

(scenario
 :describe "BL1-FONTLOCK-PY-KEYWORD: python-ts-mode highlights `def' as a keyword (FL-1)"
 :given ((:name "py-keyword"
          :file "src/sample.py"
          :text "# top comment\n-!-def hello():\n    \"a docstring\"\n    return 42\n"))
 :when ((:open))
 :then ((:mode python-ts-mode)
        (:face-computed font-lock-keyword-face)))

(scenario
 :describe "BL1-FONTLOCK-PY-STRING: python-ts-mode highlights a string literal (FL-2)"
 :given ((:name "py-string"
          :file "src/sample.py"
          :text "# top comment\ndef hello():\n    greeting = \"a -!-string\"\n    return greeting\n"))
 :when ((:open))
 :then ((:mode python-ts-mode)
        (:face-computed font-lock-string-face)))

(scenario
 :describe "BL1-FONTLOCK-PY-COMMENT: python-ts-mode highlights a comment (FL-3)"
 :given ((:name "py-comment"
          :file "src/sample.py"
          :text "# -!-top comment\ndef hello():\n    return 42\n"))
 :when ((:open))
 :then ((:mode python-ts-mode)
        (:face-computed font-lock-comment-face)))

(scenario
 :describe "BL1-FONTLOCK-JSON-NUMBER: json-ts-mode highlights a number literal (FL-4)"
 :given ((:name "json-number"
          :file "src/sample.json"
          :text "{\n  \"name\": \"hello\",\n  \"port\": -!-8080\n}\n"))
 :when ((:open))
 :then ((:mode json-ts-mode)
        (:face-computed font-lock-number-face)))

(scenario
 :describe "BL1-FONTLOCK-BASH-KEYWORD: bash-ts-mode highlights `if' as a keyword (FL-5)"
 :given ((:name "bash-keyword"
          :file "src/sample.sh"
          :text "-!-if true; then\n  echo \"hi\"\nfi\n"))
 :when ((:open))
 :then ((:mode bash-ts-mode)
        (:face-computed font-lock-keyword-face)))

(provide 'l1-fontlock-treesit)

;;; l1-fontlock-treesit.el ends here
