;;; l1-fill-column.el --- M-q fills at the preferred line width -*- lexical-binding: t -*-

;;; Commentary:
;; The preferred line width is 120 columns (084 CF-01): M-q refills a
;; long paragraph of short words into lines of at most 120 columns,
;; and, with words three columns apart, the longest line is at least
;; 110 columns.  The width is the requirement, not a value read from
;; the configuration: Emacs's default of 70 fails.

;;; Code:

(require 'scenario)

(scenario
 :describe "CF-01: M-q fills a paragraph at 120 columns"
 :given ((:name "org" :file "notes.org"
          :text "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz-!-\n")
         (:name "text" :file "notes.txt"
          :text "aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt uu vv ww xx yy zz-!-\n"))
 :when ((:keys "M-q"))
 :then ((:longest-line-within 110 120)))

(provide 'l1-fill-column)

;;; l1-fill-column.el ends here
