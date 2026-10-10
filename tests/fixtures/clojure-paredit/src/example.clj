(ns example
  "Plan 062 fixture: reference shapes for paredit-under-LSP scenarios.

  Each function below names the user-action it backs; the scenario
  rows in `tests/scenarios/l3-batch-clojure-paredit-lsp.el' and
  `tests/scenarios/l3-batch-clojure-lsp-navigation.el' use
  `:point-on' to land at a known substring.")

(defn add
  "Used by BL3-CLJ-PAREDIT-SLURP-LSP: starts at `(+ 1)' so a slurp
  with point inside the inner sexp picks up the trailing `2'."
  []
  (+ 1) 2)

(defn  weird-spacing
  "Used by BL3-CLJ-LSP-FORMAT-BUFFER: deliberately mis-formatted so
  `C-c l = =' has something to repair.  The form on disk carries
  every shape cljfmt rewrites under this fixture's `.cljfmt.edn'
  (`:remove-multiple-non-indenting-spaces? true', which is needed
  because cljfmt preserves inner whitespace in its default
  profile):

  - extra space between the function name and the argument vector
    (`(defn  weird-spacing');
  - inner whitespace in the argument vector (`[ x  ]');
  - non-canonical body indent (four leading spaces where two are
    canonical) and inner spaces in `( *   x    x)';
  - the trailing closing paren on its own dedented line.

  After `C-c l = =' the canonical shape is:

      (defn weird-spacing
        \"...docstring...\"
        [x]
        (* x x))

  Combining canonical-shape regexes with `:buffer-text-changed'
  in the L3 scenario makes it fail for the right reason both
  ways: green when the formatter actually rewrote the buffer, red
  when it did not (the on-disk text does not already match the
  canonical regexes)."
   [ x  ]
    ( *   x    x)
 ) 
 