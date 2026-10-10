;;; docs-test.el --- The documents' links lead somewhere -*- lexical-binding: t -*-

;;; Commentary:
;; Plan 095.  Logic tests of the documents, not of the configuration.
;;
;; Every internal link of the Org and Markdown files that git tracks
;; outside tests/ (whose files are test data) must lead to a file that
;; exists and, when the link names a heading, a custom ID, a target or
;; a text, to a place in that file, as following it with C-c C-o
;; would.  A link to an absolute path or to ~/ leaves the checkout and
;; is not checked.  The guides under docs/, outside the work records
;; of docs/work/, and AGENTS.md must hold no em dash or en dash: the
;; dash ban of AGENTS.md.
;;
;; tests/mutate runs the suite on a copy that holds only init.el,
;; early-init.el and tests/.  With no README.org there, the copy has
;; no document, and the specs on the checkout's documents check
;; nothing.
;;
;; Plan 099.  The public export copies the files that the manifest,
;; publish/manifest.toml, selects, and rewrites none.  So a published
;; document must link only to published files: a link to a file the
;; export leaves out would lead nowhere in the public repository.
;; Only the private repository holds a manifest.  Elsewhere the specs
;; on the manifest check nothing, and a link to a missing file already
;; fails as one that leads nowhere.

;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-element)

(defconst docs-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
  "The checkout under test, which holds tests/.")

(defun docs-test--tracked (&rest pathspecs)
  "Return the files of the checkout under test that git tracks and PATHSPECS match.
The names are relative to `docs-test--root'.  Return nil in a copy
without README.org, which holds no document."
  (and (file-exists-p (expand-file-name "README.org" docs-test--root))
       (let ((default-directory docs-test--root))
         (apply #'process-lines "git" "ls-files" "--" pathspecs))))

(defun docs-test--found-p (file search)
  "Return non-nil when FILE exists and holds a place for SEARCH.
SEARCH is a link's search option, such as \"*Heading\", \"#id\" or a
line number, or nil.  `org-link-search' looks for it as following the
link would, but never offers to create a missing heading."
  (and (file-exists-p file)
       (or (null search)
           (string-match-p "\\`[0-9]+\\'" search)
           (with-temp-buffer
             (insert-file-contents file)
             (when (string-suffix-p ".org" file)
               (delay-mode-hooks (org-mode)))
             (let ((org-link-search-must-match-exact-headline t)
                   (org-execute-file-search-functions nil))
               (condition-case nil
                   (progn (org-link-search search nil t) t)
                 (error nil)))))))

(defun docs-test--links (root relative)
  "Return the internal links of the document RELATIVE, as (LINK FILE SEARCH).
RELATIVE is an Org or Markdown file, named relative to ROOT.  LINK is
the link as written, FILE the file it leads to and SEARCH the place
in that file, or nil.  In Org, a file link with a relative path, and
a link to a heading, a custom ID or a target of the same file, are
internal; in Markdown, a link to a relative path, whose #anchor is
ignored."
  (let* ((file (expand-file-name relative root))
         (directory (file-name-directory file))
         links)
    (with-temp-buffer
      (insert-file-contents file)
      (if (string-suffix-p ".md" relative)
          (while (re-search-forward "](\\([^]():#/~ \n][^():# \n]*\\)[)# ]" nil t)
            (setq links (append links (list (list (match-string 1)
                                                  (expand-file-name (match-string 1) directory)
                                                  nil)))))
        (delay-mode-hooks (org-mode))
        (setq links
              (org-element-map (org-element-parse-buffer) 'link
                (lambda (link)
                  (let ((type (org-element-property :type link))
                        (path (org-element-property :path link))
                        (raw (org-element-property :raw-link link)))
                    (cond ((member type '("fuzzy" "custom-id")) (list raw file raw))
                          ((and (equal type "file") (not (file-name-absolute-p path)))
                           (list raw (expand-file-name path directory)
                                 (org-element-property :search-option link))))))))))
    links))

(defun docs-test--broken-links (root files)
  "Return the internal links of FILES that lead nowhere, as \"FILE: LINK\".
FILES are Org or Markdown files, named relative to ROOT."
  (let (broken)
    (dolist (relative files (nreverse broken))
      (dolist (link (docs-test--links root relative))
        (unless (docs-test--found-p (nth 1 link) (nth 2 link))
          (push (format "%s: %s" relative (car link)) broken))))))

(defconst docs-test--manifest "publish/manifest.toml"
  "The export manifest of a checkout, named relative to it.")

(defconst docs-test--published-script "\
import glob, re, subprocess, sys, tomllib
with open(sys.argv[1], 'rb') as file:
    manifest = tomllib.load(file)
for key in ('include', 'exclude'):
    if not (isinstance(manifest[key], list)
            and all(isinstance(pattern, str) for pattern in manifest[key])):
        sys.exit(key + ' is not a list of strings')
def matches(key, path):
    return any(re.match(glob.translate(pattern, recursive=True, include_hidden=True), path)
               for pattern in manifest[key])
tracked = subprocess.run(['git', 'ls-files', '-z'], capture_output=True, check=True, text=True)
for path in tracked.stdout.split('\\0'):
    if (path and not path.startswith('publish/')
            and matches('include', path) and not matches('exclude', path)):
        print(path)
"
  "Python that prints the tracked files a manifest publishes.
Its argument names the manifest.  The publishing tool defines a pattern
of the manifest as one of Python's `glob.translate', which Python 3.13
brought.")

(defun docs-test--published (root)
  "Return the tracked files under ROOT that its export manifest publishes.
The names are relative to ROOT.  A file is published when a pattern of
the manifest's `include' matches it and none of its `exclude' does;
the files under publish/ never are.  Return nil without a manifest."
  (let ((default-directory root))
    (and (file-exists-p docs-test--manifest)
         (process-lines "python3" "-c" docs-test--published-script docs-test--manifest))))

(defun docs-test--unpublished-links (root files published)
  "Return the internal links of FILES that leave PUBLISHED, as \"FILE: LINK\".
FILES are Org or Markdown files and PUBLISHED the published files, all
named relative to ROOT.  A link stays in PUBLISHED when it leads to
one of these files, or to a directory that holds one."
  (let (unpublished)
    (dolist (relative files (nreverse unpublished))
      (dolist (link (docs-test--links root relative))
        (let ((target (file-relative-name (nth 1 link) root)))
          (unless (seq-some (lambda (path)
                              (or (equal path target)
                                  (string-prefix-p (file-name-as-directory target) path)))
                            published)
            (push (format "%s: %s" relative (car link)) unpublished)))))))

(describe "The documents of the checkout"

  (it "report the links that lead nowhere, and only those"
    (let ((root (file-name-as-directory (make-temp-file "docs-test-" t))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "sub" root))
            (with-temp-file (expand-file-name "b.org" root)
              (insert "* Kept heading\n:PROPERTIES:\n:CUSTOM_ID: kept\n:END:\n"))
            (with-temp-file (expand-file-name "a.org" root)
              (insert "* Present\n:PROPERTIES:\n:CUSTOM_ID: here\n:END:\n"
                      "A <<target>>.\n\n"
                      "[[file:b.org::*Kept heading]] [[file:b.org::*Lost heading]]\n"
                      "[[file:b.org::#kept]] [[file:b.org::#lost]] [[./b.org]]\n"
                      "[[file:sub/c.md]] [[file:gone.org]] [[file:b.org::2]]\n"
                      "[[*Present]] [[*Absent]] [[#here]] [[#elsewhere]] [[target]]\n"
                      "[[file:~/outside.org]] [[file:/outside.org]]\n"
                      "[[https://example.invalid/page.org][the web]]\n"))
            (with-temp-file (expand-file-name "sub/c.md" root)
              (insert "[up](../b.org) [anchor](../b.org#kept) [gone](gone.md)\n"
                      "[web](https://example.invalid/x.md) [abs](/outside.md)"
                      " [home](~/outside.md) [here](#top)\n"))
            (expect (docs-test--broken-links root '("a.org" "sub/c.md"))
                    :to-equal '("a.org: file:b.org::*Lost heading"
                                "a.org: file:b.org::#lost"
                                "a.org: file:gone.org"
                                "a.org: *Absent"
                                "a.org: #elsewhere"
                                "sub/c.md: gone.md")))
        (delete-directory root t))))

  (it "have internal links that all lead somewhere"
    (expect (docs-test--broken-links
             docs-test--root (docs-test--tracked "*.org" "*.md" ":!tests/"))
            :to-equal nil))

  (it "use no em dash or en dash in the guides or AGENTS.md"
    (let (dashed)
      (dolist (relative (docs-test--tracked "AGENTS.md" "docs/" ":!docs/work/"))
        (with-temp-buffer
          (insert-file-contents (expand-file-name relative docs-test--root))
          (while (re-search-forward "[\u2013\u2014]" nil t)
            (push (format "%s:%d" relative (line-number-at-pos)) dashed))))
      (expect (nreverse dashed) :to-equal nil))))

(describe "The published documents"

  (it "are the tracked files a manifest includes and does not exclude"
    ;; Only a checkout with a manifest needs to read one, with Python 3.13.
    (when (file-exists-p (expand-file-name docs-test--manifest docs-test--root))
      (let ((root (file-name-as-directory (make-temp-file "docs-test-" t))))
        (unwind-protect
            (progn
              (let ((default-directory root))
                (dolist (file (list "README.org" "init.el" ".dir-locals.el" "lisp/deep.el"
                                    "docs/a.org" "docs/sub/b.org" "docs/work/plan.org"
                                    "docs/sub/draft.org" "conf/app.toml"
                                    "conf/docs/work/app.toml"
                                    "publish/overlay/LICENSE" docs-test--manifest))
                  (make-directory (file-name-directory (expand-file-name file)) t)
                  (write-region "" nil file nil 'silent))
                (write-region
                 (concat "sweep = false\n"
                         "include = [\"README.org\", \"*.el\", \"docs/**\","
                         " \"**/*.toml\", \"**/LICENSE\"]\n"
                         "exclude = [\"docs/work/**\", \"**/draft.org\"]\n")
                 nil docs-test--manifest nil 'silent)
                ;; --force: a file name of the fixture may be ignored by the
                ;; user's own git configuration.
                (dolist (arguments '(("init" "--quiet") ("add" "--force" "--all")))
                  (unless (zerop (apply #'call-process "git" nil nil nil arguments))
                    (error "The command git %s failed" arguments))))
              ;; * stays in one directory and matches a dot file, ** crosses
              ;; directories, a pattern matches from the root, exclude wins,
              ;; and publish/ is never listed.
              (expect (docs-test--published root)
                      :to-equal '(".dir-locals.el" "README.org" "conf/app.toml"
                                  "conf/docs/work/app.toml" "docs/a.org"
                                  "docs/sub/b.org" "init.el")))
          (delete-directory root t)))))

  (it "have the links that leave them reported, and only those"
    (let ((root (file-name-as-directory (make-temp-file "docs-test-" t))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "notes" root))
            (with-temp-file (expand-file-name "a.org" root)
              (insert "* Present\n"
                      "[[file:b.org]] [[file:b.org::*Heading]] [[file:notes/]] [[*Present]]\n"
                      "[[file:private/c.org]] [[file:private/]] [[file:gone.org]] [[file:note]]\n"
                      "[[file:~/outside.org]] [[https://example.invalid/page.org][the web]]\n"))
            (with-temp-file (expand-file-name "notes/d.md" root)
              (insert "[up](../b.org) [private](../private/c.org)"
                      " [web](https://example.invalid/x.md)\n"))
            (expect (docs-test--unpublished-links
                     root '("a.org" "notes/d.md") '("a.org" "b.org" "notes/d.md"))
                    :to-equal '("a.org: file:private/c.org"
                                "a.org: file:private/"
                                "a.org: file:gone.org"
                                "a.org: file:note"
                                "notes/d.md: ../private/c.org")))
        (delete-directory root t))))

  (it "link only to published files"
    (let ((published (docs-test--published docs-test--root)))
      ;; With a manifest, the literate source is published: a reader that
      ;; found nothing would otherwise leave nothing to check.
      (when (file-exists-p (expand-file-name docs-test--manifest docs-test--root))
        (expect published :to-contain "README.org"))
      (expect (docs-test--unpublished-links
               docs-test--root
               (seq-intersection (docs-test--tracked "*.org" "*.md" ":!tests/") published)
               published)
              :to-equal nil))))

(provide 'docs-test)

;;; docs-test.el ends here
