"""The mutants of tests/mutate: plausible regressions of init.el.

Each mutant is a list of (OLD, NEW) edits of init.el; every OLD text
must occur exactly once in the init.el it applies to, after the edits
before it.  The K and U mutants are the 31 of the 085 mutation baseline
(docs/work/findings/085-mutation-baseline.org): K for areas the suite
claimed to cover then, U for daily workflows it did not.  The J
mutants are the mutations of the 14 daily journeys of plan 094, one
per journey, each meant to make that journey's spec fail.
"""

from __future__ import annotations

import dataclasses

END = "(provide 'init)"  # the last form of init.el, to append forms before it
WIDTH = "(defconst my/line-width 120"
WS_OFF = (("(prog-mode . ws-butler-mode)", "(prog-mode . ignore)"),
          ("(text-mode . ws-butler-mode)", "(text-mode . ignore)"))
GLOBAL_DTW = (*WS_OFF, (WIDTH, "(add-hook 'before-save-hook"
                               " #'delete-trailing-whitespace)\n" + WIDTH))
PAREDIT_CONFIG = "  (put 'paredit-forward-delete 'delete-selection 'supersede)"
PAREDIT_HOOK = "  :hook ((emacs-lisp-mode) . paredit-mode)\n"
C_S = (('("C-s"     . consult-line)', '("C-s"     . isearch-forward)'),)
CORFU_AUTO = (("(corfu-auto t)", "(corfu-auto nil)"),)
RECENTF = (("      (recentf-mode 1)))", "      (ignore 1)))"),)
CAPTURE_TARGET = (('(file+headline ,my/gtd-file "Inbox")',
                   '(file+headline ,my/gtd-file "X")'),)
DIFF_HL_PROG = (("(prog-mode  . diff-hl-mode)", "(prog-mode  . ignore)"),)
NO_RG = ("(setq exec-path (seq-remove (lambda (directory) (file-executable-p"
         ' (expand-file-name "rg" directory))) exec-path))')


@dataclasses.dataclass(frozen=True)
class Mutant:
    """A plausible regression of init.el, and the tests meant to catch it."""

    ident: str
    area: str
    edits: tuple[tuple[str, str], ...]
    covered: bool = True  # the suite claims to cover the area: it must kill the mutant
    # The profile and FILTER of the tests/run meant to kill it: the whole
    # fast profile, or the L3 file that tests the area.
    scope: tuple[str, str] = ("fast", "")
    expected: str | None = None  # how the name of the spec that must fail starts


def journey(number: int, slug: str, area: str,
            edits: tuple[tuple[str, str], ...]) -> Mutant:
    """Return the mutant of journey NUMBER: its file alone runs, its spec must fail."""
    return Mutant(f"J{number:02d}-{slug}", area, edits,
                  scope=("full", f"journey-{number:02d}-"), expected=f"J-{number:02d}:")


def appended(form: str) -> tuple[tuple[str, str], ...]:
    """Return the edit that runs FORM at the end of init.el."""
    return ((END, f"{form}\n{END}"),)


MUTANTS = (
    # K01 was equivalent in the baseline: it removed a global binding
    # that paredit's own map shadows.  093 removed that binding; the
    # mutant now rebinds the key in paredit's map.
    Mutant("K01-slurp-key", "paredit C-<right>",
           ((PAREDIT_CONFIG,
             "  (keymap-set paredit-mode-map \"C-<right>\" #'right-word)\n"
             + PAREDIT_CONFIG),)),
    Mutant("K02-paredit-clojure", "paredit in Clojure",
           (("(clojure-mode       . paredit-mode)", "(clojure-mode       . ignore)"),)),
    Mutant("K03-format-key", "C-c r formats",
           (("(keymap-set prog-mode-map \"C-c r\" #'apheleia-format-buffer)",
             "(keymap-set prog-mode-map \"C-c r\" #'ignore)"),)),
    Mutant("K04-ws-butler-off", "ws-butler strips on save", WS_OFF),
    Mutant("K05-global-dtw", "ws-butler only touched lines", GLOBAL_DTW),
    Mutant("K06-fold-key", "C-c z z folds",
           (('("C-c z z"   . treesit-fold-toggle)', '("C-c z z"   . ignore)'),)),
    Mutant("K07-bb-mode", ".bb opens clojure-mode",
           (("(add-to-list 'auto-mode-alist '(\"\\\\.bb\\\\'\" . clojure-mode))",
             "nil"),)),
    Mutant("K08-mc-next", "C-> multiple cursors",
           (('("C->" . mc/mark-next-like-this)', '("C->" . ignore)'),)),
    Mutant("K09-org-next", "Org NEXT state",
           (('"TODO(t)" "NEXT(n)" "WAIT(w@/!)"', '"TODO(t)" "WAIT(w@/!)"'),)),
    Mutant("K10-python-ts", "python-ts-mode remap",
           ((("(my/treesit-remap-when-ready 'python     'python-mode"
              "     'python-ts-mode)"), "nil"),)),
    Mutant("K11-indent-bars", "indent-bars in prog-mode",
           (("(prog-mode . indent-bars-mode)", "(prog-mode . ignore)"),)),
    Mutant("K12-expreg", "C-= expreg",
           (('("C-=" . expreg-expand)', '("C-=" . ignore)'),)),
    Mutant("K13-dashboard", "dashboard on new frame",
           (("(initial-buffer-choice #'my/initial-buffer)",
             "(initial-buffer-choice nil)"),)),
    Mutant("K14-f9", "F9 outline",
           (("(global-set-key (kbd \"<f9>\") #'my/symbol-outline-toggle)", "nil"),)),
    Mutant("K15-f8", "F8 treemacs",
           (("(global-set-key (kbd \"<f8>\") #'treemacs)", "nil"),)),
    Mutant("K16-startup-error", "init.el errors early",
           ((WIDTH, '(error "mutant startup error")\n' + WIDTH),)),
    Mutant("K17-lazy-config-error", "paredit :config errors on load",
           ((PAREDIT_HOOK,
             PAREDIT_HOOK + '  :config (error "mutant paredit config")\n'),)),
    Mutant("K18-apheleia-python", "Python formatter mapping",
           ((("(setf (alist-get 'python-ts-mode apheleia-mode-alist)"
              " '(isort black))"), "nil"),),
           scope=("full", "l3-apheleia-python-format")),
    Mutant("U01-line-width", "fill width 120",
           ((WIDTH, "(defconst my/line-width 70"),)),
    Mutant("U02-c-s", "C-s consult-line", C_S),
    Mutant("U03-c-x-b", "C-x b consult-buffer",
           (('("C-x b"   . consult-buffer)', '("C-x b"   . switch-to-buffer)'),)),
    Mutant("U04-vertico", "vertico completion UI",
           ((":hook (emacs-startup . vertico-mode)",
             ":hook (emacs-startup . ignore)"),)),
    Mutant("U05-corfu-auto", "corfu auto popup", CORFU_AUTO),
    Mutant("U06-recentf", "recentf", RECENTF),
    Mutant("U07-diff-hl", "diff-hl in prog-mode", DIFF_HL_PROG,
           scope=("full", "journey-07-")),
    Mutant("U08-magit-key", "C-x g magit",
           ((':bind ("C-x g" . magit-status)', ':bind ("C-x g" . ignore)'),),
           scope=("full", "journey-06-")),
    Mutant("U09-capture-target", "Org capture into Inbox", CAPTURE_TARGET),
    Mutant("U10-theme", "dark theme",
           (("  (load-theme 'sanityinc-tomorrow-night t nil))",
             "  (load-theme 'wombat t nil))"),), covered=False),
    Mutant("U11-ace-window", "M-o ace-window",
           ((':bind ("M-o" . ace-window)', ':bind ("M-o" . ignore)'),)),
    Mutant("U12-which-key", "which-key",
           ((":hook (emacs-startup . which-key-mode)",
             ":hook (emacs-startup . ignore)"),), covered=False),
    Mutant("U13-trash", "deletes go to trash",
           (("(delete-by-moving-to-trash t)", "(delete-by-moving-to-trash nil)"),)),
    journey(1, "recentf", "recentf-mode off", RECENTF),
    journey(2, "c-s-isearch", "C-s bound to isearch-forward", C_S),
    journey(3, "no-rg", "rg missing from exec-path", appended(NO_RG)),
    journey(4, "wgrep-no-save", "wgrep auto-save off",
           (("(wgrep-auto-save-buffer t)", "(wgrep-auto-save-buffer nil)"),)),
    journey(5, "no-projects", "known project list emptied",
           appended("(setq project-list-exclude '(\".\"))")),
    journey(6, "magit-s", "magit staging key unbound in the status map",
           appended("(with-eval-after-load 'magit-status"
                    " (keymap-set magit-status-mode-map \"s\" #'ignore))")),
    journey(7, "diff-hl-off", "diff-hl off",
           (*DIFF_HL_PROG,
            ("(conf-mode  . diff-hl-mode)", "(conf-mode  . ignore)"),
            ("(text-mode  . diff-hl-mode)", "(text-mode  . ignore)"))),
    journey(8, "capture-target", "capture template target changed",
           CAPTURE_TARGET),
    journey(9, "agenda-next", "NEXT section removed from the custom command",
           ((('            (todo "NEXT"\n'
              '                  ((org-agenda-overriding-header "Next actions")))\n'),
             ""),)),
    journey(10, "corfu-auto", "corfu-auto nil", CORFU_AUTO),
    journey(11, "m-dot", "M-. rebound to a no-op",
           appended("(keymap-global-set \"M-.\" #'ignore)")),
    journey(12, "global-dtw", "global delete-trailing-whitespace on save",
           GLOBAL_DTW),
    journey(13, "aw-keys", "aw-keys emptied",
           (("(aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))", "(aw-keys nil)"),)),
    # cider-use-overlays is an obsolete alias of cider-eval-result-display,
    # whose legacy value nil means the echo area.
    journey(14, "cider-overlays", "cider-use-overlays nil",
           (("(cider-eval-result-display 'overlay)",
             "(cider-eval-result-display nil)"),)),
)
