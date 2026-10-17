# AGENTS.md

This file guides LLM agents working in this repository.  The
documentation for a human reader starts at `docs/README.org`.

## Command-line tools

I use `zsh` as my default interactive shell.

| Tool    | Purpose                                                       |
|---------|---------------------------------------------------------------|
| `eza`   | Modern `ls` replacement with tree view, icons, and git status |
| `fzf`   | Fuzzy finder for files, history, and arbitrary lists          |
| `rg`    | ripgrep, extremely fast recursive text search                 |
| `fd`    | Fast, user-friendly alternative to `find`                     |
| `bat`   | `cat` clone with syntax highlighting and git integration      |
| `jq`    | Command-line JSON processor                                   |
| `delta` | Syntax-highlighting pager for `git diff` output               |
| `difft` | Difftastic, structural/syntax-aware diff tool                 |

Standard GNU coreutils are available. Prefer the tools above over
their traditional counterparts (e.g. `rg` over `grep`, `fd` over
`find`, `eza` over `ls`) for speed and richer output.

## Source of truth

`README.org` is the literate source. `early-init.el` and `init.el` are
**generated** from it via `org-babel-tangle` (`C-c C-v t` in Emacs, or
batch).  Run from the directory containing `README.org`:

``` zsh
emacs --batch \
  --eval "(require 'org)" \
  --eval "(org-babel-tangle-file \"$(pwd)/README.org\")"
```

This works the same in every worktree, because `--batch` skips the
user init file and `$(pwd)` resolves to the current directory.

**Only edit `init.el` or `early-init.el` directly if trying temporary,
reversible changes**. For persistent changes edit `README.org`
accordingly instead and retangle. Direct edits to the `.el` files are
temporary patches until the next tangle.  Commit `README.org` together
with the `init.el` and `early-init.el` it tangles.

## Repository layout

| File or directory              | Purpose                                                                         |
|--------------------------------|---------------------------------------------------------------------------------|
| `README.org`                   | Literate configuration: every package, with its reference and explanation       |
| `early-init.el`, `init.el`     | Tangled from `README.org`                                                       |
| `local.el.example`             | Skeleton of the machine-local `local.el`, which git ignores                     |
| `straight/versions/default.el` | Package lock file                                                               |
| `docs/`                        | Tutorials, how-to guides, reference and explanation; index in `docs/README.org` |
| `tests/`                       | Tests; reference in `docs/reference/tests.org`                                  |
| `./tmp`                        | Temporary files, ignored by git. Do not use `/tmp`.                             |

## Repository and worktrees

The private repository is a bare git repository, with no working tree
of its own.  Its worktrees:

| Path                             | Branch     | Purpose                                                                         |
|----------------------------------|------------|---------------------------------------------------------------------------------|
| `~/.emacs.d/`                    | `main`     | The live configuration the user runs, the primary checkout of `workflow:plans`  |
| `~/.emacs.d/.worktree/NNN-slug/` | `NNN-slug` | One plan: created for it, removed after its merge                               |

`.gitignore` ignores `/.worktree/`, so the worktrees never show in the
live checkout.

## Public export

This repository is published as a mechanical export: a tool copies the
files that `publish/manifest.toml` includes and adds those of
`publish/overlay/`.  It rewrites nothing, so the boundary is kept in
the files themselves:

- Personal values go only in files the export manifest denies, or in
  the git-ignored `local.el`: names, addresses, accounts, hosts, home
  directory paths, key identifiers, employers, private projects, time
  zone, locale, keyboard layout.  Every other file uses neutral
  defaults and examples.
- A new file is classified in `publish/manifest.toml`, as included or
  excluded, in the commit that adds it.  The manifest ignores
  `publish/` itself.
- `publish/manifest.toml` is the one list of what is exported and
  what is not.  A published file does not link to a file that is not:
  it names it in plain text, as `tests/docs-test.el` checks.

The workflow below runs in the private repository: the work records
under `docs/work/` and the task file `todo.org`, which it names, are
among the files the manifest denies.

## Workflow

Every change follows the skill `workflow:plans`; load it before the
first edit.  In this repository: plans are
`docs/work/plans/NNN-slug.org`, tracked; the checks are
`tests/run static` and `tests/run fast FILTER` while working, then
`tests/run full`, `tests/integrity/session/run` and the sweep of
"Dash ban" before the merge; the extra steps are these:

- At step 1, the plan is written in its own worktree, not in
  `~/.emacs.d`, not even under `docs/work/`: create the worktree and
  its branch from `main` with step 3, its command then its check,
  before writing the plan, then commit the plan and its row of the
  index `docs/work/README.org` as the branch's first commit, before
  approval.  Step 3 is then done: after approval, only its check is
  repeated.
- At step 4, a change of the configuration is made in `README.org`
  and retangled, as "Source of truth" says, with the tests that "Test
  strategy" asks for.
- At step 6, resolve conflicts in `README.org`, then retangle; never
  merge `init.el`, `early-init.el` or `docs/reference/keys.org` by
  hand; where rows of the index conflict, keep the rows of both
  sides.  Then, even when `main` had not moved, regenerate the key
  reference with `tests/integrity/session/run cheatsheet` and commit
  `docs/reference/keys.org` if it changed.
- At step 7, the commit of the `git mv` to `docs/work/changelog/`
  also gives the plan's row in the index its new link and its status.
- At step 9, right before the merge, in the worktree,
  `git merge-base --is-ancestor main HEAD` must succeed.  If it
  fails, `main` moved after the last catch-up: do step 6 again, with
  the checks before the merge, and merge only once the owner, told
  the outcome, confirms again.  So `~/.emacs.d` receives only a tree
  the checks ran on.
- Plans running in parallel share the registries that the index
  describes (known defects, expected skips, outcomes): each plan
  writes only its own entries.

Plans and changelog entries are Org files (`* Heading`, `=verbatim=`,
`#+BEGIN_SRC ... #+END_SRC`, Org tables); Markdown plans written
before this rule stay as they are.  The open tasks of this repository
wait in `todo.org`.

Never edit the live checkout `~/.emacs.d` (its files outside
`.worktree/`) other than its `local.el`: the merge of step 9 is its
one other change.  This is stricter than the shared rule, which allows
writing under `docs/work/`, but for `local.el`, which the shared rule
does not allow.  Never connect to the user's running Emacs daemon, and
never install git hooks.

### Commits

- Commit only the hunks you changed, not every change in a path you
  touched: stricter than `workflow:plans`.
- A title that needs "and" is two commits.
- Pass `-m` to `git commit` and `git merge`, and `--no-pager` to
  commands that page: nothing may wait for input.

### Running the tests from a worktree

`tests/run` works from any worktree, with no setup; see "Running the
tests" below.  Each session it starts has its own private init
directory (`--init-directory`), whose `early-init.el` and `init.el`
load the worktree's files, with empty `var/` and `etc/`, its own
sockets and its own `TMPDIR`.  Packages come read-only from the live
`~/.emacs.d/straight` (or `$EMACS_LIVE_DIR/straight`) in
`straight-safe-mode`; nothing is written there.

This replaces the earlier rule against `--init-dir`: pointing
`--init-directory` at a directory that only loads the worktree's
files is how the tests start Emacs the way the user does, without the
live configuration.  Never point it at the live `~/.emacs.d`.

`tests/run full` must be green before merging.

### Running the tests

``` zsh
tests/run                         # fast: self-tests, logic tests, non-L3 scenarios
tests/run fast l1-expreg          # only the files whose path under tests/ contains the filter
tests/run full                    # fast, L3 files, then the isolation check
tests/run static                  # checks without a session (plan 086)
tests/integrity/session/run       # checks in a session, including the keys docs/ cites (plan 086)
EMACS_TEST_GUI=0 tests/run fast   # no graphical frame; GUI-only files are expected skips
```

`docs/reference/tests.org` lists every test command, tier, header and
environment variable.

The profile comes before the filter; a bare filter is a usage error.
`tests/run.sh` and `tests/test-integration.sh` are aliases of
`tests/run fast` and `tests/run full`.  The exit status is 1 on any
failure, load error, unexpected skip, stale expected-skip entry or
deadline, and 2 on a usage error.  Sessions live under
`$TMPDIR/emacs-test-*/` (the system temporary directory, which keeps
socket paths short; an exception to the `./tmp` rule of the
layout table).  The directory of a green session is removed; a
failing one is kept and printed, with `messages.txt`,
`results.jsonl` and `emacs.log`.

Test frames never reach your screen, so they never take focus: the
runner starts a private Xvfb server for each session and gives the
session only that display, or no display at all with
`EMACS_TEST_GUI=0`; sessions therefore share no clipboard either.
Without Xvfb, or with an Emacs that cannot draw on X (the NS build of
macOS ignores `DISPLAY`), `tests/run` stops with a usage error; run it
with `EMACS_TEST_GUI=0` there.  The graphical tier is therefore GTK on the
X11 backend of Xvfb, with no window manager: not the pgtk-on-Wayland
frames of a normal Linux session, nor macOS frames.

`tests/hooks/pre-commit` runs `tests/run static` and
`tests/hooks/pre-merge-commit` runs `tests/run fast`.  They are not
installed; `git config core.hooksPath tests/hooks` enables them.  On
macOS, export `EMACS_TEST_GUI=0` for the merge hook, or every merge
stops at the usage error above.

The harness needs Emacs 30 or later: the scenario language uses
`handler-bind`.

## Test strategy

### Tiers

- *Static checks* (`tests/run static`, plan 086): checks that need
  no session.
- *Logic tests*: `tests/**/*-test.el` outside `tests/scenarios/`,
  in one batch session with the configuration loaded.  They call pure
  decision functions of the configuration directly (for example
  `tests/org-tasks-test.el`, `tests/python-helpers-test.el`), with
  stubs only at system boundaries: PATH lookup, processes, the clock,
  D-Bus, the file system.  The harness self-tests under
  `tests/helpers/` are logic tests too.
- *Scenarios*: each file of `tests/scenarios/` runs in a fresh
  `emacs --fg-daemon`, started the way the user starts Emacs, in a
  client frame on its own private Xvfb display.  A scenario presses real keys
  and checks what the user then sees.  The self-tests that need such
  a session are scenario files too (`l1-scenario-language*.el`,
  `l1-session-*.el`).
- *L3 scenarios*: files declaring `;; scenario-tier: l3` use real
  external tools, such as language servers.  They run only in `full`.

Header comments at the top of a scenario file:
- `;; scenario-tier: l3` for real external tools;
- `;; scenario-display: graphic` when it needs a graphical frame, for
  example to format a header line;
- `;; scenario-startup: first-file FIXTURE` when its single row needs
  the buffer the runner opens right after startup: FIXTURE is a path
  under `tests/fixtures/`, whose first directory is copied into a
  fresh git repository; use the row `(:existing-buffer REGEXP)` (see
  `tests/scenarios/l1-session-first-file.el`).

### The scenario language

```elisp
(scenario
 :describe "C-x b switches to the buffer a typed pattern selects"
 :given ((:name "org" :file "notes.org" :text "* TODO Call-!-\n"))
 :when ((:keys "C-x b") (:type "Messa") (:sample "prompt") (:keys "<return>"))
 :then ((:buffer-name "\\`\\*Messages\\*\\'")
        (:sampled "prompt" :minibuffer "Messa")))
```

- Every slot is data, unquoted, and validated when the form expands;
  nothing is evaluated.  Each row of `:given` is one spec.
- Rows (`tests/helpers/scenario/row.el`): `(:file PATH :text TEXT)`
  with the point marker `-!-`, `(:project FIXTURE :entry PATH)` for a
  copy of `tests/fixtures/FIXTURE`, or `(:existing-buffer REGEXP)`;
  `:point-on SUBSTRING` places point.  `:system` stubs a real system
  boundary: `(:appearance dark)`, `(:record-browse-url t)`,
  `(:lsp-root ".")` (`tests/helpers/scenario/system.el`).
- `:when` is one key sequence (`tests/helpers/keystrokes.el`):
  `(:keys "C-x b")` with GUI key names (`<return>`, not `RET`),
  `(:type "text")`, `(:open)`, `(:idle MS)`, `(:sample NAME)`, and
  `(:redisplay-while-typing)` (Emacs redisplays with the next key
  pending, so jit-lock defers; graphical files only).  A
  prompt receives the keys that follow it; the keys must not end
  inside a prompt.  An error, or a quit outside a prompt, ends the
  keys: the step that raises it must be the last one, probes such as
  `(:idle MS)` included, and a key bound to a keyboard macro must not
  raise one.
- `:wait-for` waits as an idle user, for example for
  `(:lsp-attached clojure-lsp)` (`tests/helpers/scenario/wait.el`).
- `:then` lists outcomes, each defined once in
  `tests/helpers/outcomes/`; a plan adds its outcomes in its own file
  there.  An outcome means the same with and without frames:
  `:face-shown` reads the faces redisplay left (graphical files only),
  `:face-computed` the faces font-lock computes.  `(my/scenario-vocabulary)` generates the reference of the
  steps and outcomes; print it with:

  ``` zsh
  emacs --batch -Q -L tests/helpers -L ~/.emacs.d/straight/build/buttercup \
    -l scenario --eval '(princ (my/scenario-vocabulary))'
  ```
- Any error the user would see fails the spec, unless `:then` expects
  `(:user-error REGEXP)`.
- The rows of one file share one session.  A scenario that needs the
  first file of a fresh session belongs alone in its own file.

### Skip accounting

A spec may skip only for a reason that holds in the session.  Each
plan owns one file `tests/expected-skips/NNN.el` of entries
`(SPEC-REGEXP REASON)`, with REASON one of
`(:missing-executable "NAME")`, `(:no-gui)` or `(:platform SYSTEM)`.
A skip without a matching entry whose reason holds fails, as do a
stale entry (its specs ran and none skipped) and any `xit` or
`xdescribe`.  A scenario waiting for a language server skips with
"missing executable NAME" when the server is not installed.

### Rules

- No new test asserts a `:custom` value unless that value is also
  exercised by behaviour elsewhere.  A test that only mirrors
  configuration is rejected at code review.
- Every new feature ships a scenario or a logic test, or justifies
  its absence in the plan file.
- What a user does is tested by a scenario that presses real keys;
  never through `call-interactively` or `(funcall (key-binding ...))`.
  A lost or shadowed binding then fails where the key is pressed.
- Logic tests call pure functions directly, with stubs only at system
  boundaries.
- No mock or advice on production code paths.  A system boundary a
  scenario must stub goes in `tests/helpers/scenario/system.el`, as a
  `:system` row key, with its reason.
- User-facing keybindings stay in vanilla `use-package` `:bind` /
  `define-key` / `keymap-set` form; no custom macro wraps them.
  `docs/reference/keys.org` is generated from the live keymaps (plan
  086), so `README.org` keeps no key summary of its own: a key table
  sits next to the code that binds its keys.
- Every user-facing command bound to a key has a non-empty
  docstring.  The cheatsheet uses its first line verbatim.

## Dash ban (no Unicode dashes)

The shared rules (`instructions/global.md` of shift-left) ban the em
dash (`U+2014`) and the en dash (`U+2013`), but in a verbatim licence:
here `publish/overlay/LICENSE`.  Use ASCII instead: comma, semicolon,
colon, parentheses, hyphen-minus (`-`), or "to" for ranges.  Older
text keeps its dashes until it is edited.

This repository adds one exception: a quoted character literal or
regex pattern inside `verbatim` or `SRC` markup whose purpose is to
name or detect the banned characters.

Before a merge, this sweep of the lines the plan added, from BASE, the
commit it branched from, reports nothing:

``` zsh
git --no-pager diff BASE -- . ':!publish/overlay/LICENSE' | rg '^\+.*(\x{2014}|\x{2013})'
```
