#!/usr/bin/env bash
# Prepare a clone of this configuration for its first start: the steps
# Emacs does not take on its own.  It runs with or without Nix, with
# the Emacs that EMACS names, or emacs, and the programs on PATH.  The
# flake's setup app puts its programs first, and names its grammars and
# enchant in TREESIT_GRAMMARS and JINX_ENCHANT: store paths that this
# script keeps alive with garbage collector roots under var/nix/.
#
# Usage: setup.sh [--strict] [--no-cider] [packages|modules|all] [DIRECTORY]
#
#   packages  install every package at the version of the lock file,
#             then check that each sits at its pin: a report, or a
#             failure with --strict
#   modules   the tree-sitter grammars, jinx's module, and what CIDER
#             injects at jack-in, fetched into ~/.m2 (not with --no-cider;
#             without clojure, a report, or a failure with --strict)
#   all       both, the default
#
# DIRECTORY is the configuration, the current directory by default.
# Byte code belongs to the Emacs that compiled it: run this with the
# Emacs you will start.
set -euo pipefail

strict='' cider=1 phase=all
while [ $# -gt 0 ]; do
  case $1 in
    --strict) strict=1 ;;
    --no-cider) cider='' ;;
    packages | modules | all) phase=$1 ;;
    -*)
      echo "setup: unknown option $1" >&2
      exit 2
      ;;
    *) break ;;
  esac
  shift
done
dir=$(cd "${1:-.}" && pwd -P)
emacs=${EMACS:-emacs}
if [ ! -f "$dir/early-init.el" ] || [ ! -f "$dir/init.el" ]; then
  echo "setup: $dir holds no Emacs configuration" >&2
  exit 2
fi

# Load the configuration as its first start does, then the arguments.
in_emacs() {
  "$emacs" --batch --init-directory="$dir" \
    --load "$dir/early-init.el" --load "$dir/init.el" "$@"
}

# Keep the store path $2 alive under var/nix/$1, where Nix runs this.
root() {
  if [ -n "$2" ] && command -v nix-store > /dev/null; then
    mkdir -p "$dir/var/nix"
    nix-store --add-root "$dir/var/nix/$1" --indirect --realise "$2" > /dev/null
  fi
}

packages() {
  # Forges other than GitHub drop a clone now and then; straight.el
  # resumes where it stopped, so try again before giving up.
  local attempt
  for attempt in 1 2 3; do
    in_emacs && break
    [ "$attempt" = 3 ] && return 1
    echo "setup: attempt $attempt to install the packages failed; trying again"
  done
  if ! (cd "$dir" && "$emacs" --batch --load "$dir/nix/check-pins.el"); then
    [ -n "$strict" ] && return 1
    echo "setup: the packages above are off their pins"
  fi
}

modules() {
  mkdir -p "$dir/tree-sitter"
  if [ -n "${TREESIT_GRAMMARS:-}" ]; then
    root grammars "$TREESIT_GRAMMARS"
    local grammars=$TREESIT_GRAMMARS grammar
    [ -e "$dir/var/nix/grammars" ] && grammars=$dir/var/nix/grammars
    # The directory may hold grammars already: link each one.
    for grammar in "$grammars"/lib/*; do
      ln -sf "$grammar" "$dir/tree-sitter/"
    done
  else
    # After the require: init.el's :config sets it to prompt, and a let
    # before the require would bind it lexically.
    in_emacs --eval '(progn (require (quote treesit-auto))
                       (setq treesit-auto-install t)
                       (treesit-auto-install-all))'
  fi
  root enchant "${JINX_ENCHANT:-}"
  in_emacs --eval '(progn (require (quote jinx)) (jinx--load-module)
                     (unless (treesit-language-available-p (quote python))
                       (error "The Python grammar does not load")))'
  if [ -n "$cider" ] && ! command -v clojure > /dev/null; then
    echo "setup: no clojure on PATH: CIDER's dependencies are left for a jack-in to fetch" >&2
    [ -n "$strict" ] && return 1
  elif [ -n "$cider" ]; then
    # The versions this CIDER and clj-refactor inject at jack-in, into
    # ~/.m2/repository, which Java may not take for the home's.
    local deps
    deps=$(in_emacs --eval '(progn (require (quote cider)) (require (quote clj-refactor))
             (princ (format "{:mvn/local-repo \"%s\" :deps {nrepl/nrepl {:mvn/version \"%s\"} cider/cider-nrepl {:mvn/version \"%s\"} refactor-nrepl/refactor-nrepl {:mvn/version \"%s\"}}}"
                            (expand-file-name "~/.m2/repository/")
                            cider-injected-nrepl-version cider-injected-middleware-version
                            cljr-injected-middleware-version)))')
    # From journey 14's project, where the tests are, so that its own
    # dependencies come too.
    local project=$dir/tests/fixtures/journey-cider
    [ -d "$project" ] || project=$dir
    (cd "$project" && clojure -Sdeps "$deps" -P)
  fi
}

case $phase in
  packages) packages ;;
  modules) modules ;;
  all)
    # Two commands, not `packages && modules': set -e holds in neither
    # function of an && list.
    packages
    modules
    ;;
esac
