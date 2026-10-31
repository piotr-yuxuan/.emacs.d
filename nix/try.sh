# Try this configuration without installing it: the flake's default app.
# It copies the configuration into a directory of its own, a new
# temporary one or EMACS_TRY_DIRECTORY, prepares it there with the
# flake's Emacs, and starts that Emacs on it.  Arguments go to Emacs.
#
# The flake sets EMACS (its Emacs), EMACS_TRY_SOURCE (the configuration
# in the store), EMACS_TRY_PROGRAMS (the programs' bin directory) and
# EMACS_TRY_SETUP (the setup script).  Nothing is written outside the
# directory but by Nix: CIDER's dependencies, which go to ~/.m2, are
# left for a jack-in to fetch.

dir=${EMACS_TRY_DIRECTORY:-$(mktemp -d "${TMPDIR:-/tmp}/emacs-try.XXXXXX")}
mkdir -p "$dir"
echo "Trying the configuration in $dir; its first start clones every package." >&2

# The store's copy is read-only: copy it over the files of an earlier
# trial, then make it writable, straight/ too, where straight.el clones
# and builds.  Leave alone the packages, which are writable already and
# many, and the links, which chmod would follow, some of the tests'
# fixtures in loops.
writable() {
  find "$dir" \( -path "$dir/straight/repos" -o -path "$dir/straight/build" \) -prune \
    -o ! -type l -exec chmod u+w {} +
}
writable
cp -R "$EMACS_TRY_SOURCE/." "$dir/"
writable

"$EMACS_TRY_SETUP" --no-cider all "$dir"

# Evaluated once init.el has run, so after exec-path-from-shell has
# taken the login shell's PATH: the flake's programs come first again.
# The server's name keeps the host's own Emacs server its socket.
exec "$EMACS" --init-directory="$dir" --eval "(progn
  (setenv \"PATH\" (concat \"$EMACS_TRY_PROGRAMS:\" (getenv \"PATH\")))
  (push \"$EMACS_TRY_PROGRAMS\" exec-path)
  (unless (daemonp) (setq server-name \"try\")))" "$@"
