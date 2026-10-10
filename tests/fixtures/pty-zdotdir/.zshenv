# Fixture zsh startup of the PTY tier (plan 091), read instead of the
# user's own files: tests/pty/pty-session.el points ZDOTDIR here.  HOME
# stays the user's.  zsh still reads /etc/zshenv first; NO_GLOBAL_RCS
# skips the other global files.

setopt NO_GLOBAL_RCS

# The session inherits the user's environment; drop what changes how
# fzf lists and shows candidates, so every run sees fzf's defaults.
unset FZF_DEFAULT_OPTS FZF_DEFAULT_OPTS_FILE FZF_DEFAULT_COMMAND \
      FZF_CTRL_T_COMMAND FZF_CTRL_T_OPTS FZF_ALT_C_COMMAND FZF_ALT_C_OPTS \
      FZF_CTRL_R_OPTS

# EDITOR and VISUAL: the private server of the test session, whose
# socket pty-session.el exports.  Without that socket, editing fails
# instead of reaching the user's own Emacs.
if [[ -S ${PTY_FIXTURE_SERVER-} ]]; then
  export EDITOR="emacsclient --socket-name=$PTY_FIXTURE_SERVER"
else
  export EDITOR=false
fi
export VISUAL=$EDITOR ALTERNATE_EDITOR=false

# Programs the scenarios start, such as btop, keep their configuration
# and state in the session's temporary directory, not in the user's.
export XDG_CONFIG_HOME=${TMPDIR:-/tmp}/pty-xdg/config \
       XDG_STATE_HOME=${TMPDIR:-/tmp}/pty-xdg/state \
       XDG_CACHE_HOME=${TMPDIR:-/tmp}/pty-xdg/cache
mkdir -p -- $XDG_CONFIG_HOME $XDG_STATE_HOME $XDG_CACHE_HOME
