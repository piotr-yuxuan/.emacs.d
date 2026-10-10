# Fixture interactive shell of the PTY tier (plan 091).  It sources the
# terminal integration of the user's ~/.zshrc, as
# docs/how-to/terminals.org documents it under "Prepare the shell,
# once": eat's integration script, the OSC 133 and
# OSC 7 hooks, fzf's key bindings and the edit-command-line widget.  It
# has no prompt theme and no plugins, so every terminal starts the
# same shell.  The user's block that points VISUAL at the default
# server is left out on purpose: see .zshenv.

bindkey -e

# The prompt names the working directory, so the scenarios can tell
# where the shell is and that it waits for input.
PS1='%1~ ready> '
RPS1=''

# A history of one entry, for fzf's C-r to find, in the session's
# temporary directory.  eat's integration reads the file; zsh never
# writes it back.
HISTFILE=${TMPDIR:-/tmp}/pty-zsh-history
print -r -- 'echo history-pick' >| $HISTFILE
HISTSIZE=100
SAVEHIST=0

# eat's shell integration, when eat started the shell.
[[ -n ${EAT_SHELL_INTEGRATION_DIR-} ]] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# OSC 133 prompt annotations and OSC 7 directory tracking.
autoload -Uz add-zsh-hook
_emt_precmd()  { printf '\e]133;A\e\\'; }
_emt_preexec() { printf '\e]133;C\e\\'; }
_emt_chpwd()   { printf '\e]7;file://%s%s\e\\' "${HOST:-$(hostname -s)}" "$PWD"; }
add-zsh-hook precmd  _emt_precmd
add-zsh-hook preexec _emt_preexec
add-zsh-hook chpwd   _emt_chpwd
_emt_chpwd

# fzf key bindings: C-r history, C-t files, M-c directories.
source <(fzf --zsh)

# C-x C-e opens the command line in $VISUAL.
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# pty-throughput: print 200000 lines, as `seq 1 200000' does, while a
# background loop times one round trip to the session's server every
# 100 ms.  Each round trip is written to $PTY_LATENCY_FILE in
# milliseconds, one per line; the loop stops with the lines, when the
# shell is gone, or after 600 round trips.  The shell goes when its
# terminal is killed before seq ends, and the loop, a disowned job,
# gets no hangup then.  The last line printed tells how long seq took.
pty-throughput() {
  zmodload zsh/datetime
  : >| $PTY_LATENCY_FILE
  {
    local start
    local -i ms
    repeat 600; do
      kill -0 $$ 2>/dev/null || break
      start=$EPOCHREALTIME
      $=EDITOR --eval t >/dev/null 2>&1 || print -r -- failed >> $PTY_LATENCY_FILE
      ms=$(( (EPOCHREALTIME - start) * 1000 ))
      print -r -- $ms >> $PTY_LATENCY_FILE
      sleep 0.1
    done
  } &!
  local sampler=$! start=$EPOCHREALTIME
  seq 1 200000
  local -i seconds=$(( EPOCHREALTIME - start ))
  kill $sampler 2>/dev/null
  print -r -- "throughput-done in ${seconds} s"
}
