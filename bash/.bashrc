# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# python
export PIPENV_VENV_IN_PROJECT=1
export VIRTUAL_ENV_DISABLE_PROMPT=1

export EDITOR="emacs -nw"

# agrega mis binarios de usuario al path
export PATH="$HOME/.local/bin:$PATH"

# prompt con oh my posh
eval "$(oh-my-posh init bash --config ~/.posh_config.yaml)"

# yazi — cambia el directorio del shell al salir de yazi
function y() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
    command yazi "$@" --cwd-file="$tmp"
    IFS= read -r -d '' cwd < "$tmp"
    [ "$cwd" != "$PWD" ] && [ -d "$cwd" ] && builtin cd -- "$cwd"
    command rm -f -- "$tmp"
}
