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

# emacs en la terminal cuando algun proceso me exige modificar un archivo
export EDITOR="emacsclient -t"

# agrega mis binarios de usuario al path
export PATH="~/.local/bin/:$PATH"

# prompt con oh my posh
eval "$(oh-my-posh init bash --config ~/.posh_config.yaml)"
