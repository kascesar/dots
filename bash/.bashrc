force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
     if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
         # We have color support; assume it's compliant with Ecma-48
         # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
         # a case would tend to support setf rather than setaf.)
         color_prompt=yes
     else
         color_prompt=
     fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

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
export PATH="~/.local/bin/:$PATH"
PIPENV_VENV_IN_PROJECT=1

# Set up fzf key bindings and fuzzy completion
PATH=$PATH:/home/cesar/.local/bin
# eval "$(oh-my-posh init bash --config 'https://raw.githubusercontent.com/kascesar/dots/refs/heads/main/bash/.posh_config.yaml')"

eval "$(oh-my-posh init bash --config ~/.posh_config.yaml)"
