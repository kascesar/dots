# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# # uncomment for a colored prompt, if the terminal has the capability; turned
# # off by default to not distract the user: the focus in a terminal window
# # should be on the output of commands, not on the prompt
# #force_color_prompt=yes

# if [ -n "$force_color_prompt" ]; then
#     if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
#         # We have color support; assume it's compliant with Ecma-48
#         # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
#         # a case would tend to support setf rather than setaf.)
#         color_prompt=yes
#     else
#         color_prompt=
#     fi
# fi

# if [ "$color_prompt" = yes ]; then
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
# fi
# unset color_prompt force_color_prompt

# load custom prompt
if [ -f ~/.prompt ]; then
    source ~/.prompt
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi


# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# adding user python bin path

export PATH="~/.local/bin/:$PATH"
export DS_TEMPLATE="https://github.com/kascesar/data-science-template"


# spark vars

export CONFLUENCE_ACCESS_TOKEN="ATATT3xFfGF04FUzndBblRXVjlXeLKv3A23NWgJxYLq_CfJh-7K6OVu5BNVsoZerBI5ld79rvPGrBMt0PNc-JsIAlF_2jpOcVa4xTvEpkXQ5HueMjFAYKdyHoA3xCSfaTRzpWl7YAJp_Tzo5Jdo304d0z2E2wDXfKVqa9kWT4x5dFN-W1tmaels=05767A05"
#export CONFLUENCE_PASSWORD="cesar123912"
export CONFLUENCE_ENDPOINT="https://openworldlatam.atlassian.net/wiki"
# export CONFLUENCE_USERNAME="cmunoz@openworldlatam.com"

export SPARK_HOME=/home/cesar/spark
export PATH=$PATH:$SPARK_HOME/bin

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi
