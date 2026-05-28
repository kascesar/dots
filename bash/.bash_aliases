# -*- mode: sh; -*-

alias nano="nano -lET 4 "
if command -v batcat &>/dev/null; then
    alias cat="batcat --style changes,header,rule,snip --theme 'Monokai Extended Origin' "
elif command -v bat &>/dev/null; then
    alias cat="bat --style changes,header,rule,snip --theme 'Monokai Extended Origin' "
fi
alias ls="exa --icons --group-directories-first -l "
