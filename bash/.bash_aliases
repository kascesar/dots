# -*- mode: sh; -*-

# alias nano="nano -lET 4"
alias et="emacsclient -t"

function ec () {
         emacsclient -cq "$@" &
}

alias v=less
alias sclip="xclip -selection c"
alias gclip="xclip -selection c -o"
