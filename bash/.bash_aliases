# -*- mode: sh; -*-

alias nano="nano -lET 4 "

##  bat
if command -v batcat &>/dev/null; then
    alias cat="batcat --style changes,header,rule,snip --theme 'Monokai Extended Origin' "
elif command -v bat &>/dev/null; then
    alias cat="bat --style changes,header,rule,snip --theme 'Monokai Extended Origin' "
fi

## exa (eza)
if command -v eza &>/dev/null; then
    alias ls="eza --icons --group-directories-first -l "
fi

## yazi
# Suelo olvidar el nombre de este complemento
# agrego el alias al que asocio el funcionamiento
alias files=y
alias folder=y

alias calibre-sync='rclone bisync Calibre:calibre ~/Calibre'

## chat-tts
# Lector en voz alta del chat de Twitch (servicio de usuario)
alias chat-on='systemctl --user start chat-tts'
alias chat-off='systemctl --user stop chat-tts'
