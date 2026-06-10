#!/bin/bash
set -e

DOTS="$(cd "$(dirname "$0")" && pwd)"

# ==============================================================================
# Symlinks
# Crea el directorio destino si no existe, hace backup del archivo original
# si no era ya un symlink, y establece el link apuntando al repo.
# ==============================================================================
symlink() {
    local src="$1"
    local dst="$2"
    mkdir -p "$(dirname "$dst")"
    if [ -e "$dst" ] && [ ! -L "$dst" ]; then
        echo "  backup: $dst => $dst.bak"
        mv "$dst" "$dst.bak"
    fi
    ln -sf "$src" "$dst"
    echo "  $dst -> $src"
}

echo "==> bash..."
symlink "$DOTS/bash/.bashrc"           "$HOME/.bashrc"
symlink "$DOTS/bash/.bash_aliases"     "$HOME/.bash_aliases"
symlink "$DOTS/bash/.posh_config.yaml" "$HOME/.posh_config.yaml"

echo ""
echo "==> git..."
symlink "$DOTS/git/.gitconfig" "$HOME/.gitconfig"

echo ""
echo "==> micro..."
symlink "$DOTS/micro/settings.json"         "$HOME/.config/micro/settings.json"
symlink "$DOTS/micro/syntax/gitcommit.yaml" "$HOME/.config/micro/syntax/gitcommit.yaml"

echo ""
echo "==> yazi..."
symlink "$DOTS/yazi/yazi.toml"    "$HOME/.config/yazi/yazi.toml"
symlink "$DOTS/yazi/keymap.toml" "$HOME/.config/yazi/keymap.toml"

echo ""
echo "==> kitty..."
symlink "$DOTS/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
symlink "$DOTS/kitty/theme.conf" "$HOME/.config/kitty/theme.conf"

echo ""
echo "==> emacs..."
bash "$DOTS/emacs/install.sh"

echo ""
echo "==> pop!_shell..."
if gnome-extensions list 2>/dev/null | grep -q "pop-shell@system76.com"; then
    bash "$DOTS/popshell/system76style.sh" --keybindings
else
    echo "  Pop!_Shell no detectada — instala primero con install-deps.sh."
    echo "  Luego cierra sesion, vuelve a entrar, y ejecuta setup.sh de nuevo."
fi

echo ""
echo "==> cheatsheet..."
symlink "$DOTS/cheatsheet/cheatsheet"    "$HOME/.local/bin/cheatsheet"
symlink "$DOTS/cheatsheet/cheatsheet.md" "$HOME/cheatsheet.md"

# Ventanas nuevas centradas en Mutter
gsettings set org.gnome.mutter center-new-windows true

# Atajo Super+H → cheatsheet (añade custom1 sin pisar keybindings existentes)
EXISTING=$(gsettings get org.gnome.settings-daemon.plugins.media-keys custom-keybindings \
    | tr -d "[]'" | tr ',' '\n' | grep -v '^$' | xargs)
KB_PATH="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
if echo "$EXISTING" | grep -qF "custom1"; then
    echo "  atajo custom1 ya existe — omitiendo"
else
    NEW_LIST=$(gsettings get org.gnome.settings-daemon.plugins.media-keys custom-keybindings \
        | sed "s|]$|, '$KB_PATH']|")
    gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "$NEW_LIST"
    gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:"$KB_PATH" \
        name    'Cheatsheet'
    gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:"$KB_PATH" \
        command "$HOME/.local/bin/cheatsheet"
    gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:"$KB_PATH" \
        binding '<Super>h'
    echo "  Super+H → cheatsheet registrado"
fi

echo ""
echo "==> blur my shell..."
if gnome-extensions list 2>/dev/null | grep -q "blur-my-shell@aunetx"; then
    gnome-extensions enable blur-my-shell@aunetx
    dconf load /org/gnome/shell/extensions/blur-my-shell/ < "$DOTS/popshell/blur-my-shell.dconf"
    echo "  blur-my-shell: configuracion aplicada."
else
    echo "  Blur My Shell no detectada — instala primero con install-deps.sh."
fi

echo ""
echo "Listo. Abre una nueva terminal para aplicar los cambios de bash."
