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
