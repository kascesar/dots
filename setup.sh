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
symlink "$DOTS/yazi/yazi.toml" "$HOME/.config/yazi/yazi.toml"

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
    echo "  Pop!_Shell no detectada — omitiendo."
    echo "  Instala la extensión y luego ejecuta: bash popshell/system76style.sh"
fi

echo ""
echo "Listo. Abre una nueva terminal para aplicar los cambios de bash."
