#!/bin/bash
set -e

# ==============================================================================
# Detección de gestor de paquetes
# ==============================================================================
if command -v dnf &>/dev/null; then
    PM="dnf"
    pkg_install() { sudo dnf install -y "$@"; }
elif command -v apt &>/dev/null; then
    PM="apt"
    pkg_install() { sudo apt install -y "$@"; }
else
    echo "ERROR: Gestor de paquetes no soportado (se requiere dnf o apt)."
    exit 1
fi

echo "Gestor de paquetes: $PM"
echo ""

# ==============================================================================
# Paquetes
# Fedora : bat (binario: bat),    eza, git-delta (binario: delta), micro, kitty
# Ubuntu : bat (binario: batcat), eza, git-delta,                  micro, kitty
# ==============================================================================
echo "==> Instalando paquetes..."
pkg_install bat eza git-delta micro kitty hunspell hunspell-en hunspell-es \
    ffmpeg jq fd-find ripgrep fzf zoxide wl-clipboard ImageMagick p7zip

# yazi — requiere repo externo
echo ""
echo "==> Instalando yazi..."
if [ "$PM" = "dnf" ]; then
    sudo dnf copr enable -y lihaohong/yazi
    sudo dnf install -y yazi
elif [ "$PM" = "apt" ]; then
    curl -sS https://debian.griffo.io/EA0F721D231FDD3A0A17B9AC7808B4DD62C41256.asc \
        | gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/debian.griffo.io.gpg
    echo "deb https://debian.griffo.io/apt $(lsb_release -sc 2>/dev/null) main" \
        | sudo tee /etc/apt/sources.list.d/debian.griffo.io.list
    sudo apt update
    sudo apt install -y yazi
fi

# ==============================================================================
# oh-my-posh — no está en repositorios estándar
# ==============================================================================
if ! command -v oh-my-posh &>/dev/null; then
    echo ""
    echo "==> Instalando oh-my-posh..."
    curl -s https://ohmyposh.dev/install.sh | bash -s -- -d ~/.local/bin
else
    echo "oh-my-posh: ya instalado."
fi

echo ""
echo "==> Instalando fuente MesloLGL Nerd Font..."
oh-my-posh font install meslo

echo ""
echo "Listo. Ejecuta setup.sh para aplicar las configuraciones."
