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
pkg_install bat eza git-delta micro kitty hunspell hunspell-en hunspell-es

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
