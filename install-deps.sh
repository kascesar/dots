#!/bin/bash
set -e

# ==============================================================================
# Detección de gestor de paquetes
# ==============================================================================
if command -v dnf &>/dev/null; then
    PM="dnf"
    is_installed() { rpm -q "$1" &>/dev/null; }
    pkg_install()  { sudo dnf install -y "$@"; }
elif command -v apt &>/dev/null; then
    PM="apt"
    is_installed() { dpkg -l "$1" 2>/dev/null | grep -q "^ii"; }
    pkg_install()  { sudo apt install -y "$@"; }
else
    echo "ERROR: Gestor de paquetes no soportado (se requiere dnf o apt)."
    exit 1
fi

echo "Gestor de paquetes: $PM"
echo ""

# Instala un paquete solo si no está ya instalado.
# Uso: try_install <paquete> [comando-para-verificar]
try_install() {
    local pkg="$1"
    local cmd="${2:-}"
    if { [ -n "$cmd" ] && command -v "$cmd" &>/dev/null; } || is_installed "$pkg"; then
        echo "  $pkg: ya instalado"
        return 0
    fi
    echo "  Instalando $pkg..."
    pkg_install "$pkg" || echo "  Advertencia: no se pudo instalar $pkg, continuando..."
}

# ==============================================================================
# Paquetes
# Fedora : bat (binario: bat),    eza, git-delta (binario: delta), micro, kitty
# Ubuntu : bat (binario: batcat), eza, git-delta,                  micro, kitty
# ==============================================================================
echo "==> Paquetes base..."
try_install bat         bat
try_install eza         eza
try_install git-delta   delta
try_install micro       micro
try_install kitty       kitty
try_install glow        glow
try_install hunspell
try_install hunspell-en
try_install hunspell-es

echo ""
echo "==> Dependencias de yazi..."
try_install ffmpeg      ffmpeg
try_install jq          jq
try_install fd-find     fd
try_install ripgrep     rg
try_install fzf         fzf
try_install zoxide      zoxide
try_install wl-clipboard wl-copy
try_install ImageMagick  magick
try_install p7zip        7z

# ==============================================================================
# yazi — requiere repo externo
# ==============================================================================
echo ""
echo "==> yazi..."
if ! command -v yazi &>/dev/null; then
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
else
    echo "  yazi: ya instalado"
fi

# ==============================================================================
# oh-my-posh — no está en repositorios estándar
# ==============================================================================
echo ""
echo "==> oh-my-posh..."
if ! command -v oh-my-posh &>/dev/null; then
    curl -s https://ohmyposh.dev/install.sh | bash -s -- -d ~/.local/bin
else
    echo "  oh-my-posh: ya instalado"
fi

echo ""
echo "==> Fuente MesloLGL Nerd Font..."
if fc-list | grep -qi "MesloLGL"; then
    echo "  MesloLGL Nerd Font: ya instalada"
else
    oh-my-posh font install meslo
fi


# ==============================================================================
# Extensiones GNOME
# Requiere sesion grafica activa (DISPLAY/WAYLAND_DISPLAY).
# ==============================================================================
if [ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ]; then
    echo ""
    echo "==> Extensiones GNOME..."

    # Pop!_Shell — disponible via paquete del sistema
    if gnome-extensions list 2>/dev/null | grep -q "pop-shell@system76.com"; then
        echo "  pop-shell: ya instalada"
    else
        echo "  Instalando Pop!_Shell..."
        if [ "$PM" = "dnf" ]; then
            sudo dnf install -y gnome-shell-extension-pop-shell
        elif [ "$PM" = "apt" ]; then
            sudo apt install -y gnome-shell-extension-pop-shell
        fi
        echo "  IMPORTANTE: Cierra sesion y vuelve a entrar para que GNOME detecte Pop!_Shell."
    fi

    # Blur My Shell — descarga desde extensions.gnome.org
    if gnome-extensions list 2>/dev/null | grep -q "blur-my-shell@aunetx"; then
        echo "  blur-my-shell: ya instalada"
    else
        echo "  Instalando Blur My Shell..."
        GNOME_VER=$(gnome-shell --version 2>/dev/null | awk '{print $3}' | cut -d'.' -f1)
        curl -fsSL "https://extensions.gnome.org/download-extension/blur-my-shell@aunetx.shell-extension.zip?shell_version=${GNOME_VER}" \
            -o /tmp/blur-my-shell.zip \
        && gnome-extensions install --force /tmp/blur-my-shell.zip \
        && rm -f /tmp/blur-my-shell.zip \
        || echo "  Advertencia: no se pudo instalar Blur My Shell, continúa manualmente."
    fi
else
    echo ""
    echo "==> Extensiones GNOME: sesion grafica no detectada — omitiendo."
fi

echo ""
echo "Listo. Ejecuta setup.sh para aplicar las configuraciones."
