#!/bin/bash
set -e

# ==============================================================================
# Instalacion de Pop!_Shell
# ==============================================================================
# GNOME no detecta la extension automaticamente despues de instalar el paquete.
# Es necesario cerrar sesion y volver a entrar antes de habilitar la extension.

if [[ "$1" == "--keybindings" ]]; then
    echo "Aplicando keybindings..."
else
    echo "Instalando Pop!_Shell..."
    if command -v dnf &>/dev/null; then
        sudo dnf install -y gnome-shell-extension-pop-shell
    elif command -v apt &>/dev/null; then
        sudo apt install -y gnome-shell-extension-pop-shell
    else
        echo "ERROR: Gestor de paquetes no soportado (se requiere dnf o apt)."
        exit 1
    fi
    echo ""
    echo "IMPORTANTE: Cierra sesion y vuelve a entrar antes de continuar."
    echo "Luego ejecuta este script de nuevo con: bash restore.sh --keybindings"
    echo ""
    exit 0
fi

# ==============================================================================
# Habilitar la extension
# ==============================================================================
echo "Habilitando pop-shell..."
gnome-extensions enable pop-shell@system76.com

# ==============================================================================
# Pop!_Shell — keybindings
# Optimizado para teclado 60% (sin flechas): usa j/k/l/i como direcciones.
# Las flechas funcionan con los mismos modificadores — comportamiento identico.
# Disposicion: j=izquierda, k=abajo, l=derecha, i=arriba
# ==============================================================================

echo "Configurando keybindings de Pop!_Shell..."

# Foco de ventana — Super+arrow/jkli
gsettings set org.gnome.shell.extensions.pop-shell focus-left  "['<Super>Left',  '<Super>KP_Left',  '<Super>j']"
gsettings set org.gnome.shell.extensions.pop-shell focus-down  "['<Super>Down',  '<Super>KP_Down',  '<Super>k']"
gsettings set org.gnome.shell.extensions.pop-shell focus-right "['<Super>Right', '<Super>KP_Right', '<Super>l']"
gsettings set org.gnome.shell.extensions.pop-shell focus-up    "['<Super>Up',    '<Super>KP_Up',    '<Super>i']"

# Mover tile en adjustment mode (Super+Enter para entrar) — flechas o Super+jkli
gsettings set org.gnome.shell.extensions.pop-shell tile-move-left  "['Left',  '<Super>j']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-down  "['Down',  '<Super>k']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-right "['Right', '<Super>l']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-up    "['Up',    '<Super>i']"

# Mover tile globalmente sin adjustment mode — Super+Shift+arrow/jkli
gsettings set org.gnome.shell.extensions.pop-shell tile-move-left-global  "['<Super><Shift>Left',  '<Super><Shift>KP_Left',  '<Super><Shift>j']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-down-global  "['<Super><Shift>Down',  '<Super><Shift>KP_Down',  '<Super><Shift>k']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-right-global "['<Super><Shift>Right', '<Super><Shift>KP_Right', '<Super><Shift>l']"
gsettings set org.gnome.shell.extensions.pop-shell tile-move-up-global    "['<Super><Shift>Up',    '<Super><Shift>KP_Up',    '<Super><Shift>i']"

# Redimensionar tile en adjustment mode — Shift+arrow/jkli
gsettings set org.gnome.shell.extensions.pop-shell tile-resize-left  "['<Shift>Left',  '<Shift>KP_Left',  '<Shift>j']"
gsettings set org.gnome.shell.extensions.pop-shell tile-resize-down  "['<Shift>Down',  '<Shift>KP_Down',  '<Shift>k']"
gsettings set org.gnome.shell.extensions.pop-shell tile-resize-right "['<Shift>Right', '<Shift>KP_Right', '<Shift>l']"
gsettings set org.gnome.shell.extensions.pop-shell tile-resize-up    "['<Shift>Up',    '<Shift>KP_Up',    '<Shift>i']"

# Mover ventana a otro monitor — Ctrl+Alt+Super+arrow (sin letra equivalente)
gsettings set org.gnome.shell.extensions.pop-shell pop-monitor-left  "['<Control><Alt><Super>Left',  '<Control><Alt><Super>KP_Left']"
gsettings set org.gnome.shell.extensions.pop-shell pop-monitor-right "['<Control><Alt><Super>Right', '<Control><Alt><Super>KP_Right']"
gsettings set org.gnome.shell.extensions.pop-shell pop-monitor-up    "['<Control><Alt><Super>Up',    '<Control><Alt><Super>KP_Up']"
gsettings set org.gnome.shell.extensions.pop-shell pop-monitor-down  "['<Control><Alt><Super>Down',  '<Control><Alt><Super>KP_Down']"

# ==============================================================================
# GNOME — keybindings nativos
# Los workspaces en GNOME moderno son horizontales (left/right).
# ==============================================================================

echo "Configurando keybindings nativos de GNOME..."

# Cambiar de workspace — Ctrl+Super+arrow/jl
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left  "['<Control><Super>Left',  '<Control><Super>j']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Control><Super>Right', '<Control><Super>l']"
# Limpiar j/i de up/down para evitar conflicto con el layout jkli (j=izq, no j=abajo)
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down  "['<Control><Super>Down',  '<Control><Super>KP_Down']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up    "['<Control><Super>Up',    '<Control><Super>KP_Up']"

# Mover ventana a otro workspace — Ctrl+Shift+Super+arrow/jkli
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left  "['<Control><Shift><Super>Left',  '<Control><Shift><Super>j']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Control><Shift><Super>Right', '<Control><Shift><Super>l']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-up    "['<Control><Shift><Super>Up',    '<Control><Shift><Super>i']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-down  "['<Control><Shift><Super>Down',  '<Control><Shift><Super>k']"

# ==============================================================================
# Dash to Panel — lanzar apps por numero (Super+1..9)
# ==============================================================================

# toggle-tiled-left usa Ctrl+Super+Left por defecto, lo que bloquea switch-to-workspace-left
gsettings set org.gnome.mutter.keybindings toggle-tiled-left "[]"

echo "Configurando Super+W para mostrar escritorios..."
gsettings set org.gnome.shell.keybindings toggle-overview "['<Super>w']"

echo "Configurando Super+numero para lanzar apps del panel..."

for i in $(seq 1 9); do
    gsettings set org.gnome.shell.keybindings "switch-to-application-$i" "['<Super>$i']"
done

echo ""
echo "Listo. Todos los keybindings configurados."
