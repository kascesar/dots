#!/bin/bash
set -e

DOTS_DIR="$(cd "$(dirname "$0")" && pwd)"
EMACS_DIR="$HOME/.emacs.d"

echo "Creando directorio $EMACS_DIR si no existe..."
mkdir -p "$EMACS_DIR"

echo "Creando symlinks..."
ln -sf "$DOTS_DIR/config.org"    "$EMACS_DIR/config.org"
ln -sf "$DOTS_DIR/init.el"       "$EMACS_DIR/init.el"
ln -sf "$DOTS_DIR/early-init.el" "$EMACS_DIR/early-init.el"

echo "Eliminando config.el cacheado si existe..."
rm -f "$EMACS_DIR/config.el"

echo "Instalando fuentes caligráficas (palmer, jamaica)..."
FONT_DIR="$HOME/.local/share/fonts"
mkdir -p "$FONT_DIR"
cp "$DOTS_DIR"/fonts/*.otf "$FONT_DIR/"
fc-cache -f "$FONT_DIR" >/dev/null

echo ""
echo "Listo. Abre Emacs — elpaca instalará los paquetes automáticamente."
