#!/bin/bash
set -e

DOTS_DIR="$(cd "$(dirname "$0")" && pwd)"
DATA="${XDG_DATA_HOME:-$HOME/.local/share}/chat-tts"
VOICES_URL="https://huggingface.co/rhasspy/piper-voices/resolve/main"

# Voces de Piper que se descargan; la primera es la que usa chat_tts.py por defecto.
# Formato: <ruta en el repo de voces>/<nombre>
VOICES=(
    "es/es_MX/claude/high/es_MX-claude-high"
)

# Python fijo vía uv: onnxruntime (dependencia de Piper) tarda en sacar wheels
# para cada Python nuevo, y un venv atado a /usr/bin/python3 se rompe cuando
# Fedora lo actualiza (pasó con 3.14 -> 3.15).
PYTHON_VERSION="3.14"

echo "Creando venv con Piper TTS en $DATA/venv (Python $PYTHON_VERSION vía uv)..."
mkdir -p "$DATA/voices"
if ! "$DATA/venv/bin/python" -c "import sys; assert sys.version.startswith('$PYTHON_VERSION')" 2>/dev/null; then
    rm -rf "$DATA/venv"
    uv venv -q --python "$PYTHON_VERSION" "$DATA/venv"
fi
VIRTUAL_ENV="$DATA/venv" uv pip install -q piper-tts

echo "Descargando voces..."
for voice in "${VOICES[@]}"; do
    name="$(basename "$voice")"
    for ext in onnx onnx.json; do
        if [ -f "$DATA/voices/$name.$ext" ]; then
            echo "  $name.$ext: ya descargada"
        else
            curl -fsSL -o "$DATA/voices/$name.$ext" "$VOICES_URL/$voice.$ext"
            echo "  $name.$ext"
        fi
    done
done

echo "Creando symlinks..."
mkdir -p "$HOME/.local/bin" "$HOME/.config/chat-tts" "$HOME/.config/systemd/user"
ln -sf "$DOTS_DIR/chat-tts"         "$HOME/.local/bin/chat-tts"
ln -sf "$DOTS_DIR/bots.txt"         "$HOME/.config/chat-tts/bots.txt"
ln -sf "$DOTS_DIR/chat-tts.service" "$HOME/.config/systemd/user/chat-tts.service"
systemctl --user daemon-reload

echo ""
echo "Listo. Actívalo con 'chat-on' (o: systemctl --user start chat-tts)."
