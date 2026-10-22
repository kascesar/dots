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

echo "Creando venv con Piper TTS en $DATA/venv..."
mkdir -p "$DATA/voices"
if [ ! -x "$DATA/venv/bin/python" ]; then
    python3 -m venv "$DATA/venv"
fi
"$DATA/venv/bin/pip" install -q --disable-pip-version-check piper-tts

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
