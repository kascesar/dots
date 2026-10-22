"""Lee en voz alta el chat de Twitch, ignorando bots y comandos.

Se conecta al chat como espectador anónimo (no necesita token) y lee cada
mensaje con Piper TTS. El audio sale como la app "chat-tts" de PipeWire,
así que OBS puede capturarlo aparte con "Captura de audio de aplicación".
"""

import argparse
import json
import os
import queue
import re
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time
import wave
from pathlib import Path

from piper import PiperVoice, SynthesisConfig

# venv y voces viven fuera del repo (los crea install.sh)
DATA = Path(os.environ.get("XDG_DATA_HOME", Path.home() / ".local/share")) / "chat-tts"
CONFIG = Path(os.environ.get("XDG_CONFIG_HOME", Path.home() / ".config")) / "chat-tts"
BOTS_FILE = CONFIG / "bots.txt"

URL = re.compile(r"https?://\S+|www\.\S+", re.I)
REPEATS = re.compile(r"(.)\1{3,}")


def load_bots():
    if not BOTS_FILE.exists():
        print(f"Aviso: no existe {BOTS_FILE}, no se ignorará ningún bot", file=sys.stderr)
        return set()
    lines = (l.strip().lower() for l in BOTS_FILE.read_text().splitlines())
    return {l for l in lines if l and not l.startswith("#")}


def parse_tags(raw):
    return dict(kv.split("=", 1) if "=" in kv else (kv, "") for kv in raw.split(";"))


def strip_emotes(text, emotes_tag):
    """Quita los emotes de Twitch (Kappa, LUL, ...) usando las posiciones del tag."""
    if not emotes_tag:
        return text
    spans = []
    for emote in emotes_tag.split("/"):
        for pos in emote.split(":", 1)[1].split(","):
            start, end = pos.split("-")
            spans.append((int(start), int(end)))
    chars = list(text)
    for start, end in spans:
        for i in range(start, min(end + 1, len(chars))):
            chars[i] = ""
    return "".join(chars)


def clean(text, max_chars):
    text = URL.sub("un enlace", text)
    text = REPEATS.sub(r"\1\1\1", text)
    text = " ".join(text.split())
    if len(text) > max_chars:
        text = text[:max_chars].rsplit(" ", 1)[0] + "..."
    return text


def is_bot(user, bots, args):
    user = user.lower()
    if user in bots or user == args.channel.lower() and args.skip_self:
        return True
    return args.skip_bot_suffix and user.endswith("bot")


def chat(args, out):
    """Lee el IRC de Twitch y pone (usuario, mensaje) en la cola."""
    while True:
        try:
            sock = socket.create_connection(("irc.chat.twitch.tv", 6667), timeout=330)
            sock.sendall(b"CAP REQ :twitch.tv/tags\r\n")
            sock.sendall(b"NICK justinfan%d\r\n" % (int(time.time()) % 100000))
            sock.sendall(f"JOIN #{args.channel.lower()}\r\n".encode())
            print(f"Conectado al chat de {args.channel}", flush=True)
            buf = b""
            while True:
                data = sock.recv(4096)
                if not data:
                    raise ConnectionError("conexión cerrada")
                buf += data
                *lines, buf = buf.split(b"\r\n")
                for line in lines:
                    line = line.decode("utf-8", "replace")
                    if line.startswith("PING"):
                        sock.sendall(b"PONG :tmi.twitch.tv\r\n")
                    elif " PRIVMSG " in line:
                        tags, rest = line[1:].split(" ", 1) if line.startswith("@") else ("", line)
                        tags = parse_tags(tags)
                        login = rest[1:].split("!", 1)[0]
                        message = rest.split(" :", 1)[1]
                        out.put((login, tags.get("display-name") or login, tags.get("emotes", ""), message))
        except (OSError, ConnectionError) as err:
            print(f"Chat desconectado ({err}), reconectando en 5 s...", file=sys.stderr, flush=True)
            time.sleep(5)


class Ducker:
    """Baja el volumen de las demás apps (el juego) mientras habla la voz.

    Guarda el volumen original de cada stream y lo devuelve tal cual al terminar.
    """

    def __init__(self, factor):
        self.factor = factor
        self.saved = {}

    def duck(self):
        if self.factor >= 1 or self.saved:
            return
        out = subprocess.run(["pactl", "-f", "json", "list", "sink-inputs"],
                             capture_output=True, text=True, check=False).stdout
        for stream in json.loads(out or "[]"):
            if stream["properties"].get("application.name") == "chat-tts":
                continue
            values = [ch["value"] for ch in stream["volume"].values()]
            self.saved[stream["index"]] = values
            self._set(stream["index"], [int(v * self.factor) for v in values])

    def restore(self):
        for index, values in self.saved.items():
            self._set(index, values)
        self.saved = {}

    @staticmethod
    def _set(index, values):
        subprocess.run(["pactl", "set-sink-input-volume", str(index), *map(str, values)],
                       capture_output=True, check=False)


def speak(voice, syn, text, volume):
    with tempfile.NamedTemporaryFile(suffix=".wav") as tmp:
        with wave.open(tmp.name, "wb") as wav:
            voice.synthesize_wav(text, wav, syn_config=syn)
        subprocess.run(
            ["pw-play", "-P", "{ application.name = chat-tts node.description = chat-tts }",
             "--volume", str(volume), tmp.name],
            check=False,
        )


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("channel", nargs="?", default="kascesar", help="canal de Twitch (por defecto: kascesar)")
    p.add_argument("--voz", dest="voice", default="es_MX-claude-high",
                   help=f"voz de Piper en {DATA / 'voices'} (es_MX-claude-high, es_AR-daniela-high, es_MX-ald-medium)")
    p.add_argument("--velocidad", dest="speed", type=float, default=1.0,
                   help="velocidad de lectura; 1.3 = 30%% más rápido")
    p.add_argument("--volumen", dest="volume", type=float, default=1.0, help="0.0 a 1.0")
    p.add_argument("--atenuar", dest="duck", type=float, default=0.6,
                   help="volumen de las demás apps mientras habla (0.6 = 60%%; 1 = no bajar)")
    p.add_argument("--max", dest="max_chars", type=int, default=200, help="largo máximo leído por mensaje")
    p.add_argument("--max-cola", dest="max_queue", type=int, default=5,
                   help="si se acumulan más mensajes, se saltan los más viejos")
    p.add_argument("--sin-nombre", dest="say_name", action="store_false", help="no leer quién escribió")
    p.add_argument("--ignorarme", dest="skip_self", action="store_true",
                   help="no leer los mensajes del dueño del canal")
    p.add_argument("--sufijo-bot", dest="skip_bot_suffix", action="store_true",
                   help="ignorar además a todo usuario cuyo nombre termine en 'bot'")
    args = p.parse_args()

    voice = PiperVoice.load(DATA / "voices" / f"{args.voice}.onnx")
    syn = SynthesisConfig(length_scale=1 / args.speed)
    bots = load_bots()
    print(f"Ignorando {len(bots)} bots (lista en {BOTS_FILE}) y mensajes que empiezan con !", flush=True)

    messages = queue.Queue()
    threading.Thread(target=chat, args=(args, messages), daemon=True).start()
    ducker = Ducker(args.duck)
    # Cerrar la terminal (SIGHUP) o matar el proceso también devuelve el volumen.
    for sig in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(sig, lambda *_: sys.exit(0))

    try:
        while True:
            login, name, emotes, message = messages.get()
            while messages.qsize() > args.max_queue:
                messages.get()
            if message.startswith("!") or is_bot(login, bots, args):
                print(f"(saltado) {name}: {message}", flush=True)
                continue
            if message.startswith("\x01ACTION "):
                message = message[8:].rstrip("\x01")
            text = clean(strip_emotes(message, emotes), args.max_chars)
            if not text.strip(" .,!?¿¡"):
                continue
            print(f"{name}: {text}", flush=True)
            ducker.duck()
            speak(voice, syn, f"{name} dice: {text}" if args.say_name else text, args.volume)
            # Si ya hay otro mensaje esperando, el juego sigue bajo hasta el último.
            if messages.empty():
                ducker.restore()
    finally:
        ducker.restore()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
