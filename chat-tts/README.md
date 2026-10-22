# chat-tts

Lee en voz alta el chat de Twitch mientras hago stream, con voces locales de [Piper TTS](https://github.com/OHF-Voice/piper1-gpl). Todo corre en el equipo: sin cuentas, sin API y sin costo.

- Se conecta al chat como espectador anónimo (no necesita token).
- Ignora a los bots de `bots.txt` y los comandos que empiezan con `!`.
- Quita los emotes de Twitch, lee los links como "un enlace" y recorta los "jajajajjjjj".
- Mientras habla, baja las demás apps (el juego) al 60% de su volumen y luego las devuelve a su volumen.
- Si el chat va muy rápido, salta los mensajes más viejos para no quedar atrasado.

## Dependencias

| Software | Fedora | Ubuntu |
|---|---|---|
| Python 3 + venv | `python3` | `python3-venv` |
| `pw-play` | `pipewire-utils` | `pipewire-bin` |
| `pactl` | `pulseaudio-utils` | `pulseaudio-utils` |

`install.sh` (lo llama `setup.sh`) crea un venv con `piper-tts` y descarga la voz en `~/.local/share/chat-tts/`, fuera del repo porque pesa varios cientos de MB.

> [!WARNING]
> El paquete `piper` de Fedora es el configurador de mouses gamer, no el TTS. No sirve para esto.

## Uso

```bash
chat-on     # alias de: systemctl --user start chat-tts
chat-off    # alias de: systemctl --user stop chat-tts
journalctl --user -u chat-tts -f   # ver en vivo qué lee y qué salta
```

No arranca solo al iniciar sesión; se activa a mano antes de transmitir.

Para probar otras opciones sin el servicio, ejecutar `chat-tts` directamente:

```bash
chat-tts --voz es_AR-daniela-high   # otra voz (hay que descargarla, ver abajo)
chat-tts --velocidad 1.3            # leer 30% más rápido
chat-tts --atenuar 0.25             # bajar más el juego (1 = no bajarlo)
chat-tts --sin-nombre               # no decir "fulano dice:"
chat-tts --ignorarme                # no leer los mensajes del dueño del canal
chat-tts --sufijo-bot               # ignorar además todo usuario que termine en "bot"
chat-tts otrocanal                  # leer otro canal (por defecto: kascesar)
```

Para dejar opciones fijas en el servicio, agregarlas a `ExecStart` en `chat-tts.service` y ejecutar `systemctl --user daemon-reload && systemctl --user restart chat-tts`.

## Voces

Por defecto usa `es_MX-claude-high`. Otras voces en español del [catálogo de Piper](https://huggingface.co/rhasspy/piper-voices/tree/main/es): `es_AR-daniela-high` (femenina) y `es_MX-ald-medium` (masculina). Para usar otra, agregarla a `VOICES` en `install.sh` y volver a ejecutarlo.

## OBS

Si OBS ya captura el audio del escritorio, la voz sale en el stream sin hacer nada. Para controlar su volumen aparte, agregar **Captura de audio de aplicación (PipeWire)** y elegir la app `chat-tts` (aparece solo mientras está hablando).

> [!NOTE]
> La atenuación cambia el volumen en el sistema, así que también se escucha en los audífonos, y afecta a todas las apps que estén sonando (Discord, Spotify...), no solo al juego.

Para mostrar el chat en pantalla, agregar una fuente **Navegador** con `https://giambaj.it/twitch/jchat/v2/?channel=kascesar`. No usar URLs de `dashboard.twitch.tv`: hacen que OBS se caiga en bucle.
