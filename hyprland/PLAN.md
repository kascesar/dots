# Plan: Hyprland como sesión paralela a GNOME

## Contexto
Pop!_Shell está en riesgo con GNOME 51: el paquete declara compatibilidad solo hasta GNOME 50 y System76 ya no lo impulsa. En la investigación, Hyprland 0.57 ganó en tiling, configuración con scripts y efectos; GNOME ganó en HDR y en estabilidad.

Objetivo: instalar Hyprland como **sesión adicional en GDM**, sin tocar GNOME. Usarla unos días, medir el impacto con datos y recién ahí decidir. Queda decidido:
- Componentes **mínimos DIY**: todo de los repos de Fedora, salvo Hyprland.
- Un **script de medición** para comparar las dos sesiones.

Equipo: Lenovo 81FB (Ryzen Raven Ridge, Vega iGPU), 10 GB RAM, eDP SDR, Fedora 44 / GNOME 50.6. Teclado 60% con jkli. Layouts xkb: `latam, es, us`. Tema oscuro Adwaita. Touchpad sin natural scroll.

---

## Impacto en el sistema (qué cambia y qué no)

| Área | Cambio | Riesgo |
|---|---|---|
| Paquetes | +1 COPR (Hyprland, xdg-desktop-portal-hyprland, hyprpolkitagent, hyprlock, hypridle, hyprpaper) y desde Fedora: waybar, fuzzel, mako, grim, slurp, brightnessctl, playerctl, network-manager-applet. Unos 150–250 MB | Bajo. No reemplaza nada de GNOME |
| GDM | Aparece "Hyprland" en el selector de sesión (ícono de engranaje) | Ninguno; GNOME sigue siendo la sesión por defecto |
| `~/.config` | Nuevos `hypr/`, `waybar/`, `mako/`, `fuzzel/` (symlinks al repo) | Ninguno; GNOME no los lee |
| Configs compartidas | kitty, bash, yazi, emacs, gtk (dconf) se usan igual en las dos sesiones | GTK lee el tema y el modo oscuro desde dconf, así que se ve igual |
| Portales | Hyprland usa `xdg-desktop-portal-hyprland` + `-gtk` (para el selector de archivos). GNOME sigue con `-gnome`. Se separan solos por `XDG_CURRENT_DESKTOP` | Bajo. Si compartir pantalla falla, revisar `hyprland-portals.conf` |
| Keyring | GDM desbloquea gnome-keyring vía PAM también en la sesión Hyprland | Bajo |
| Servicios de usuario | `chat-tts.service` (systemd --user) corre igual en las dos sesiones | Ninguno |
| Rollback | `dnf remove` de los paquetes + `dnf copr disable` + borrar `hyprland/` y los symlinks | Total, en unos 2 minutos |

**Lo que se pierde dentro de la sesión Hyprland** (GNOME no se ve afectado):
- El overview de GNOME (Super+W).
- Super+1..9 abriendo apps del dash. En Hyprland pasan a ser workspaces 1..9.
- El panel de Ajustes de GNOME para monitores y wifi. Se reemplaza con `hyprctl`/Lua, nm-applet y `nm-connection-editor`.
- Super+Y (activar/desactivar tiling): no hay equivalente directo.

---

## Archivos del repo

Módulo nuevo `hyprland/`, que replica el patrón de `popshell/`:
```
hyprland/
  README.md          # dependencias, tabla de keybindings, mantenimiento, rollback
  hyprland.lua       # monitores, input, look&feel, autostart y requires
  keybinds.lua       # tu esquema jkli traducido (separado para leerlo fácil)
  hypridle.conf
  hyprlock.conf
  hyprpaper.conf
  waybar/config.jsonc
  waybar/style.css
  mako/config
  fuzzel/fuzzel.ini
  bench.sh           # script de medición (más abajo)
```

Cambios en scripts existentes:
- `install-deps.sh`: agregar una sección `==> Hyprland...`.
  - Solo cuando `PM=dnf` (en Ubuntu se omite con un mensaje).
  - Sigue el mismo patrón que el bloque de yazi con COPR: `sudo dnf copr enable -y <copr>` y luego `try_install`.
  - Termina con `sudo dnf versionlock add hyprland xdg-desktop-portal-hyprland` (dnf5 trae versionlock; ya está verificado).
- `setup.sh`: agregar una sección `==> hyprland...` que usa la función `symlink()` que ya existe:
  - `hyprland/*.lua` → `~/.config/hypr/`
  - `hyprland/hypr{idle,lock,paper}.conf` → `~/.config/hypr/`
  - `waybar`, `mako` y `fuzzel` → `~/.config/<app>/`
- `README.md` (raíz): una línea en la tabla de módulos.

COPR: al implementar, elegir el que tenga **0.57.x construido para fedora-44**. Primera opción `lionheartp/Hyprland`; alternativa `mpapacc/hyprland`. El de `solopasha` está abandonado.

---

## Contenido clave de la config (sintaxis Lua 0.57, verificada contra el `example/hyprland.lua` de upstream y la wiki)

`hyprland.lua`:
- `hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })`
- `hl.config({ input = { kb_layout = "latam,es,us", kb_options = "grp:..." , touchpad = { natural_scroll = false } } })`. Elegir con el usuario la tecla para cambiar de layout, o un bind con `hyprctl switchxkblayout`.
- `general.layout = "dwindle"`, gaps y bordes moderados.
- `decoration.blur`: activado pero liviano (`size=3, passes=1`), porque la Vega es débil. Medirlo después.
- Autostart con `hl.on("hyprland.start", function() hl.exec_cmd("waybar") ... end)`, para waybar, mako, hyprpaper, hypridle, hyprpolkitagent y nm-applet.
- `hl.env("XCURSOR_SIZE","24")`, `QT_QPA_PLATFORM=wayland;xcb`.
- Regla de ventana: la clase `cheatsheet` va flotante y centrada (es la ventana kitty que abre `cheatsheet/cheatsheet`).
- `require("keybinds")`.

`keybinds.lua`: traducción 1:1 de `popshell/system76style.sh` (`j=←, k=↓, l=→, i=↑`, con las flechas en paralelo):

| Acción (Pop!_Shell) | Tecla | Dispatcher Hyprland |
|---|---|---|
| Foco | `Super+jkli` | `hl.dsp.focus({ direction = "l/d/r/u" })` |
| Mover tile | `Super+Shift+jkli` | `hl.dsp.window.move({ direction = ... })` |
| Adjustment mode | `Super+Enter` | `hl.define_submap("ajuste", ...)`: adentro, `Shift+jkli` = `hl.dsp.window.resize({x,y,relative=true})` con `{ repeating = true }`, `jkli` = mover, `Escape/Return` = `hl.dsp.submap("reset")` |
| Workspace ←/→ | `Ctrl+Super+j/l` | `hl.dsp.focus({ workspace = "e-1" / "e+1" })` |
| Mover a workspace | `Ctrl+Shift+Super+j/l` | `hl.dsp.window.move({ workspace = "e-1"/"e+1" })` |
| Mover a monitor | `Ctrl+Alt+Super+jkli` | `hl.dsp.window.move({ monitor = "l/r/u/d" })` |
| Flotante | `Super+g` | `hl.dsp.window.float({ action = "toggle" })` |
| Stacking | `Super+s` | `hl.dsp.group.toggle()` |
| Cheatsheet | `Super+h` | `hl.dsp.exec_cmd("~/.local/bin/cheatsheet")` |
| Launcher | `Super+Space` | `exec_cmd("fuzzel")` |
| Terminal | `Super+Return`? | **choque con adjustment**: usar `Super+t` para kitty |
| Workspaces 1..9 | `Super+1..9` | `focus({ workspace = i })` (cambia el significado respecto a GNOME) |
| Media / brillo | teclas XF86 | copiar tal cual del ejemplo upstream (wpctl, brightnessctl, playerctl) |
| Captura | `Print` | `grim -g "$(slurp)" - \| wl-copy` |
| Bloquear | `Super+Escape` | `exec_cmd("hyprlock")` |

Ctrl+Super+i/k (workspace arriba/abajo en GNOME) no tiene sentido en Hyprland, porque sus workspaces son una lista lineal por monitor. Se dejan sin asignar.

---

## Script de medición `hyprland/bench.sh`
Bash corto, sin dependencias nuevas:
1. Avisa si hay corriente conectada (lee `/sys/class/power_supply/ADP1/online`). La medición de batería necesita estar desenchufado.
2. Espera 60 s para que el sistema quede en reposo.
3. Durante N segundos (300 por defecto) toma una muestra por segundo de:
   - `power_now` de `BAT1` (µW → W)
   - CPU total desde `/proc/stat`
   - RAM usada desde `/proc/meminfo`
   - RSS de los procesos de la sesión (`gnome-shell` o `Hyprland`, `waybar`, `mako`, etc., vía `ps`)
4. Imprime una línea de resumen con `$XDG_CURRENT_DESKTOP`, W promedio, %CPU promedio, RAM y RSS de la sesión. También la agrega a `~/.local/state/de-bench.csv` para comparar después.

Protocolo: mismo brillo, wifi encendido y las mismas apps abiertas (kitty + navegador con una pestaña). Correrlo 2 veces por sesión. En Hyprland, una corrida con blur y otra con `hyprctl keyword decoration:blur:enabled false`, para aislar cuánto cuesta el blur.

---

## Qué significa mantenerlo

| Tarea | Frecuencia | Esfuerzo |
|---|---|---|
| Actualizar Hyprland: quitar el versionlock, leer las notas de la versión, actualizar, revisar el log, volver a bloquear | Cada release menor, aprox. 6–8 semanas. **Esperar al .1 o .2** | 15–30 min. Algunas veces hay que ajustar el Lua (la API viene cambiando de 0.55 a 0.57) |
| Waybar, mako, fuzzel | Llegan con `dnf upgrade` | Casi nada |
| Esperar que el COPR publique la versión nueva | Cada salida de Fedora (F45 ahora) | Si el COPR se atrasa, quedarse en GNOME unos días. El versionlock evita que se rompa sin aviso |
| Plugins (hyprexpo, etc.) | **Ninguno en este plan.** Hay que recompilarlos con cada versión | Se agregan solo si después hacen falta |
| Pop!_Shell en GNOME | Igual que ahora | Igual que ahora |

Regla práctica: nunca actualizar Hyprland si no tienes tiempo para revisarlo. GNOME queda siempre como sesión de respaldo.

Criterio para decidir a las 1–2 semanas:
- Comparar el CSV de `bench.sh`.
- Revisar si el flujo jkli se siente igual o mejor.
- Ver cuántas veces tuviste que volver a GNOME por algo que faltaba.

---

## Fases de implementación
1. `hyprland/` con `hyprland.lua` y `keybinds.lua` mínimos, más la sección en `install-deps.sh`. Instalar y abrir la sesión: tiling y keybindings funcionando.
2. waybar, mako, fuzzel, hyprlock, hypridle, hyprpaper y su autostart.
3. Agregar los symlinks en `setup.sh`. `README.md` del módulo con la tabla de keybindings, el mantenimiento y el rollback.
4. `bench.sh` y las mediciones en las dos sesiones.
5. Opcional, después de medir: efectos (wobble de la 0.57, animaciones, más blur).

## Verificación
- Antes de iniciar sesión: `Hyprland --verify-config` o equivalente (si no existe en 0.57, mirar los errores con `hyprctl configerrors` dentro de la sesión).
- Dentro de la sesión:
  - `hyprctl configerrors` vacío.
  - Probar cada fila de la tabla de keybindings, con flechas y con jkli.
  - Abrir 3 ventanas y probar el submap de ajuste.
  - Compartir pantalla en el navegador para probar el portal.
  - Abrir un diálogo de archivos (portal gtk).
  - Ejecutar algo con `pkexec` (agente polkit).
  - Bloquear y desbloquear (hyprlock).
  - Teclas de volumen y brillo.
  - `Super+h` abre el cheatsheet flotante.
- Cerrar sesión, entrar a GNOME y confirmar que Pop!_Shell, la extensión de blur y los atajos siguen iguales (`bash popshell/system76style.sh` no debería cambiar nada).
- `bash hyprland/bench.sh` en las dos sesiones deja filas en el CSV.
