# yazi

Gestor de archivos de terminal. Se invoca con `y` en lugar de `yazi` para que al salir el shell cambie al directorio donde quedó yazi.

## Archivos

| Archivo | Destino |
|---|---|
| `yazi.toml` | `~/.config/yazi/yazi.toml` |
| `keymap.toml` | `~/.config/yazi/keymap.toml` |

Los symlinks los crea `setup.sh` desde la raíz del repo.

## Comportamiento configurado

- **Editor:** `micro` (bloquea yazi hasta cerrar el editor)
- **Enter en carpeta:** abre el gestor de archivos del sistema vía `xdg-open`
- **Otros archivos:** `xdg-open` con la aplicación por defecto del sistema

## Keybindings (teclado 60%)

Dirección: `j=←  k=↓  l=→  i=↑` — mismo layout que Pop!_Shell y el resto del sistema.
Las flechas siguen funcionando igual en cualquier teclado.

| Tecla | Acción |
|---|---|
| `j` / `←` | Subir al directorio padre |
| `k` / `↓` | Archivo siguiente |
| `l` / `→` | Entrar al directorio / abrir archivo |
| `i` / `↑` | Archivo anterior |

## Dependencias

Yazi requiere las siguientes herramientas para funcionalidad completa:

| Herramienta | Uso |
|---|---|
| `file` | Detección de tipo de archivo |
| `ffmpeg` | Thumbnails de video |
| `7z` | Preview y extracción de archivos comprimidos |
| `jq` | Preview de JSON |
| `fd` | Búsqueda de archivos |
| `rg` | Búsqueda de contenido |
| `fzf` | Navegación rápida por subárboles |
| `zoxide` | Navegación por historial de directorios |
| `wl-copy` | Clipboard en Wayland |
| `resvg` | Preview de SVG |
| `magick` | Preview de HEIC, JPEG XL y fuentes |

Todas se instalan con `install-deps.sh`.

## Instalación de yazi

Yazi no está en los repos estándar — se instala vía repositorio externo (ver `install-deps.sh`).
