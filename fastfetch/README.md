# fastfetch

Configuración de fastfetch, que muestra un resumen del sistema junto al logo de la distro.

## Archivos

| Archivo | Destino | Descripción |
|---|---|---|
| `config.jsonc` | `~/.config/fastfetch/config.jsonc` | Módulos y formato de salida |

El paquete lo instala `install-deps.sh` y el symlink lo crea `setup.sh`, ambos desde la raíz del repo.

## Configuración aplicada

- **Módulos:** título, sistema, kernel, uptime, paquetes, shell, escritorio, gestor de ventanas, terminal, CPU, GPU, memoria, disco, bluetooth y paleta de colores.
- **`bluetooth`** solo aparece si hay dispositivos conectados; fastfetch oculta los módulos sin datos. Para ver el error: `fastfetch -s bluetooth --show-errors`.

## Versión 2.69+

Fedora trae la 2.68, así que la config no usa opciones de la 2.69 (una versión anterior podría rechazarlas como desconocidas). Al actualizar se puede agregar:

- `logo.animationFrame: 0` — reproduce un logo GIF/APNG en kitty (requiere un archivo propio).
- `logo.position: "auto"` — pone el logo arriba en terminales angostas.
- `logo.cache` — reemplaza a `logo.recache`.
