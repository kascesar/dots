# dots

Configuraciones personales de flujo de trabajo. Diseñado para funcionar de forma descentralizada — los archivos críticos son symlinks al repo, por lo que un `git pull` en cualquier equipo aplica los cambios inmediatamente.

## Inicio rápido

```bash
# 1. Instalar dependencias de software (solo al formatear el equipo)
bash install-deps.sh

# 2. Aplicar configuraciones y symlinks
bash setup.sh
```

## Estructura

| Carpeta | Contenido |
|---|---|
| `bash/` | `.bashrc`, `.bash_aliases`, configuración de oh-my-posh |
| `emacs/` | Config literate en `config.org`, script de symlinks |
| `git/` | `.gitconfig` con delta como pager |
| `kitty/` | `kitty.conf` y tema de colores |
| `micro/` | Editor de terminal, configurado como editor de commits |
| `popshell/` | Keybindings de Pop!_Shell para teclado 60% |
| `qmk/` | Firmware de teclado (independiente del resto) |

Cada carpeta tiene su propio README con detalles de dependencias y configuración específica.

## Cómo funciona

- `setup.sh` — crea los symlinks de cada config al lugar correcto del OS. Seguro de correr múltiples veces; hace backup de archivos preexistentes (`.bak`).
- `install-deps.sh` — instala el software necesario. Soporta `dnf` (Fedora) y `apt` (Ubuntu/Debian).
- Emacs **no se instala** con el script — ver `emacs/README.md` para compilarlo manualmente.
