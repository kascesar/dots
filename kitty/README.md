# kitty

Configuración del emulador de terminal kitty con tema de colores personalizado.

## Archivos

| Archivo | Destino | Descripción |
|---|---|---|
| `kitty.conf` | `~/.config/kitty/kitty.conf` | Config principal: splits, keybindings, fuente, cursor |
| `gruvbox-dark-hard.conf` | `~/.config/kitty/gruvbox-dark-hard.conf` | Paleta Gruvbox Dark (hard contrast), la activa |
| `theme.conf` | `~/.config/kitty/theme.conf` | Paleta anterior (fondo como la terminal de GNOME) |

El tema activo se elige con la línea `include` al inicio de `kitty.conf`.

Los symlinks los crea `setup.sh` desde la raíz del repo: enlaza todos los `*.conf` de esta carpeta, así que un tema nuevo solo hay que agregarlo aquí.

## Dependencias

| Software | Fedora | Ubuntu |
|---|---|---|
| `kitty` | `kitty` | `kitty` |
| MesloLGL Nerd Font | via oh-my-posh | via oh-my-posh |

```bash
# Fedora
sudo dnf install kitty

# Ubuntu
sudo apt install kitty

# Fuente (ambos, después de instalar oh-my-posh)
oh-my-posh font install meslo
```

## Keybindings

`kitty_mod` = `Ctrl+Alt`

| Atajo | Acción |
|---|---|
| `kitty_mod+t` | Split horizontal |
| `kitty_mod+v` | Split vertical |
| `kitty_mod+←↓→↑` | Navegar entre splits |
| `Ctrl+Shift+c` | Copiar |
| `Ctrl+Shift+v` | Pegar |
