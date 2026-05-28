# bash

Configuración del shell: `.bashrc`, `.bash_aliases` y prompt con oh-my-posh.

## Archivos

| Archivo | Destino | Descripción |
|---|---|---|
| `.bashrc` | `~/.bashrc` | Exporta variables de entorno, carga aliases y prompt |
| `.bash_aliases` | `~/.bash_aliases` | Aliases para `cat`, `ls` |
| `.posh_config.yaml` | `~/.posh_config.yaml` | Tema de oh-my-posh |

Los symlinks los crea `setup.sh` desde la raíz del repo.

## Dependencias

| Software | Fedora | Ubuntu | Uso |
|---|---|---|---|
| `bat` | `bat` | `bat` (binario: `batcat`) | Alias de `cat` con syntax highlight |
| `eza` | `eza` | `eza` | Alias de `ls` con iconos |
| `oh-my-posh` | via curl | via curl | Prompt personalizado |

```bash
# Fedora
sudo dnf install bat eza

# Ubuntu
sudo apt install bat eza

# oh-my-posh (ambos)
curl -s https://ohmyposh.dev/install.sh | bash -s -- -d ~/.local/bin
oh-my-posh font install meslo
```

> En Fedora el binario es `bat`; en Ubuntu es `batcat`. El `.bash_aliases` detecta cuál está disponible automáticamente.
