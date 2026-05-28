# git

Configuración global de git con `delta` como pager para diffs mejorados.

## Archivos

| Archivo | Destino |
|---|---|
| `.gitconfig` | `~/.gitconfig` |

El symlink lo crea `setup.sh` desde la raíz del repo.

## Dependencias

| Software | Fedora | Ubuntu |
|---|---|---|
| `git-delta` | `git-delta` | `git-delta` |

```bash
# Fedora
sudo dnf install git-delta

# Ubuntu
sudo apt install git-delta
```

## Configuración aplicada

- Pager: `delta` con modo `side-by-side` y tema claro
- `diffFilter`: delta en modo color-only para `git add -p`
- `merge.conflictstyle`: `zdiff3`
- Rama por defecto: `main`
