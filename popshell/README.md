# Pop!_Shell

Configuración de Pop!_Shell para Fedora con keybindings optimizados para teclado 60% (sin flechas).
Las flechas y las letras `j/k/l/i` usan exactamente los mismos modificadores — comportamiento idéntico.

## Dependencias

| Software | Fedora | Ubuntu |
|---|---|---|
| Pop!_Shell | `gnome-shell-extension-pop-shell` | `gnome-shell-extension-pop-shell` |

```bash
# Fedora
sudo dnf install gnome-shell-extension-pop-shell
```

Los keybindings se aplican automáticamente con `setup.sh` desde la raíz del repo (si la extensión está activa).

## Uso manual

```bash
# Primera ejecución: instala el paquete
bash system76style.sh

# Cerrar sesión y volver a entrar, luego:
bash system76style.sh --keybindings
```

## Keybindings

Dirección: `j=←  k=↓  l=→  i=↑`

| Acción | Modificador | Flechas | Teclado 60% |
|---|---|---|---|
| Foco de ventana | `Super` | `Super+←↓→↑` | `Super+j/k/l/i` |
| Redimensionar tile *(adjustment mode)* | `Shift` | `Shift+←↓→↑` | `Shift+j/k/l/i` |
| Mover tile globalmente | `Super+Shift` | `Super+Shift+←↓→↑` | `Super+Shift+j/k/l/i` |
| Cambiar workspace | `Ctrl+Super` | `Ctrl+Super+←→` | `Ctrl+Super+j/l` |
| Mover ventana a workspace | `Ctrl+Shift+Super` | `Ctrl+Shift+Super+←↓→↑` | `Ctrl+Shift+Super+j/k/l/i` |
| Mover ventana a monitor | `Ctrl+Alt+Super` | `Ctrl+Alt+Super+←↓→↑` | — |

### Otros atajos de Pop!_Shell

| Acción | Atajo |
|---|---|
| Entrar a adjustment mode | `Super+Enter` |
| Activar/desactivar tiling | `Super+y` |
| Activar/desactivar floating | `Super+g` |
| Toggle stacking | `Super+s` |

### GNOME

| Acción | Atajo |
|---|---|
| Vista general (overview) | `Super+w` |
| Lanzar/enfocar app 1-9 del dash | `Super+1` … `Super+9` |

## Notas

- Los workspaces en GNOME moderno son **horizontales**, por eso cambiar workspace solo aplica a izquierda/derecha.
- El paquete `gnome-shell-extension-pop-shell` está en los repos de Fedora — no hace falta compilar desde GitHub.
- Después de instalar el paquete, GNOME no detecta la extensión hasta cerrar sesión y volver a entrar.
- Mover ventana a monitor no tiene equivalente en letras para no crear conflictos con los otros modificadores.
