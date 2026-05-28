# micro

Configuración del editor de terminal micro, usado principalmente como editor de commits de git.

## Archivos

| Archivo | Destino | Descripción |
|---|---|---|
| `settings.json` | `~/.config/micro/settings.json` | Configuración por tipo de archivo |
| `syntax/gitcommit.yaml` | `~/.config/micro/syntax/gitcommit.yaml` | Detección del tipo `gitcommit` |

Los symlinks los crea `setup.sh` desde la raíz del repo.

## Configuración aplicada

- **`colorcolumn: 72`** activo únicamente en contexto `gitcommit` (commits, merges, tags) — muestra una guía visual en la columna 72, que es el límite recomendado para el cuerpo del mensaje.
- En cualquier otro archivo micro se comporta sin modificaciones.

## Convención de commits

| Parte | Límite |
|---|---|
| Título (primera línea) | 50 caracteres |
| Cuerpo (líneas siguientes) | 72 caracteres |
