# Emacs — configuración personal

Config principal en `config.org` (org-babel literate config), cargada desde `init.el`.
Gestor de paquetes: **elpaca**.

## Instalación

```bash
bash install.sh
```

El script crea los symlinks necesarios para que Emacs cargue la config desde este repo.
Si es una instalación nueva, compilar Emacs 31 primero (ver sección al final).

---

## Keybindings globales personalizados

| Atajo | Acción |
|---|---|
| `M-j/k/l/i` | Mover cursor ←↓→↑ (teclado 60%) |
| `M-J/K/L/I` | Cambiar ventana ←↓→↑ (teclado 60%) |
| `M-←/↓/→/↑` | Cambiar ventana (flechas) |
| `C-z` | Undo (reemplaza suspend-frame) |
| `C-+` / `C--` | Aumentar / disminuir fuente |
| `C-scroll` | Zoom con mouse |
| `C-x k` | Cerrar buffer actual (sin preguntar) |
| `F7` | Abrir org-agenda |
| `F10` | Abrir Dashboard |
| `C-c r` | Archivos recientes (consult) |
| `C-c i` | Toggle imenu-list lateral |
| `C-c l/a/c` | org-store-link / org-agenda / org-capture |

### Navegación en Dired

| Atajo | Acción |
|---|---|
| `i` | Línea anterior |
| `k` | Línea siguiente |
| `l` | Abrir archivo o directorio |
| `j` | Subir al directorio padre |

### Navegación en Dashboard

| Atajo | Acción |
|---|---|
| `i` | Ítem anterior |
| `k` | Ítem siguiente |
| `l` | Abrir ítem |

---

## Búsqueda y navegación (Consult + Vertico)

| Atajo | Acción |
|---|---|
| `C-s` | Buscar en buffer actual |
| `C-S` | Buscar en todos los buffers |
| `C-x b` | Cambiar buffer con preview |
| `M-s M-g` | Grep recursivo en proyecto |
| `M-s M-f` | Buscar archivos recursivamente |
| `M-s M-o` | Buscar por heading/outline |

En el mini-buffer de `consult-buffer`: `b SPC` filtra buffers, `f` archivos, `p` proyectos, `m SPC` bookmarks.

---

## Org-mode

### Estados de tareas (en español)
`PORHACER` → `ENPROCESO` → `BLOQUEADO` / `DETENIDO` → `HECHO` / `ARCHIVAR`

Cada cambio de estado queda registrado con timestamp en un drawer.
Al hacer refile se guardan todos los buffers org automáticamente.

### Tags
`@nota` `@casa` `@finanzas` `@fecha` `@salud` `@tarea` `@coche` `@trabajo` `@personal` `crypt`

### Vistas de agenda
- `F7` → menú de agenda
- `x` → Vista trabajo (filtra @personal, muestra vencimientos y bloqueados)
- `z` → Vista personal (filtra @trabajo)

### Apariencia
- Elipsis colapsado: ` ⤵`
- Stars: `◉ ● ◎ ⊙ ⊚ ⊛`
- Checkboxes: `✓` (X) · `✗` (-) · `⬚` (vacío)
- Listas: `•` (*) · `‣` (+) · `▹` (-)
- `org-hide-emphasis-markers` activo — los marcadores `*`, `/`, `~` se ocultan

---

## Python (LSP)

Stack: **Pyright** (type checking) + **Ruff** (linting/formato) + **lsp-mode** + **lsp-ui** + **company**.

Al guardar un `.py` se ejecuta automáticamente `ruff check --fix` y `ruff format`.

Requiere en el sistema:
```bash
uv tool install pyright ruff
```

---

## Markdown

- Markup oculto (`markdown-hide-markup`)
- Headers escalados visualmente (H1 1.8x → H6 0.9x)
- Imágenes locales inline al abrir el archivo
- `olivetti-mode` activo — texto centrado con ancho cómodo
- `C-c C-o` — abrir link bajo el cursor

---

## Notas — Denote

Directorio: `~/Dropbox/denote-notes/`

| Atajo | Acción |
|---|---|
| `C-c n n` | Nueva nota |
| `C-c n f` | Buscar nota (consult-denote) |
| `C-c n l` | Insertar link |
| `C-c n b` | Ver backlinks |
| `C-c n r` | Renombrar archivo |
| `C-c n g` | Grep en notas |

---

## Temas

`ef-themes` — al iniciar carga un tema claro aleatorio (`ef-themes-load-random-light`).

---

## Git

- **Magit** — `C-x g` (estándar)
- **git-gutter** — indicadores `M` `+` `-` en el margen izquierdo, activo globalmente
- **git-timemachine** — recorre versiones del archivo interactivamente
- Repositorios escaneados desde `~/develop` (profundidad 3)

---

## Compilar Emacs 31 (Fedora)

Emacs 31 no está en los repos oficiales, hay que compilarlo.

```bash
# Dependencias
sudo dnf install -y gcc make ncurses-devel gtk3-devel libXpm-devel \
    libjpeg-devel libpng-devel libtiff-devel giflib-devel \
    gnutls-devel jansson-devel harfbuzz-devel sqlite-devel \
    libgccjit-devel libtree-sitter-devel texinfo

# Compilar
git clone --depth 1 --branch emacs-31 https://git.savannah.gnu.org/git/emacs.git
cd emacs
./autogen.sh
./configure --with-pgtk --with-native-compilation --with-tree-sitter --with-json
make -j$(nproc)
sudo make install
```

Después de instalar, copiar el `.desktop` al lugar que GNOME detecta:

```bash
sudo cp /usr/local/share/applications/emacs.desktop /usr/share/applications/
sudo update-desktop-database /usr/share/applications/
```
