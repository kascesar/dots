#!/usr/bin/env python3
import re
from pathlib import Path

HERE = Path(__file__).parent

# ── Colors ───────────────────────────────────────────────────────────────────
C_WHITE = "FFFFFF"
C_GRAY  = "EEEEEE"
C_DARK  = "DDDDDD"
C_BLUE  = "DDEEFF"
C_GREEN = "D6F0E0"
C_RED   = "FDDEDE"
C_THUMB = "E8E8F8"
C_SPAC  = "F5F5F5"

# ── Keycode → (label, color) ──────────────────────────────────────────────────
KC = {
    'KC_TAB':  ('Tab',  C_DARK), 'KC_BSPC': ('Bksp', C_DARK),
    'KC_ESC':  ('Esc',  C_DARK), 'KC_LSFT': ('Shft', C_DARK),
    'KC_LCTL': ('Ctrl', C_DARK), 'KC_LALT': ('Alt',  C_DARK),
    'KC_LGUI': ('GUI',  C_DARK), 'KC_RALT': ('RAlt', C_DARK),
    'KC_SPC':  ('Spc',  C_DARK), 'KC_ENT':  ('Ent',  C_DARK),
    **{f'KC_{c}': (c, C_WHITE) for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'},
    **{f'KC_{n}': (str(n), C_WHITE) for n in range(10)},
    # Interpretación con layout es-latam activo en el OS
    'KC_SCLN': ('ñ',  C_WHITE), 'KC_QUOT': ('{',  C_GREEN),   # AC10→ñ, AC11→{
    'KC_MINS': ("'",  C_WHITE), 'KC_BSLS': ('}',  C_GREEN),   # AE11→', BKSL→}
    'KC_COMM': (',',  C_WHITE), 'KC_DOT':  ('.',  C_WHITE),
    'KC_SLSH': ('-',  C_GREEN), 'KC_GRAVE':('|',  C_WHITE),   # AB10→-, TLDE→|
    'KC_NUBS': ('<',  C_GREEN),                                 # LSGT→<
    'KC_RBRC': ('+',  C_GREEN),                                 # AD12→+ en latam
    'KC_UP':        ('↑',    C_WHITE), 'KC_DOWN':      ('↓',    C_WHITE),
    'KC_LEFT':      ('←',    C_WHITE), 'KC_RIGHT':     ('→',    C_WHITE),
    'KC_HOME':      ('Home', C_WHITE), 'KC_END':       ('End',  C_WHITE),
    'KC_PAGE_UP':   ('PgUp', C_WHITE), 'KC_PAGE_DOWN': ('PgDn', C_WHITE),
    'KC_EXLM': ('!',  C_WHITE), 'KC_HASH': ('#',  C_WHITE),   # Shift+1→!, Shift+3→#
    'KC_DLR':  ('$',  C_WHITE), 'KC_PERC': ('%',  C_WHITE),
    # Shift+N latam (keycodes con significado invertido vs US):
    'KC_DQUO': ('[',  C_GREEN),   # Shift+AC11 → [ en latam
    'KC_ASTR': ('(',  C_GREEN),   # Shift+8    → ( en latam
    'KC_RPRN': ('=',  C_GREEN),   # Shift+0    → = en latam
    'KC_RCBR': ('*',  C_WHITE),   # Shift+AD12 → * en latam
    'KC_PIPE': (']',  C_WHITE),   # Shift+BKSL → ] en latam
    'KC_AMPR': ('/',  C_WHITE),   # Shift+7    → / en latam
    'KC_CIRC': ('&',  C_WHITE),   # Shift+6    → & en latam
    'KC_LT':   (';',  C_WHITE),   # Shift+,    → ; en latam
    'KC_GT':   (':',  C_WHITE),   # Shift+.    → : en latam
    'KC_QUES': ('_',  C_GREEN),   # Shift+AB10 → _ en latam
    'KC_UNDS': ('?',  C_WHITE),   # Shift+AE11 → ? en latam
    'KC_AT':   ('"',  C_WHITE),   # Shift+2    → " en latam
    'KC_LPRN': (')',  C_WHITE),   # Shift+9    → ) en latam
    # Latam macros
    'LATAM_GRV':  ('`',  C_WHITE),
    'LATAM_CIRC': ('^',  C_WHITE),
    'KC_F11':  ('F11', C_WHITE),
    'KC_F11':  ('F11', C_WHITE),
    '_______': ('',   C_GRAY),  'XXXXXXX': ('',   C_GRAY),
    'QK_BOOT': ('Boot', C_RED),
    'UG_TOGG': ('TOG',  C_DARK), 'UG_NEXT': ('MOD',  C_DARK),
    'UG_HUEU': ('HUE+', C_WHITE),'UG_HUED': ('HUE-', C_WHITE),
    'UG_SATU': ('SAT+', C_WHITE),'UG_SATD': ('SAT-', C_WHITE),
    'UG_VALU': ('VAL+', C_WHITE),'UG_VALD': ('VAL-', C_WHITE),
    'EMACS_X0':     ('X-0', C_BLUE), 'EMACS_X1':     ('X-1', C_BLUE),
    'EMACS_X2':     ('X-2', C_BLUE), 'EMACS_X3':     ('X-3', C_BLUE),
    'EMACS_XK':     ('X-k', C_BLUE), 'EMACS_XCS':    ('XCS', C_GREEN),
    'EMACS_XG':     ('XG',  C_BLUE),
    'EMACS_INDENT': ('IN',  C_BLUE), 'EMACS_DEDENT': ('DE',  C_BLUE),
}

LT_TAP_SYM = {'KC_ENT': '↵', 'KC_SPC': '␣'}

# Top labels para teclas con comportamiento Shift documentado (editorial)
SHIFT_LABELS = {
    'KC_QUOT': 'S:[',   # { → Shift → [
    'KC_BSLS': 'S:]',   # } → Shift → ]
    'KC_SLSH': 'S:_',   # - → Shift → _
    'KC_NUBS': 'S:>',   # < → Shift → >
    'KC_MINS': 'S:?',   # ' → Shift → ?
}

# Keycodes RALT() → símbolo resultante en latam
RALT_MAP = {
    'KC_RBRC': ('~',  C_WHITE),   # AltGr+AD12 → ~
    'KC_MINS': ('\\', C_WHITE),   # AltGr+AE11 → backslash
    'KC_2':    ('@',  C_WHITE),   # AltGr+2    → @
    'KC_QUOT': ('^',  C_WHITE),   # AltGr+AC11 → dead_circumflex (parte de macro)
    'KC_BSLS': ('`',  C_WHITE),   # AltGr+BKSL → dead_grave (parte de macro)
}

# ── Parseo de keymap.c ────────────────────────────────────────────────────────

def tokenize(s):
    tokens, depth, buf = [], 0, []
    for ch in s:
        if ch == '(':
            depth += 1; buf.append(ch)
        elif ch == ')':
            depth -= 1; buf.append(ch)
        elif ch == ',' and depth == 0:
            t = ''.join(buf).strip()
            if t:
                tokens.append(t)
            buf = []
        else:
            buf.append(ch)
    t = ''.join(buf).strip()
    if t:
        tokens.append(t)
    return tokens

def parse_keymap(path):
    source = path.read_text()
    layouts = {}
    i = 0
    while True:
        m = re.search(r'\[(\d+)\]\s*=\s*LAYOUT_split_3x6_3\s*\(', source[i:])
        if not m:
            break
        layer_num = int(m.group(1))
        start = i + m.end()
        depth, j = 1, start
        while j < len(source) and depth > 0:
            if source[j] == '(':
                depth += 1
            elif source[j] == ')':
                depth -= 1
            j += 1
        layouts[layer_num] = tokenize(source[start:j - 1])
        i = j
    return layouts

def map_key(kc):
    kc = kc.strip()

    if kc in ('_______', 'XXXXXXX'):
        return ('', '', C_GRAY)

    m = re.fullmatch(r'LT\((\d+),\s*(\w+)\)', kc)
    if m:
        n, tap = m.group(1), m.group(2)
        sym = LT_TAP_SYM.get(tap, tap.replace('KC_', ''))
        return ('hold', f'LT{n}{sym}', C_BLUE)

    m = re.fullmatch(r'MO\((\d+)\)', kc)
    if m:
        return ('', f'MO{m.group(1)}', C_BLUE)

    m = re.fullmatch(r'(?:LWIN|LGUI)\((\w+)\)', kc)
    if m:
        inner = m.group(1)
        lbl = KC.get(inner, (inner.replace('KC_', ''), C_WHITE))[0]
        return ('', f'W+{lbl}', C_WHITE)

    m = re.fullmatch(r'RALT\((\w+)\)', kc)
    if m:
        inner = m.group(1)
        label, color = RALT_MAP.get(inner, (f'⎇+{inner.replace("KC_","")}', C_WHITE))
        return ('', label, color)

    m = re.fullmatch(r'LSFT\((\w+)\)', kc)
    if m:
        inner = m.group(1)
        if inner == 'KC_NUBS':
            return ('', '>', C_GREEN)
        lbl = KC.get(inner, (inner.replace('KC_', ''), C_WHITE))[0]
        return ('', lbl, C_WHITE)

    top = SHIFT_LABELS.get(kc, '')
    label, color = KC.get(kc, (kc.replace('KC_', ''), C_WHITE))
    return (top, label, color)

# ── Render LaTeX ──────────────────────────────────────────────────────────────

def esc(s):
    s = s.replace('\\', r'\textbackslash{}')
    s = s.replace('{', r'\{').replace('}', r'\}')
    s = s.replace('_', r'\_').replace('^', r'\^{}')
    s = s.replace('~', r'\textasciitilde{}')
    s = s.replace('&', r'\&').replace('%', r'\%')
    s = s.replace('#', r'\#').replace('$', r'\$')
    s = s.replace('<', r'\textless{}').replace('>', r'\textgreater{}')
    s = s.replace('|', r'\textbar{}')
    s = s.replace('"', r"''")
    s = s.replace('←', r'$\leftarrow$')
    s = s.replace('→', r'$\rightarrow$')
    s = s.replace('↑', r'$\uparrow$')
    s = s.replace('↓', r'$\downarrow$')
    s = s.replace('␣', r'[SPC]')
    s = s.replace('↵', r'$\hookleftarrow$')
    return s

def key_cell(top, bot, color):
    top_part = (r'{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}' + esc(top) + r'}\\') if top else ''
    bot_part = (r'{\fontsize{7.5}{8}\selectfont\textbf{' + esc(bot) + r'}}') if bot else ''
    inner = top_part + bot_part
    return r'\cellcolor[HTML]{' + color + r'}\begin{tabular}[c]{@{}c@{}}' + inner + r'\end{tabular}'

def layer_table(rows_data, thumb_data):
    key_w, lbl_w, spc_w = '1.12cm', '1.7cm', '0.25cm'
    col_parts = [r'>{\centering\arraybackslash}p{' + lbl_w + '}']
    for _ in range(6):
        col_parts.append(r'>{\centering\arraybackslash}p{' + key_w + '}')
    col_parts.append(r'>{\centering\arraybackslash}p{' + spc_w + '}')
    for _ in range(6):
        col_parts.append(r'>{\centering\arraybackslash}p{' + key_w + '}')
    col_spec = '|' + '|'.join(col_parts) + '|'

    lines = ['```{=latex}',
             r'\renewcommand{\arraystretch}{1.6}',
             r'\begin{tabular}{' + col_spec + '}',
             r'\hline']

    for row_label, cells in rows_data:
        lbl = (r'\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{' + esc(row_label) + r'}}')
        row = [lbl]
        for c in cells[:6]:
            row.append(key_cell(*c))
        row.append(r'\cellcolor[HTML]{' + C_SPAC + r'}{}')
        for c in cells[6:12]:
            row.append(key_cell(*c))
        lines.append(' & '.join(row) + r' \\')
        lines.append(r'\hline')

    lbl = r'\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}}'
    thumb_row = [lbl,
                 r'\cellcolor[HTML]{' + C_SPAC + r'}{}',
                 r'\cellcolor[HTML]{' + C_SPAC + r'}{}']
    for c in thumb_data[:3]:
        top, bot, col = c
        thumb_row.append(key_cell(top, bot, col if col != C_WHITE else C_THUMB))
    thumb_row.append(r'\cellcolor[HTML]{' + C_SPAC + r'}{}')
    thumb_row.append(r'\cellcolor[HTML]{' + C_SPAC + r'}{}')
    thumb_row.append(r'\cellcolor[HTML]{' + C_SPAC + r'}{}')
    for c in thumb_data[3:]:
        top, bot, col = c
        thumb_row.append(key_cell(top, bot, col if col != C_WHITE else C_THUMB))
    lines.append(' & '.join(thumb_row) + r' \\')
    lines += [r'\hline', r'\end{tabular}', '```']
    return '\n'.join(lines)

def build_layer_table(tokens):
    keys = [map_key(t) for t in tokens]
    rows = [
        ('Fila 1', keys[0:12]),
        ('Fila 2', keys[12:24]),
        ('Fila 3', keys[24:36]),
    ]
    thumbs = keys[36:42]
    return layer_table(rows, thumbs)

# ── Main ──────────────────────────────────────────────────────────────────────

layers = parse_keymap(HERE / 'keymap.c')

t = [build_layer_table(layers[n]) for n in range(5)]

doc = """\
---
title: "Crkbd --- Guía de uso"
author: kascesar
date: "v2 --- 2025"
geometry: "a4paper,landscape,top=1.8cm,bottom=1.8cm,left=1.8cm,right=1.8cm"
fontsize: 9pt
mainfont: "Liberation Sans"
monofont: "DejaVu Sans Mono"
monofontoptions: "Scale=0.78"
colorlinks: true
linkcolor: NavyBlue
header-includes: |
  \\usepackage{booktabs}
  \\usepackage{longtable}
  \\usepackage{array}
  \\usepackage{colortbl}
  \\usepackage{xcolor}
  \\usepackage{mdframed}
  \\definecolor{accentblue}{HTML}{1A56A8}
  \\definecolor{accentgreen}{HTML}{0F6E56}
  \\definecolor{greenbg}{HTML}{D6F0E0}
  \\definecolor{amberbg}{HTML}{FFF8E7}
  \\newmdenv[backgroundcolor=greenbg,linecolor=accentgreen,linewidth=1pt,innerleftmargin=8pt,innerrightmargin=8pt,innertopmargin=4pt,innerbottommargin=4pt]{dvpnote}
  \\newmdenv[backgroundcolor=amberbg,linecolor=orange!60,linewidth=1pt,innerleftmargin=8pt,innerrightmargin=8pt,innertopmargin=4pt,innerbottommargin=4pt]{warningnote}
  \\usepackage{fancyhdr}
  \\pagestyle{fancy}
  \\fancyhf{}
  \\fancyhead[L]{\\textcolor{accentblue}{\\textbf{Crkbd · kascesar}}}
  \\fancyhead[R]{\\textcolor{gray}{keymap v2 --- layout es-latam}}
  \\fancyfoot[C]{\\thepage}
  \\renewcommand{\\headrulewidth}{0.4pt}
---

# Descripción general

El **Corne Keyboard (crkbd)** es un teclado ergonómico split de 42 teclas (3x6 + 3 pulgares por lado).
Esta configuración tiene **5 capas** optimizadas para Emacs y un gestor de ventanas tipo tiling.

\\begin{dvpnote}
\\textbf{Layout es-latam.} Layer 2 organizado por categoría.
Fila 1: espejo de la fila numérica latam con Shift.
Fila 2: guiones y slashes juntos (izq) --- llaves y corchetes (der).
Fila 3: ángulos, comillas y operadores.
Las teclas marcadas \\texttt{S:x} producen ese carácter al mantener Shift.
\\end{dvpnote}

```bash
qmk compile -kb crkbd/rev1 -km kascesar
```

---

# Resumen de capas

| Activación       | Capa    | Función                    |
|------------------|---------|----------------------------|
| `LT1↵` (hold)   | Layer 1 | Números + Navegación       |
| `LT2↵` (hold)   | Layer 2 | Símbolos (es-latam)        |
| `LT4[SPC]` (hold)   | Layer 4 | Emacs + Gestor de ventanas |
| Layer 1 + `MO3` | Layer 3 | Sistema / RGB              |
| Layer 2 + `MO3` | Layer 3 | Sistema / RGB              |

---

# Layer 0 --- Base (QWERTY)

Capa de uso diario. Distribución QWERTY estándar con modificadores en los pulgares.

""" + t[0] + """

| Tecla  | Tap     | Hold           |
|--------|---------|----------------|
| `LT1↵` | Enter   | Activa Layer 1 |
| `LT4[SPC]` | Espacio | Activa Layer 4 |
| `LT2↵` | Enter   | Activa Layer 2 |

---

# Layer 1 --- Números y Navegación

Se activa manteniendo `LT1↵` (pulgar interior izquierdo).

""" + t[1] + """

- Lado izquierdo: dígitos 1--5 en fila superior, 6--0 en fila media
- Lado derecho: flechas, PgUp, PgDn, Home, End
- `MO3`: accede a Layer 3 mientras se mantiene

---

# Layer 2 --- Símbolos (es-latam)

Se activa manteniendo `LT2↵` (pulgar interior derecho).

""" + t[2] + """

| Tecla  | Shift | Categoría        |
|--------|-------|------------------|
| `-`    | `_`   | Guiones          |
| `\\`   |       | Slashes          |
| `\\|`  |       | Slashes          |
| `+`    | `*`   | Aritméticos      |
| `{`    | `[`   | Llaves/Corchetes |
| `}`    | `]`   | Llaves/Corchetes |
| `<`    | `>`   | Ángulos          |
| `'`    | `?`   | Comillas/Signos  |

---

# Layer 3 --- Sistema / RGB

Se activa combinando `MO3` desde Layer 1 o Layer 2.

""" + t[3] + """

| Tecla    | Acción                              |
|----------|-------------------------------------|
| `Boot`   | Entra en modo bootloader (flashear) |
| `TOG`    | Enciende / apaga RGB underglow      |
| `MOD`    | Cambia modo de animación RGB        |
| `HUE+/-` | Ajusta el tono de color             |
| `SAT+/-` | Ajusta la saturación                |
| `VAL+/-` | Ajusta el brillo                    |

\\begin{warningnote}
Para flashear: activar Layer~3 (L1+MO3 o L2+MO3), luego presionar \\textbf{Boot}.
\\end{warningnote}

---

# Layer 4 --- Emacs + Gestor de ventanas

\\begin{dvpnote}
\\textbf{Cambio en v2:} \\texttt{F10} reemplazado por \\texttt{XCS} (\\texttt{C-x C-s}, guardar buffer).
\\end{dvpnote}

Se activa manteniendo cualquiera de los dos `LT4[SPC]`.

""" + t[4] + """

## Macros de Emacs (lado izquierdo)

| Tecla | Secuencia        | Descripción                             |
|-------|------------------|-----------------------------------------|
| `X-0` | `C-x 0`          | Cierra la ventana actual                |
| `X-1` | `C-x 1`          | Cierra todas las otras ventanas         |
| `X-2` | `C-x 2`          | Divide la ventana horizontalmente       |
| `X-3` | `C-x 3`          | Divide la ventana verticalmente         |
| `X-k` | `C-x k`          | Mata (cierra) el buffer actual          |
| `XCS` | `C-x C-s`        | **Guarda el buffer actual** [nuevo]     |
| `XG`  | `C-x g`          | Abre Magit                              |
| `DE`  | `C-u -4 C-x TAB` | Desindenta 4 espacios                   |
| `IN`  | `C-u 4 C-x TAB`  | Indenta 4 espacios                      |

## Gestor de ventanas (lado derecho)

| Tecla   | Atajo       | Acción                                 |
|---------|-------------|----------------------------------------|
| `W+T`   | `Super+T`   | Abrir terminal                         |
| `W+S`   | `Super+S`   | Buscar / lanzador de apps              |
| `W+↑`   | `Super+↑`   | Mover ventana arriba / maximizar       |
| `W+F11` | `Super+F11` | (personalizado para tu WM)             |
| `W+Y`   | `Super+Y`   | (personalizado para tu WM)             |
| `W+Q`   | `Super+Q`   | Cerrar ventana                         |
| `W+←`   | `Super+←`   | Mover ventana a la izquierda           |
| `W+↓`   | `Super+↓`   | Mover ventana abajo                    |
| `W+→`   | `Super+→`   | Mover ventana a la derecha             |

---

# Cambios v2

| Qué cambió              | Antes              | v2                              |
|-------------------------|--------------------|---------------------------------|
| L2 fila sup. derecha    | `* + ^ ( )`        | `{ [ ( = -` (cierres con Shift) |
| L2 fila media izquierda | `` ` _ _ _ _ ``    | `` ` < \\\\ ; ^ ``              |
| L2 fila inf. izquierda  | `\\\\ | / - _`     | `" > : _ ~`                     |
| L4 fila media izq.      | `_ XG F10 DE IN`   | `_ XG XCS DE IN`                |
| Nueva macro             | ---                | `XCS` = `C-x C-s`               |

---

# Compilar y flashear

```bash
# Compilar
qmk compile -kb crkbd/rev1 -km kascesar

# Flashear (misma secuencia para cada mitad)
qmk flash -kb crkbd/rev1 -km kascesar
```

**Orden:** compilar → izquierda (bootloader → flash) → derecha (bootloader → flash)
→ conectar TRRS → conectar USB izquierda al computador.

\\begin{warningnote}
Conectar TRRS \\emph{antes} del USB. Desconectar USB \\emph{antes} de desconectar TRRS.
\\end{warningnote}

---

# Solución de problemas

| Síntoma                         | Causa                            | Solución                                     |
|---------------------------------|----------------------------------|----------------------------------------------|
| Solo funciona una mitad         | Derecha no flasheada             | Flashear la mitad derecha                    |
| Teclas invertidas (L/R)         | Mitad incorrecta al USB          | Usar mitad izquierda como master             |
| No aparece en DFU               | Reset muy lento (Caterina)       | Presionar reset 2 veces rápido               |
| avrdude no encuentra el puerto  | Permisos USB                     | `sudo usermod -aG dialout $USER` y reloguear |
"""

(HERE / 'README.md').write_text(doc)
print("README.md generado ok")
