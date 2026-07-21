#!/usr/bin/env python3
"""Genera los 42 SVG de grabado (Zone Keyboards) desde keymap.c.

Cada tecla lleva las capas en las esquinas y NADA en el centro si es una
letra o puntuación del alfabeto (para sobrevivir un futuro cambio a Dvorak):

    ┌──────────────┐
    │ capa1  capa2      │   ↖ números/navegación   ↗ símbolos
    │                          │
    │ capa4  capa3      │   ↙ emacs/ventanas       ↘ sistema/RGB
    └──────────────┘

Uso:  python3 gen_engraving.py [--paths]   (--paths convierte texto a curvas)
"""
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).parent
OUT = HERE / 'grabado'

# ponytail: reusamos KC/map_key de gen_readme sin importarlo, porque ese módulo
# reescribe README.md al importarse. Cortamos justo antes del código suelto.
_src = (HERE / 'gen_readme.py').read_text().split('\nlayers = parse_keymap')[0]
_gr = {'__file__': str(HERE / 'gen_readme.py')}
exec(compile(_src, 'gen_readme.py', 'exec'), _gr)
parse_keymap, map_key = _gr['parse_keymap'], _gr['map_key']
_gr['KC']['TILDE_ESC'] = ('´ Esc', _gr['C_WHITE'])   # tap = ´, hold = Esc

# ── Layout ───────────────────────────────────────────────────────────────────
# Orden de LAYOUT_split_3x6_3 → identificador del manual del proveedor.
IDS = (
    ['L1', 'L2', 'L3', 'L4', 'L5', 'L6'] + ['R6', 'R5', 'R4', 'R3', 'R2', 'R1'] +
    ['L7', 'L8', 'L9', 'L10', 'L11', 'L12'] + ['R12', 'R11', 'R10', 'R9', 'R8', 'R7'] +
    ['L13', 'L14', 'L15', 'L16', 'L17', 'L18'] + ['R18', 'R17', 'R16', 'R15', 'R14', 'R13'] +
    ['L19', 'L20', 'L21'] + ['R21', 'R20', 'R19']
)

CORNERS = [(1, 'nw'), (2, 'ne'), (4, 'sw'), (3, 'se')]

# Teclas cuya posición NO cambia con Dvorak: van con su nombre al centro.
NO_CENTER = re.compile(r'KC_[A-Z]$|KC_(SCLN|MINS|COMM|DOT|SLSH|QUOT|GRAVE)$')

# ── SVG ──────────────────────────────────────────────────────────────────────
SIZE_MM = 13          # área grabable estimada de una keycap MX
FONT = 'DejaVu Sans'

POS = {  # x, y, text-anchor
    'nw': (8, 25, 'start'), 'ne': (92, 25, 'end'),
    'sw': (8, 95, 'start'), 'se': (92, 95, 'end'),
    'c':  (50, 60, 'middle'),
}


def esc(s):
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')


def fit(s, base):
    """Achica la fuente según el largo del texto."""
    return base if len(s) <= 1 else round(base * 1.6 / (len(s) ** 0.75), 1)


def text(slot, label, base):
    x, y, anchor = POS[slot]
    return (f'  <text x="{x}" y="{y}" text-anchor="{anchor}" '
            f'font-family="{FONT}" font-size="{fit(label, base)}" '
            f'fill="#000000">{esc(label)}</text>')


def svg(center, corners):
    body = [text('c', center, 30)] if center else []
    body += [text(slot, lbl, 22) for slot, lbl in corners]
    return (f'<svg xmlns="http://www.w3.org/2000/svg" '
            f'width="{SIZE_MM}mm" height="{SIZE_MM}mm" viewBox="0 0 100 100">\n'
            + '\n'.join(body) + '\n</svg>\n')


# ── Construcción ─────────────────────────────────────────────────────────────
def label_of(kc):
    top, bot, _ = map_key(kc)
    return bot.strip()


def main():
    layers = parse_keymap(HERE / 'keymap.c')
    OUT.mkdir(exist_ok=True)
    keys = {}

    for i, kid in enumerate(IDS):
        base = layers[0][i].strip()
        center = '' if NO_CENTER.fullmatch(base) else label_of(base)
        corners = []
        for n, slot in CORNERS:
            lbl = label_of(layers[n][i].strip())
            if lbl and lbl not in center:  # no repetir lo que ya dice el centro
                corners.append((slot, lbl))
        keys[kid] = (center, corners)
        (OUT / f'{kid}.svg').write_text(svg(center, corners))

    preview(keys)
    print(f'{len(IDS)} SVG en {OUT}/  +  preview.html')

    if '--paths' in sys.argv:
        subprocess.run(['inkscape', '--export-text-to-path', '--export-overwrite',
                        *sorted(str(p) for p in OUT.glob('*.svg'))], check=True)
        print('texto convertido a curvas')


def preview(keys):
    """Hoja de contacto con la disposición física, para revisar antes de enviar."""
    def cell(kid):
        center, corners = keys[kid]
        d = dict(corners)
        return (f'<div class=k>'
                f'<i class=nw>{esc(d.get("nw",""))}</i><i class=ne>{esc(d.get("ne",""))}</i>'
                f'<b>{esc(center)}</b>'
                f'<i class=sw>{esc(d.get("sw",""))}</i><i class=se>{esc(d.get("se",""))}</i>'
                f'<u>{kid}</u></div>')

    rows = []
    for r in range(3):
        left = IDS[r * 12: r * 12 + 6]
        right = IDS[r * 12 + 6: r * 12 + 12]
        rows.append('<div class=r>' + ''.join(map(cell, left)) +
                    '<span class=gap></span>' + ''.join(map(cell, right)) + '</div>')
    rows.append('<div class="r t">' + ''.join(map(cell, IDS[36:39])) +
                '<span class=gap></span>' + ''.join(map(cell, IDS[39:42])) + '</div>')

    (OUT / 'preview.html').write_text(f"""<!doctype html><meta charset=utf-8>
<title>Grabado Corne</title><style>
body{{background:#222;color:#eee;font:14px/1.2 'DejaVu Sans',sans-serif;padding:20px}}
.r{{display:flex;justify-content:center;margin:4px 0}}
.r.t{{margin-left:0}} .gap{{width:60px}}
.k{{position:relative;width:64px;height:64px;margin:2px;background:#fafafa;
   color:#000;border-radius:5px}}
.k i{{position:absolute;font-size:10px;font-style:normal;color:#333}}
.nw{{left:4px;top:3px}} .ne{{right:4px;top:3px}}
.sw{{left:4px;bottom:12px}} .se{{right:4px;bottom:12px}}
.k b{{position:absolute;inset:0;display:flex;align-items:center;
     justify-content:center;font-size:13px}}
.k u{{position:absolute;left:0;right:0;bottom:-1px;text-align:center;
     font-size:8px;color:#999;text-decoration:none}}
</style>{''.join(rows)}""")


if __name__ == '__main__':
    main()
