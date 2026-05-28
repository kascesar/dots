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
  \usepackage{booktabs}
  \usepackage{longtable}
  \usepackage{array}
  \usepackage{colortbl}
  \usepackage{xcolor}
  \usepackage{mdframed}
  \definecolor{accentblue}{HTML}{1A56A8}
  \definecolor{accentgreen}{HTML}{0F6E56}
  \definecolor{greenbg}{HTML}{D6F0E0}
  \definecolor{amberbg}{HTML}{FFF8E7}
  \newmdenv[backgroundcolor=greenbg,linecolor=accentgreen,linewidth=1pt,innerleftmargin=8pt,innerrightmargin=8pt,innertopmargin=4pt,innerbottommargin=4pt]{dvpnote}
  \newmdenv[backgroundcolor=amberbg,linecolor=orange!60,linewidth=1pt,innerleftmargin=8pt,innerrightmargin=8pt,innertopmargin=4pt,innerbottommargin=4pt]{warningnote}
  \usepackage{fancyhdr}
  \pagestyle{fancy}
  \fancyhf{}
  \fancyhead[L]{\textcolor{accentblue}{\textbf{Crkbd · kascesar}}}
  \fancyhead[R]{\textcolor{gray}{keymap v2 --- layout es-latam}}
  \fancyfoot[C]{\thepage}
  \renewcommand{\headrulewidth}{0.4pt}
---

# Descripción general

El **Corne Keyboard (crkbd)** es un teclado ergonómico split de 42 teclas (3x6 + 3 pulgares por lado).
Esta configuración tiene **5 capas** optimizadas para Emacs y un gestor de ventanas tipo tiling.

\begin{dvpnote}
\textbf{Layout es-latam.} Layer 2 organizado por categoría.
Fila 1: espejo de la fila numérica latam con Shift.
Fila 2: guiones y slashes juntos (izq) --- llaves y corchetes (der).
Fila 3: ángulos, comillas y operadores.
Las teclas marcadas \texttt{S:x} producen ese carácter al mantener Shift.
\end{dvpnote}

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

```{=latex}
\renewcommand{\arraystretch}{1.6}
\begin{tabular}{|>{\centering\arraybackslash}p{1.7cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{0.25cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|}
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 1}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Tab}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Q}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{E}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{R}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{T}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Y}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{U}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{I}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{O}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{P}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Bksp}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 2}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Shft}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{A}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{S}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{D}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{F}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{G}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{H}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{J}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{K}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{L}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{ñ}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:?}\\{\fontsize{7.5}{8}\selectfont\textbf{'}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 3}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ctrl}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Z}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{C}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{V}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{B}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{N}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{M}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{,}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{.}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:\_}\\{\fontsize{7.5}{8}\selectfont\textbf{-}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{TILDE\_ESC}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Alt}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}hold}\\{\fontsize{7.5}{8}\selectfont\textbf{LT1$\hookleftarrow$}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}hold}\\{\fontsize{7.5}{8}\selectfont\textbf{LT4[SPC]}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}hold}\\{\fontsize{7.5}{8}\selectfont\textbf{LT4[SPC]}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}hold}\\{\fontsize{7.5}{8}\selectfont\textbf{LT2$\hookleftarrow$}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{GUI}}\end{tabular} \\
\hline
\end{tabular}
```

| Tecla  | Tap     | Hold           |
|--------|---------|----------------|
| `LT1↵` | Enter   | Activa Layer 1 |
| `LT4[SPC]` | Espacio | Activa Layer 4 |
| `LT2↵` | Enter   | Activa Layer 2 |

---

# Layer 1 --- Números y Navegación

Se activa manteniendo `LT1↵` (pulgar interior izquierdo).

```{=latex}
\renewcommand{\arraystretch}{1.6}
\begin{tabular}{|>{\centering\arraybackslash}p{1.7cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{0.25cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|}
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 1}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Tab}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{1}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{2}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{3}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{4}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{5}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{PgUp}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{PgDn}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{$\uparrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Home}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{End}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Bksp}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 2}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Shft}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{6}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{7}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{8}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{9}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{0}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{$\leftarrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{$\downarrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{$\rightarrow$}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 3}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ctrl}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Esc}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{GUI}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Spc}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ent}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{MO3}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{RAlt}}\end{tabular} \\
\hline
\end{tabular}
```

- Lado izquierdo: dígitos 1--5 en fila superior, 6--0 en fila media
- Lado derecho: flechas, PgUp, PgDn, Home, End
- `MO3`: accede a Layer 3 mientras se mantiene

---

# Layer 2 --- Símbolos (es-latam)

Se activa manteniendo `LT2↵` (pulgar interior derecho).

```{=latex}
\renewcommand{\arraystretch}{1.6}
\begin{tabular}{|>{\centering\arraybackslash}p{1.7cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{0.25cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|}
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 1}} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{!}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{''}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\#}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\%}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\&}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{/}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{(}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{)}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{=}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{?}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Bksp}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 2}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Shft}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:\_}\\{\fontsize{7.5}{8}\selectfont\textbf{-}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\_}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\textasciitilde{}}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\textbackslash\{\}}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\textbar{}}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:[}\\{\fontsize{7.5}{8}\selectfont\textbf{\{}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:]}\\{\fontsize{7.5}{8}\selectfont\textbf{\}}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{[}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{]}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\^{}}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 3}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ctrl}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:\textgreater{}}\\{\fontsize{7.5}{8}\selectfont\textbf{\textless{}}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{\textgreater{}}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{5.5}{6}\selectfont\color[HTML]{666666}S:?}\\{\fontsize{7.5}{8}\selectfont\textbf{'}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{`}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{@}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{;}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{:}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{*}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{+}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{GUI}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{MO3}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Spc}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ent}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{RAlt}}\end{tabular} \\
\hline
\end{tabular}
```

| Tecla  | Shift | Categoría        |
|--------|-------|------------------|
| `-`    | `_`   | Guiones          |
| `\`   |       | Slashes          |
| `\|`  |       | Slashes          |
| `+`    | `*`   | Aritméticos      |
| `{`    | `[`   | Llaves/Corchetes |
| `}`    | `]`   | Llaves/Corchetes |
| `<`    | `>`   | Ángulos          |
| `'`    | `?`   | Comillas/Signos  |

---

# Layer 3 --- Sistema / RGB

Se activa combinando `MO3` desde Layer 1 o Layer 2.

```{=latex}
\renewcommand{\arraystretch}{1.6}
\begin{tabular}{|>{\centering\arraybackslash}p{1.7cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{0.25cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|}
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 1}} & \cellcolor[HTML]{FDDEDE}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Boot}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Bksp}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 2}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{TOG}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{HUE+}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{SAT+}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{VAL+}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 3}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{MOD}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{HUE-}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{SAT-}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{VAL-}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{GUI}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Spc}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ent}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{RAlt}}\end{tabular} \\
\hline
\end{tabular}
```

| Tecla    | Acción                              |
|----------|-------------------------------------|
| `Boot`   | Entra en modo bootloader (flashear) |
| `TOG`    | Enciende / apaga RGB underglow      |
| `MOD`    | Cambia modo de animación RGB        |
| `HUE+/-` | Ajusta el tono de color             |
| `SAT+/-` | Ajusta la saturación                |
| `VAL+/-` | Ajusta el brillo                    |

\begin{warningnote}
Para flashear: activar Layer~3 (L1+MO3 o L2+MO3), luego presionar \textbf{Boot}.
\end{warningnote}

---

# Layer 4 --- Emacs + Gestor de ventanas

\begin{dvpnote}
\textbf{Cambio en v2:} \texttt{F10} reemplazado por \texttt{XCS} (\texttt{C-x C-s}, guardar buffer).
\end{dvpnote}

Se activa manteniendo cualquiera de los dos `LT4[SPC]`.

```{=latex}
\renewcommand{\arraystretch}{1.6}
\begin{tabular}{|>{\centering\arraybackslash}p{1.7cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{0.25cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|>{\centering\arraybackslash}p{1.12cm}|}
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 1}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Tab}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X-0}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X-1}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X-3}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X-2}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{X-k}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+T}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+S}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+$\uparrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+F11}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+Y}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+Q}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 2}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Shft}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{XG}}\end{tabular} & \cellcolor[HTML]{D6F0E0}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{XCS}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{DE}}\end{tabular} & \cellcolor[HTML]{DDEEFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{IN}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+$\leftarrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+$\downarrow$}}\end{tabular} & \cellcolor[HTML]{FFFFFF}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+$\rightarrow$}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Fila 3}} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Ctrl}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Esc}}\end{tabular} \\
\hline
\cellcolor[HTML]{D0D8EE}{\fontsize{7}{8}\selectfont\textbf{Pulgares}} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{DDDDDD}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{Alt}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{F5F5F5}{} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} & \cellcolor[HTML]{E8E8F8}\begin{tabular}[c]{@{}c@{}}{\fontsize{7.5}{8}\selectfont\textbf{W+Y}}\end{tabular} & \cellcolor[HTML]{EEEEEE}\begin{tabular}[c]{@{}c@{}}\end{tabular} \\
\hline
\end{tabular}
```

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
| L2 fila media izquierda | `` ` _ _ _ _ ``    | `` ` < \\ ; ^ ``              |
| L2 fila inf. izquierda  | `\\ | / - _`     | `" > : _ ~`                     |
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

\begin{warningnote}
Conectar TRRS \emph{antes} del USB. Desconectar USB \emph{antes} de desconectar TRRS.
\end{warningnote}

---

# Solución de problemas

| Síntoma                         | Causa                            | Solución                                     |
|---------------------------------|----------------------------------|----------------------------------------------|
| Solo funciona una mitad         | Derecha no flasheada             | Flashear la mitad derecha                    |
| Teclas invertidas (L/R)         | Mitad incorrecta al USB          | Usar mitad izquierda como master             |
| No aparece en DFU               | Reset muy lento (Caterina)       | Presionar reset 2 veces rápido               |
| avrdude no encuentra el puerto  | Permisos USB                     | `sudo usermod -aG dialout $USER` y reloguear |
