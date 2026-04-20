# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "marimo",
# ]
# ///
"""
test.py — Guía de referencia y demo visual del sistema de slides de BancoEstado.
Correr con: marimo edit test.py
"""

import marimo

__generated_with = "0.23.1"
app = marimo.App(width="full")


@app.cell
async def _():
    import sys
    import marimo as mo

    # Definimos las variables como None para evitar errores de referencia
    cierre = intro = portada = seccion = None

    if sys.platform == "emscripten":
        from pyodide.http import pyfetch
        import importlib
        import sys

        try:
            # Intentamos descargar desde la raíz absoluta del servidor
            _resp = await pyfetch("./banners.py") 
        
            if _resp.status == 200:
                with open("/tmp/banners.py", "w") as _f:
                    _f.write(await _resp.string())
            
                if "/tmp" not in sys.path:
                    sys.path.insert(0, "/tmp")
            
                import banners
                importlib.reload(banners)
            
                cierre, intro, portada, seccion = banners.cierre, banners.intro, banners.portada, banners.seccion
            else:
                # Si da 404, imprimimos dónde está intentando buscar
                import js
                actual_url = js.window.location.href
                print(f"Error 404: No se halló banners.py en el servidor.")
                print(f"URL del notebook: {actual_url}")
        except Exception as e:
            print(f"Error crítico: {e}")
    else:
        try:
            from banners import cierre, intro, portada, seccion
        except ImportError:
            print("banners.py no encontrado localmente.")
    return cierre, intro, mo, portada, seccion


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    # Sistema de Slides — Guía de referencia
    **BancoEstado · Analítica Avanzada · Modelos**

    Este archivo es la guía de referencia para construir presentaciones con `banners.py`.
    Cada función produce una **slide completa lista para mostrar** — una llamada = una slide.
    Debajo de cada bloque de documentación hay un demo en vivo que muestra cómo se ve el resultado.

    ---

    ## Importar — una sola vez por archivo de presentación

    ```python
    from banners import portada, intro, seccion, cierre
    ```

    ## Las cuatro funciones

    | Función | Propósito | Posición típica |
    |---------|-----------|-----------------|
    | `portada(...)` | Slide de título, banner grande centrado | Primera slide |
    | `intro(...)` | Contexto o planteamiento del problema | Slides 2–3 |
    | `seccion(...)` | Slide intermedia de sección | Cualquier slide del medio |
    | `cierre(...)` | Slide de cierre con mensaje contundente | Última slide |

    ## Cómo se estructura una slide

    Cada slide es un stack vertical: **banner arriba, contenido abajo**.
    El banner se genera automáticamente a partir de los parámetros de texto que entregues.
    El área de contenido se controla completamente con el parámetro `content`.
    """)
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## El parámetro `content` — tres modos

    `content` es el parámetro más importante. Acepta tres tipos distintos y la slide
    ajusta su layout automáticamente según lo que se le pase.

    ---

    ### Modo 1 — String simple → markdown de ancho completo

    Pasar un string de Python. Se renderiza como markdown full-width bajo el banner.
    Usar para párrafos, listas y tablas. Para tablas, usar comillas simples triples:

    ```python
    seccion(
        title="Antes vs Después",
        content='''
    | Antes | Después |
    |-------|---------|
    | Proceso manual | Automatizado |
    | Sin versiones  | Git-tracked  |
        '''
    )
    ```

    ---

    ### Modo 2 — Lista de strings → columnas iguales con cajitas

    Pasar una lista de Python. Cada string se convierte en una columna (una cajita).
    Mejor resultado con 2, 3 o 4 items. Cada string soporta markdown completo.

    ```python
    seccion(
        title="Tres decisiones clave",
        content=[
            "**Decisión 1** — Descripción del primer punto.",
            "**Decisión 2** — Descripción del segundo punto.",
            "**Decisión 3** — Descripción del tercer punto.",
        ],
        content_kind="neutral"
    )
    ```

    El color de las cajitas se controla con `content_kind`:

    | Valor | Color | Cuándo usarlo |
    |-------|-------|---------------|
    | `"neutral"` | Gris | Default. Contenido factual, sin carga emocional. |
    | `"info"` | Azul | Contexto, información de fondo. |
    | `"success"` | Verde | Logros, soluciones, resultados positivos. |
    | `"warn"` | Amarillo | Riesgos, advertencias, cosas que requirieron atención. |
    | `"danger"` | Rojo | Problemas críticos, bloqueos, fallas graves. |

    ---

    ### Modo 3 — Componente marimo → se coloca directamente bajo el banner

    Pasar cualquier componente marimo: `mo.mermaid(...)`, `mo.image(...)`, `mo.hstack(...)`, etc.
    El componente se coloca tal cual, sin envoltorio adicional.

    ```python
    # Diagrama mermaid
    seccion(title="Pipeline", content=mo.mermaid("graph LR\n  A --> B --> C"))

    # Imagen desde archivo
    seccion(title="Arquitectura", content=mo.image("img/arquitectura.png", width="80%"))

    # Dos imágenes lado a lado
    seccion(
        title="Antes y después",
        content=mo.hstack([
            mo.image("img/antes.png", width="45%"),
            mo.image("img/despues.png", width="45%"),
        ])
    )
    ```

    Cuando se necesitan varios elementos apilados verticalmente, envolver con `mo.vstack`:

    ```python
    seccion(
        title="Evidencia",
        content=mo.vstack([
            mo.md("El flujo:"),
            mo.mermaid("graph LR\n  A --> B --> C"),
            mo.md("Los números:"),
            mo.hstack([mo.stat("42", label="Modelos migrados", caption="Abril 2026")]),
        ])
    )
    ```
    """)
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## `portada()` — Slide de título

    Banner grande y centrado con gradiente naranja completo.
    Usar como **primera slide** de cada presentación.

    ### Parámetros

    - `title` *(str, requerido)* — Título principal del proyecto. Corto y directo.
    - `subtitle` *(str, default "\")* — Una frase que enmarca el contexto del proyecto.
    - `date` *(str, default "\")* — Fecha al pie del banner. Ejemplo: `"Abril 2026"`.
    - `team` *(str, default "BancoEstado · Analítica Avanzada · Modelos")* — Etiqueta del equipo en versalitas sobre el título. Cambiar solo si se presenta en nombre de otro equipo.
    - `content` *(str | list[str] | componente mo, default None)* — Contenido bajo el banner.
    - `content_kind` *(str, default "neutral")* — Color de las cajitas cuando content es lista.

    ### Uso mínimo

    ```python
    portada("Mi Proyecto", date="Abril 2026")
    ```

    ### Uso completo

    ```python
    portada(
        title="Mi Proyecto",
        subtitle="Frase corta que enmarca el contexto.",
        date="Abril 2026",
        content=[
            "**Punto clave 1** — Algo relevante para la audiencia.",
            "**Punto clave 2** — El resultado concreto logrado.",
            "**Punto clave 3** — El impacto para el equipo o el negocio.",
        ],
        content_kind="success",
    )
    ```
    """)
    return


@app.cell
def _(portada):
    portada(
        title="Mi Proyecto",
        subtitle="Frase corta que enmarca el contexto.",
        date="Abril 2026",
        content=[
            "**Punto clave 1** — Algo relevante para la audiencia.",
            "**Punto clave 2** — El resultado concreto logrado.",
            "**Punto clave 3** — El impacto para el equipo o el negocio.",
        ],
        content_kind="neutral",
        icon={
            'src':'https://upload.wikimedia.org/wikipedia/commons/b/b3/Logo_BancoEstado.svg',
            'size': '60px'
        }
    )
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## `intro()` — Slide de contexto o planteamiento del problema

    Banner compacto con gradiente completo. Diseñado para slides que abren un bloque temático,
    típicamente **slides 2 y 3**. Tiene dos parámetros exclusivos: `alert` y `footer`.

    ### Parámetros

    - `title` *(str, requerido)* — Título de la slide.
    - `subtitle` *(str, default "\")* — Frase de apoyo dentro del banner, bajo el título.
    - `tag` *(str, default "\")* — Etiqueta de sección sobre el título en versalitas. Ejemplo: `"Contexto"`, `"Problema"`. Si se deja vacío, se usa el nombre del equipo.
    - `team` *(str, default línea del equipo)* — Etiqueta del equipo. Cambiar solo si es necesario.
    - `alert` *(str, default "\")* — Callout full-width entre el banner y las columnas. Enunciar el problema o riesgo principal en una frase clara. Acepta markdown.
    - `alert_kind` *(str, default "warn")* — Color del alert. Mismas opciones que `content_kind`.
    - `content` *(str | list[str] | componente mo, default None)* — Contenido bajo el alert.
    - `content_kind` *(str, default "neutral")* — Color de las cajitas cuando content es lista.
    - `footer` *(str, default "\")* — Cita de cierre al pie de la slide como blockquote. Usar para una afirmación contundente o pregunta guía.

    ### Uso mínimo

    ```python
    intro("El contexto: por qué estamos aquí", tag="Contexto")
    ```

    ### Uso completo

    ```python
    intro(
        title="El contexto: por qué estamos aquí",
        subtitle="Lo que motivó este trabajo y lo que estaba en juego.",
        tag="Contexto",
        alert="Los modelos corrían en servidores con fecha de apagado confirmada.",
        alert_kind="warn",
        content=[
            "**Riesgo operacional** — Los modelos dejarían de producir predicciones.",
            "**Sin trazabilidad** — Sin registro de qué versión estaba activa.",
            "**Dependencia de personas** — Proceso manual y sin documentar.",
        ],
        content_kind="warn",
        footer="La pregunta no era si migrar — era cómo hacerlo de forma sostenible.",
    )
    ```
    """)
    return


@app.cell
def _(intro):
    intro(
        title="El contexto: por qué estamos aquí",
        subtitle="Lo que motivó este trabajo y lo que estaba en juego.",
        tag="Contexto",
        alert="Los modelos corrían en servidores con fecha de apagado confirmada.",
        alert_kind="warn",
        content=[
            "**Riesgo operacional** — Los modelos dejarían de producir predicciones.",
            "**Sin trazabilidad** — Sin registro de qué versión estaba activa.",
            "**Dependencia de personas** — Proceso manual y sin documentar.",
        ],
        content_kind="warn",
        footer="La pregunta no era si migrar — era cómo hacerlo de forma sostenible.",
    )
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## `seccion()` — Slide intermedia de sección

    Fondo oscuro con acento de borde izquierdo naranja. Ocupa menos espacio vertical,
    dejando más aire al contenido. Usar para **todas las slides del medio**.

    ### Parámetros

    - `title` *(str, requerido)* — Título de la sección.
    - `subtitle` *(str, default "\")* — Frase de apoyo dentro del banner, a la derecha del título.
    - `number` *(str, default "\")* — Número de sección, se muestra grande a la izquierda. Ejemplo: `"01"`, `"02"`, `"03"`. Dejar vacío para omitirlo.
    - `content` *(str | list[str] | componente mo, default None)* — Contenido bajo el banner.
    - `content_kind` *(str, default "neutral")* — Color de las cajitas cuando content es lista.
    - `footer` *(str, default "\")* — Cita de cierre al pie de la slide.

    ### Uso mínimo

    ```python
    seccion("Lo que se construyó")
    ```

    ### Con columnas

    ```python
    seccion(
        title="Lo que se construyó",
        subtitle="Tres decisiones que definen el resultado.",
        number="03",
        content=[
            "**Decisión 1** — Infraestructura parametrizada para cualquier modelo.",
            "**Decisión 2** — Despliegue con un solo comando.",
            "**Decisión 3** — Documentación técnica generada automáticamente.",
        ],
    )
    ```

    ### Con tabla — usar comillas simples triples para el string

    ```python
    seccion(
        title="Antes vs después",
        content='''
    | Antes | Después |
    |-------|---------|
    | Hardcodeado por modelo  | Parametrizado para cualquier modelo |
    | n modelos = n proyectos | n modelos = 1 framework             |
        '''
    )
    ```

    ### Con diagrama mermaid

    Pasar `mo.mermaid(string)` como content. El string sigue sintaxis estándar Mermaid.
    Tipos más usados: `graph TD` (top-down), `graph LR` (left-right), `gantt`, `sequenceDiagram`.

    ```python
    seccion(
        title="Flujo del pipeline",
        content=mo.mermaid('''
    graph LR
        A[Glue Job] --> B[Athena Table]
        B --> C[SageMaker Pipeline]
        C --> D[Predictions]
        ''')
    )
    ```

    ### Con imagen

    ```python
    # Desde path local
    seccion(title="Arquitectura", content=mo.image("img/arquitectura.png", width="80%"))

    # Desde bytes — preferido, más portable entre máquinas
    from pathlib import Path
    seccion(
        title="Arquitectura",
        content=mo.image(Path("img/arquitectura.png").read_bytes(), width="80%")
    )
    ```

    El parámetro `width` acepta cualquier valor CSS: `"80%"`, `"400px"`, `"100%"`.
    """)
    return


@app.cell
def _(seccion):
    seccion(
        title="Lo que se construyó",
        subtitle="Tres decisiones que definen el resultado.",
        number="03",
        content=[
            "**Decisión 1** — Infraestructura parametrizada para cualquier modelo.",
            "**Decisión 2** — Despliegue con un solo comando.",
            "**Decisión 3** — Documentación técnica generada automáticamente.",
        ],
    )
    return


@app.cell
def _(mo, seccion):
    seccion(
        title="Flujo del pipeline",
        content=mo.mermaid("""
    graph LR
    A[Glue Job] --> B[Athena Table]
    B --> C[SageMaker Pipeline]
    C --> D[Predictions]
        """)
    )
    return


@app.cell
def _(seccion):
    seccion(
        title="Antes vs después",
        content="""
    | Antes | Después |
    |-------|---------|
    | Hardcodeado por modelo  | Parametrizado para cualquier modelo |
    | n modelos = n proyectos | n modelos = 1 framework             |
        """
    )
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## `cierre()` — Slide de cierre

    Banner grande y centrado con gradiente completo, mismo peso visual que `portada`.
    Usar como **última slide**. El parámetro exclusivo aquí es `stats`.

    ### Parámetros

    - `title` *(str, requerido)* — El mensaje de cierre. Hacerlo contundente y conclusivo.
    - `subtitle` *(str, default "\")* — Frase de apoyo bajo el título dentro del banner.
    - `team` *(str, default línea del equipo)* — Etiqueta del equipo al pie del banner.
    - `content` *(str | list[str] | componente mo, default None)* — Contenido opcional sobre las métricas.
    - `content_kind` *(str, default "neutral")* — Color de las cajitas cuando content es lista.
    - `stats` *(lista de tuplas de 3 strings, default None)* — Fila de cajitas de métricas bajo el banner. Cada tupla tiene exactamente tres strings: `(valor, label, caption)`.
        - `valor` — Texto grande destacado. Usar números, símbolos o 1–3 palabras cortas.
        - `label` — Etiqueta corta bajo el valor.
        - `caption` — Texto explicativo pequeño al pie de la cajita.

    ### Uso mínimo

    ```python
    cierre("Un framework para todos los modelos.")
    ```

    ### Uso completo con stats

    ```python
    cierre(
        title="Un framework para todos los modelos.",
        subtitle="La infraestructura está resuelta. El equipo se enfoca solo en ciencia de datos.",
        stats=[
            ("Bajo",  "Esfuerzo por modelo nuevo", "Solo configurar datos"),
            ("↑ ROI", "Retorno acumulado",          "Cada migración amortiza más"),
            ("100%",  "Trazabilidad",               "Registro automático extremo a extremo"),
            ("1",     "Código base compartido",     "Todos contribuyen al mismo repo"),
        ]
    )
    ```

    ### Notas sobre stats

    - Rango cómodo: 2 a 4 tuplas. Más de 5 se ve cargado visualmente.
    - `valor` se muestra grande — mantenerlo corto: número, símbolo o 1–3 palabras.
    - `label` y `caption` pueden ser frases completas.
    """)
    return


@app.cell
def _(cierre):
    cierre(
        title="Un framework para todos los modelos.",
        subtitle="La infraestructura está resuelta. El equipo se enfoca solo en ciencia de datos.",
        stats=[
            ("Bajo",  "Esfuerzo por modelo nuevo", "Solo configurar datos"),
            ("↑ ROI", "Retorno acumulado",          "Cada migración amortiza más"),
            ("100%",  "Trazabilidad",               "Registro automático extremo a extremo"),
            ("1",     "Código base compartido",     "Todos contribuyen al mismo repo"),
        ]
    )
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## Parámetro `colors` — personalizar colores del banner

    Todas las funciones aceptan `colors` como parámetro opcional.
    Si no se pasa, se usan los colores naranjos del equipo por defecto.

    ### Para `portada`, `intro` y `cierre`

    Tupla de 3 colores hex que definen el gradiente: `(inicio, medio, fin)`.

    ```python
    portada(
        title="Mi Proyecto",
        colors=("#1e3a5f", "#1d4ed8", "#3b82f6")   # tema azul
    )

    intro(
        title="El contexto",
        colors=("#14532d", "#15803d", "#22c55e")   # tema verde
    )
    ```

    ### Para `seccion`

    Tupla de 3 colores: `(color_borde, bg_inicio, bg_fin)`.
    El borde izquierdo toma el primer color; el fondo oscuro toma los otros dos.

    ```python
    seccion(
        title="Sección con tema azul",
        colors=("#3b82f6", "#0f172a", "#1e293b")
    )
    ```

    ### Paletas de ejemplo

    | Tema | colors |
    |------|--------|
    | Naranja (default equipo) | `("#7c2d12", "#c2410c", "#ea580c")` |
    | Azul oscuro | `("#1e3a5f", "#1d4ed8", "#3b82f6")` |
    | Verde | `("#14532d", "#15803d", "#22c55e")` |
    | Púrpura | `("#3b0764", "#7e22ce", "#a855f7")` |
    | Gris oscuro | `("#111827", "#374151", "#6b7280")` |
    """)
    return


@app.cell
def _(portada):
    portada(
        title="Proyecto con tema azul",
        subtitle="Mismo sistema, colores diferentes.",
        date="Abril 2026",
        colors=("#1e3a5f", "#1d4ed8", "#3b82f6"),
        content=[
            "**Punto clave 1** — El color no cambia la estructura.",
            "**Punto clave 2** — Solo se pasa la tupla `colors`.",
            "**Punto clave 3** — El resto funciona igual.",
        ],
        icon=None
    )
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## Parámetro `icon` — ícono institucional en el banner

    Todas las funciones aceptan `icon` como parámetro opcional.
    El ícono se embebe directamente en el HTML del banner como imagen base64.

    ### Cómo pasarlo

    ```python
    # Desde path string — la ruta es relativa al directorio donde corre marimo
    portada("Mi Proyecto", icon="img/logo.png")

    # Desde objeto Path
    from pathlib import Path
    portada("Mi Proyecto", icon=Path("img/logo.png"))

    # Desde bytes — útil si ya tienes la imagen cargada
    portada("Mi Proyecto", icon=Path("img/logo.png").read_bytes())
    ```

    ### Posición según el tipo de banner

    - `portada`, `intro`, `cierre` — esquina inferior derecha del banner (posición absoluta).
    - `seccion` — extremo derecho del banner, alineado verticalmente al centro.

    ### Formatos soportados

    PNG, JPG, SVG, GIF, WebP. Para logotipos se recomienda PNG con fondo transparente o SVG.

    ### Nota sobre rutas

    Si el ícono no se encuentra en la ruta indicada, el banner se muestra sin ícono
    (no lanza error). Esto evita que una ruta incorrecta rompa toda la presentación.
    """)
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(r"""
    ---

    ## Imágenes como contenido de slide

    Las imágenes van en el parámetro `content` usando `mo.image(...)`.
    Son distintas al `icon` del banner: estas van en el cuerpo de la slide.

    ### Desde un path local

    ```python
    seccion(
        title="Arquitectura del sistema",
        content=mo.image("img/arquitectura.png", width="80%")
    )
    ```

    ### Desde bytes (preferido — más portable)

    ```python
    from pathlib import Path

    seccion(
        title="Arquitectura del sistema",
        content=mo.image(Path("img/arquitectura.png").read_bytes(), width="80%")
    )
    ```

    ### Dos imágenes lado a lado

    ```python
    seccion(
        title="Antes y después",
        content=mo.hstack([
            mo.image("img/antes.png", width="48%"),
            mo.image("img/despues.png", width="48%"),
        ])
    )
    ```

    ### Texto + imagen combinados (content mixto)

    Cuando la lista mezcla strings y componentes mo, los elementos se apilan verticalmente.
    Los strings se convierten en `mo.md()` y los componentes mo se usan directamente.

    ```python
    seccion(
        title="Evidencia",
        content=[
            "El siguiente diagrama muestra el flujo completo del pipeline:",
            mo.mermaid("graph LR\n  A[Glue] --> B[Athena] --> C[SageMaker]"),
            "Y esta imagen muestra la arquitectura de infraestructura:",
            mo.image("img/arquitectura.png", width="70%"),
        ]
    )
    ```

    ### Ajustar tamaño

    El parámetro `width` acepta cualquier valor CSS:
    - `"80%"` — porcentaje del contenedor
    - `"400px"` — pixeles fijos
    - `"100%"` — ancho completo
    """)
    return


@app.cell
def _(mo, seccion):
    seccion(
        title="Contenido mixto — texto + diagrama",
        subtitle="Strings y componentes mo en la misma lista.",
        content=[
            "El siguiente diagrama muestra el flujo del pipeline de inferencia:",
            mo.mermaid("""
    graph LR
    A[Glue Job] --> B[Athena Table]
    B --> C[SageMaker Pipeline]
    C --> D[Predicciones S3]
            """),
            "Cada etapa corre de forma independiente y con trazabilidad automática.",
        ],
        footer="La infraestructura ya está resuelta — esto corre sin intervención manual.",
    )
    return


if __name__ == "__main__":
    app.run()
