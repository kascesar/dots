"""
banners.py — Slides reutilizables para presentaciones del equipo.

BancoEstado · Analítica Avanzada · Modelos

API principal:
    from banners import portada, intro, seccion, cierre

Cada función retorna una slide completa (mo.vstack) lista para mostrar en una celda.

Ejemplo mínimo:
    portada("Mi Proyecto", subtitle="Contexto breve", date="Abril 2026")

Ejemplo con colores personalizados:
    portada("Mi Proyecto", colors=("#1e3a5f", "#1d4ed8", "#3b82f6"))

Ejemplo con ícono institucional:
    portada("Mi Proyecto", icon="img/logo.png")
"""

import base64
import ssl
import urllib.request
import marimo as mo
from pathlib import Path

# ── Paleta por defecto del equipo ─────────────────────────────────────────────

_DEFAULT_COLORS = ("#7c2d12", "#c2410c", "#ea580c")   # start, mid, end
_DEFAULT_SECCION_COLORS = ("#ea580c", "#1c0a00", "#1f1210")  # border, bg_start, bg_end

TEXT_MAIN  = "#ffffff"
TEXT_SUB   = "#ffedd5"
TEXT_MUTED = "#fdba74"
TEXT_TAG   = "#fed7aa"
TEAM_LINE  = "BancoEstado · Analítica Avanzada · Modelos"

# ── API pública ───────────────────────────────────────────────────────────────


def portada(
    title: str,
    subtitle: str = "",
    date: str = "",
    team: str = TEAM_LINE,
    content=None,
    content_kind: str = "neutral",
    footer: str = "",
    colors: "tuple[str, str, str] | None" = None,
    icon=None,
):
    """
    Slide de portada (slide 1). Banner grande y centrado.

    Parámetros
    ----------
    title        : Título principal del proyecto.
    subtitle     : Frase corta de contexto.
    date         : Fecha (ej. "Abril 2026").
    team         : Línea del equipo. Usa el default salvo excepción.
    content      : str, list[str|mo], o componente marimo (mo.mermaid, mo.image, etc.).
    content_kind : Estilo de las cajitas cuando content es list[str].
    footer       : Cita al pie de la slide. Se muestra como blockquote.
    colors       : Tupla (color1, color2, color3) en hex para el gradiente del banner.
                   Ej: ("#1e3a5f", "#1d4ed8", "#3b82f6") para azul.
    icon         : Dict {"src": <path|url|bytes>, "size": "60px"} o None.
    """
    banner = _banner_portada(title, subtitle, date, team, colors, icon)
    footer_block = [mo.md(f"> {footer}")] if footer else []
    return mo.vstack([banner, *_build_content(content, content_kind), *footer_block])


def intro(
    title: str,
    subtitle: str = "",
    tag: str = "",
    team: str = TEAM_LINE,
    alert: str = "",
    alert_kind: str = "warn",
    content=None,
    content_kind: str = "neutral",
    footer: str = "",
    colors: "tuple[str, str, str] | None" = None,
    icon=None,
):
    """
    Slide de introducción o contexto (slides 2–3).
    Banner compacto con gradiente, ideal para abrir un bloque temático.

    Parámetros
    ----------
    title        : Título de la slide.
    subtitle     : Frase de apoyo bajo el título.
    tag          : Etiqueta de sección (ej. "Contexto", "Problema"). Si está vacío usa team.
    team         : Línea del equipo. Usa el default salvo excepción.
    alert        : Callout destacado full-width sobre las columnas. Markdown.
    alert_kind   : Estilo del alert (warn, danger, info, success, neutral).
    content      : str, list[str|mo], o componente marimo.
    content_kind : Estilo de las cajitas cuando content es list[str].
    footer       : Cita al pie de la slide. Se muestra como blockquote.
    colors       : Tupla (color1, color2, color3) en hex para el gradiente del banner.
    icon         : Dict {"src": <path|url|bytes>, "size": "60px"} o None.
    """
    banner = _banner_intro(title, subtitle, tag, team, colors, icon)
    alert_block = [mo.callout(mo.md(alert), kind=alert_kind)] if alert else []
    footer_block = [mo.md(f"> {footer}")] if footer else []
    return mo.vstack([banner, *alert_block, *_build_content(content, content_kind), *footer_block])


def seccion(
    title: str,
    subtitle: str = "",
    number: str = "",
    content=None,
    content_kind: str = "neutral",
    footer: str = "",
    colors: "tuple[str, str, str] | None" = None,
    icon=None,
):
    """
    Slide intermedia de sección.
    Borde izquierdo naranja, fondo oscuro sutil. Deja más aire al contenido.

    Parámetros
    ----------
    title        : Título de la sección.
    subtitle     : Frase de apoyo.
    number       : Número de sección (ej. "03"). Se muestra grande a la izquierda.
    content      : str, list[str|mo], o componente marimo.
    content_kind : Estilo de las cajitas cuando content es list[str].
    footer       : Cita al pie de la slide.
    colors       : Tupla (border_color, bg_start, bg_end) en hex.
                   Ej: ("#3b82f6", "#0f172a", "#1e293b") para azul.
    icon         : Dict {"src": <path|url|bytes>, "size": "60px"} o None.
    """
    banner = _banner_seccion(title, subtitle, number, colors, icon)
    footer_block = [mo.md(f"> {footer}")] if footer else []
    return mo.vstack([banner, *_build_content(content, content_kind), *footer_block])


def cierre(
    title: str,
    subtitle: str = "",
    team: str = TEAM_LINE,
    content=None,
    content_kind: str = "neutral",
    stats: "list[tuple[str, str, str]] | None" = None,
    footer: str = "",
    colors: "tuple[str, str, str] | None" = None,
    icon=None,
):
    """
    Slide de cierre. Banner grande centrado con mensaje contundente.

    Parámetros
    ----------
    title        : Mensaje principal de cierre.
    subtitle     : Frase de apoyo.
    team         : Línea del equipo.
    content      : str, list[str|mo], o componente marimo.
    content_kind : Estilo de las cajitas cuando content es list[str].
    stats        : Lista de métricas: [(valor, label, caption), ...].
    footer       : Cita al pie de la slide. Se muestra como blockquote.
    colors       : Tupla (color1, color2, color3) en hex para el gradiente del banner.
    icon         : Dict {"src": <path|url|bytes>, "size": "60px"} o None.
    """
    banner = _banner_cierre(title, subtitle, team, colors, icon)
    stats_block = (
        [mo.hstack(
            [mo.stat(v, label=l, caption=c, bordered=True) for v, l, c in stats],
            justify="space-around",
        )]
        if stats else []
    )
    footer_block = [mo.md(f"> {footer}")] if footer else []
    return mo.vstack([banner, *_build_content(content, content_kind), *stats_block, *footer_block])


# ── Constructores de banner (HTML) ────────────────────────────────────────────


def _banner_portada(title, subtitle, date, team, colors, icon) -> mo.Html:
    c1, c2, c3 = colors if colors else _DEFAULT_COLORS
    gradient = f"linear-gradient(135deg, {c1} 0%, {c2} 50%, {c3} 100%)"
    team_html     = _tag_line(team, TEXT_TAG) if team else ""
    subtitle_html = f'<p style="font-size:1.3rem;color:{TEXT_SUB};margin:1rem 0 0.25rem;">{subtitle}</p>' if subtitle else ""
    date_html     = f'<p style="font-size:0.9rem;color:{TEXT_MUTED};margin-top:0.5rem;">{date}</p>' if date else ""
    icon_html     = _icon_corner(icon, bottom="1rem", right="1.5rem", default_size="44px")
    return mo.Html(f"""
    <div style="position:relative;text-align:center;padding:3rem 2rem 1.5rem;
                background:{gradient};border-radius:1rem;color:{TEXT_MAIN};margin-bottom:1.5rem;">
        {team_html}
        <h1 style="font-size:3.2rem;font-weight:800;margin:0.5rem 0;color:{TEXT_MAIN};line-height:1.15;">{title}</h1>
        {subtitle_html}
        {date_html}
        {icon_html}
    </div>
    """)


def _banner_intro(title, subtitle, tag, team, colors, icon) -> mo.Html:
    c1, c2, c3 = colors if colors else _DEFAULT_COLORS
    gradient  = f"linear-gradient(135deg, {c1} 0%, {c2} 50%, {c3} 100%)"
    label     = tag if tag else team
    top_html  = _tag_line(label, TEXT_TAG)
    subtitle_html = f'<p style="font-size:1.1rem;color:{TEXT_SUB};margin:0.5rem 0 0;">{subtitle}</p>' if subtitle else ""
    icon_html = _icon_corner(icon, bottom="0.75rem", right="1.25rem", default_size="36px")
    return mo.Html(f"""
    <div style="position:relative;padding:1.6rem 2rem;background:{gradient};
                border-radius:0.75rem;color:{TEXT_MAIN};margin-bottom:1.25rem;">
        {top_html}
        <h2 style="font-size:2rem;font-weight:700;margin:0;color:{TEXT_MAIN};line-height:1.2;">{title}</h2>
        {subtitle_html}
        {icon_html}
    </div>
    """)


def _banner_seccion(title, subtitle, number, colors, icon) -> mo.Html:
    border_color, bg1, bg2 = colors if colors else _DEFAULT_SECCION_COLORS
    number_html   = (
        f'<div style="font-size:2.5rem;font-weight:900;color:{border_color};'
        f'line-height:1;margin-bottom:0.2rem;opacity:0.85;flex-shrink:0;">{number}</div>'
    ) if number else ""
    subtitle_html = f'<p style="font-size:0.95rem;color:#d1d5db;margin:0.3rem 0 0;">{subtitle}</p>' if subtitle else ""
    icon_html     = _icon_inline(icon, default_size="32px", style="margin-left:auto;flex-shrink:0;opacity:0.8;")
    return mo.Html(f"""
    <div style="padding:1.1rem 1.5rem;background:linear-gradient(90deg,{bg1} 0%,{bg2} 100%);
                border-left:5px solid {border_color};border-radius:0 0.5rem 0.5rem 0;
                color:{TEXT_MAIN};margin-bottom:1.25rem;display:flex;align-items:center;gap:1.2rem;">
        {number_html}
        <div style="flex:1;">
            <h3 style="font-size:1.5rem;font-weight:700;margin:0;color:{TEXT_MAIN};">{title}</h3>
            {subtitle_html}
        </div>
        {icon_html}
    </div>
    """)


def _banner_cierre(title, subtitle, team, colors, icon) -> mo.Html:
    c1, c2, c3 = colors if colors else _DEFAULT_COLORS
    gradient      = f"linear-gradient(135deg, {c1} 0%, {c2} 50%, {c3} 100%)"
    subtitle_html = f'<p style="font-size:1.2rem;color:{TEXT_SUB};margin:1rem 0 1.5rem;">{subtitle}</p>' if subtitle else ""
    team_html     = (
        f'<div style="font-size:0.78rem;letter-spacing:0.15em;text-transform:uppercase;'
        f'color:{TEXT_MUTED};margin-top:1rem;">{team}</div>'
    ) if team else ""
    icon_html = _icon_corner(icon, bottom="1rem", right="1.5rem", default_size="44px")
    return mo.Html(f"""
    <div style="position:relative;text-align:center;padding:3rem 2rem 2rem;
                background:{gradient};border-radius:1rem;color:{TEXT_MAIN};margin-bottom:1.5rem;">
        <h1 style="font-size:2.6rem;font-weight:800;color:{TEXT_MAIN};margin-bottom:0.5rem;line-height:1.2;">{title}</h1>
        {subtitle_html}
        {team_html}
        {icon_html}
    </div>
    """)


# ── Helper de contenido ───────────────────────────────────────────────────────


def _build_content(content, kind: str) -> list:
    """
    Convierte content en componentes marimo:

    - None              → []
    - str               → [mo.md(content)]           markdown full-width
    - list[str]         → [mo.hstack([callouts])]    columnas iguales
    - list[str|mo]      → [mo.vstack([items])]       mix vertical (str→mo.md, mo→directo)
    - componente mo     → [content]                  pasa directo sin envolver
    """
    if content is None:
        return []
    if isinstance(content, str):
        return [mo.md(content)]
    if isinstance(content, list):
        all_strings = all(isinstance(c, str) for c in content)
        if all_strings:
            # Todas strings → columnas side-by-side con callout
            cols = [mo.callout(mo.md(c), kind=kind) for c in content]
            return [mo.hstack(cols, justify="space-between")]
        else:
            # Mix de strings y componentes → stack vertical
            items = [mo.md(c) if isinstance(c, str) else c for c in content]
            return [mo.vstack(items)]
    # Componente marimo directo
    return [content]


# ── Helpers privados ──────────────────────────────────────────────────────────


def _tag_line(text: str, color: str) -> str:
    return (
        f'<div style="font-size:0.85rem;letter-spacing:0.2em;text-transform:uppercase;'
        f'color:{color};margin-bottom:0.5rem;">{text}</div>'
    )


def _icon_src(icon) -> str:
    """
    Devuelve el valor del atributo src para un <img> como data URI base64.
    - URL   → descarga en Python y embebe (evita CORS del browser)
    - Path  → lee archivo y embebe
    - bytes → embebe directamente
    """
    _EXT_MIME = {"jpg": "jpeg", "jpeg": "jpeg", "png": "png",
                 "svg": "svg+xml", "gif": "gif", "webp": "webp"}

    if isinstance(icon, str) and icon.startswith(("http://", "https://")):
        ext = icon.split("?")[0].rsplit(".", 1)[-1].lower()
        mime = _EXT_MIME.get(ext, "png")
        req = urllib.request.Request(icon, headers={"User-Agent": "Mozilla/5.0"})
        ctx = ssl.create_default_context()
        ctx.check_hostname = False
        ctx.verify_mode = ssl.CERT_NONE
        with urllib.request.urlopen(req, timeout=10, context=ctx) as r:
            data = r.read()
            ct = r.headers.get("Content-Type", "")
        if "svg" in ct:
            mime = "svg+xml"
        elif "jpeg" in ct or "jpg" in ct:
            mime = "jpeg"
        elif "png" in ct:
            mime = "png"
        b64 = base64.b64encode(data).decode()
        return f"data:image/{mime};base64,{b64}"

    if isinstance(icon, (str, Path)):
        p = Path(icon)
        mime = _EXT_MIME.get(p.suffix.lower().lstrip("."), "png")
        b64 = base64.b64encode(p.read_bytes()).decode()
        return f"data:image/{mime};base64,{b64}"

    # bytes
    b64 = base64.b64encode(icon).decode()
    return f"data:image/png;base64,{b64}"


def _icon_corner(icon: "dict | None", bottom: str, right: str, default_size: str) -> str:
    """Ícono en esquina con posición absoluta (para banners centrados).
    icon = {"src": <path|url|bytes>, "size": "60px"}
    """
    if icon is None:
        return ""
    try:
        size = icon.get("size", default_size)
        src  = _icon_src(icon["src"])
        return (
            f'<img src="{src}" '
            f'style="position:absolute;bottom:{bottom};right:{right};'
            f'height:{size};opacity:0.85;border-radius:4px;">'
        )
    except Exception:
        return ""


def _icon_inline(icon: "dict | None", default_size: str, style: str = "") -> str:
    """Ícono inline (para banners horizontales como seccion).
    icon = {"src": <path|url|bytes>, "size": "32px"}
    """
    if icon is None:
        return ""
    try:
        size = icon.get("size", default_size)
        src  = _icon_src(icon["src"])
        return f'<img src="{src}" style="height:{size};{style}border-radius:4px;">'
    except Exception:
        return ""
