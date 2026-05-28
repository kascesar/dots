/*
 * Copyright 2019 @foostan
 * Copyright 2020 Drashna Jaelre <@drashna>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H

/* ── Keycodes personalizados ─────────────────────────────────────────────── */

enum custom_keycodes {
    EPRM = SAFE_RANGE,
    TILDE_ESC,    // tap = dead_acute (´), hold = Escape
    LATAM_GRV,    // backtick: dead_grave + space
    LATAM_CIRC,   // caret:    dead_circumflex + space
    EMACS_X0,     // C-x 0        — cierra la ventana actual
    EMACS_X1,     // C-x 1        — cierra las otras ventanas
    EMACS_X2,     // C-x 2        — parte la ventana horizontalmente
    EMACS_X3,     // C-x 3        — parte la ventana verticalmente
    EMACS_XK,     // C-x k        — cierra el buffer actual
    EMACS_XCS,    // C-x C-s      — guarda el buffer actual
    EMACS_INDENT, // C-u 4 C-x TAB  — indenta 4 espacios
    EMACS_DEDENT, // C-u -4 C-x TAB — desindenta 4 espacios
    EMACS_XG      // C-x g          — abre Magit
};

/* ── Macros ──────────────────────────────────────────────────────────────── */

static uint16_t tilde_esc_timer;

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case TILDE_ESC:
            if (record->event.pressed) {
                tilde_esc_timer = timer_read();
            } else {
                if (timer_elapsed(tilde_esc_timer) < TAPPING_TERM) {
                    tap_code(KC_LBRC);  // dead acute en es-latam
                } else {
                    tap_code(KC_ESC);
                }
            }
            return false;

        case LATAM_GRV:
            if (record->event.pressed) {
                tap_code16(RALT(KC_BSLS));  // dead_grave (BKSL nivel 3)
                tap_code(KC_SPC);            // → `
            }
            return false;

        case LATAM_CIRC:
            if (record->event.pressed) {
                tap_code16(RALT(KC_QUOT));  // dead_circumflex (AC11 nivel 3)
                tap_code(KC_SPC);            // → ^
            }
            return false;
    }

    if (record->event.pressed) {
        switch (keycode) {
            case EMACS_X0:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_0);
                return false;

            case EMACS_X1:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_1);
                return false;

            case EMACS_X2:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_2);
                return false;

            case EMACS_X3:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_3);
                return false;

            case EMACS_XK:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_K);
                return false;

            case EMACS_XCS:
                // C-x C-s — guardar buffer
                register_code(KC_LCTL);
                tap_code(KC_X);
                tap_code(KC_S);
                unregister_code(KC_LCTL);
                return false;

            case EMACS_INDENT:
                // C-u 4 C-x TAB — indent-rigidly +4
                register_code(KC_LCTL);
                tap_code(KC_U);
                unregister_code(KC_LCTL);
                tap_code(KC_4);
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_TAB);
                return false;

            case EMACS_DEDENT:
                // C-u -4 C-x TAB — indent-rigidly -4
                register_code(KC_LCTL);
                tap_code(KC_U);
                unregister_code(KC_LCTL);
                tap_code(KC_MINS);
                tap_code(KC_4);
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_TAB);
                return false;

            case EMACS_XG:
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_G);
                return false;
        }
    }
    return true;
}

/* ── Capas ───────────────────────────────────────────────────────────────── */

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

    /* Layer 0 — Base (QWERTY)
     * ,--------------------------------------------.    ,--------------------------------------------.
     * | Tab  |  Q   |  W   |  E   |  R   |  T   |    |  Y   |  U   |  I   |  O   |  P   | Bksp |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Shft |  A   |  S   |  D   |  F   |  G   |    |  H   |  J   |  K   |  L   |  ;   |  '   |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Ctrl |  Z   |  X   |  C   |  V   |  B   |    |  N   |  M   |  ,   |  .   |  /   | Esc  |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | Alt  |LT1↵  |LT4 ␣ |    |LT4 ␣ |LT2↵  | GUI  |
     *                      `--------------------'    `--------------------'
     */
    [0] = LAYOUT_split_3x6_3(
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,         KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC,
        KC_LSFT, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,         KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_MINS,
        KC_LCTL, KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,         KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, TILDE_ESC,
                          KC_LALT, LT(1, KC_ENT), LT(4, KC_SPC),  LT(4, KC_SPC), LT(2, KC_ENT), KC_LGUI
    ),

    /* Layer 1 — Números y Navegación  (mantener LT1↵ izquierdo)
     * ,--------------------------------------------.    ,----------------------------------------------------.
     * | Tab  |  1   |  2   |  3   |  4   |  5   |    | PgUp | PgDn |  ↑   | Home | End  | Bksp |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Shft |  6   |  7   |  8   |  9   |  0   |    |      |  ←   |  ↓   |  →   |      |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Ctrl |      |      |      |      |      |    |      |      |      |      |      | Esc  |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | GUI  |[HOLD]| Spc  |    | Ent  | MO3  | RAlt |
     *                      `--------------------'    `--------------------'
     */
    [1] = LAYOUT_split_3x6_3(
        KC_TAB,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,         KC_PAGE_UP, KC_PAGE_DOWN, KC_UP,   KC_HOME,  KC_END,  KC_BSPC,
        KC_LSFT, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,         _______,    KC_LEFT,      KC_DOWN, KC_RIGHT, _______, _______,
        KC_LCTL, _______, _______, _______, _______, _______,      _______,    _______,      _______, _______,  _______, KC_ESC,
                          KC_LGUI, _______, KC_SPC,                KC_ENT,     MO(3),        KC_RALT
    ),

    /* Layer 2 — Símbolos estilo DVP  (mantener LT2↵ derecho)
     *
     * Filosofía: los delimitadores de apertura son directos (sin Shift),
     * los de cierre se obtienen con Shift en la misma tecla.
     * Los operadores más frecuentes en código (= - < >) suben a la fila
     * superior derecha bajo los dedos fuertes.
     *
     * ,--------------------------------------------.    ,--------------------------------------------.
     * |  ~   |  !   |  @   |  #   |  $   |  %   |    |  {   |  [   |  (   |  =   |  -   | Bksp |
     * |      |      |      |      |      |      |    | S:}  | S:]  | S:)  | S:+  | S:_  |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Shft |  `   |  <   |  \   |  ;   |  ^   |    |  /   |  *   |  &   |  |   |  '   |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Ctrl |  "   |  >   |  :   |  _   |  ~   |    |  ?   |  !   |  @   |  ^   |  %   |      |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | GUI  | MO3  | Spc  |    | Ent  |[HOLD]| RAlt |
     *                      `--------------------'    `--------------------'
     *
     * Nota: las teclas marcadas S: producen ese carácter al presionar Shift.
     * En QMK las teclas de la fila superior derecha usan KC_xx sin shift —
     * el Shift del OS invierte la lógica (cierre en Shift, apertura directo).
     */
    /* Layer 2 — Símbolos (es-latam)
     * Fila 1: espejo de la fila numérica latam con Shift
     * Fila 2: guiones/slashes izq | abre-brackets der  (S: da el cierre)
     * Fila 3: ángulos/comillas/@ izq | ; : * der
     *
     * ,--------------------------------------------.    ,--------------------------------------------.
     * |  !   |  "   |  #   |  $   |  %   |  &   |    |  /   |  (   |  )   |  =   |  ?   | Bksp |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Shft |  -   |  _   |  ~   |  \   |  |   |    |  {   |  }   |  [   |  ]   |  ^   |      |
     * |      | S:_  |      |      |      |      |    | S:[  | S:]  |      |      |      |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Ctrl |  <   |  >   |  '   |  `   |  @   |    |  ;   |  :   |  *   |      |      |  +   |
     * |      | S:>  |      | S:?  |      |      |    |      |      |      |      |      |      |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | GUI  | MO3  | Spc  |    | Ent  |      | RAlt |
     *                      `--------------------'    `--------------------'
     */
    [2] = LAYOUT_split_3x6_3(
        KC_EXLM,    KC_AT,      KC_HASH,    KC_DLR,     KC_PERC,    KC_CIRC,      KC_AMPR, KC_ASTR, KC_LPRN, KC_RPRN, KC_UNDS, KC_BSPC,
        KC_LSFT,    KC_SLSH,  KC_QUES, RALT(KC_RBRC), RALT(KC_MINS), KC_GRAVE,   KC_QUOT,  KC_BSLS, KC_DQUO,  KC_PIPE, LATAM_CIRC, _______,
        KC_LCTL,    KC_NUBS, LSFT(KC_NUBS), KC_MINS,  LATAM_GRV,  RALT(KC_2),   KC_LT,    KC_GT,   KC_RCBR, _______, _______, KC_RBRC,
                                KC_LGUI,    MO(3),      KC_SPC,                   KC_ENT,  _______, KC_RALT
    ),

    /* Layer 3 — Sistema / RGB  (MO3 desde L1 o L2)
     * ,--------------------------------------------.    ,--------------------------------------------.
     * | Boot |      |      |      |      |      |    |      |      |      |      |      | Bksp |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | TOG  | HUE+ | SAT+ | VAL+ |      |      |    |      |      |      |      |      |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | MOD  | HUE- | SAT- | VAL- |      |      |    |      |      |      |      |      |      |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | GUI  |      | Spc  |    | Ent  |      | RAlt |
     *                      `--------------------'    `--------------------'
     */
    [3] = LAYOUT_split_3x6_3(
        QK_BOOT, _______, _______, _______, _______, _______,      _______, _______, _______, _______, _______, KC_BSPC,
        UG_TOGG, UG_HUEU, UG_SATU, UG_VALU, _______, _______,      _______, _______, _______, _______, _______, _______,
        UG_NEXT, UG_HUED, UG_SATD, UG_VALD, _______, _______,      _______, _______, _______, _______, _______, _______,
                          KC_LGUI, _______, KC_SPC,                KC_ENT,  _______, KC_RALT
    ),

    /* Layer 4 — Emacs + Gestor de ventanas  (mantener LT4␣)
     *
     * Cambio respecto a versión anterior:
     *   - F10 reemplazado por EMACS_XCS (C-x C-s, guardar buffer)
     *
     * ,--------------------------------------------.    ,----------------------------------------------------.
     * | Tab  | X0   | X1   | X3   | X2   | XK   |    |Win+T |Win+S |Win+↑ |W+F11 |Win+Y |Win+Q |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Shft |      | XG   | XCS  | DE   | IN   |    |      |Win+← |Win+↓ |Win+→ |      |      |
     * |------+------+------+------+------+------|    |------+------+------+------+------+------|
     * | Ctrl |      |      |      |      |      |    |      |      |      |      |      | Esc  |
     * `--------------------+------+------+------|    |------+------+------+--------------------'
     *                      | Alt  |      |[HOLD]|    |[HOLD]|Win+Y |      |
     *                      `--------------------'    `--------------------'
     */
    [4] = LAYOUT_split_3x6_3(
        KC_TAB,  EMACS_X0,  EMACS_X1,  EMACS_X3,  EMACS_X2,    EMACS_XK,      LWIN(KC_T), LWIN(KC_S),    LWIN(KC_UP),   LWIN(KC_F11), LWIN(KC_Y),  LWIN(KC_Q),
        KC_LSFT, _______,   EMACS_XG,  EMACS_XCS, EMACS_DEDENT,EMACS_INDENT,  _______,    LWIN(KC_LEFT), LWIN(KC_DOWN), LWIN(KC_RIGHT),_______,     _______,
        KC_LCTL, _______,   _______,   _______,   _______,     _______,       _______,    _______,       _______,       _______,       _______,     KC_ESC,
                            KC_LALT,   _______,   _______,                    _______,    LWIN(KC_Y),    _______
    ),
};
