 /*
Copyright 2019 @foostan         

 i  
Copyright 2020 Drashna Jaelre <@drashna>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include QMK_KEYBOARD_H

/* Macros */
/* enum custom_keycodes {EPRM = SAFE_RANGE, MACRO_3, MACRO_1, MACRO_0, MACRO_4, MACRO_2};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  if (record->event.pressed) {
    switch (keycode) {
    case MACRO_0: SEND_STRING(SS_LCTL(SS_TAP(X_X)) SS_TAP(X_0)); return false;
    case MACRO_1: SEND_STRING(SS_LCTL(SS_TAP(X_X)) SS_TAP(X_1)); return false;
    case MACRO_2: SEND_STRING(SS_LCTL(SS_TAP(X_X)) SS_TAP(X_2)); return false;
    case MACRO_3: SEND_STRING(SS_LCTL(SS_TAP(X_X)) SS_TAP(X_3)); return false;
    case MACRO_4: SEND_STRING(SS_LCTL(SS_TAP(X_X)) SS_TAP(X_K)); return false;
    case TMUX_BH: SEND_STRING(SS_LCTL())
    }
  }
  return true;
};
*/
/* Macros */
enum custom_keycodes {
    EPRM = SAFE_RANGE,
    EMACS_X0,    // cierra la ventana actual
    EMACS_X1,    // cierra las otras ventanas
    EMACS_X2,    // parte la ventana horizontalmente
    EMACS_X3,    // parte la ventana verticalmente
    EMACS_XK,    // cierra el buffer actual
    EMACS_XLEFT, // agranda la ventana hacia la izquierda
    EMACS_XRIGHT // agranda la ventana hacia la derecha
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (record->event.pressed) {
        switch (keycode) {
            case EMACS_X0: // cierra la ventana actual
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_0);
                return false;

            case EMACS_X1: // cierra las otras ventanas
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_1);
                return false;

            case EMACS_X2: // parte la ventana horizontalmente
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_2);
                return false;

            case EMACS_X3: // parte la ventana verticalmente
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_3);
                return false;

            case EMACS_XK: // cierra el buffer actual
                register_code(KC_LCTL);
                tap_code(KC_X);
                unregister_code(KC_LCTL);
                tap_code(KC_K);
                return false;
	    case EMACS_XLEFT: // agranda la ventana hacia la izquierda
	      // Enviakkr C-C
	      register_code(KC_LCTL);
	      tap_code(KC_C);
	      unregister_code(KC_LCTL);
    
	      tap_code(KC_LBRC); // [
	      return false;

	case EMACS_XRIGHT: // agranda la ventana hacia la derecha
	     // Enviakkr C-C
	      register_code(KC_LCTL);
	      tap_code(KC_C);
	      unregister_code(KC_LCTL);
    
	      tap_code(KC_RBRC); //  ]
	      return false;

        }
    }
    return true; // permitir el procesamiento de otros keycodes
}



const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [0] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
     KC_TAB,   KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,                         KC_Y,    KC_U,    KC_I,    KC_O,   KC_P,  KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
     KC_LSFT,  KC_A,    KC_S,    KC_D,    KC_F,    KC_G,                         KC_H,    KC_J,    KC_K,    KC_L, KC_SCLN, KC_QUOT,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
     KC_LCTL,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                         KC_N,    KC_M, KC_COMM,  KC_DOT, KC_SLSH,  KC_ESC,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                            KC_LALT,  LT(1, KC_ENT),  LT(4, KC_SPC),     LT(4, KC_SPC),  LT(2, KC_ENT),  KC_LGUI
                            //`------------------------------------'  `--------------------------------------'

  ),

    [1] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,------------------------------------------------------------.
     KC_TAB,    KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                        KC_PAGE_UP, KC_PAGE_DOWN, KC_UP, KC_HOME, KC_END,  KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |----------+--------+---------+--------+-------------+--------|
      KC_LSFT,   KC_6,    KC_7,    KC_8,    KC_9,    KC_0,                        _______,  KC_LEFT, KC_DOWN, KC_RIGHT,  _______,  _______, 
  //|--------+--------+--------+--------+--------+--------|                    |----------+--------+---------+---------+----------+----------|
     KC_LCTL,  _______, _______, _______, _______, _______,                       _______,  _______, _______, _______,   _______,   KC_ESC,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+----------+--------+---------+---------+----------+----------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT,   MO(3), KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),
                                                                                  
    [2] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
    KC_TILD, KC_EXLM, KC_AT, KC_HASH,  KC_DLR, KC_PERC,                         KC_ASTR, KC_PLUS, KC_CIRC, KC_LPRN, KC_RPRN,  KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
    KC_LSFT, KC_GRAVE, _______, _______, _______, _______,                       KC_SLSH,  KC_MINS, KC_EQL,  KC_LBRC, KC_RBRC, _______,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
     KC_LCTL, KC_BSLS, KC_PIPE, KC_SLSH, KC_MINS, KC_UNDS,                      KC_AMPR, _______, _______,  KC_LCBR, KC_RCBR, _______,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI,   MO(3),  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),

    [3] = LAYOUT_split_3x6_3(
  //,-----------------------------------------------------.                    ,-----------------------------------------------------.
      QK_BOOT, _______, _______, _______, _______, _______,                      _______, _______, _______, _______, _______, KC_BSPC,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      RGB_TOG, RGB_HUI, RGB_SAI, RGB_VAI, _______, _______,                      _______, _______, _______, _______, _______, _______,
  //|--------+--------+--------+--------+--------+--------|                    |--------+--------+--------+--------+--------+--------|
      RGB_MOD, RGB_HUD, RGB_SAD, RGB_VAD, _______, _______,                      _______, _______, _______, _______, _______, _______,
  //|--------+--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------+--------|
                                          KC_LGUI, _______,  KC_SPC,     KC_ENT, _______, KC_RALT
                                      //`--------------------------'  `--------------------------'
  ),
    [4] = LAYOUT_split_3x6_3(
        //,-----------------------------------------------------.                 ,-----------------------------------------------------------------------------.
        KC_TAB, EMACS_X0, EMACS_X1, EMACS_X3, EMACS_X2, EMACS_XK,               LWIN(KC_T), LWIN(KC_S),  LWIN(KC_UP),   LWIN(KC_O),    LWIN(KC_ENTER), LWIN(KC_Q),
        //|--------+--------+--------+--------+--------+--------|                 |-----------+-------------+--------------+---------------+---------+-----------|
        KC_LSFT,_______,   KC_F7,  KC_F8, EMACS_XLEFT, EMACS_XRIGHT,              _______, LWIN(KC_LEFT), LWIN(KC_DOWN), LWIN(KC_RIGHT), _______, _______,
        //|--------+--------+--------+--------+--------+--------|                 |----------+--------------+--------------+---------------+--------+--------|
        KC_LCTL, _______, _______, _______, _______, _______,                    _______,  _______,  _______, _______, _______, KC_ESC,
        //|--------+--------+--------+--------+--------+--------|  |--------+--------+--------+--------+--------+--------|--------|
                                        KC_LALT, _______,  _______,    _______, LWIN(KC_ENT), _______
                                      //`--------------------------'  `--------------------------'
 ),
};
