// Copyright 2012 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// A KeyMap consists of bindings from key strokes to functions.
//
// Key strokes are represented as the name of the key (see keyCodes above),
// optionally prefixed by one or more modifiers (see modCodes above). For
// example: CTRL+X or SHIFT+ALT+BACK_SLASH
//
// A binding is one or more key strokes separated by commas and/or spaces
// that map to a handler function. The set of bindings is represented as a
// Object. For example:
//    {
//      'ALT+UP': function() { scroll(-1); },
//      'ALT+DOWN': function() { scroll(1); },
//      'ESCAPE, CTRL+U': function() { cancelMode(); }
//    }
// In the third line, the same function is bound to two different key strokes.
//
// The KeyMap can be the set of bindings itself, or if convienent, it may
// have two members: bindings and functions. In this case, the bindings can
// map to a string, and that is used to locate the handler function in
// functions.
//    {
//      functions: {
//        pageUp:   function() { scroll(-10); },
//        pageDown: function() { scroll(10); },
//        lineUp:   function() { scroll(-1); },
//        lineDown: function() { scroll(1); }
//      },
//      bindings: {
//        'PAGE_DOWN':   'pageDown',
//        'PAGE_UP':     'pageUp',
//        'SPACE':       'pageDown',
//        'SHIFT+SPACE': 'pageUp',
//      }
//    }
//
// The handlers are slightly different than jQuery event handlers: They don't
// get the event object, and a return value of undefined means the event has
// been handled, and should no longer be processed. To influence the event
// handling, one of the following exported values can be returned:
//    PASS    -- continue to pass on the event for processing
//    HANDLED -- stop all further event processing, same as returning undefined
//    STOP_PROPIGATION -- continue processing event on the current DOM element,
//                        but no longer propigate it

define(['jquery'], function($) {
  "use strict";


  var keyCodes = Object.create(null);
  // from https://developer.mozilla.org/en-US/docs/DOM/KeyboardEvent
  keyCodes['CANCEL'] = 3;
  keyCodes['HELP'] = 6;
  keyCodes['BACK_SPACE'] = 8;
  keyCodes['TAB'] = 9;
  keyCodes['CLEAR'] = 12;
  keyCodes['RETURN'] = 13;
  keyCodes['ENTER'] = 14;
  keyCodes['SHIFT'] = 16;
  keyCodes['CONTROL'] = 17;
  keyCodes['ALT'] = 18;
  keyCodes['PAUSE'] = 19;
  keyCodes['CAPS_LOCK'] = 20;
  keyCodes['ESCAPE'] = 27;
  keyCodes['SPACE'] = 32;
  keyCodes['PAGE_UP'] = 33;
  keyCodes['PAGE_DOWN'] = 34;
  keyCodes['END'] = 35;
  keyCodes['HOME'] = 36;
  keyCodes['LEFT'] = 37;
  keyCodes['UP'] = 38;
  keyCodes['RIGHT'] = 39;
  keyCodes['DOWN'] = 40;
  keyCodes['SELECT'] = 41;
  keyCodes['PRINT'] = 42;
  keyCodes['EXECUTE'] = 43;
  keyCodes['PRINTSCREEN'] = 44;
  keyCodes['INSERT'] = 45;
  keyCodes['DELETE'] = 46;
  keyCodes['0'] = 48;
  keyCodes['1'] = 49;
  keyCodes['2'] = 50;
  keyCodes['3'] = 51;
  keyCodes['4'] = 52;
  keyCodes['5'] = 53;
  keyCodes['6'] = 54;
  keyCodes['7'] = 55;
  keyCodes['8'] = 56;
  keyCodes['9'] = 57;
  keyCodes['COLON'] = 58;
  keyCodes['SEMICOLON'] = 59;
  keyCodes['LESS_THAN'] = 60;
  keyCodes['EQUALS'] = 61;
  keyCodes['GREATER_THAN'] = 62;
  keyCodes['QUESTION_MARK'] = 63;
  keyCodes['AT'] = 64;
  keyCodes['A'] = 65;
  keyCodes['B'] = 66;
  keyCodes['C'] = 67;
  keyCodes['D'] = 68;
  keyCodes['E'] = 69;
  keyCodes['F'] = 70;
  keyCodes['G'] = 71;
  keyCodes['H'] = 72;
  keyCodes['I'] = 73;
  keyCodes['J'] = 74;
  keyCodes['K'] = 75;
  keyCodes['L'] = 76;
  keyCodes['M'] = 77;
  keyCodes['N'] = 78;
  keyCodes['O'] = 79;
  keyCodes['P'] = 80;
  keyCodes['Q'] = 81;
  keyCodes['R'] = 82;
  keyCodes['S'] = 83;
  keyCodes['T'] = 84;
  keyCodes['U'] = 85;
  keyCodes['V'] = 86;
  keyCodes['W'] = 87;
  keyCodes['X'] = 88;
  keyCodes['Y'] = 89;
  keyCodes['Z'] = 90;
  keyCodes['CONTEXT_MENU'] = 93;
  keyCodes['NUMPAD0'] = 96;
  keyCodes['NUMPAD1'] = 97;
  keyCodes['NUMPAD2'] = 98;
  keyCodes['NUMPAD3'] = 99;
  keyCodes['NUMPAD4'] = 100;
  keyCodes['NUMPAD5'] = 101;
  keyCodes['NUMPAD6'] = 102;
  keyCodes['NUMPAD7'] = 103;
  keyCodes['NUMPAD8'] = 104;
  keyCodes['NUMPAD9'] = 105;
  keyCodes['MULTIPLY'] = 106;
  keyCodes['ADD'] = 107;
  keyCodes['SEPARATOR'] = 108;
  keyCodes['SUBTRACT'] = 109;
  keyCodes['DECIMAL'] = 110;
  keyCodes['DIVIDE'] = 111;
  keyCodes['F1'] = 112;
  keyCodes['F2'] = 113;
  keyCodes['F3'] = 114;
  keyCodes['F4'] = 115;
  keyCodes['F5'] = 116;
  keyCodes['F6'] = 117;
  keyCodes['F7'] = 118;
  keyCodes['F8'] = 119;
  keyCodes['F9'] = 120;
  keyCodes['F10'] = 121;
  keyCodes['F11'] = 122;
  keyCodes['F12'] = 123;
  keyCodes['F13'] = 124;
  keyCodes['F14'] = 125;
  keyCodes['F15'] = 126;
  keyCodes['F16'] = 127;
  keyCodes['F17'] = 128;
  keyCodes['F18'] = 129;
  keyCodes['F19'] = 130;
  keyCodes['F20'] = 131;
  keyCodes['F21'] = 132;
  keyCodes['F22'] = 133;
  keyCodes['F23'] = 134;
  keyCodes['F24'] = 135;
  keyCodes['NUM_LOCK'] = 144;
  keyCodes['SCROLL_LOCK'] = 145;
  keyCodes['CIRCUMFLEX'] = 160;
  keyCodes['EXCLAMATION'] = 161;
  keyCodes['DOUBLE_QUOTE'] = 162;
  keyCodes['HASH'] = 163;
  keyCodes['DOLLAR'] = 164;
  keyCodes['PERCENT'] = 165;
  keyCodes['AMPERSAND'] = 166;
  keyCodes['UNDERSCORE'] = 167;
  keyCodes['OPEN_PAREN'] = 168;
  keyCodes['CLOSE_PAREN'] = 169;
  keyCodes['ASTERISK'] = 170;
  keyCodes['PLUS'] = 171;
  keyCodes['PIPE'] = 172;
  keyCodes['HYPHEN_MINUS'] = 173;
  keyCodes['OPEN_CURLY_BRACKET'] = 174;
  keyCodes['CLOSE_CURLY_BRACKET'] = 175;
  keyCodes['TILDE'] = 176;
  keyCodes['COMMA'] = 188;
  keyCodes['PERIOD'] = 190;
  keyCodes['SLASH'] = 191;
  keyCodes['BACK_QUOTE'] = 192;
  keyCodes['OPEN_BRACKET'] = 219;
  keyCodes['BACK_SLASH'] = 220;
  keyCodes['CLOSE_BRACKET'] = 221;
  keyCodes['QUOTE'] = 222;
  keyCodes['META'] = 224;

  function keyNameToCode(key) {
    var code = keyCodes[key];
    if (code === undefined) {
      console.log('input.js: unknown key name:', key);
    }
    return code;
  }

  // must be distinct bits, and distinct from all keyCodes
  var ALT_CODE = 0x1000000;
  var CTRL_CODE = 0x2000000;
  var META_CODE = 0x4000000;
  var SHIFT_CODE = 0x8000000;

  var modCodes = Object.create(null);
  modCodes['ALT'] = ALT_CODE;
  modCodes['CTRL'] = CTRL_CODE;
  modCodes['META'] = META_CODE;
  modCodes['SHIFT'] = SHIFT_CODE;


  function modNameToCode(mod) {
    var code = modCodes[mod];
    if (code === undefined) {
      console.log('input.js: unknown modifier name:', mod);
    }
    return code;
  }

  function compileKeyMap(map) {
    var functions;
    var bindings = map;
    if (map.bindings) {
      bindings = map.bindings;
      functions = map.functions;
    }

    var lookup = Object.create(null);

    for (var specs in bindings) {
      if (!bindings.hasOwnProperty(specs)) continue;

      var handler = bindings[specs];
      if (typeof(handler) !== 'function') {
        var func;
        if (functions && functions.hasOwnProperty(handler)) {
          func = functions[handler];
        }
        if (typeof(func) !== 'function') {
          console.log('input.js: bad binding:', handler);
          continue;
        }
        handler = func;
      }

      specs.split(/[, ]+/).forEach(function(spec) {
        var parts = spec.split('+');
        if (parts.length === 0) return;
        var keyCode = keyNameToCode(parts.pop());
        var modCode = 0;
        parts.forEach(function(mod) { modCode |= modNameToCode(mod); });
        var k = 'k' + String(modCode+keyCode);
        lookup[k] = handler;
      });
    }
    return lookup;
  }

  // handler completion codes
  var PASS = 0;
  var HANDLED = 1;
  var STOP_PROPIGATION = 2;


  function handleKeyEvent(lookup, e, t) {
    var code = e.keyCode;
    if (e.altKey) code += ALT_CODE;
    if (e.ctrlKey) code += CTRL_CODE;
    if (e.metaKey) code += META_CODE;
    if (e.shiftKey) code += SHIFT_CODE;
    var k = 'k' + String(code);
    var f = lookup[k];
    if (f === undefined) return;
    switch (f(t)) {
      case PASS: return;
      case STOP_PROPIGATION: e.stopPropagation(); return;
      default: return false;
    }
  }

  function keyHandler(map) {
    var lookup = compileKeyMap(map);
    return function(e, t) {
      return handleKeyEvent(lookup, e, t);
    }
  }

  return {
    PASS: PASS,
    HANDLED: HANDLED,
    STOP_PROPIGATION: STOP_PROPIGATION,
    keyHandler: keyHandler
  };
});
