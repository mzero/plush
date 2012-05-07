// Copyright (C) 2012 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

define([
    'order!js/libs/hterm/pubsub.js',
    'order!js/libs/hterm/dialogs.js',

    'order!js/libs/hterm/hterm.js',
    'order!js/libs/hterm/scrollport.js',
    'order!js/libs/hterm/terminal.js',
    'order!js/libs/hterm/terminal_io.js',
    'order!js/libs/hterm/options.js',
    'order!js/libs/hterm/screen.js',
    'order!js/libs/hterm/text_attributes.js',
    'order!js/libs/hterm/vt.js',
    'order!js/libs/hterm/keyboard.js',
    'order!js/libs/hterm/default_keymap.js',
    'order!js/libs/hterm/preference_manager.js'], function() {
        return hterm;
});
