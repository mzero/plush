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
    'order!js/libs/hterm/hterm_deps.js',
    'order!js/libs/hterm/hterm.js',
    ],
    function() {
        lib.init(function() { });
            // TODO(mzero): Should somehow delay this module from being ready
            // until lib.init() calls the callback.

        hterm.Keyboard.prototype.encode =
        hterm.VT.prototype.decode =
          function(str) { return str; };
          // Monkey patching objects so that they don't do the UTF-8
          // encode/decode. This is so the terminal doesn't encode characters
          // sent, or decode inputs to interpret().
          //
          // TODO(mzero): This will affect the data passed to
          // copyStringToClipboard() as well: It won't be decoded but it should
          // be, as it is sent (I think) encoded from the host.

        return hterm;
    }
);
