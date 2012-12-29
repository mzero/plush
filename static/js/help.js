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

define(['jquery', 'input'], function($, input) {
  'use strict';

  var fetchingHelp = false;
  var helpDiv;
  var helpShowing = false;

  function startHelp() {
    if (fetchingHelp) return;
    if (helpDiv) {
      helpDiv.show();
      helpShowing = true;
      return;
    }
    fetchingHelp = true;
    $('#help-holder').load('help.html #help', function() {
      fetchingHelp = false;
      helpDiv = $('#help');
      helpDiv.find('#help-opener').on('click', endHelp);
      helpDiv.find('#help-closer').on('click',
        function() { endHelp(); return false; });
      startHelp();
    })
  }

  function endHelp() {
    helpDiv.hide();
    helpShowing = false;
  }

  var helpModeKeydown = input.keyHandler({
    bindings: {
      'ESCAPE, ALT+SHIFT+SLASH': endHelp,
      'ALT, CTRL, SHIFT': function () { return input.PASS; }
    },
    default: function() { endHelp(); return input.PASS; }
  });

  var normalKeydown = input.keyHandler({
    'ALT+SHIFT+SLASH': startHelp
  });

  function helpKeydown(e) {
    if (helpShowing) return helpModeKeydown(e);
    else             return normalKeydown(e);
  }

  return {
    start: startHelp,
    keydown: helpKeydown
  }
});
