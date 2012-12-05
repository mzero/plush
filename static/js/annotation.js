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

define(['jquery', 'api', 'input'], function($, api, input) {
  "use strict";

  var COMPLETION_DELAY = 50;


  function repeat(s,n) {
    while (s.length < n) { s = s + s; }
    return s.substr(0,n);
  }

  function repeatSpan(s,n) {
    while (s.length < n) { s = s + s; }
    var e = $('<pre></pre>');
    e.text(s.substr(0,n));
    return e;
  }

  function commonPrefix(w1, w2) {
    var len = Math.min(w1.length, w2.length);
    for (var i = 0; i < len; i++) {
      if (w1[i] != w2[i]) break;
    }
    return w1.substr(0, i);
  }


  var annoElem = $('#annotations');
  var annotations = [];
  var currentShowingElem = null;
  var completionSpan;
  var completionArray;
  var completionCommonPrefix;
  var completionElem;

  var commandline = $('#commandline');

  function clearAnnotations() {
    annoElem.empty();
    if (currentShowingElem) {
      currentShowingElem.hide();
      currentShowingElem = null;
    }
    annotations = [];
    completionSpan = null;
    completionArray = null;
    completionCommonPrefix = null;
    completionElem = null;
  }

  function updateAnnotations(comp) {
    clearAnnotations();

    var i = 1;
    var spans = comp.spans || [];
    spans.forEach(function(span) {
      var m = "";
      var c = [];
      span.annotations.forEach(function(anno) {
        if (anno.expansion) {
          m += 'expands to ' + anno.expansion;
        }
        else if (anno.commandType) {
          m += anno.commandType + " command"
          if (anno.path) {
            m += " @ " + anno.path
          }
        }
        else if (anno.command) {
          m += anno.command + "\n" + anno.synopsis + "\n";
        }
        else if (anno.option) {
          m += anno.option ;
        }
        else if (anno.unused) {
          m += "unused";
        }
        else if (anno.completions) {
          c = c.concat(anno.completions);
        }
        m += "\n";
      });

      m = m.trim()
      if (m || c.length) {
        var a = { start: span.start, end: span.end };

        if (i < span.start) {
          annoElem.append(repeatSpan(' ', span.start - i));
          i = span.start;
        }
        var blockElem = repeatSpan(' ', span.end - span.start);
        blockElem.addClass('annotation');
        annoElem.append(blockElem);
        i = span.end;

        if (m) {
          var msgElem = $('<span></span>', { "class": "message" });
          msgElem.text(m);
          msgElem.hide();
          blockElem.append(msgElem);
          a.msgElem = msgElem;
        }

        if (c.length) {
          completionSpan = a;
          completionArray = c;
          completionCommonPrefix = c[0];

          completionElem = $('<ul></ul>', { "class": "completions" });
          c.forEach(function(t) {
            var textElem = $('<li></li>');
            textElem.text(t);
            completionElem.append(textElem);
            completionCommonPrefix = commonPrefix(completionCommonPrefix, t);
          });
          completionElem.hide();
          blockElem.append(completionElem);
        }

        annotations.push(a);
      }
    });
  }

  function commandChange() {
    if (completing) return;

    var loc = commandline[0].selectionStart;
    if (typeof loc !== 'number') return;

    var nextA;
    for (var i = 0; i < annotations.length; i += 1) {
      var a = annotations[i];
      if (a.start <= loc && loc < a.end) {
        nextA = a;
        break;
      }
    }

    if (nextA) {
      if (nextA.msgElem != currentShowingElem) {
        if (currentShowingElem) currentShowingElem.hide('fast');
        currentShowingElem = nextA.msgElem;
        if (currentShowingElem) currentShowingElem.show('fast');
      }
    }

  }


  var completing = false;
  var completingPrefix;
  var completingOriginal
  var completingPostfix;
  var completingFocus;

  function startCompletions() {
    if (!completionSpan) return;

    completing = true;
    clearTimeout(checkTimer);

    var input = commandline.val();
    completingPrefix = input.substring(0, completionSpan.start - 1);
    completingOriginal = input.substring(completionSpan.start - 1,
                                          completionSpan.end);
    completingPostfix = input.substring(completionSpan.end);
    completingFocus = null;

    insertCompletion(completionCommonPrefix);
    if (currentShowingElem) currentShowingElem.hide();
    currentShowingElem = $('.completions');
    if (currentShowingElem) currentShowingElem.show();
  }

  function insertCompletion(text) {
    commandline.val(completingPrefix + text + completingPostfix);
      // TODO: should shell escape test here
    var s = completingPrefix.length + text.length;
    commandline[0].setSelectionRange(s,s);
  }

  function nextCompletion(dir) {
    var nextFocus;
    if (completingFocus) {
      var oldFocus = completingFocus;
      switch (dir) {
        case -1: nextFocus = oldFocus.prevAll('li').first(); break;
        case  1: nextFocus = oldFocus.nextAll('li').first(); break;
      }
      oldFocus.removeClass('focus');
    }
    if (!nextFocus) {
      switch(dir) {
        case -1: nextFocus = completionElem.find('li').last(); break;
        case  1: nextFocus = completionElem.find('li').first(); break;
      }
    }
    if (nextFocus && nextFocus.length > 0) {
      nextFocus.addClass('focus');
      completingFocus = nextFocus;
      insertCompletion(nextFocus.text());
    } else {
      completingFocus = null;
    }
  }

  function finishCompletion() {
    $('.completions').hide();
    completing = false;
    completingFocus = null;
    currentShowingElem = null;
    commandline.focus();
    requestRunComplete();
  }

  function finishCompletionAll() {
    insertCompletion(completionArray.join(' ') + ' ');
    finishCompletion();
  }

  function cancelCompletion() {
    insertCompletion(completingOriginal);
    finishCompletion();
  }


  var triggerCompletionKeydown = input.keyHandler({
    'TAB':            function() { startCompletions(); },
  });

  var completionModeKeydown = input.keyHandler({
    bindings: {
      'DOWN, TAB':          function() { nextCompletion(1); },
      'UP, SHIFT+TAB':      function() { nextCompletion(-1); },
      'RETURN':             function() { finishCompletion(); },
      'ASTERISK, EQUALS, EQUALS2':
                            function() { finishCompletionAll(); },
      'ESCAPE':             function() { cancelCompletion(); }
    },
    default: function() { finishCompletion(); return input.PASS; }
  });

  function keydown(e) {
    if (!completing) {
      return triggerCompletionKeydown(e);
    } else {
      return completionModeKeydown(e);
    }
  }



  var checkTimer = null;
  var lastCheckedCommandLine;

  function requestRunComplete() {
    clearTimeout(checkTimer);
    if (completing) return;
    checkTimer = setTimeout(runComplete, COMPLETION_DELAY);
  }

  function runComplete() {
    var input = commandline.val();
    if (input === lastCheckedCommandLine) return;
    lastCheckedCommandLine = input;

    input = input.replace(/'/g,"'\\''");
    var loc = commandline[0].selectionStart;
    loc = (typeof loc === 'number') ? ' -c ' + loc : '';
    var cmd = "complete" + loc + " '" + input + "'";
    api.runStatus(cmd, function (d) {
      if ('jsonout' in d) {
        d.jsonout.forEach(updateAnnotations);
      }
      commandChange();
    });
  }


  return {
    clearAnnotations: clearAnnotations,
    commandChange: commandChange,
    keydown: keydown,
    requestRunComplete: requestRunComplete
  }
});
