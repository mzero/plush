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

define(['api', 'util', 'input', 'jobs'], function(api, util, input, jobs) {
  "use strict";

  var historyPanel = $('#history');
  var historyItemProto = historyPanel.children('.proto')
    .detach()
    .removeClass('proto');

  function addEntry(cmd) {
    var item = historyItemProto.clone();
    item.find('.command').text(cmd);
    historyPanel.append(item);
  }

  var historyJobs = Object.create(null);
  var historyOutputToFetch = [];
  var HISTORY_STARTUP_DELAY = 100;
  var OUTPUT_UPDATE_RATE = 50;
  var OUTPUT_PREFETCH = 0;

  function listResult(items) {
    for (var i in items) {
      var item = items[i];
      var job = item.job;
      if (!(job in historyJobs)) {
        historyJobs[job] = true;
        historyOutputToFetch.push(job);
      }
      if ('cmd' in item) {
        addEntry(item.cmd);
        var j = jobs.newJob(item.cmd, job, true);
        historyJobs[job] = j;
        j.setDeferredOutput(fetchOutput);
      }
      else {
        handleHistoryItem(item);
      }
    }
    if (OUTPUT_PREFETCH > 0) {
      historyOutputToFetch = historyOutputToFetch.splice(-OUTPUT_PREFETCH);
      setTimeout(streamFetch, OUTPUT_UPDATE_RATE);
    } else {
      historyOutputToFetch = [];
    }
  }

  function fetchOutput(job) {
      api.api('history', { historyOutput: [ job ]}, outputResult);
  }

  function outputResult(items) {
    for (var i in items) {
      handleHistoryItem(items[i]);
    }
  }

  function streamFetch() {
    var job = historyOutputToFetch.pop();
    if (job) {
      api.api('history', { historyOutput: [ job ]}, streamResult);
    }
  }

  function streamResult(items) {
    outputResult(items);
    setTimeout(streamFetch, OUTPUT_UPDATE_RATE);
  }

  function handleHistoryItem(item) {
    var j = historyJobs[item.job];
    if (!j) return;

    if ('stdout' in item) {
      j.addOutput('stdout', item.stdout);
    }
    if ('stderr' in item) {
      j.addOutput('stderr', item.stderr);
    }
    if ('jsonout' in item) {
      var s = "";
      item.jsonout.forEach(function(j) {
        s += JSON.stringify(j, null, 4);
        s += "\n";
      });
      j.addOutput('stdout', s);
    }
    if ('parseError' in item) {
      j.addOutput('error', item.parseError);
    }
    if ('exitcode' in item) {
      j.setComplete(item.exitcode);
    }
  }

  var searchMode = false;
  var searchFocus = null;
  var searchPriorFocus = null;
  var searchLastDir = -1;
  var commandline;

  function startSearch(dir) {
    searchMode = true;
    searchFocus = null;
    searchLastDir = dir;
    historyPanel.show();
    commandline = $('#commandline');
    commandline.focus();
    search();
  }

  function cancelSearch() {
    historyPanel.hide();
    if (searchFocus) searchFocus.removeClass('focus');
    searchMode = false;
    searchFocus = null;
    searchPriorFocus = null;
    commandline.focus();
  }

  function search() {
    var s = commandline.val() || '';
    historyPanel.children() // NOTE: faster than .find('.item')
      .each(function(idx) {
          var n = $(this);
          if (s.length === 0 || n.find('.command').text().indexOf(s) >= 0) {
            n.addClass('match');
          } else {
            n.removeClass('match');
          }
        });
    if (!searchFocus || !(searchFocus.hasClass('match'))) {
      nextFocus(searchLastDir);
    }
    setTimeout(rescrollMatch, 10);
  }

  function rescrollMatch() {
    if (searchFocus) {
      util.scrollIntoView(historyPanel, searchFocus);
    }
  }

  function clearSearch() {
    commandline.val('');
    search();
  }

  function endSearch() {
    if (searchFocus) {
      var selected = searchFocus.find('.command').text();
      var n = selected.length;
      commandline.val(selected);
      commandline.focus();
      commandline.get(0).setSelectionRange(n, n);
    }
    cancelSearch();
  }

  function focusEdge(dir) {
    var js = historyPanel.find('.match');
    switch (dir) {
      case -1: return js.first();
      case  1: return js.last();
    }
  }
  function nextFocus(dir) {
    searchLastDir = dir;
    var next = null;
    if (searchFocus) {
      searchPriorFocus = searchFocus;
      switch (dir) {
        case -1: next = searchFocus.prevAll('.match').first();  break;
        case  1: next = searchFocus.nextAll('.match').first(); break;
      }
      searchFocus.removeClass('focus');
      if (next === null || next.length === 0) {
        next = focusEdge(dir);
      }
    } else {
      if (searchPriorFocus && searchPriorFocus.hasClass('match')) {
        next = searchPriorFocus;
      } else {
        next = focusEdge(-dir);
          // -dir because if there is no focus, and the user goes UP,
          // they want to start with the last item.
      }
    }
    if (next && next.length > 0) {
      next.addClass('focus');
      util.scrollIntoView(historyPanel, next);
      searchFocus = next;
    } else {
      searchFocus = null;
    }
  }

  var triggerKeydown = input.keyHandler({
    'CTRL+R, ALT+SLASH':      function() { startSearch(-1); },
    'CTRL+S, ALT+BACK_SLASH': function() { startSearch(1); },
  });
  var searchModeKeydown = input.keyHandler({
    'CTRL+R, ALT+SLASH,      UP,   ALT+UP':   function() { nextFocus(-1); },
    'CTRL+S, ALT+BACK_SLASH, DOWN, ALT+DOWN': function() { nextFocus(1); },
    'ESCAPE, CTRL+G':                         function() { cancelSearch(); },
    'RETURN':                                 function() { endSearch(); },
    'CTRL+U':                                 function() { clearSearch(); }
  });

  function keydown(e) {
    if (!searchMode) {
      return triggerKeydown(e);
    } else {
      return searchModeKeydown(e);
    }
  }

  function commandChange() {
    if (searchMode) {
      search();
      return false;
    }
  }

  setTimeout(function() { api.api('history', null, listResult); },
    HISTORY_STARTUP_DELAY);

  return {
    addEntry: addEntry,
    keydown: keydown,
    commandChange: commandChange
  };

});
