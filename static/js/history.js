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
  var HISTORY_STARTUP_DELAY = 1;
  var HISTORY_UPDATE_RATE = 200;

  function listResult(items) {
    // first, regroup these by job
    var byJob = Object.create(null);
    var jobList = [];
    for (var i in items) {
      var item = items[i];
      var job = item.job;
      if (!(job in byJob)) {
        byJob[job] = [];
        jobList.push(job);
      }
      byJob[job].push(item);
    }

    // process them some at a time
    function handleSome() {
      var addThisRound = 8;
        // Sometimes the height of a job is non-integral, but scrollTop can
        // only be set in integral pixels. Since we've only seen fractions of
        // 0.5 in the height, this value should be a multiple of 2. Setting to
        // a power of two just to be cautious.

      var insaneScrollPosition = 4000000; // must be bigger than we ever expect
      var scrollback = $('#scrollback');
      var scrollPos0 = scrollback.scrollTop();
      scrollback.scrollTop(insaneScrollPosition);
      var scrollMax0 = scrollback.scrollTop();
        // Remember where the scroll position was, and how big the scrolling
        // area was. There is no call toactually measure the later, so scroll to
        // insaneScrollPosition, and read back the maximum scroll position value
        // which is used as a proxy for height. (Which works so long as the
        // window doesn't resize during this operaiton.)

      while (jobList.length > 0           // as long as there are jobs....
          && (scrollback.scrollTop() == 0 // ... and more space to fill
             || addThisRound > 0)) {      // ... or we've added enough
        var job = jobList.pop();          // needs to be newest -> oldest
        var jobItems = byJob[job];
        delete byJob[job];
        addThisRound -= 1;

        for (var k in jobItems) {
          var item = jobItems[k];
          if ('cmd' in item) {
            addEntry(item.cmd);
            var j = jobs.addHistoricalJob(item.cmd, job);
            historyJobs[job] = j;
            j.setDeferredOutput(fetchOutput);
          }
          else {
            handleHistoryItem(item);
          }
        }
        scrollback.scrollTop(insaneScrollPosition);
          // Scrolling to the end. When we read back the scroll position at the
          // top of loop, it will be non-zero if filled up the area enough to
          // scroll at all.
      }

      scrollback.scrollTop(insaneScrollPosition);
      var scrollMax1 = scrollback.scrollTop();
      var scrollPos1 = scrollPos0 + (scrollMax1 - scrollMax0);
      scrollback.scrollTop(scrollPos1);
        // Using the same technique to find the maximum scroll position after
        // adding all the entries, now reset the scroll position to where it
        // started, but adjusting for the measured increase in height.

      if (jobList.length > 0) {
        setTimeout(handleSome, HISTORY_UPDATE_RATE);
      }
    }

    // kick it off
    handleSome();
  }

  function fetchOutput(job) {
      api.api('history', { historyOutput: [ job ]}, outputResult);
  }

  function outputResult(items) {
    for (var i in items) {
      handleHistoryItem(items[i]);
    }
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
