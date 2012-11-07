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

define(['jobs'], function(jobs) {
  "use strict";

  var api;

  var historyJobs = Object.create(null);
  var historyOutputToFetch = [];
  var OUTPUT_UPDATE_RATE = 50;
  var OUTPUT_PREFETCH = 0;

  function initHistory(a){
    api = a;
    api('history', null, listResult);
  };

  function listResult(items) {
    for (var i in items) {
      var item = items[i];
      var job = item.job;
      if (!(job in historyJobs)) {
        historyJobs[job] = true;
        historyOutputToFetch.push(job);          
      }
      if ('cmd' in item) {
        var j = jobs.newJob(api, item.cmd, job);
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
      api('history', { historyOutput: [ job ]}, outputResult);
  }
  
  function outputResult(items) {
    for (var i in items) {
      handleHistoryItem(items[i]);
    }
  }

  function streamFetch() {
    var job = historyOutputToFetch.pop();
    if (job) {
      api('history', { historyOutput: [ job ]}, streamResult);
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
  var searchLastDir = -1;
  var commandline;

  function startSearch(dir) {
    $('#scrollback').addClass('history-search');
    searchMode = true;
    searchFocus = null;
    searchLastDir = dir;
    commandline = $('#commandline');
    search(commandline.val());
    commandline.focus();
  }

  function cancelSearch() {
    $('#scrollback').removeClass('history-search');
    var j = $('#scrollback .job');
    j.show('fast')
    j.removeClass('history-match history-focus');
    searchMode = false;
    searchFocus = null;
    commandline.focus();
  }

  var ANIM_SPEED = 100;

  function search(s) {
    s = s || '';
    $('#scrollback .job')
      .each(function(idx) {
          var n = $(this);
          if (s.length === 0 || n.find('.command').text().indexOf(s) >= 0) {
            n.show(ANIM_SPEED);
            n.addClass('history-match');
          } else {
            n.hide(ANIM_SPEED);
            n.removeClass('history-match');
          }
        });
    if (!searchFocus || !(searchFocus.hasClass('history-match'))) {
      nextFocus(searchLastDir);
    }
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

  function nextFocus(dir) {
    searchLastDir = dir;
    var next = null;
    if (searchFocus) {
      switch (dir) {
        case -1: next = searchFocus.prevAll('.history-match').first();  break;
        case  1: next = searchFocus.nextAll('.history-match').first(); break;
      }
      searchFocus.removeClass('history-focus');
    } else {
      var js = $('#scrollback .history-match');
      switch (dir) {
        case -1: next = js.last();  break;
        case  1: next = js.first(); break;
      }
    }
    if (next && next.length > 0) {
      next.addClass('history-focus');
      searchFocus = next;
    } else {
      searchFocus = null;
    }
  }

  function keydown(e) {
    if (!searchMode) {
      if (e.ctrlKey && !(e.altKey || e.shiftKey || e.metaKey)) {
        switch (e.which) {
          case 82: startSearch(-1); return false; // CTRL+R
          case 83: startSearch(1);  return false; // CTRL+S
        }
      }
    } else {
      if (!(e.ctrlKey || e.shiftKey || e.metaKey)) { // ALT+ is optional
        switch (e.which) {
          case 38: nextFocus(-1);   return false;  // UP
          case 40: nextFocus(1);    return false;  // DOWN
        }
      }
      if (!(e.ctrlKey || e.altKey || e.shiftKey || e.metaKey)) {
        switch (e.which) {
          case 13: endSearch();     return false; // RETURN
          case 27: endSearch();     return false; // ESC
        }
      } else if (e.ctrlKey && !(e.altKey || e.shiftKey || e.metaKey)) {
        switch (e.which) {
          case 71: cancelSearch();  return false; // CTRL+G
          case 82: nextFocus(-1);   return false; // CTRL+R
          case 83: nextFocus(1);    return false; // CTRL+S
        }
      } 
    }
  }

  function commandChange(s) {
    if (searchMode) {
      search(s);
      return false;
    }
  }

  return {
    initHistory: initHistory,
    keydown: keydown,
    commandChange: commandChange
  };

});
