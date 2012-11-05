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


  return {
    initHistory: initHistory
  };

});
