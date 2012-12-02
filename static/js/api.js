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

define(['jquery'], function($) {
  "use strict";

  var POLL_INTERVAL = 50;

  var key = (function initializeKey() {
    var key = window.location.hash.slice(1);
    if ('' == key) {
      key = sessionStorage.getItem("key");
    } else {
      sessionStorage.setItem("key", key);
    }
    window.location.hash = "";
    return key;
  })();


  function api(call, req, respFn) {
    if (!respFn) {
      respFn = cmdResult;
    }

    $.ajax({
      contentType: 'application/json',
      data: JSON.stringify({key: key, req: req}),
      dataType: 'json',
      error: function(xhr, stat, err) {
        console.log('api.api ajax error: ' + stat + ' ' + err, xhr);
          // TODO: should have a better place to put errors
      },
      processData: false,
      success: function(data, stat, xhr) {
        respFn(data);
      },
      type: 'POST',
      url: '/api/' + call
    });
  }

  var runningCommands = Object.create(null);
  var runningCount = 0;
  var pollRunningOrScheduled = false;

  function pollResult(data) {
    data.forEach(function(d) {
      var job = ('job' in d) ? d.job : "default";
      var r = runningCommands[job];
      if (!r) return;

      if (r.reportEach) {
          r.reportEach(d);
      } else {
        if ('stdout' in d) {
          if (!('stdout' in r)) r.stdout = "";
          r.stdout += d.stdout;
        }
        if ('stderr' in d) {
          if (!('stderr' in r)) r.stderr = "";
          r.stderr += d.stderr;
        }
        if ('jsonout' in d) {
          if (!('jsonout' in r)) r.jsonout = [];
          r.jsonout = r.jsonout.concat(d.jsonout);
        }
        if ('parseError' in d) {
          if (!('parseError in r')) r.parseError = "";
          r.parseError += d.parseError;
        }
      }

      if ('running' in d && !d.running) {
        delete runningCommands[job];
        runningCount -= 1;
        if (r.reportComplete) {
          r.exitcode = d.exitcode;
          r.reportComplete(r);
        }
      }
    });

    if (runningCount > 0 && !pollRunningOrScheduled) {
      pollRunningOrScheduled = true;
      setTimeout(function () {
        api('poll', null, function(data) {
          pollRunningOrScheduled = false;
          pollResult(data);
       });
      }, POLL_INTERVAL);
    }
  }


  function runResult(d) {
    pollResult([d]);
  }

  var runCount = 0;
  function run(prefix, cmd, record, r, job) {
    if (!job) {
      job = prefix + (++runCount);
    }
    runningCommands[job] = r;
    runningCount += 1;
    api('run', {job: job, record: record, cmd: cmd}, runResult);
  }

  // Run a user command, recorded into history, and stream the output
  // to the outputFn as it comes in.
  function runUser(cmd, outputFn, job) {
    run("userCmd", cmd, true, { reportEach: outputFn }, job);
  }

  // Run a status command, not recorded into history, and return all the output
  // to the resultsFn once the command completes.
  function runStatus(cmd, resultsFn) {
    run("statusCmd", cmd, false, { reportComplete: resultsFn });
  }

  return {
    api: api,
    runUser: runUser,
    runStatus: runStatus
  };
});
