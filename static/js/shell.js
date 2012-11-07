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

define(['history', 'cwd', 'jobs', 'jquery'], function(history, cwd, jobs, $){
  "use strict";

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

  var screen = $('#screen');
  var commandline = $('#commandline');

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

  function updateContext(ctx) {
    if (ctx.cwd) {
      cwd.parseToDom(ctx.cwd, runCommand);
    }
    var envList = $('#context-env');
    var shList = $('#context-shell');
    envList.empty();
    shList.empty();
    var vars = ctx.vars || [];
    vars.forEach(function(v) {
      var dt = $('<dt></dt>', { class: v.mode });
      var dd = $('<dd></dd>', { class: v.mode });
      dt.text(v.name);
      dd.text(v.value);
      if (v.scope === 'env') { envList.append(dt); envList.append(dd); }
      if (v.scope === 'shell') { shList.append(dt); shList.append(dd); }
    });
  }
  
  var completionSpan = null;

  function updateAnnotations(comp) {
    var annoElem = $('#annotations');
    annoElem.empty();
    completionSpan = null;

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
          blockElem.append(msgElem);
        }

        if (c.length) {
          completionSpan = span;
          var compElem = $('<ul></ul>', { "class": "completions" });
          c.forEach(function(t) {
            var textElem = $('<li></li>');
            textElem.attr('tabindex', '100');
            textElem.text(t);
            compElem.append(textElem);
          });
          blockElem.append(compElem);
        }
      }
    });
  }
  
  function api(call, req, respFn) {
    $.ajax({
      contentType: 'application/json',
      data: JSON.stringify({key: key, req: req}),
      dataType: 'json',
      error: function(xhr, stat, err) {
        jobs.unknownJob.addOutput('err', 'error', stat + ': ' + err);
      },
      processData: false,
      success: function(data, stat, xhr) {
        respFn(data);
      },
      type: 'POST',
      url: '/api/' + call
    });
  }
  
  function pollResult(data) {
    var jobsRunning = false;
    var jobsDone = false;

    data.forEach(function(d) {
      var job = ('job' in d) ? d.job : "unknown";
      var j = jobs.fromJob(job);

      if ('stdout' in d) {
        j.addOutput('stdout', d.stdout);
      }
      if ('stderr' in d) {
        j.addOutput('stderr', d.stderr);
      }
      if ('jsonout' in d) {
        if (job === 'ctx') {
          d.jsonout.forEach(updateContext);
        } else if (job === 'comp') {
          d.jsonout.forEach(updateAnnotations);
        } else {
          var s = "";
          d.jsonout.forEach(function(j) {
            s += JSON.stringify(j, null, 4);
            s += "\n";
          });
          j.addOutput('stdout', s);
        }
      }
      if ('running' in d) {
        if (d.running) {
          j.setRunning();
          jobsRunning = true;
        }
        else {
          if (job !== 'ctx' && job !== 'comp') {
            j.setComplete(d.exitcode);
            commandline.focus();
            jobsDone = true;
          }
        }
      }
      if ('parseError' in d) {
        j.addOutput('error', data.parseError);
      }
    });
    if (jobsRunning) {
      setTimeout(poll, 25);
    }
    if (jobsDone) {
      runContext();
    }
  }

  function startCompletions(e) {
    var input = commandline.val();
    var compArr = [];
    $('.completions li').each(function() {
      compArr.push($(this).text());
    });
    var common = commonPrefix(compArr);
    var comp = input.substring(completionSpan.start - 1,
                               completionSpan.end);
    if (common == comp) {
      $('.completions').show();
      $('.completions li:first').focus();
    } else {
      insertCompletion(common);
      requestRunComplete();
    }
    return false;
  }

  function commonPrefix(arr) {
    if (arr.length == 0) {
      return "";
    } else {
      var prefix = arr[0];
      for (var i = 1, len = arr.length; i < len; i++) {
        prefix = commonTwo(prefix, arr[i]);
      }
      return prefix;
    }
  }

  function commonTwo(w1, w2) {
    var len = Math.min(w1.length, w2.length);
    for (var i = 0; i < len; i++) {
      if (w1[i] != w2[i]) break;
    }
    return w1.substr(0, i);
  }

  function insertCompletion(completion) {
    var input = commandline.val();
    commandline.val(
      input.substr(0, completionSpan.start - 1)
      + completion
      + input.substr(completionSpan.end - 1));
  }

  $('#input-area').on('keydown', '.completions', function(e) {
    switch (e.keyCode) {
      case 13:
        insertCompletion($(e.target).text());
        commandline.focus();
        $('.completions').hide();
        return false;
    }
  })

  function runCommand(cmd) {
    var j = jobs.newJob(api, cmd);
    api('run', {job: j.job, cmd: cmd}, cmdResult);
  }

  function runCommandline(e) {
    var cmd = commandline.val();
    commandline.val('');
    if (cmd.trim() !== '') {
      $('#annotations').text('')
      runCommand(cmd);
    }
    return false;
  }

  function reenterTopic() {
    var c = commandline.val();
    var t = jobs.topicCommand();
    c = c.length ? ' # ' + c : '';
    commandline.val(t + c);
    commandline.focus();
  }

  var poll = (function () {
    var runningOrScheduled = false;
    return function poll() {
      if (runningOrScheduled) {
        return;
      }
      setTimeout(function () {
        api('poll', null, function(data) {
          pollResult(data);
          runningOrScheduled = false;
	      });
      }, 0);
      runningOrScheduled = true;
    };
  })();

  var checkTimer = null;
  commandline.keydown(function(e) {
    var r = history.keydown(e);
    if (r !== undefined) return r;

    if (!(e.ctrlKey || e.altKey || e.shiftKey || e.metaKey)) {
      switch (e.keyCode) {
        case 9: return startCompletions(e)  // TAB
        case 13: return runCommandline(e);  // RETURN
        case 32: // SPACE
        case 37: // LEFT
        case 39: // RIGHT
          e.stopPropagation(); // don't let window handler grab these
          break;
      };
    }
  });

  commandline.keyup(function(e) {
    var r = history.commandChange($(this).val());
    if (r !== undefined) return r;

    requestRunComplete();
  });

  function requestRunComplete() {
    clearTimeout(checkTimer);
    checkTimer = setTimeout(runComplete, 200);
  }

  $(window).on('keydown', function(e) {
    var r = history.keydown(e);
    if (r !== undefined) return r;

    // commands valid no matter what the modifiers
    if (!(e.ctrlKey || e.altKey || e.shiftKey || e.metaKey)) {
      switch (e.keyCode) {
      }
    }
    if (!(e.ctrlKey || e.shiftKey || e.metaKey)) { // ALT+ is optional
      switch (e.keyCode) {
        case 13: reenterTopic();       return false;  // RETURN
        case 37: commandline.focus();  return false;  // LEFT
        case 38: jobs.nextTopic(1);    return false;  // UP
        case 39: jobs.nextTopic(0);    return false;  // RIGHT
        case 40:                                      // DOWN
          if (jobs.atLastTopic())      commandline.focus();
          else                         jobs.nextTopic(-1);
          return false;
      }
    }
    return jobs.keydown(e);
  });


  function runContext() {
    api('run', {job: 'ctx', record: false, cmd: 'context'}, cmdResult);
  }

  function cmdResult(data) {
    var job = ('job' in data) ? data.job : "unknown";
    var j = jobs.fromJob(job);

    if ('parseError' in data) {
      j.addOutput('error', data.parseError);
    }
    if (data.running) {
      j.setRunning();
      poll();
    }
  }
  
  function runComplete() {
    var input = commandline.val();
    input = input.replace(/'/g,"'\\''");
    var loc = commandline[0].selectionStart;
    loc = (typeof loc === 'number') ? ' -c ' + loc : '';
    var cmd = "complete" + loc + " '" + input + "'";
    api('run', {job: 'comp', record: false, cmd: cmd}, cmdResult);
  }

  runContext();
  history.initHistory(api);
});
