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

define(['keys', 'history', 'cwd', 'jquery', 'hterm'], function(keys, historyApi, cwd, $){
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
  var scrollback = $('#scrollback');
  var jobProto = scrollback.children('.job.proto').detach();
  jobProto.removeClass('proto');
  var commandline = $('#commandline');

  function repeat(s,n) {
    while (s.length < n) { s = s + s; }
    return s.substr(0,n);
  }

  function countOccurances(s, c) {
    var l=c.length;
    var n=-1;
    var p=0;
    do { p=s.indexOf(c,p)+l; n++; } while (p>0);
    return n;
  }

  function repeatSpan(s,n) {
    while (s.length < n) { s = s + s; }
    var e = $('<pre></pre>');
    e.text(s.substr(0,n));
    return e;
  }

  var jobCount = 0;
  var jobstate = {}
  
  function addJobDiv(cmd) {
    var job = "job" + (++jobCount);
    var node = jobProto.clone();
    node.attr('id', job);
    node.find('.command').text(cmd);
    node.appendTo(scrollback);

    var output = node.find('.output-container');
    output.scroll(function() { 
      // TODO(jasvir): Do the same for scrollBottom
      if ($(this).scrollTop() === 0) {
        $(this).css('-webkit-mask-image', 'none');
      } else {
        $(this).css('-webkit-mask-image', 
		    '-webkit-gradient(linear, left top, 0 10, from(rgba(0,0,0,0)), to(rgba(0,0,0,1)))');
      }
    });


    var sender = function(s) {
      api('input', {job: job, input: s}, function() {});
    };
    var signaler = function(s) {
      s = 'kill'; // TODO: remove this when int and quit work
      api('input', {job: job, signal: s}, function() {});
    };

    var input = node.find('.input-container');
    var inputField = input.find('input');
    inputField.keyup(function(e) {
      if (e.keyCode == 13) {
        var s = $(this).val() + '\n';
        sender(s);
        $(this).val('');
      }  
    });
    input.find('.send-eof').bind('click', function() { sender('\x04'); });
    input.find('.send-sigint').bind('click', function() { signaler('int'); });
    input.find('.send-sigquit').bind('click', function() { signaler('quit'); });
    input.find('.send-sigkill').bind('click', function() { signaler('kill'); });
    inputField.focus();

    var j = {
      node: node,

      output: output,
      outputArea: output.find('.output'),
      lastOutputSpan: null,
      lastOutputType: null,
      linesOutput: 0,
      newlinesOutput: 0,
      terminal: null,
      maxState: null,

      input: input,
    }

    node.find('.view-hide').bind('click', function() { sizeOutput(j, 'hide'); });
    node.find('.view-tiny').bind('click', function() { sizeOutput(j, 'tiny'); });
    node.find('.view-page').bind('click', function() { sizeOutput(j, 'page'); });
    node.find('.view-full').bind('click', function() { sizeOutput(j, 'full'); });

    jobstate[job] = j;
    return job;
  }

  var LINES_IN_TINY = 3;
  var LINES_IN_PAGE = 24;

  function sizeOutput(j, m) {
    j.output.removeClass('output-hide output-tiny output-page output-full');
    j.output.addClass('output-' + m);
  }
  function adjustOutput(j) {
    var n = j.linesOutput;
    if (n == 0 && j.input) n = 1;

    var m, s;
    if (n == 0)                   m = s = 'hide';
    else if (n <= LINES_IN_TINY)  m = s = 'tiny';
    else if (n <= LINES_IN_PAGE)  m = s = 'page';
    else                          { m = 'full'; s = 'page'; }

    if (j.terminal) {
      m = 'full';
      s = 'full';
    }

    if (j.maxState !== m) {
      j.node.removeClass('max-hide max-tiny max-page max-full');
      j.node.addClass('max-' + m);
      sizeOutput(j, s);
    }
  }
  function removeJobInput(job) {
    if (job in jobstate) {
      var j = jobstate[job];
      var input = j.input;
      if (input) {
        input.remove();
        j.input = null;
        adjustOutput(j);
      }
    }
    commandline.focus();
  }
  
  function setJobClass(job, cls) {
    if (job in jobstate) {
      var node = jobstate[job].node;
      node.removeClass('running complete').addClass(cls);
    }
  }
    
  function addVTOutput(job, txt) {
    removeJobInput(job);
    var where = $('#' + job + ' .output');
    if (where.length != 1) { return; }
    var node = $('<div></div>', { 'class': 'terminal' })
    node.appendTo(where);
    var term = new hterm.Terminal();
    term.setAutoCarriageReturn(true);
    term.decorate(node.get(0));
    term.setFontSize(13);
    term.setWidth(80);
    term.setHeight(24);   
    term.interpret(txt);
    var sendInput = function(s) {
      api('input', {job: job, input: s}, function(){});
    };
    term.io.onVTKeystroke = sendInput;
    term.io.sendString = sendInput;
    term.installKeyboard();
    jobstate[job].terminal = term;
    jobstate[job].terminalNode = node;
    adjustOutput(jobstate[job]);
    node.get(0).scrollIntoView(true);
  }
  
  function removeVTOutput(job) {
    if (job in jobstate) {
      var j = jobstate[job];
      if (j.terminal) {
        j.terminal.uninstallKeyboard();
        j.terminal = null;
        j.terminalNode.remove();
        j.terminalNode = null;
      }
    }
  }

  function addOutput(job, cls, txt) {
    if (job in jobstate) {
      var j = jobstate[job];
      if (j.terminal) {
        return j.terminal.interpret(txt);
      } else if (txt.match('\u001b[\[]')) {
        return addVTOutput(job, txt);
      }

      if (j.lastOutputType == cls && j.lastOutputSpan) {
        j.lastOutputSpan.append(document.createTextNode(txt));
      }
      else {
        j.lastOutputType = cls;
        j.lastOutputSpan = $('<span></span>', { 'class': cls }).text(txt);
        j.lastOutputSpan.appendTo(j.outputArea);
      }
      j.newlinesOutput += countOccurances(txt, '\n');
      j.linesOutput = j.newlinesOutput + (txt[txt.length-1] === '\n' ? 0 : 1);
      adjustOutput(j);
      j.lastOutputSpan.get(0).scrollIntoView(false);
    } else {
      var node = $('<span></span>', { 'class': cls }).text(txt);
      node.appendTo(scrollback);
      node[0].scrollIntoView(true);
    }
  }
  
  function updateContext(ctx) {
    if (ctx.cwd) {
      var partialCmd = commandline.val();
      $('#context-cwd').empty().append(
        cwd.parseToDom(ctx.cwd,
          function(event) { 
            runCommand(commandline, "cd " + event.data.dir); 
            commandline.val(partialCmd);
          })
      );
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
  
  function updateAnnotations(comp) {
    var annoElem = $('#annotations');
    annoElem.empty();
    
    var i = 1;
    var spans = comp.spans || [];
    spans.forEach(function(span) {
      var m = "";
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
        m += "\n";
      });
      
      m = m.trim()
      if (m) {
        if (i < span.start) {
          annoElem.append(repeatSpan(' ', span.start - i));
          i = span.start;
        }
        var blockElem = repeatSpan(' ', span.end - span.start);
        blockElem.addClass('annotation');
        annoElem.append(blockElem);
        i = span.end;
        
        var msgElem = $('<span></span>', { "class": "message" });
        msgElem.text(m);
        blockElem.append(msgElem);
      }
    });
  }
  
  function api(call, req, respFn) {
    $.ajax({
      contentType: 'application/json',
      data: JSON.stringify({key: key, req: req}),
      dataType: 'json',
      error: function(xhr, stat, err) {
        addOutput('err', 'error', stat + ': ' + err);
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

      if ('stdout' in d) {
        addOutput(job, 'stdout', d.stdout);
      }
      if ('stderr' in d) {
        addOutput(job, 'stderr', d.stderr);
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
          addOutput(job, 'stdout', s);
        }
      }
      if ('running' in d) {
        if (d.running) {
          setJobClass(job, "running");
          jobsRunning = true;
        }
        else {
          if (job !== 'ctx') {
            setJobClass(job, d.exitcode == 0 ? "complete" : "failed");
            removeJobInput(job);
            removeVTOutput(job);
            jobsDone = true;
          }
        }
      }
    });
    if (jobsRunning) {
      setTimeout(poll, 25);
    }
    if (jobsDone) {
      runContext();
    }
  }

  function prevCommand(that, cmd, e) {
    that.val(historyApi.previous(cmd));
  }

  function nextCommand(that, cmd, e) {
    that.val(historyApi.next(cmd));
  }

  function runCommand(that, cmd, e) {
    var job = addJobDiv(cmd);
    that.val('');
    historyApi.add(cmd);
    $('#annotations').text('')
    api('run', {job: job, cmd: cmd}, cmdResult);
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
  commandline.keyup(function(e) {
    if (checkTimer) { clearTimeout(checkTimer); checkTimer = null; }
    keys(e, {
      runCommand: runCommand,
      nextCommand: nextCommand,
      prevCommand: prevCommand,
      default: function() { checkTimer = setTimeout(runComplete, 1000); }
    })($(this), $(this).val());
  });

  function runContext() {
    api('run', {job: 'ctx', cmd: 'context'}, cmdResult);
  }
  
  function cmdResult(data) {
    var job = ('job' in data) ? data.job : "unknown";

    if ('parseError' in data) {
      addOutput(job, 'error', data.parseError);
    }
    if (data.running) {
      setJobClass(job, "running");
      poll();
    }
  }
  
  function runComplete() {
    var cmd = commandline.val();
    cmd = cmd.replace(/'/g,"'\\''");
    cmd = "complete '" + cmd + "'";
    api('run', {job: 'comp', cmd: cmd}, cmdResult);
  }
  
  runContext();

});
