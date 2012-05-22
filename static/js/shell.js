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

  var totalHeight = screen.outerHeight();
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

  var jobCount = 0;
  var foregroundJob = null;
  
  function addJobDiv(cmd) {
    var job = "job" + (++jobCount);
    var node = jobProto.clone();
    node.attr('id', job);
    node.find('.command').text(cmd);
    node.appendTo(scrollback);

    var output = node.find('.output-container');
    node.find('.view-hide').bind('click', function() {
      output.attr('class', 'output-container output-hide');
    });
    node.find('.view-line').bind('click', function() {
      output.attr('class', 'output-container output-line');
    });
    node.find('.view-page').bind('click', function() {
      output.attr('class', 'output-container output-page');
    });
    node.find('.view-full').bind('click', function() {
      output.attr('class', 'output-container output-full');
    });

    var sender = function(s) {
      api('input', {job: job, input: s}, function() {});
    };

    var input = node.find('.input-container');
    input.find('input').keyup(function(e) {
      if (e.keyCode == 13) {
        var s = $(this).val() + '\n';
        sender(s);
        $(this).val('');
      }  
    });
    input.find('.send-eof').bind('click', function() { sender('\x04'); });
    input.find('.send-sigint').bind('click', function() { sender('\x03'); });
    input.find('.send-sigquit').bind('click', function() { sender('\x1C'); });

    return job;
  }

  function removeJobInput(job) {
    $('#'+job + ' .input-container').remove();
  }
  
  function setJobClass(job, cls) {
    var where = $('#' + job);
    if (where.length != 1) { return; }
    where.removeClass('running complete').addClass(cls);
  }
  
  var terminals = {};
  
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
    terminals[job] = term;
    totalHeight += node.outerHeight();
    scrollback.animate({scrollTop: totalHeight });
  }
  
  function removeVTOutput(job) {
    if (job in terminals) {
      terminals[job].uninstallKeyboard();
      $('#'+job + ' .terminal').remove();
      delete terminals[job];
    }
  }

  function addOutput(job, cls, txt) {
    if (job in terminals) {
      return terminals[job].interpret(txt);
    } else if (txt.match('\u001b[\[]')) {
      return addVTOutput(job, txt);
    }
    var where = $('#' + job + ' .output');
    if (where.length != 1) { where = scrollback; }
    var node = $('<span></span>', { 'class': cls }).text(txt);
    node.appendTo(where);
    totalHeight += node.outerHeight();
    scrollback.animate({scrollTop: totalHeight });
    // TODO(jasvir): Almost certainly the wrong place to do this
    // $("#commandline")[0].scrollIntoView(true);
  }
  
  function updateContext(ctx) {
    if (ctx.cwd) {
      var partialCmd = $('#commandline').val();
      $('#context-cwd').empty().append(
        cwd.parseToDom(ctx.cwd,
          function(event) { 
            runCommand($('#commandline'), "cd " + event.data.dir); 
            $('#commandline').val(partialCmd);
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

    foregroundJob = null;
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
          foregroundJob = job;
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
    // TODO: this isn't really the right place for this code
    if (foregroundJob) {
        var eof = cmd == "EOF";
        var input = eof ? "" : cmd + "\n";
        api('input', {job: foregroundJob, input: input, eof: eof}, function(){});
        that.val('');
        return;
    };
    
    var job = addJobDiv(cmd);
    that.val('');
    historyApi.add(cmd);
    $('#annotations').text('')
    api('run', {job: job, cmd: cmd}, cmdResult);
    $("#commandline").focus();
    foregroundJob = job;
  }

  function poll() {
    api('poll', null, pollResult);
  }

  var checkTimer = null;
  $('#commandline').keyup(function(e) {
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
    var cmd = $('#commandline').val();
    cmd.replace(/'/g,"'\\''");
    cmd = "complete '" + cmd + "'";
    api('run', {job: 'comp', cmd: cmd}, cmdResult);
  }
  
  runContext();

});
