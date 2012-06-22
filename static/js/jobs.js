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

define(['jquery', 'hterm'], function($){
  'use strict';


  var scrollback = $('#scrollback');
  var jobProto = scrollback.children('.job.proto').detach();
  jobProto.removeClass('proto');

  function countOccurances(s, c) {
    var l=c.length;
    var n=-1;
    var p=0;
    do { p=s.indexOf(c,p)+l; n++; } while (p>0);
    return n;
  }

  function jobFromElement(elem) {
    return $(elem).parents('.job').data('job') || unknownJob;
  }

  function jobFromJob(job) {
    return $('#' + job).data('job') || unknownJob;
  }

  scrollback.on('scroll', '.output-container', function() {
    // TODO(jasvir): Do the same for scrollBottom
    if ($(this).scrollTop() === 0) {
      $(this).css('-webkit-mask-image', 'none');
    } else {
      $(this).css('-webkit-mask-image',
        '-webkit-gradient(linear, left top, 0 10,'
        + ' from(rgba(0,0,0,0)), to(rgba(0,0,0,1)))');
    }
  });

  scrollback.on('keydown', '.input-container input', function(e) {
    if (e.keyCode == 13) {
      var s = $(this).val() + '\n';
      jobFromElement(this).sender(s);
      $(this).val('');
      return false;
    }
  });

  scrollback.on('click', '.send-eof',
    function(e) { jobFromElement(this).sender('\x04'); });
  scrollback.on('click', '.send-sigint',
    function(e) { jobFromElement(this).signaler('int'); });
  scrollback.on('click', '.send-sigquit',
    function(e) { jobFromElement(this).signaler('quit'); });
  scrollback.on('click', '.send-sigkill',
    function(e) { jobFromElement(this).signaler('kill'); });

  scrollback.on('click', '.view-hide',
    function(e) { jobFromElement(this).sizeOutput('hide'); });
  scrollback.on('click', '.view-tiny',
    function(e) { jobFromElement(this).sizeOutput('tiny'); });
  scrollback.on('click', '.view-page',
    function(e) { jobFromElement(this).sizeOutput('page'); });
  scrollback.on('click', '.view-full',
    function(e) { jobFromElement(this).sizeOutput('full'); });


  var jobCount = 0;

  function newJob(api, cmd) {
    var job = "job" + (++jobCount);

    var node = jobProto.clone();
    node.attr('id', job);
    node.find('.command').text(cmd);
    node.appendTo(scrollback);

    var output = node.find('.output-container');
    var outputArea = output.find('.output');
    var lastOutputSpan = null;
    var lastOutputType = null;
    var linesOutput = 0;
    var newlinesOutput = 0;
    var terminal = null;
    var maxState = null;

    var input = node.find('.input-container');
    input.find('input').focus();

    function sender(s) {
      api('input', {job: job, input: s}, function() {});
    };

    function signaler(s) {
      s = 'kill'; // TODO: remove this when int and quit work
      api('input', {job: job, signal: s}, function() {});
    };

    function sizeOutput(m) {
      output.removeClass('output-hide output-tiny output-page output-full');
      output.addClass('output-' + m);
    };

    var LINES_IN_TINY = 3;
    var LINES_IN_PAGE = 24;

    function adjustOutput() {
      var n = linesOutput;
      if (n == 0 && input) n = 1;

      var m, s;
      if (n == 0)                   m = s = 'hide';
      else if (n <= LINES_IN_TINY)  m = s = 'tiny';
      else if (n <= LINES_IN_PAGE)  m = s = 'page';
      else                          { m = 'full'; s = 'page'; }

      if (terminal) {
        m = 'full';
        s = 'full';
      }

      if (maxState !== m) {
        node.removeClass('max-hide max-tiny max-page max-full');
        node.addClass('max-' + m);
        sizeOutput(s);
      }
    };

    function removeInput() {
      if (input) {
        input.remove();
        input = null;
        adjustOutput();
      }
    };

    function setClass(cls) {
      node.removeClass('running complete').addClass(cls);
    };

    function addVTOutput(txt) {
      removeInput();
      var where = output.find('.output');
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
      term.io.onVTKeystroke = sender;
      term.io.sendString = sender;
      term.installKeyboard();
      terminal = term;
      terminalNode = node;
      adjustOutput();
      node.get(0).scrollIntoView(true);
    };

    function removeVTOutput() {
      if (terminal) {
        terminal.uninstallKeyboard();
        terminal = null;
        terminalNode.remove();
        terminalNode = null;
      }
    };

    function addOutput(cls, txt) {
      if (terminal) {
        return terminal.interpret(txt);
      } else if (txt.match('\u001b[\[]')) {
        return addVTOutput(txt);
      }

      if (lastOutputType == cls && lastOutputSpan) {
        lastOutputSpan.append(document.createTextNode(txt));
      }
      else {
        lastOutputType = cls;
        lastOutputSpan = $('<span></span>', { 'class': cls }).text(txt);
        lastOutputSpan.appendTo(outputArea);
      }
      newlinesOutput += countOccurances(txt, '\n');
      linesOutput =
          newlinesOutput + (txt[txt.length-1] === '\n' ? 0 : 1);
      adjustOutput();
      lastOutputSpan.get(0).scrollIntoView(false);
    }

    node.data('job', {
      sender: sender,
      signaler: signaler,
      sizeOutput: sizeOutput,
      addOutput: addOutput,
      setClass: setClass,
      removeInput: removeInput,
      removeVTOutput: removeVTOutput
    });

    return { job: job };
  }

  var unknownJob = {
    addOutput: function(cls, txt) {
      var node = $('<span></span>', { 'class': cls }).text(txt);
      node.appendTo(scrollback);
      node[0].scrollIntoView(true);
    },
    setClass: function(cls) { },
    removeInput: function() { },
    removeVTOutput: function() { }
  };

  return {
    newJob: newJob,
    fromJob: jobFromJob
  };
});
