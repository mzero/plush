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

define(['jquery', 'underscore', 'hterm'], function($, _){
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

  function Job(api, cmd) {
    this.job = "job" + (++jobCount);

    this.node = jobProto.clone();
    this.node.attr('id', this.job);
    this.node.find('.command').text(cmd);
    this.node.appendTo(scrollback);
    this.node.data('job', this);

    this.output = this.node.find('.output-container');
    this.outputArea = this.output.find('.output');
    this.lastOutputSpan = null;
    this.lastOutputType = null;
    this.linesOutput = 0;
    this.newlinesOutput = 0;
    this.terminal = null;
    this.maxState = null;

    this.input = this.node.find('.input-container');
    this.input.find('input').focus();

    this.api = api;
  }

  Job.fromElement = jobFromElement;
  Job.fromJob = jobFromJob;

  Job.prototype.sender = function(s) {
    this.api('input', {job: this.job, input: s}, function() {});
  };

  Job.prototype.signaler = function(s) {
    s = 'kill'; // TODO: remove this when int and quit work
    this.api('input', {job: this.job, signal: s}, function() {});
  };

  Job.prototype.sizeOutput = function(m) {
    this.output.removeClass('output-hide output-tiny output-page output-full');
    this.output.addClass('output-' + m);
  };

  var LINES_IN_TINY = 3;
  var LINES_IN_PAGE = 24;

  Job.prototype.adjustOutput = function() {
    var n = this.linesOutput;
    if (n == 0 && this.input) n = 1;

    var m, s;
    if (n == 0)                   m = s = 'hide';
    else if (n <= LINES_IN_TINY)  m = s = 'tiny';
    else if (n <= LINES_IN_PAGE)  m = s = 'page';
    else                          { m = 'full'; s = 'page'; }

    if (this.terminal) {
      m = 'full';
      s = 'full';
    }

    if (this.maxState !== m) {
      this.node.removeClass('max-hide max-tiny max-page max-full');
      this.node.addClass('max-' + m);
      this.sizeOutput(s);
    }
  };

  Job.prototype.removeInput = function() {
    if (this.input) {
      this.input.remove();
      this.input = null;
      this.adjustOutput();
    }
  };

  Job.prototype.setClass = function(cls) {
    this.node.removeClass('running complete').addClass(cls);
  };

  Job.prototype.addVTOutput = function(txt) {
    this.removeInput();
    var where = this.output.find('.output');
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
    var sendInput = _.bind(this.sender, this);
    term.io.onVTKeystroke = sendInput;
    term.io.sendString = sendInput;
    term.installKeyboard();
    this.terminal = term;
    this.terminalNode = node;
    this.adjustOutput();
    node.get(0).scrollIntoView(true);
  };

  Job.prototype.removeVTOutput = function() {
    if (this.terminal) {
      this.terminal.uninstallKeyboard();
      this.terminal = null;
      this.terminalNode.remove();
      this.terminalNode = null;
    }
  };

  Job.prototype.addOutput = function(cls, txt) {
    if (this.terminal) {
      return this.terminal.interpret(txt);
    } else if (txt.match('\u001b[\[]')) {
      return this.addVTOutput(txt);
    }

    if (this.lastOutputType == cls && this.lastOutputSpan) {
      this.lastOutputSpan.append(document.createTextNode(txt));
    }
    else {
      this.lastOutputType = cls;
      this.lastOutputSpan = $('<span></span>', { 'class': cls }).text(txt);
      this.lastOutputSpan.appendTo(this.outputArea);
    }
    this.newlinesOutput += countOccurances(txt, '\n');
    this.linesOutput =
        this.newlinesOutput + (txt[txt.length-1] === '\n' ? 0 : 1);
    this.adjustOutput();
    this.lastOutputSpan.get(0).scrollIntoView(false);
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

  return Job;
});
