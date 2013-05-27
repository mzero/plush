// Copyright 2013 Google Inc. All Rights Reserved.
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

define(['jquery', 'hterm'],
function($, input){
  'use strict';

  var TRACE = false;

  // cterm's Terminal is somewhat like a light-weight hterm. It only manages one
  // line of output. If any terminal functions are invoked that require more
  // than that, a call back is used to revert to a full hterm.
  //
  // The one-line terminal aims to support colored output, and simple updating
  // output such as produced by scp, test frameworks, and other programs that
  // produce status like output without resorting to full term capabailities.
  //
  // The model is very simple: Text can only be appended to a line. However,
  // to support the kind of overwriting that some programs do, provisional
  // overwriting is done by erasing the line entirely in anticipation of full
  // overwrite.

  var Terminal = function(output, container, goFullScreen) {
    this.output_ = output;        // div that contains spans
    this.container_ = container;  // div that contains output - and scrolls

    this.goFullScreen_ = goFullScreen;

    this.attrs_ = new hterm.TextAttributes(window.document);
    this.cursor_ = new hterm.RowCol(0, 0);
    this.autoCarriageReturn_ = false;

    this.currLineSpans_ = [];
    this.currSpan_ = null;
    this.currLength_ = 0;
    this.clearLength_ = 0;
    this.lastLineSpans_ = null;
    this.lastLength_ = 0;
    this.spanClass_ = '';

    // both VT and the client use this object for sending back to the process
    this.io = new hterm.Terminal.IO(this);

    // this state is the state the VT expects to be able to read and/or write
    // within the terminal object. For the most part, this code doesn't care
    // about these values:
    this.allowColumnWidthChanges_ = null;   // written
    this.keyboard = {
        altIsMeta: false,                   // read
        altSendsEscape: null,               // written
        applicationCursor: null,            // written
        applicationKeypad: null,            // written
        backspaceSendsBackspace: null,      // written
        metaSendsEscape: null               // written
    };
    this.screenSize = { width: 80 };        // read
    this.scrollOnKeystroke = null;          // written
    this.scrollOnOutput = null;             // written

    this.vt_ = new hterm.VT(this);  // set this last, relies on the above!
    this.vt_.decodeUTF8 = function (str) { return str; };
      // Monkey patching the VT so that it doesn't try to decode inputs to
      // interpret(). Note that this will affect the data passed to
      // copyStringToClipboard() as well: It won't be decoded where it needs
      // to be. However that function causes fall back... so no worries!
  };

  if (TRACE) {
    Terminal.prototype.trace_ = function(s) {
      console.log('cterm.Terminal ' + s);
    };
  } else {
    Terminal.prototype.trace_ = function(s) { };
  }

  Terminal.prototype.fallback_ = function(s) {
    if (TRACE) { this.trace_('falling back: ' + s); }
    this.goFullScreen_();
  }

  Terminal.prototype.setSpanClass = function(cls) {
    if (cls !== this.spanClass_) {
      this.spanClass_ = cls;
      if (this.currLength_ > 0) {
        this.newLine();
      }
    }
  }

  Terminal.prototype.interpret = function(str) {
    // See Note above about monkey patching decodeUTF8 for why this works
    this.vt_.interpret(str);
  };

  Terminal.prototype.getLineCount = function() {
    return this.cursor_.row + (this.currLength_ > 0 ? 1 : 0)
  }

  Terminal.prototype.ensureSpan_ = function() {
    if (this.currSpan_ === null) {
      if (this.attrs_.isDefault()) {
        this.currSpan_ = $('<span></span>', { 'class': this.spanClass_ });
      } else {
        this.currSpan_ = $(this.attrs_.createContainer());
        this.currSpan_.addClass(this.spanClass_);
      }
      this.currSpan_.appendTo(this.output_);
      this.currLineSpans_.push(this.currSpan_);
    }
  }

  Terminal.prototype.print = function(str) {
    if (this.cursor_.column < this.currLength_) {
      // this is an overwrite situation
      this.clearLength_ = this.currLength_;
      while (this.currLineSpans_.length > 0) {
        this.currLineSpans_.pop().remove();
      }
      this.currSpan_ = null;
      this.currLength_ = 0;
    }
    if (this.currSpan_ !== null
        && !this.attrs_.matchesContainer(this.currSpan_.get(0))) {
      this.currSpan_ = null
    }
    this.ensureSpan_();
    var t = this.currSpan_.text();
    t = t + str
    this.currSpan_.text(t);
    this.currLength_ = t.length;
    this.cursor_.column = this.currLength_;
  }

  Terminal.prototype.forwardTabStop = function() {
    this.print('        '.substring(this.cursor_.column % 8));
  }

  Terminal.prototype.newLine = function() {
    if (this.currLength_ < this.clearLength_) {
      this.fallback_('newLine: did not overwrite line as promised');
    }
    this.ensureSpan_();
    this.currSpan_.text(this.currSpan_.text() + '\n');

    this.lastLineSpans_ = this.currLineSpans_;
    this.lastLength_ = this.currLength_;

    this.currLineSpans_ = [];
    this.currSpan_ = null;
    this.currLength_ = 0;
    this.clearLength_ = 0;

    this.cursor_.column = 0;
    this.cursor_.row += 1;
  };

  Terminal.prototype.lineFeed = function() {
    var c = this.cursor_.column;
    this.newLine();
    this.setCursorColumn(c);
  }

  Terminal.prototype.formFeed = function() {
    if (this.autoCarriageReturn_) {
      this.newLine();
    } else {
      this.lineFeed();
    }
  }

  Terminal.prototype.spaces_ = function(n) {
    var s = '                                                        ';
    while (s.length < n) {
      s = s + s;
    }
    this.print(s.substring(0,n));
  }

  Terminal.prototype.setCursorColumn = function(c) {
    var delta = c - this.cursor_.column;
    if (delta > 0) {
      this.spaces_(delta);
    } else if (c == 0) {
      this.cursor_.column = c;
    } else if (delta < 0) {
      this.fallback_('setCursorColumn to mid-line: ' + c);
    }
  }

  Terminal.prototype.setCursorPosition = function(r, c) {
    if (this.cursor_.row !== r) {
      this.fallback_('setCursorPosition to row ' + r);
      return;
    }
    this.setCursorColumn(c);
  }

  Terminal.prototype.eraseLine = function() {
    var c = this.cursor_.column;
    while (this.currLineSpans_.length > 0) {
      this.currLineSpans_.pop().remove();
    }
    this.currSpan_ = null;
    this.currLength_ = 0;
    this.clearLength_ = 0;
    this.cursor_.column = 0;
    if (c > 0) {
      this.spaces_(c);
    }
  }

  Terminal.prototype.eraseToRight = function() {
    if (this.cursor_.column < this.currLength_) {
      this.eraseLine();
    } else {
      this.clearLength_ = 0;
    }
  }

  Terminal.prototype.cursorUp = function(n) {
    if (n === 0) return;
    if (n === 1
        && this.currLineSpans_.length === 0
        && this.lastLineSpans_ !== null) {
      // we can support cursor up if nothing has been put on the new line yet
      this.currLineSpans_ = this.lastLineSpans_;
      this.currSpan_ = this.currLineSpans_[this.currLineSpans_.length - 1];
      this.currLength_ = this.lastLength_;
      this.clearLength_ = 0;

      this.lastLineSpans_ = null;
      this.lastLength_ = 0;

      var t = this.currSpan_.text().replace(/\n$/, '');
      this.currSpan_.text(t);
      return;
    }
    this.fallback_('cursorUp: ' + n);
  }

  function fallback(s) { return function() { this.fallback_(s); } }
  function ignore(s)   { return function() { this.trace_(s + ' ignored'); } }

  // these functions constitute the API from VT back to the terminal
  Terminal.prototype.backwardTabStop       = fallback('backwardTabStop');
  Terminal.prototype.clear                 = fallback('clear');
  Terminal.prototype.clearAllTabStops      = ignore('clearAllTabStops');
  Terminal.prototype.clearHome             = fallback('clearHome');
  Terminal.prototype.clearTabStopAtCursor  = fallback('clearTabStopAtCursor');
  Terminal.prototype.copyStringToClipboard = fallback('copyStringToClipboard');
  Terminal.prototype.cursorDown            = fallback('cursorDown');
  Terminal.prototype.cursorLeft            = fallback('cursorLeft');
  Terminal.prototype.cursorRight           = fallback('cursorRight');
  Terminal.prototype.cursorUp;               // see above
  Terminal.prototype.deleteChars           = fallback('deleteChars');
  Terminal.prototype.deleteLines           = fallback('deleteLines');
  Terminal.prototype.eraseAbove            = fallback('eraseAbove');
  Terminal.prototype.eraseBelow            = fallback('eraseBelow');
  Terminal.prototype.eraseLine;              // see above
  Terminal.prototype.eraseToLeft           = fallback('eraseToLeft');
  Terminal.prototype.eraseToRight;           // see above
  Terminal.prototype.fill                  = fallback('fill');
  Terminal.prototype.formFeed;               // see above
  Terminal.prototype.forwardTabStop;         // see above
  Terminal.prototype.getBackgroundColor    = function() { return this.attrs_.DEFAULT_COLOR; };
  Terminal.prototype.getCursorColumn       = function() { return this.cursor_.column; };
  Terminal.prototype.getCursorRow          = function() { return this.cursor_.row; };
  Terminal.prototype.getForegroundColor    = function() { return this.attrs_.DEFAULT_COLOR; };
  Terminal.prototype.getTextAttributes     = function() { return this.attrs_; }
  Terminal.prototype.insertLines           = fallback('insertLines');
  Terminal.prototype.insertSpace           = fallback('insertSpace');
  Terminal.prototype.lineFeed;               // see above
  Terminal.prototype.print;                  // see above
  Terminal.prototype.reset                 = ignore('reset');
  Terminal.prototype.restoreCursor         = function(c) { this.setCursorPosition(c.row, c.column); }
  Terminal.prototype.restoreOptions        = ignore('restoreOptions');
  Terminal.prototype.reverseLineFeed       = fallback('reverseLineFeed');
  Terminal.prototype.ringBell              = fallback('ringBell');
  Terminal.prototype.saveCursor            = function() { return this.cursor_; };
  Terminal.prototype.setAbsoluteCursorRow  = fallback('setAbsoluteCursorRow');
  Terminal.prototype.setAlternateMode      = ignore('setAlternateMode');
  Terminal.prototype.setAutoCarriageReturn = function(b) { this.autoCarriageReturn_ = b; };
  Terminal.prototype.setCursorBlink        = ignore('setCursorBlink');
  Terminal.prototype.setCursorColumn;        // see above
  Terminal.prototype.setCursorPosition;      // see above
  Terminal.prototype.setCursorVisible      = ignore('setCursorVisible');
  Terminal.prototype.setInsertMode         = fallback('setInsertMode');
  Terminal.prototype.setOriginMode         = fallback('setOriginMode');
  Terminal.prototype.setReverseVideo       = fallback('setReverseVideo');
  Terminal.prototype.setReverseWraparound  = fallback('setReverseWraparound');
  Terminal.prototype.setSelectionEnabled   = ignore('setSelectionEnabled');
  Terminal.prototype.setTabStop            = fallback('setTabStop');
  Terminal.prototype.setTextAttributes     = function(attrs) { this.attrs_ = attrs; };
  Terminal.prototype.setVTScrollRegion     = fallback('setVTScrollRegion');  //(top, bottom);
  Terminal.prototype.setWidth              = ignore('setWidth');
  Terminal.prototype.setWindowTitle        = ignore('setWindowTitle');
  Terminal.prototype.setWraparound         = fallback('setWraparound');
  Terminal.prototype.softReset             = ignore('softReset');
  Terminal.prototype.vtScrollDown          = fallback('vtScrollDown');
  Terminal.prototype.vtScrollUp            = fallback('vtScrollUp');


  return {
    Terminal: Terminal
  };
});
