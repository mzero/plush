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

  var Terminal = function(output, goFullScreen) {
    this.output_ = output;        // div that contains spans
    this.goFullScreen_ = goFullScreen;

    this.attrs_ = new hterm.TextAttributes(window.document);
    this.cursor_ = new hterm.RowCol(0, 0);
    this.autoCarriageReturn_ = false;

    this.currLineSpans_ = [];
    this.currLength_ = 0;
    this.lastLineSpans_ = null;
    this.lastLength_ = 0;
    this.spanClass_ = '';
    this.inputSpan_ = $('<span class="cursor"> </span>');
    this.inputSpanVisible_ = false;

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

    this.vt = new hterm.VT(this);  // set this here, relies on the above!
    this.keyboard = new hterm.Keyboard(this);
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

  Terminal.prototype.attrMatch_ = function(elem) {
    return this.attrs_.matchesContainer(elem.get(0));
  };

  Terminal.prototype.interpret = function(str) {
    this.vt.interpret(str);
  };

  Terminal.prototype.getLineCount = function() {
    return this.cursor_.row + (this.currLength_ > 0 ? 1 : 0)
  };

  Terminal.prototype.setCursorVisible = function(show) {
    if (show) {
      if (!this.inputSpanVisible_) {
        if (this.currLineSpans_.length > 0) {
          this.inputSpan_.insertBefore(this.currLineSpans_[0]);
        } else {
          this.inputSpan_.appendTo(this.output_);
        }
        this.inputSpanVisible_ = true;
      }
    } else {
      if (this.inputSpanVisible_) {
        this.inputSpan_.remove();
        this.inputSpanVisible_ = false;
      }
    }
  };

  Terminal.prototype.repositionInputSpan_ = function() {
    if (this.inputSpanVisible_) {
      this.setCursorVisible(false);
      this.setCursorVisible(true);
    }
  };

  Terminal.prototype.modifyLine_ = function(del, ins) {
    var cIndex, dIndex;
    var cSpan = null, dSpan = null;
    var cSpanText = '', dSpanText = '';
    var cOffset = 0;

    // find the span that contains the cursor
    if (this.cursor_.column >= this.currLength_) {
      // common case optimization: appending to the end of a line
      if (this.currLineSpans_.length > 0) {
        cIndex = this.currLineSpans_.length - 1;
        cSpan = this.currLineSpans_[cIndex];
        cSpanText = cSpan.text();
        cOffset = cSpanText.length;
      }
    } else {
      cOffset = this.cursor_.column;
      for (cIndex = 0; cIndex < this.currLineSpans_.length; cIndex += 1) {
        cSpan = this.currLineSpans_[cIndex];
        cSpanText = cSpan.text();
        if (cSpanText.length >= cOffset) break;
        cOffset -= cSpanText.length;
      }
    }

    if (cSpan !== null) {
      // delete characters from the cursor span
      var e = Math.min(del, cSpanText.length - cOffset);
      var preText = cSpanText.slice(0, cOffset);
      var postText = cSpanText.slice(cOffset + e);
      del -= e;
      if (postText.length === 0) {
        cSpan.text(cSpanText = preText);
        this.currLength_ -= e;
      } else {
        // this is the insert in the middle of a span case, assert(del === 0)
        if (this.attrMatch_(cSpan)) {
          cSpan.text(preText + ins + postText);
          this.currLength_ += ins.length - e;
          return;
        }
        cSpan.text(cSpanText = preText);
        var nSpan = cSpan.clone();
        nSpan.text(postText);
        nSpan.insertAfter(cSpan);
        this.currLineSpans_.splice(cIndex + 1, 0, nSpan);
      }

      // delete characters from subsequent spans
      dIndex = cIndex + 1;
      while (true) {
        if (dIndex < this.currLineSpans_.length) {
          dSpan = this.currLineSpans_[dIndex];
          dSpanText = dSpan.text();
        } else {
          dSpan = null;
          break;
        }
        if (del === 0) break;

        var f = Math.min(del, dSpanText.length);
        dSpanText = dSpanText.slice(f);
        del -= f;
        this.currLength_ -= f;

        if (dSpanText.length > 0) {
          dSpan.text(dSpanText);
          break; // assert(del === 0)
        } else {
          dSpan.remove();
          this.currLineSpans_.splice(dIndex,1);
        }
      }
    }

    if (ins.length === 0) return;
    this.currLength_ += ins.length;

    // finally - insert between cSpan and dSpan
    if (cSpan !== null && this.attrMatch_(cSpan)) {
      cSpan.text(cSpanText + ins);
    } else if (dSpan !== null && this.attrMatch_(dSpan)) {
      dSpan.text(ins + dSpanText);
    } else {
      var nSpan;
      if (this.attrs_.isDefault()) {
        nSpan = $('<span></span>', { 'class': this.spanClass_ });
      } else {
        nSpan = $(this.attrs_.createContainer());
        nSpan.addClass(this.spanClass_);
      }
      nSpan.text(ins);
      if (cSpan !== null) {
        nSpan.insertAfter(cSpan)
        this.currLineSpans_.splice(cIndex+1, 0, nSpan);
      } else {
        nSpan.appendTo(this.output_);
        this.currLineSpans_.push(nSpan);
      }
    }
  }

  Terminal.prototype.print = function(str) {
    this.modifyLine_(str.length, str);
    this.cursor_.column = Math.min(
      this.cursor_.column + str.length, this.currLength_);
  }

  Terminal.prototype.forwardTabStop = function() {
    this.print('        '.substring(this.cursor_.column % 8));
  }

  Terminal.prototype.newLine = function() {
    this.lastLineSpans_ = this.currLineSpans_;
    this.lastLength_ = this.currLength_;

    this.cursor_.column = this.currLength_;
    this.modifyLine_(0, '\n');

    this.currLineSpans_ = [];
    this.currLength_ = 0;

    this.cursor_.column = 0;
    this.cursor_.row += 1;

    this.repositionInputSpan_();
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
    c = Math.max(0, c);
    if (c > this.cursor_.column) {
      this.spaces_(c - this.cursor_.column);
    } else {
      this.cursor_.column = c;
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
    this.currLength_ = 0;
    this.cursor_.column = 0;
    if (c > 0) {
      this.spaces_(c);
    }
  }

  Terminal.prototype.eraseToRight = function() {
    this.modifyLine_(this.currLength_ - this.cursor_.column, '');
  }

  Terminal.prototype.cursorLeft = function(n) {
    this.setCursorColumn(this.cursor_.column - (n || 1));
  }

  Terminal.prototype.cursorRight = function(n) {
    this.setCursorColumn(this.cursor_.column + (n || 1));
  }

  Terminal.prototype.cursorUp = function(n) {
    if (n === 0) return;
    if (n === 1
        && this.currLineSpans_.length === 0
        && this.lastLineSpans_ !== null) {
      // we can support cursor up if nothing has been put on the new line yet
      this.currLineSpans_ = this.lastLineSpans_;
      this.currLength_ = this.lastLength_;

      this.lastLineSpans_ = null;
      this.lastLength_ = 0;

      var n = this.currLineSpans_[this.currLineSpans_.length - 1];
      var t = n.text().replace(/\n$/, '');
      n.text(t);

      this.repositionInputSpan_();
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
  Terminal.prototype.cursorLeft;             // see above
  Terminal.prototype.cursorRight;            // see above
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
  Terminal.prototype.setCursorVisible;       // see above
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

  Terminal.prototype.installKeyboard = function(elem) {
    this.keyboard.installKeyboard((elem || this.output_).get(0));
  };
  Terminal.prototype.uninstallKeyboard = function() {
    this.keyboard.installKeyboard(null);
  };
  Terminal.prototype.onVTKeystroke = function(str) {
    this.io.onVTKeystroke(str);
  };

  Terminal.prototype.getDocument = function() {
    return this.output_.get(0).ownerDocument;
  };

  return {
    Terminal: Terminal
  };
});
