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

define(['jquery'], function($){
  'use strict';

  function escapeShellArgument(text) {
    return text.replace(/[ \n\\'"$&|;<>]/g, "\\$&");
  }

  function scrollIntoView(scroller, inner, offset) {
    var sTop = scroller.scrollTop();
    var sBottom = sTop + scroller.height();
    var sTop0 = sTop;

    var iTop = inner.offset().top - scroller.children().offset().top;
      // TODO(mzero): this doesn't work if the scroller has non-scrolled content
    var iBottom = iTop + inner.outerHeight(true);

    if (offset) {
      iTop += offset;
    }

    if (iBottom > sBottom) {
      sTop += iBottom - sBottom;
    }
    if (iTop < sTop) {
      sTop -= sTop - iTop;
    }
    if (sTop != sTop0) {
      scroller.scrollTop(sTop);
    }
  }

  // Use this to debug the timing of any function. Usage:
  //    function foo(x, y) { ... }
  //    foo = util.timeFn(foo);
  function timeFn(f) {
    return function() {
      var tStart = new Date();
      var result = f.apply(this, arguments);
      var tEnd = new Date();
      console.log(f.name + ' took ' + (tEnd - tStart) + 'ms');
      return result;
    };
  }
  
  return {
    escapeShellArgument: escapeShellArgument,
    scrollIntoView: scrollIntoView,
    timeFn: timeFn
  };
});
