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

define(function() {
  "use strict";
  var history = [];
  var ring = 0;

  function rotateHistory(n){
    if (history.length == 0) {
      return undefined;
    }
    ring += n;
    ring = ring % history.length;
    return history[ring];
  };

  function previous() {
    return rotateHistory(-1);
  };

  function next() {
    return rotateHistory(1);
  }

  function add(line) {
    history.push(line);
    ring++;
  }

  function reset() {
    ring = history.length;
  }

  return {
    add: add,
    next: next,
    previous: previous
  };

});
