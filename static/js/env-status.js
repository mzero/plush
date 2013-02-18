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

define(['jquery', 'api', 'util'], function($, api, util) {
  "use strict";

  function statusResults(vars) {
    var envList = $('#context-env');
    var shList = $('#context-shell');
    envList.empty();
    shList.empty();
    vars.slice(10).forEach(function(v) {
      var dt = $('<dt></dt>', { class: v.mode });
      var dd = $('<dd></dd>', { class: v.mode });
      dt.text(v.name);
      dd.text(v.value);
      if (v.scope === 'env') { envList.append(dt); envList.append(dd); }
      if (v.scope === 'shell') { shList.append(dt); shList.append(dd); }
    });
  }

  // Figure out what variables are relavant to the user.
  // Probbably something like:
  //   * always display all variables set this "session"
  //   * display shell vars if there is space
  //   * display env vars if there is still space
  function displayHeuristic(varList) {
    return varList.slice(10);
  }
  function updateStatusPane(ctx, cwd, runCommand) {
    statusResults(ctx.vars || []);
  }

  return {
    updateStatusPane: updateStatusPane,
  }

});
