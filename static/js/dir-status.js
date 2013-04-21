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

define(['jquery', 'api', 'util', 'ls-status', 'svn-status'],
  function($, api, util, lsStatus, svnStatus) {

  "use strict";

  function updateStatusPane(ctx, cwd, runCommand) {
    var lsCmd = 'ls -d .svn';
    api.runStatus(lsCmd, function (d) { statusResults(ctx, cwd, runCommand, d); });
  }

  function statusResults(ctx, cwd, runCommand, d) {
    if (!d.stdout) {
      lsStatus.updateStatusPane(ctx, cwd, runCommand);   
    } else if (d.stdout.indexOf('.svn') >= 0) {
      svnStatus.updateStatusPane(ctx, cwd, runCommand);
    } else {
      lsStatus.updateStatusPane(ctx, cwd, runCommand);
    }
  }

  return {
    updateStatusPane: updateStatusPane,
  }

});
