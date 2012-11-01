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

define(['jquery'], function($) {

  var cwdList = $('#context-cwd');
  var cwdItemProto = cwdList.children('.cwd-item.proto').detach();
  cwdItemProto.removeClass('proto');

  var cwdSubsProto = cwdList.children('.cwd-subs.proto')
    .detach()
    .removeClass('proto');

  function parseToDom(cwd, runCommand) {
    cwdList.empty();

    var dirSoFar = "";
    ['/'].concat(cwd.split('/')).forEach(function(piece) {
      if ("" !== piece) {
        dirSoFar = dirSoFar + piece + (piece == '/' ? '' : '/');
        var cmd = 'cd ' + dirSoFar;
        var cwdItem = cwdItemProto.clone();
        cwdItem.find('a')
          .text(piece)
          .bind('click', function(event) {runCommand(cmd);});
        cwdList.append(cwdItem);
      }
    });

    var cwdSubs = cwdSubsProto.clone();
    cwdSubs.find('input')
      .bind('change', function (event) {
        var cmd = 'cd ' + dirSoFar + $(this).val();
        runCommand(cmd);
      })
      .val('');

    cwdList.append(cwdSubs);
  }

  return {
    parseToDom: parseToDom
  }

});
