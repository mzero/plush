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

  function search(curr, runCommand) {
    var input = $('<li></li>').append(
      $('<input></input>')
        .bind('change', function (event) {
            runCommand("cd " + curr + $(this).val());
        }));
    return input;
  }

  function parseToDom(cwd, runCommand) {
    var ol = $('<ol></ol>');
    var dirSoFar = "";
    ['/'].concat(cwd.split('/')).forEach(function(piece) {
      if ("" !== piece) {
        dirSoFar = dirSoFar + piece + (piece == '/' ? '' : '/');
        ol.append($('<li></li>')
          .append(
            $('<a></a>', { href: '#'})
              .text(piece)
              .bind('click', function(event) {runCommand("cd " + dirSoFar);})
            ));
      }
    });
    ol.append(search(dirSoFar, runCommand));
    $('#context-cwd').empty().append(ol);
  }

  return {
    parseToDom: parseToDom
  }

});
