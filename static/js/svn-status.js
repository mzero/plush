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

  var svnStatusPane = $('#status-dir');
  var svnStatusPaneTBody = svnStatusPane.find('tbody');
  var fileTrProto = svnStatusPaneTBody.children('.file.proto')
    .clone()
    .removeClass('proto')
    .removeClass('hidden');

  // ugly hack, until this function is in a separate module
  var runCommandFn;

  function statusResults(dir, data) {
    $('#status-dir-cwd').text("SVN: " + dir);

    svnStatusPaneTBody.children('.file').detach();

    if ('stdout' in data) {
     parseSvnOutput(data.stdout).forEach(function(file) {
        var fileTr = fileTrProto.clone();

        formatFileName(fileTr, dir, file);

        var dl = fileTr.find('.svn-details');

        formatFileType(dl, file);

        svnStatusPaneTBody.append(fileTr);
      });
    }

    if ('stderr' in data) {
      $('<pre></pre>')
        .text(data.stderr)
        .appendTo(svnStatusPane);
    }
  }

  function formatFileName(tr, dir, file) {
    var span = tr.find('.name');
    span.addClass('svntype-' + file.type);
    span.text(file.name);
  }

  function fileDetailLine(node, name, value) {
    if (value) {
      node.find('.value.' + name).append(value);
    } else {
      node.find('.' + name).detach();
    }
  }

  function formatFileType(node, file) {
    if (name) {
      node.find('.value.type').text(name).prop('title', file.type);
    } else {
      node.find('.value.type').text(file.type);
    }
  }

  function updateStatusPane(ctx, cwd, runCommand) {
    runCommandFn = runCommand;
    // DANGER WILL ROBINSON -- this is a cheap hack -- should be done server side
    var svnCmd = 'svn status --depth=immediates --xml -v';
    api.runStatus(svnCmd, function (d) { statusResults(cwd, d); });
  }

  function parseSvnOutput(out) {
    var files = [];
    // DANGER WILL ROBINSON -- this is a cheap hack so I can easily parse xml and _is_ dangerous
    var xmlParse = $(out);
    var entries = xmlParse.find('entry');
    while (entries.length) {
      var entry = entries.first();
      entries = entries.next();
      var file = {
        author: entry.find('author').text(),
        date: entry.find('date').text(),
        type: entry.find('wc-status').attr('item'),
        revision: entry.find('wc-status').attr('revision'),
        name: entry.attr('path')
      };
      files.push(file);
    };
    return files;
  }

  return {
    updateStatusPane: updateStatusPane,
  }

});
