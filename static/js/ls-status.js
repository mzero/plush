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

  var lsStatusPane = $('#status-dir');
  var lsStatusPaneTBody = lsStatusPane.find('tbody');
  var fileTrProto = lsStatusPaneTBody.children('.file.proto')
    .clone()
    .removeClass('proto')
    .removeClass('hidden');

  // ugly hack, until this function is in a separate module
  var runCommandFn;

  var fileTypeNames = {
    '-': "File",
    'd': "Directory",
    'l': "Symbolic link",
    'p': "FIFO",
    'c': "Character special file",
    'b': "Block special file"
  };

  function statusResults(dir, data) {
    $('#status-dir-cwd').text(dir);

    lsStatusPaneTBody.children('.file').detach();

    if ('stdout' in data) {
     parseLsOutput(data.stdout).forEach(function(file) {
        var fileTr = fileTrProto.clone();

        formatFileName(fileTr, dir, file);

        var dl = fileTr.find('.ls-details');

        formatFileType(dl, file);
        formatFileSize(dl, file);
        formatFileDeviceInfo(dl, file);
        fileDetailLine(dl, 'owner', file.owner);
        fileDetailLine(dl, 'group', file.group);
        fileDetailLine(dl, 'time', file.datetime);
        formatFilePerms(dl, file);
        fileDetailLine(dl, 'links', file.linkCount);
        fileDetailLine(dl, 'target', file.target);

        lsStatusPaneTBody.append(fileTr);
      });
    }

    if ('stderr' in data) {
      $('<pre></pre>')
        .text(data.stderr)
        .appendTo(lsStatusPane);
    }
  }

  function formatFileName(tr, dir, file) {
    var linkToDir = file.type === 'l' && file.typeSymbol === '/';
    var span = tr.find('.name');

    span.addClass('type-' + file.type);

    if (linkToDir) {
      span.addClass('type-d');
    }

    if (file.type === 'd' || linkToDir) {
      var cmd = 'cd ' + util.escapeShellArgument(dir + '/' + file.name);

      $('<a>')
        .prop('href', '#')
        .text(file.name)
        .on('click', function(e) {runCommandFn(cmd)})
        .appendTo(span);
    } else {
      span.text(file.name);
    }
  }

  function fileDetailLine(node, name, value) {
    if (value) {
      node.find('.value.' + name).append(value);
    } else {
      node.find('.' + name).detach();
    }
  }

  function formatFileType(node, file) {
    var name = fileTypeNames[file.type];

    if (name) {
      node.find('.value.type').text(name).prop('title', file.type);
    } else {
      node.find('.value.type').text(file.type);
    }
  }

  function formatFilePerms(node, file) {
    var dd = node.find('.value.perms');

    dd.find('.owner').text(file.ownerPerm);
    dd.find('.group').text(file.groupPerm);
    dd.find('.other').text(file.otherPerm);

    if (file.altAccessMethod) {
      dd.find('.alt').text(file.altAccessMethod);
    } else {
      dd.find('.alt').detach();
    }
  }

  function formatFileSize(node, file) {
    if (file.size) {
      node.find(".value.size").text(file.size);
    } else {
      node.find(".size").detach();
    }
  }

  function formatFileDeviceInfo(node, file) {
    if (file.deviceInfo) {
      node.find(".value.info").text(file.deviceInfo);
    } else {
      node.find(".info").detach();
    }
  }

  function updateStatusPane(ctx, cwd, runCommand) {
    runCommandFn = runCommand;
    var lsCmd = 'ls -lF ' + util.escapeShellArgument(cwd);
    api.runStatus(lsCmd, function (d) { statusResults(cwd, d); });
  }

  function stripCSI(out) {
    // If CLICOLOR is set `ls` will output ANSI CSI SGR codes that
    // should be stripped before display here. See also:
    // http://en.wikipedia.org/wiki/ANSI_escape_code
    // http://en.wikipedia.org/wiki/Control_Sequence_Introducer#CSI_codes
    return out.replace(/\x1b\[[^m]*m/g, '');
  }

  function parseLsOutput(out) {
    var files = [];

    stripCSI(out).split(/\r?\n/).slice(1, -1).forEach(function(line) {
      var items = line.split(/ +/);
      var mode = items[0];
      var type = mode.substring(0, 1);
      var file = {
        line: line,
        type: type,
        ownerPerm: mode.substring(1, 4),
        groupPerm: mode.substring(4, 7),
        otherPerm: mode.substring(7, 10),
        altAccessMethod: mode.substring(10),
        linkCount: items[1],
        owner: items[2],
        group: items[3],
      }

      if (type === 'b' || type === 'c') {
        file.deviceInfo = items.slice(4, 6).join(" ");
        file.datetime = items.slice(6, 9).join(" ");
        file.name = items[9];
      } else {
        file.size = items[4];
        file.datetime = items.slice(5, 8).join(" ");

        if (type === 'l') {
          var nameAndTarget = items[8].split(' -> ');
          file.name = nameAndTarget[0];
          file.target = nameAndTarget[1];
        } else {
          file.name = items[8];
        }
      }

      if (type === 'l') {
        file.target = extractTypeSymbol(file.target, file);
      } else {
        file.name = extractTypeSymbol(file.name, file);
      }

      files.push(file);
    });

    return files;
  }

  function extractTypeSymbol(value, fileObj) {
    value = value.trim();

    switch (value.charAt(value.length - 1)) {
      case '/':
      case '@':
      case '|':
      case '*':
        fileObj.typeSymbol = value.charAt(value.length - 1);
        value = value.substring(0, value.length - 1);
        break;
    }

    return value;
  }

  return {
    updateStatusPane: updateStatusPane,
  }

});
