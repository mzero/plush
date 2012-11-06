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
  "use strict";

  var lsStatusPane = $('#status-ls');
  var lsStatusPaneTBody = lsStatusPane.find('tbody');
  var fileTrProto = lsStatusPaneTBody.children('.file.proto')
    .detach()
    .removeClass('proto');

  var currentlyWaitingFor = '';
  var queuedDirectory = '';
  var collectedStdOut = '';
  var collectedStdErr = '';

  // ugly hack, until these functions are in a separate module
  var runCommandFn;
  var apiFn;

  var fileTypeNames = {
    '-': "File",
    'd': "Directory",
    'l': "Symbolic link",
    'p': "FIFO",
    'c': "Character special file",
    'b': "Block special file"
  };

  function results(data) {
    if ('stdout' in data) {
      collectedStdOut = collectedStdOut + data.stdout;
    }
    if ('stderr' in data) {
      collectedStdErr = collectedStdErr + data.stderr;
    }
    if ('running' in data) {
      if (!data.running) {
        $('#status-ls-cwd').text(currentlyWaitingFor);

        lsStatusPaneTBody.children('.file').detach();

        if (collectedStdOut !== '') {
          parseLsOutput(collectedStdOut).forEach(function(file) {
            var fileTr = fileTrProto.clone();

            formatFileName(fileTr, file);

            var dl = fileTr.find('.details');

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

        if (collectedStdErr != '') {
          $('<pre></pre>')
            .text(collectedStdErr)
            .appendTo(lsStatusPane);
        }

        collectedStdOut = '';
        collectedStdErr = '';
        currentlyWaitingFor = '';

        if (queuedDirectory) {
          var temp = queuedDirectory;
          queuedDirectory = '';
          updateStatusPane(temp, apiFn, runCommandFn);
        }
      }
    }
  }

  function formatFileName(tr, file) {
    var linkToDir = file.type === 'l' && file.typeSymbol === '/';
    var span = tr.find('.name');

    span.addClass('type-' + file.type);

    if (linkToDir) {
      span.addClass('type-d');
    }

    if (file.type === 'd' || linkToDir) {
      var cmd = 'cd ' + currentlyWaitingFor + '/' + file.name;

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

  function updateStatusPane(cwd, api, runCommand) {
    apiFn = api;
    runCommandFn = runCommand;

    if (currentlyWaitingFor) {
      queuedDirectory = cwd;
    } else {
      currentlyWaitingFor = cwd;

      var lsCmd = 'ls -lF ' + cwd;

      api('run', {job: 'ls-status', cmd: lsCmd, record: false});
    }
  }

  function parseLsOutput(out) {
    var files = [];

    out.split("\n").slice(1, -1).forEach(function(line) {
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
        var extra = items.length - 9;
        file.deviceInfo = items.slice(4, 5 + extra).join(" ");
        file.datetime = items.slice(5 + extra, 8 + extra).join(" ");
        file.name = items[8 + extra];
      } else {
        file.size = items[4];
        file.datetime = items.slice(5, 8).join(" ");
        file.name = items[8];

        if (type === 'l') {
          if (items.length !== 11) {
            console.error('unexpected ls line:', items);
          }
          file.target = items[10];
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
    results: results,
  }

});
