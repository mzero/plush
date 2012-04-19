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

/**
 * Rendering of a job result
 */
define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/job.html'
], function($, _, Backbone, jobTemplate){
  var jobView = Backbone.View.extend({
    el: $('#plush-jobs'),
    render: function () {
      var data = { 'result': 'my results', 'clazz': 'stdout'  };
      var compiledTemplate = _.template( jobTemplate, data );
      this.el.append( compiledTemplate );
    }
  });
  return new jobView;
});
