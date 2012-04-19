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
 * Rendering of a jobs
 */
define([
  'jquery',
  'underscore',
  'backbone',
  'collections/jobs',
  'text!templates/jobs.html'
], function($, _, Backbone, jobsCollection, jobTemplate){
  var jobsView = Backbone.View.extend({
    initialize: function () {
      this.collection = jobsCollection;
      this.collection.bind("add", this.addJob, this);
    },
    addJob: function (job) {
      alert('new job' + job);
    },
    el: $('#plush-jobs'),
    render: function() {
      var data = {
        jobs: this.collection.models,
        _: _ 
      };
      var compiledTemplate = _.template( jobTemplate, data );
      $(this.el).html( compiledTemplate );
    }
  });
  return new jobsView;
});
