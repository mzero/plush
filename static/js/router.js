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

define(['jquery', 'underscore', 'backbone',
  // Views
  'views/main',
  'views/search',
  'views/jobs'
	], function($, _, Backbone,
	    mainView, searchView, jobsView) {

  var jobs = null;
  var PlushRouter = Backbone.Router.extend({
    routes: {
      // Define some URL routes
      '/main': 'showMain',
      
      // Default
      '*actions': 'showMain'
    },
    showMain: function() {
      mainView.render();
      searchView.render();
      jobsView.render();
    },
    defaultAction: function(actions){
      // We have no matching route, lets display the home page 
      alert(actions);
    }
  });

  var initialize = function() {
    var plushRouter = new PlushRouter;
    Backbone.history.start();
  };

  return { 
    initialize: initialize
  };
});
