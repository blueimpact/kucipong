'use strict';

// Pull in desired CSS/SASS files
require('./styles/chat.scss');

// Inject bundled Elm app into div#main
var Elm = require('Main');
var app = Elm.Main.embed(document.getElementById('main'));

// Pass configuration to elm
// var ENV = process.env;
// app.ports.config.send(ENV);

// FFI for localStorage
app.ports.askStoreUserSettings.subscribe(function(o) {
  localStorage.setItem('userSettings', JSON.stringify(o.settings));
  app.ports.onStoreUserSettings.send(o.key);
});

app.ports.askLoadUserSettings.subscribe(function() {
  try {
    var settings = JSON.parse(localStorage.getItem('userSettings'));
  } catch (e) {
    var settings = null;
  }
  app.ports.onLoadUserSettings.send(settings);
});
