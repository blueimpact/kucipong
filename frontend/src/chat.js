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
app.ports.askStoreUserSettings.subscribe(function(settings) {
  localStorage.setItem('userSettings');
  app.ports.onStoreUserSettings.send();
});

app.ports.askLoadUserSettings.subscribe(function() {
  var settings = JSON.parse(localStorage.getItem('userSettings'));
  app.ports.onLoadUserSettings.send(settings);
});
