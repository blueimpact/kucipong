"use strict";

var maps = Array.prototype.slice.call(document.getElementsByClassName('js-googleMap') || []);

maps.forEach(function (map) {
  var apiKey = map.getAttribute('data-api-key');
  var address = map.getAttribute('data-address');

  if (!!address) {
    map.setAttribute('src', "https://www.google.com/maps/embed/v1/place?key=" + apiKey + "&q=" + address);
  }
});
