"use strict";

var storeAddress = document.getElementById('storeAddress');
var mapFrame = document.getElementById('js-map-frame');
var mapFrameWrapper = document.getElementById('js-map-frame-wrapper');
if (!!mapFrame) {
  storeAddress.addEventListener('change', function (eve) {
    var val = storeAddress.value;
    if (!!val) {
      mapFrameWrapper.setAttribute('aria-busy', 'true');
      mapFrame.setAttribute('src', mapFrame.getAttribute('data-map-base-api') + val);
    } else {
      mapFrame.setAttribute('aria-hidden', 'true');
      mapFrame.removeAttribute('src');
    }
  });

  mapFrame.addEventListener('load', function () {
    mapFrameWrapper.removeAttribute('aria-busy');
    mapFrame.removeAttribute('aria-hidden');
  });
}

