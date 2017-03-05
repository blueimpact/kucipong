"use strict";

var imageSelector = require('./_image-selector');
var resetSubcategories = function (key) {
  Array.prototype.slice.call(document.querySelectorAll('[data-subcategory-of]')).forEach(function (x) {
    return x.setAttribute('aria-hidden', 'true');
  });
};

var setSubcategories = function (key) {
  resetSubcategories();
  document.querySelector('[data-subcategory-of="' + key + '"]').removeAttribute('aria-hidden', 'true');
};

var businessCategory = document.getElementById('storeBusinessCategory');

// Hide all subcategories at first.
setSubcategories(businessCategory.value);

businessCategory.addEventListener('change', function (eve) {
  setSubcategories(eve.target.value);
});

var storeAddress = document.getElementById('storeAddress');
var mapFrame = document.getElementById('js-map-frame');
var mapFrameWrapper = document.getElementById('js-map-frame-wrapper');

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

/* ======================
 *  Image selector
 * ====================== */

imageSelector({
  preview: 'storeImage',
  selector: 'storeImageSelector',
  uploadBtn: 'storeImageLabel',
  resetBtn: 'resetImage',
  defaultImage: 'defaultImage'
});
