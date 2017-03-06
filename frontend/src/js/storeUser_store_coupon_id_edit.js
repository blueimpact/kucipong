"use strict";

require('date-input-polyfill');
var imageSelector = require('./_image-selector');

var couponType = document.getElementById("couponType");
couponType.addEventListener('change', function () {
  window.setTimeout(function () {
    window.location.replace('#type-' + couponType.value);
  }, 200);
});

// Initialize
if (!!couponType.value) {
  window.location.replace('#type-' + couponType.value);
  window.scrollTo(0, 0);
}

/* ======================
 *  Image selector
 * ====================== */

imageSelector({
  preview: 'couponImage',
  selector: 'couponImageSelector',
  uploadBtn: 'couponImageLabel',
  resetBtn: 'resetImage',
  defaultImage: 'defaultImage'
});
