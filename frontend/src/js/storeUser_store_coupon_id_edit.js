"use strict";

require('date-input-polyfill');

var couponType = document.getElementById("couponType");
couponType.addEventListener('change', function () {
  window.setTimeout(function () {
    window.location.replace('#type-' + couponType.value);
  }, 200);
});

// Initialize
if (!!couponType.value) {
  window.location.replace('?top#type-' + couponType.value);
}
if (window.location.search === '?top') {
  window.scrollTo(0, 0);
}
