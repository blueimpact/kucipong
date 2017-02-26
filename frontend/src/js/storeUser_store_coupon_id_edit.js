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
  window.location.replace('#type-' + couponType.value);
  window.scrollTo(0, 0);
}
