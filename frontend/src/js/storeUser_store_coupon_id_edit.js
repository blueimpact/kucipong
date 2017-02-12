"use strict";

require('date-input-polyfill');

var couponType = document.getElementById("couponType");
var onChangeCouponType = function () {
  window.setTimeout(function () {
    window.location.replace('#type-' + couponType.value)
  }, 200);
};
couponType.addEventListener('change', onChangeCouponType);
onChangeCouponType();
