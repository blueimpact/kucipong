"use strict";

require('date-input-polyfill');

var showByCouponType = function () {
  var selected = document.getElementById("couponType").value;
  Array.prototype.slice.call(
    document.getElementsByClassName("js-visible") || []
  ).forEach(function (elem){
    elem.classList.remove("js-visible");
    elem.classList.add("js-invisible");
  });
  var toShow = {
    "\#{couponTypeToText CouponTypeDiscount}": document.getElementById("js-couponDiscount"),
    "\#{couponTypeToText CouponTypeGift}": document.getElementById("js-couponGift"),
    "\#{couponTypeToText CouponTypeSet}": document.getElementById("js-couponSet"),
    "\#{couponTypeToText CouponTypeOther}": document.getElementById("js-couponOther")
  }[selected];
  toShow.classList.remove("js-invisible");
  toShow.classList.add("js-visible");
};
document.getElementById("couponType").addEventListener("change", showByCouponType, false);
showByCouponType();
