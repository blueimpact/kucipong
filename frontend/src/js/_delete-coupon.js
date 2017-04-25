"use strict";

var request = require('superagent');

var targets = Array.prototype.slice.call(
  document.getElementsByClassName('js-deleteCoupon') || []
);
targets.forEach(function (target) {
  target.addEventListener('click', function(eve) {
    var key = target.getAttribute('data-coupon-id');
    if (!key) return;
    document.getElementById('alert').setAttribute('data-coupon-id', key);
    document.getElementById('alert-message').textContent = 'Are you sure to remove this coupon?';
    window.location.replace('#alert');
  }, false);
});

document.getElementById('alert-negative').addEventListener('click', function (eve) {
  var key = document.getElementById('alert').getAttribute('data-coupon-id');
  if (!key) return;
  request.post('/store/coupon/delete/' + key)
  .end(function (err, data) {
    location.replace('/store/coupon/');
  });
}, false);
