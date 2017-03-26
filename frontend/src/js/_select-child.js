"use strict";

var withoutScrolling = function withoutScrolling(f) {
  var x = window.pageXOffset;
  var y = window.pageYOffset;
  f();
  window.scrollTo(x, y);
};

var targets = Array.prototype.slice.call(document.querySelectorAll('[data-select-child]') || []);
targets.forEach(function (target) {

  var onChange = function onChange() {
    withoutScrolling(function () {
      window.location.replace('#' + target.getAttribute('data-select-child') + '-' + target.value);
    });
  };

  target.addEventListener('change', function () {
    window.setTimeout(onChange, 200);
  }, false);
  if (!!target.value) {
    onChange();
  }
});
