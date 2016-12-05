"use strict";

var resetSubcategories = function (key) {
  Array.prototype.slice.call(document.querySelectorAll('[data-subcategory-of]')).forEach(function (x) {
    return x.style.display = 'none';
  });
};

var setSubcategories = function (key) {
  resetSubcategories();
  document.querySelector('[data-subcategory-of="' + key + '"]').style.display = 'flex';
};

var businessCategory = document.getElementById('storeBusinessCategory');

// Hide all subcategories at first.
setSubcategories(businessCategory.value);

businessCategory.addEventListener('change', function (eve) {
  setSubcategories(eve.target.value);
});
