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
 *  Image uploader
 * ====================== */

// On click add/delete image button.
var uploadBtn = document.getElementById('storeImageLabel');
var shouldUpdate = document.getElementById('storeImageShouldUpdate');
uploadBtn.addEventListener('click', function (eve) {
  if (uploadBtn.getAttribute('for') === 'storeImageSelector') {
    return;
  }
  uploadBtn.setAttribute('for', 'storeImageSelector');
  shouldUpdate.value = 'true';
  eve.preventDefault();
}, false);

// On load new image.
var selector = document.getElementById('storeImageSelector');
var preview = document.getElementById('storeImage');
selector.addEventListener('change', function (eve) {
  var imgFiles = Array.prototype.slice.call(eve.target.files).filter(function (f) {
    return f.type.match('image.*');
  });
  imgFiles.slice(0, 1).forEach(function (f) {
    var reader = new FileReader();
    reader.onload = function (e) {
      preview.src = e.target.result;
      uploadBtn.setAttribute('for', 'storeImage');
    };
    reader.readAsDataURL(f);
  });
}, false);
