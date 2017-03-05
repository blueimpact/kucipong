"use strict";

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
 *  Image uploader
 * ====================== */

var preview = document.getElementById('storeImage');
var defaultUrl = preview.src;
var selector = document.getElementById('storeImageSelector');
var uploadBtn = document.getElementById('storeImageLabel');
var resetBtn = document.getElementById('resetImage');
// This field holds original image information.
var defaultImage = document.getElementById('defaultImage');
var clearSelector = function () {
  selector.value = '';
  selector.type = '';
  selector.type = 'file';
};

// On click add/delete image button.
uploadBtn.addEventListener('click', function (eve) {
  var isAddBtn = uploadBtn.getAttribute('for') === 'storeImageSelector';
  if (isAddBtn) {
    return;
  }
  uploadBtn.setAttribute('for', 'storeImageSelector');
  defaultImage.removeAttribute('name');
  clearSelector();
  eve.preventDefault();
}, false);

// On click reset image button.
resetBtn.addEventListener('click', function (eve) {
  defaultImage.name = 'defaultImage';
  preview.src = defaultUrl;
  uploadBtn.removeAttribute('for');
  clearSelector();
}, false);

// On load new image.
selector.addEventListener('change', function (eve) {
  var imgFiles = Array.prototype.slice.call(eve.target.files).filter(function (f) {
    return f.type.match('image.*');
  });
  imgFiles.slice(0, 1).forEach(function (f) {
    var reader = new FileReader();
    reader.onload = function (e) {
      preview.src = e.target.result;
      uploadBtn.removeAttribute('for');
    };
    reader.readAsDataURL(f);
  });
}, false);
