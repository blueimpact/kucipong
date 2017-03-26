"use strict";

var imageSelector = function imageSelector(config) {
  var preview = document.getElementById(config.preview);
  var selector = document.getElementById(config.selector);
  var uploadBtn = document.getElementById(config.uploadBtn);
  var resetBtn = document.getElementById(config.resetBtn);
  // This field holds original image information.
  var defaultImage = document.getElementById(config.defaultImage);

  var defaultUrl = preview.src;
  var clearSelector = function clearSelector() {
    selector.value = '';
    selector.type = '';
    selector.type = 'file';
  };

  // On click add/delete image button.
  uploadBtn.addEventListener('click', function (eve) {
    var isAddBtn = uploadBtn.getAttribute('for') === config.selector;
    if (isAddBtn) {
      return;
    }
    uploadBtn.setAttribute('for', config.selector);
    !!defaultImage && defaultImage.removeAttribute('name');
    clearSelector();
    eve.preventDefault();
  }, false);

  // On click reset image button.
  !!resetBtn && resetBtn.addEventListener('click', function (eve) {
    defaultImage.name = config.defaultImage;
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
};

var parents = Array.prototype.slice.call(document.getElementsByClassName('js-imageSelector') || []);
parents.forEach(function (p) {
  var config = {
    preview: (p.getElementsByClassName('js-imageSelectorPreview')[0] || {}).id,
    selector: (p.getElementsByClassName('js-imageSelectorCore')[0] || {}).id,
    uploadBtn: (p.getElementsByClassName('js-imageSelectorUploadBtn')[0] || {}).id,
    resetBtn: (p.getElementsByClassName('js-imageSelectorResetBtn')[0] || {}).id,
    defaultImage: (p.getElementsByClassName('js-imageSelectorDefaultImage')[0] || {}).id
  };
  console.log(config);
  imageSelector(config);
});
