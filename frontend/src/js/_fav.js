"use strict";

var Fav = {};

Fav.getFavs = function () {
  var parsed = JSON.parse(localStorage.getItem('favs'));
  return Array.isArray(parsed) ? parsed : [];
};

Fav.setFav = function (id) {
  var favs = Fav.getFavs();
  var newFavs = favs.filter(function (x) {
    return x !== id;
  }).concat(id);
  localStorage.setItem('favs', JSON.stringify(newFavs));
};

Fav.unsetFav = function (id) {
  var favs = Fav.getFavs();
  var newFavs = favs.filter(function (x) {
    return x !== id;
  });
  localStorage.setItem('favs', JSON.stringify(newFavs));
};

Fav.isFaved = function (id) {
  var favs = Fav.getFavs();
  return favs.indexOf(id) >= 0;
};

Fav.view = function () {
  var elements = Array.prototype.slice.call(
    document.getElementsByClassName('js-fav') || []
  );
  elements.forEach(function (elem) {
    var id = elem.getAttribute('data-coupon-id');
    // Initializer
    if (Fav.isFaved(id)) {
      elem.checked = true;
    }
    // Event listeners
    elem.addEventListener('change', function (eve) {
      if(eve.target.checked) {
        Fav.setFav(id);
      } else {
        Fav.unsetFav(id);
      }
    }, false);
  });
}

module.exports = Fav;
