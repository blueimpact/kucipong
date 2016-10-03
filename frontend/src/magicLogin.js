"use strict";

var request = require('superagent');

/* =============
 *    CONFIG
 * ============= */

var defaultUserType = 'store';

/* ================
 *  EVENT HANDLERS
 * ================ */

var form = document.getElementById('js-request-verification');

form.addEventListener('submit', function(ev) {
  ev.preventDefault();
  var userType = form.getAttribute('data-user-type') || defaultUserType;
  var userEmail = document.getElementById('email').value;
  var button = document.getElementById('js-request-verification-btn');
  button.setAttribute('disabled', true);

  postVerification(userType, userEmail, function(err, res){
    button.removeAttribute('disabled');
    if (err) {
      notify('エラーが発生しました。通信状態をお確かめの上、再度お試しください。');
      return;
    }
    if (res.body.result === "Email was successfully sent.") {
      notify('ログイン用のURLをメールしました。');
      return;
    }
    notify(res.body.result);
  });
  // "result": "Email was successfully sent."
}, false);

/* ==================
 *  HELPER FUNCTIONS
 * ================== */
function postVerification(userType, userEmail, callback) {
  request.post('/api/' + userType + '/v1/request-verification')
  .type('form')
  .send({ email: userEmail })
  .end(callback);
}

function notify(msg) {
  alert(msg);
}
