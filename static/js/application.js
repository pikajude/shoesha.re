/*global $*/

(function () {
  'use strict';
  $(document).ready(function () {
    $("#to-top").click(function (e) {
      e.preventDefault();
      $("body").animate({ scrollTop: 0 }, "slow");
    });
  });
}());
