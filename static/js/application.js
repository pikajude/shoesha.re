/*global $*/

(function () {
  'use strict';
  $(document).ready(function () {
    $("#to-top").click(function (e) {
      e.preventDefault();
      $("body").animate({ scrollTop: 0 }, "slow");
    });

    $("#display-name").bind("input", function (e) {
      $(this)[(/\s/.test($(this).val()) ? "add" : "remove") + "Class"]("invalid");
    });
  });
}());
