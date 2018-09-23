function swgr_request (url, method, data, place)
{
  $.ajax({
    // dataType: "json",
    method: method,
    url: url,
    data: data,
    success: function (data) {
      // alert(place);
      // alert(data);
      $(place).html(data);
    },
    error: function (data) {
      // alert(0);
      $(place).html("Error!");
    }
  });
}
