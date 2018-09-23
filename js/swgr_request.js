function swgr_request (url, method, dataparam, place)
{
  var config = {};
  $(dataparam).serializeArray().map(function(item) {
    if ( config[item.name] ) {
      if ( typeof(config[item.name]) === "string" ) {
            config[item.name] = [config[item.name]];
        }
        config[item.name].push(item.value);
    } else {
        config[item.name] = item.value;
    }
  });

  // alert(JSON.stringify(config));

  $.ajax({
    type: method.toUpperCase(),
    url: url,
    data: JSON.stringify(config), //dataparam,
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

  return false;
}
