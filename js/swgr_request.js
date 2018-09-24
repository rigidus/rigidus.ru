function swgr_request (url, method, dataparam, place)
{
  // alert(method);
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

  if (method == "post") {
    databody = JSON.stringify(config);
    // alert(databody);
  } else {
    databody = config;
    // alert(JSON.stringify(config));
  }

  $.ajax({
    type: method.toUpperCase(),
    url: url,
    data: databody,
    success: function (data) {
      // alert(1);
      $(place).html(data);
    },
    error: function (data) {
      // alert(0);
      $(place).html("Error!");
    }
  });

  return false;
}
