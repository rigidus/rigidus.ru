function comment_send() {
  $.post("", {
    act : "comment-add",
    parent  : $("#comment-parent").val(),
    msg     : $("#comment-message").val()
  }, function(data) {
    alert("***TODO***: Rebuild comments");
    $('.editor').hide();
  });
}

function comment_new () {
  var editor = $('.editor');
  var clone = editor.clone();
  editor.remove();
  $(clone).css("margin", "5px 0 5px 20px");
  $(clone).insertAfter("#comment-new").slideDown();
  $("input[name=parent]").val(0);
  return false;
}

function comment_del (param) {
    var msgid  = $(param).parent().attr("comment-id");
    var msgdiv = $(param).parent().parent();
    $.post("", {
      act : "comment-del",
      id  : msgid
    }, function(data) {
      if ("1" == data) {
        msgdiv.remove();
      } else {
        alert("error: deleted once");
      }
    });
}

function comment_expand (param) {
  var msgid = $(param).parent().attr("comment-id");
  var msgdiv = $(param).parent().parent();
  $.post("", {
    act : "comment-expand",
    id  : msgid
  }, function(data) {
    // alert("TODO: Rebuild comments: " + data);
    if (data != null) {
      for (var i = 0; i < data.length; i++) {
        var newmsg = $("#msg").clone();
        $(newmsg).attr("id", "msg" + data[i]["id"]);
        $(newmsg).find(".comment-msg").html(data[i]["id"] + " : " + data[i]["msg"]);
        $(newmsg).find("div[comment-id]").attr("comment-id", data[i]["id"]);
        $(newmsg).css("margin-left", data[i]["level"] * 10);
        $(newmsg).find(".comment-expand").click(function(){
          comment_expand(this);
        });
        $(newmsg).find(".comment-del").click(function(){
          comment_del(this);
        });
        $(newmsg).insertAfter(msgdiv).slideDown();
      }
    }
  }, "json");
}


$(function (){

  $('.comment-reply').click(function(){
    var editor = $('.editor');
    editor.hide();
    var msgid = $(this).parent().attr("comment-id");
    var clone = editor.clone();
    editor.remove();
    $(clone).css("margin", "5px 0 5px 20px");
    $(clone).insertAfter("div#msg"+msgid).slideDown();
    $("input[name=parent]").val(msgid);
  });

  $('.comment-del').click(function(){
    comment_del(this);
  });

  $('.comment-expand').click(function(){
    comment_expand(this)});

  $('.comment-edit').click(function(){
    var msgid = $(this).attr("id");
    $.post("", {
      act : "comment-edit",
      id  : msgid
    }, function(data) {
      alert("TODO: show comment in textarea: " + data);
    });
  });


});
