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
    var msgid  = $(this).parent().attr("comment-id");
    var msgdiv = $(this).parent().parent();
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
  });

  $('.comment-expand').click(function(){
    var msgid = $(this).attr("id");
    $.post("", {
      act : "comment-expand",
      id  : msgid
    }, function(data) {
      alert("TODO: Rebuild comments: " + data);
    });
  });

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
