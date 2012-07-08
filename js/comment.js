function comment_send() {
  // alert($("#comment-message").val());
  $.post("", {
    act : "comment-add",
    parent  : $("#comment-parent").val(),
    msg     : $("#comment-message").val()
  }, function(data) {
    alert("***TODO***: Rebuild comments");
    $('.editor').hide();
  });
}

$(function () {
  $('.comment-new').click(function(){
    var $editor = $('.editor');
    var clone = $editor.clone();
    $editor.remove();
    setTimeout(function(){
      $(clone).css("margin", "5px 0 5px 20px");
      $(clone).insertAfter(".comment-new").slideDown();
      $("input[name=parent]").val(0);
    }, 200);
    return false;
  });

  $('.comment-reply').click(function(){
    var $editor = $('.editor');
    $editor.hide();
    var msgid = $(this).attr("id");
    var clone = $editor.clone();
    $editor.remove();
    setTimeout(function(){
      $(clone).css("margin", "5px 0 5px 20px");
      $(clone).insertAfter("div#msg"+msgid).slideDown();
      $("input[name=parent]").val(msgid);
    }, 200);
  });

  $('.comment-del').click(function(){
    var msgid    = $(this).parent().attr("comment-id");
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
    $.post("",
           {
             act : "comment-expand",
             id  : msgid
           },
           function(data) {
             alert("TODO: Rebuild comments: " + data);
           });
  });
});
