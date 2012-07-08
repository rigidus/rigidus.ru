$(function () {
  $('.comment-add').click(function(){
    var $editor = $('.editor');
    var clone = $editor.clone();
    $editor.remove();
    setTimeout(function(){
      $(clone).css("margin", "5px 0 5px 20px");
      $(clone).insertAfter("div#comment-add").slideDown();
      $("input[name=parent]").val(0);
    }, 200);
    return false;
  });

  $('.comment-reply').click(function(){
    var $editor = $('.editor');
    $editor.hide();
    var mid = $(this).attr("id");
    var clone = $editor.clone();
    $editor.remove();
    setTimeout(function(){
      $(clone).css("margin", "5px 0 5px 20px");
      $(clone).insertAfter("div#msg"+mid).slideDown();
      $("input[name=parent]").val(mid);
    }, 200);
  });
});
