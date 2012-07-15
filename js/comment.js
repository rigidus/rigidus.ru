function comment_reply(param) {
  // Показать эдитор в нужной точке,
  // его кнопка "отправить" сделает остальное.
  var editor = $('.editor');
  editor.hide();
  var msgid = $(param).parent().attr("comment-id");
  var clone = editor.clone();
  editor.remove();
  $(clone).css("margin", "5px 0 5px 20px");
  $(clone).insertAfter("div#msg"+msgid).slideDown();
  $("input[name=parent]").val(msgid);
}


function comment_send() {
  // Отправка коммента
  var parent = $("#comment-parent").val();
  var msg    = $("#comment-message").val();
  var level  = parseInt($("div[comment-id='"+parent+"']").attr("level"));
  var msgdiv = $("div[comment-id='"+parent+"']").parent();
  $.post("", {
    act : "comment-expand",
    id  : parent
  }, function(data) {
    // если (data == null) - нет комментов нижнего уровня
    if (data != null) {
      // удаляем все дочерние комменты
      for (var i = 0; i < data.length; i++) {
          $("#msg" + data[i]["id"]).remove();
      }
    }
    // добавляем комментарий
    $.post("", {
      act     : "comment-add",
      parent  : parent,
      msg     : msg
    }, function(data) {
      // Спрячем эдитор
      $('.editor').hide();
      // Скопируем образец комментария и заполним его
      var newmsg = $("#msg").clone();
      $(newmsg).attr("id", "msg" + data["id"]);
      $(newmsg).find(".comment-msg").html(data["id"] + " : " + data["msg"]);
      // Заполним блок, содержащий действия
      actdiv = $(newmsg).find("div[comment-id]");
      actdiv.attr("comment-id", data["id"]);
      actdiv.attr("level",      (level+1));
      // Установим правильные оступы и эвенты
      $(newmsg).css("margin-left", ((level+1)*10));
      $(newmsg).find(".comment-del").click(function(){
        comment_del(this);
      });
      $(newmsg).find(".comment-reply").click(function(){
        comment_reply(this);
      });
      $(newmsg).find(".comment-expand-or-collapse").click(function(){
        comment_expand_or_collapse(this);
      });
      $(newmsg).find(".comment-expand-or-collapse").html("свернуть");
      // показываем все дочерние комменты
      $.post("", {
        act : "comment-expand",
        id  : parent
      }, function(data) {
        // если (data == null) - нет комментов нижнего уровня
        if (data != null) {
          for (var i = 0; i < data.length; i++) {
            var newmsg = $("#msg").clone();
            $(newmsg).attr("id", "msg" + data[i]["id"]);
            $(newmsg).find(".comment-msg").html(data[i]["id"] + " : " + data[i]["msg"]);
            actdiv = $(newmsg).find("div[comment-id]");
            actdiv.attr("comment-id", data[i]["id"]);
            actdiv.attr("level",      data[i]["level"] + level);
            $(newmsg).css("margin-left", (level + data[i]["level"]) * 10);
            $(newmsg).find(".comment-del").click(function(){
              comment_del(this);
            });
            $(newmsg).find(".comment-reply").click(function(){
              comment_reply(this);
            });
            $(newmsg).find(".comment-expand-or-collapse").click(function(){
              comment_expand_or_collapse(this);
            });
            $(newmsg).find(".comment-expand-or-collapse").html("свернуть");
            $(newmsg).insertAfter(msgdiv).slideDown();
          }
        }
      }, "json");
    }, "json");
  }, "json");
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

function comment_expand_or_collapse (param) {
  var msgid  = $(param).parent().attr("comment-id");
  var level  = parseInt($(param).parent().attr("level"));
  var msgdiv = $(param).parent().parent();
  $.post("", {
    act : "comment-expand",
    id  : msgid
  }, function(data) {
    // если (data == null) - нет комментов нижнего уровня
    if (data != null) {
      for (var i = 0; i < data.length; i++) {
        if ($(param).html() == "свернуть") {
          $("#msg" + data[i]["id"]).remove();
        } else {
          var newmsg = $("#msg").clone();
          $(newmsg).attr("id", "msg" + data[i]["id"]);
          $(newmsg).find(".comment-msg").html(data[i]["id"] + " : " + data[i]["msg"]);

          actdiv = $(newmsg).find("div[comment-id]");
          actdiv.attr("comment-id", data[i]["id"]);
          actdiv.attr("level",      data[i]["level"] + level);

          $(newmsg).css("margin-left", (level + data[i]["level"]) * 10);

          $(newmsg).find(".comment-del").click(function(){
            comment_del(this);
          });

          $(newmsg).find(".comment-reply").click(function(){
            comment_reply(this);
          });

          $(newmsg).find(".comment-expand-or-collapse").click(function(){
            comment_expand_or_collapse(this);
          });

          $(newmsg).find(".comment-expand-or-collapse").html("свернуть");

          $(newmsg).insertAfter(msgdiv).slideDown();
        }
      }
    }
    if ($(param).html() == "развернуть") {
      $(param).html("свернуть");
    } else {
      $(param).html("развернуть");
    }
  }, "json");
}




$(function (){
  $('.comment-reply').click(function(){
    comment_reply(this);
  });
  $('.comment-del').click(function(){
    comment_del(this);
  });
  $('.comment-edit').click(function(){
    alert("TODO");
  });
  $('.comment-expand-or-collapse').click(function(){
    comment_expand_or_collapse(this);
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
