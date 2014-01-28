(function($){


	$(function() {
		var offsetHeight = 100;

		$('body').scrollspy({
		   	offset: offsetHeight
		});

		$('#navbar li a').click(function (event) {
		    var scrollPos = $($(this).attr('href')).offset().top - offsetHeight;
		    $('body,html').animate({
		        scrollTop: scrollPos
		    }, 400);
		    return false;
		});

		$(".rslides").responsiveSlides({
		  auto: true,             // Boolean: Animate automatically, true or false
		  speed: 500,            // Integer: Speed of the transition, in milliseconds
		  timeout: 4000,          // Integer: Time between slide transitions, in milliseconds
		  pager: false,           // Boolean: Show pager, true or false
		  nav: false,             // Boolean: Show navigation, true or false
		  random: false,          // Boolean: Randomize the order of the slides, true or false
		  pause: false,           // Boolean: Pause on hover, true or false
		  pauseControls: true,    // Boolean: Pause when hovering controls, true or false
		  prevText: "Previous",   // String: Text for the "previous" button
		  nextText: "Next",       // String: Text for the "next" button
		  maxwidth: "",           // Integer: Max-width of the slideshow, in pixels
		  navContainer: "",       // Selector: Where controls should be appended to, default is after the 'ul'
		  manualControls: "",     // Selector: Declare custom pager navigation
		  namespace: "rslides",   // String: Change the default namespace used
		  before: function(){},   // Function: Before callback
		  after: function(){}     // Function: After callback
		});


		$("#creatOrder").submit(function() {
			submitForm($(this));
			return false;
		});

		$(document).on("click", "input, select, textarea", function() {
			if($(this).parent().hasClass('has-error')) {
				$(this).popover('show');
			}
		});

		$(document).on("keydown", "input, select, textarea", function() {
			if($(this).val().length >= 1) {
				$(this).parent().removeClass("has-error");
				$(this).popover('destroy');
			}
		});
	
	});

})(jQuery);



function submitForm(form) {
	form.find('.ajax-load').fadeIn(100);
	form.find('.alert').fadeOut(50, function() { $(this).remove(); });
	var data = form.serializeArray();
	form.find('.flash-message').fadeOut(50, function() { $(this).remove(); });
	$.post(form.attr('action'), data)
		.done(
			function(data)
			{
				data = $.parseJSON(data);
				var messages = data.messages;
				messageHandler(form, messages, data);

				if(data.csrf) 
					$("#csrf").attr('name', data.csrf.tokenKey).attr('value', data.csrf.token);

				form.find('.ajax-load').fadeOut(100);
			}
		)
		.fail(
			function()
			{
				console.log("Возникла непредвиденная ошибка при обработке Вашего запроса.");
			}
		);
}

function messageHandler(form, messages, data) {
	console.log(messages);
	$.each(messages, function(index, value) {
	    console.log(index + ' => ' + value);
	    if(value !== null) {
		    for (var i = 0; i < value.length; i++) {
				if( typeof(value[i]) == 'object' ) {
			   		var findElement = form.find("input[name='" + value[i].field + "']");
			   		if (findElement.length > 0)
			   		{
			   			if(value[i].field != 'csrf') {
							findElement.data('content', value[i].message);
							findElement.parent().addClass('has-' + index);
				   			findElement.popover({
				   				'animation': false,
				   				'title': 'Ошибка ввода',
				   				'placement' : 'right',
				   				'container' : 'body',
				   				'trigger': 'manual'
				   			});		   			
			   			}
			   			else
			   				flashMessage(form, value[i].message, index);	   			
			   		}
			   		else
						flashMessage(form, value[i].message, index);

			   	} else
			   		flashMessage(form, value, index);
			};
		}
	});
}

function flashMessage(form, msg, type) {
	if(form.children(".messages").length == 0)
		var msgWrapp = $('<div />', {class: 'messages'}).hide().prependTo(form);
	else
		var msgWrapp = form.children(".messages");

	var flash = "<div class='alert alert-" + getAlertClass(type) + "' style='display: none'>" + msg + "</div>";
	msgWrapp.append(flash);

	msgWrapp.slideDown(100, function() {
		$(this).children('.alert').fadeIn(200);
	});
}
function getAlertClass(type) {
	var pre = {
		'error': 'danger',
		'notice': 'info'
	}
	if(pre[type])
		return pre[type];
	else
		return type;
	
}

