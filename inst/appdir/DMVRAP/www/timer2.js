var wasBusy = false;
var elapsedTimer = null;
// var startTime = null;
function updateBusy() {
    var isBusy = $('html').hasClass('shiny-busy');
    //$("#spinnerhost").css("opacity","1.0");
    if (isBusy && !wasBusy) {
	var startTime = new Date().getTime();
	elapsedTimer = setInterval(function() {
	    var millisElapsed = new Date().getTime() - startTime;
	    if (1500 < millisElapsed) {
		$('#progress').text(Math.round(millisElapsed/1000) + ' seconds have elapsed');
	    }
	}, 1000);
    }
    else if (!isBusy && wasBusy) {
	clearInterval(elapsedTimer);
	$('#progress').text("");
    }
    wasBusy = isBusy;
}

