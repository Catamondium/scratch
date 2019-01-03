function Time(hrs, mins) {
	this.hrs = hrs;
	this.mins = mins;
}

Time.prototype.toString = function() { // Python __repr__() equivalent
	return `${this.hrs.toString().padStart(2, '0')}:${this.mins.toString().padStart(2, '0')}`;
}

Time.prototype.add = function(elapse) {
	var offset = this.hrs * 60 + this.mins;
	var tot = offset + elapse;
	return new Time(Math.floor(tot / 60), tot % 60);
}

function runfunc() {
	var elem = document.getElementById("para");
	elem.style.backgroundColor = "#FF0000";
	console.log("Ran");
}

var start = new Time(1, 30);
var end = start.add(90); 
