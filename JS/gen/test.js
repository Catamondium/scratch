// Generator testing
function* generator(i) {
	console.log("A");
	yield i;
	console.log("B");
	yield i + 10;
	console.log("C");
}

var gen = generator(10);

console.log(gen.next().value); // Expecting 10
console.log(gen.next().value); // Expecting 20
console.log(gen.next().value); // Expecting 30 no work
