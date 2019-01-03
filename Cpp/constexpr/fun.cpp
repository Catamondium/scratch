// constexpr compile-time constant signature f(x)
constexpr unsigned int factorial(unsigned int N) {
	return (N != 0) ? N*factorial(N-1) : 1;
}

int main() {
	constexpr unsigned int magic = factorial(5); // compile-time calculated constant
	constexpr unsigned int magic2 = factorial(10); // compile-time calculated constant
	unsigned int nonmagic = factorial(2); // runtime assign
}
