// constexpr compile-time constant signature f(x)
constexpr int factorial(int N) {
	return (N != 0) ? N*factorial(N-1) : 1;
}

int main() {
	constexpr int magic = factorial(5); // compile-time calculated constant
	constexpr int magic2 = factorial(10); // compile-time calculated constant
	int nonmagic = factorial(2); // runtime assign
}
