// constexpr compile-time constant signature f(x)
constexpr unsigned int factorial(unsigned int N) {
	// implementation since C++14 
	unsigned int ret = 1;
	for(unsigned int i = 1; i <= N; ++i) {
		ret *= i;
	}
	return ret;
	// C++11 implementation, return constrained
	//return (N == 0) ? 1 : N*factorial(N-1);
}

int main() {
	constexpr unsigned int magic = factorial(5); // compile-time calculated constant
	constexpr unsigned int magic2 = factorial(10); // compile-time calculated constant
	unsigned int nonmagic = factorial(2); // runtime assign
}
