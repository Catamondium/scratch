// constexpr compile-time constant, signature f<x>::value
template<int N>
struct factorial {
	static const int value = N*factorial<N-1>::value;
};

template<>
struct factorial<0> {
	static const int value = 1;
};

int main() {
	constexpr int magic = factorial<5>::value; // compile-time calculated constant
	constexpr int magic2 = factorial<10>::value; // compile-time calculated constant
}
