// constexpr compile-time constant, signature f<x>::value
// https://en.wikipedia.org/wiki/Template_(C%2B%2B)#Generic_programming_features_in_other_languages
template<int N>
struct factorial {
	static const int value = N*factorial<N-1>::value;
	// alternatively static const int value = (N != 0) ? N*factorial<N-1>::value : 1;
};

template<>
struct factorial<0> {
	static const int value = 1;
};

int main() {
	constexpr int magic = factorial<5>::value; // compile-time calculated constant
	constexpr int magic2 = factorial<10>::value; // compile-time calculated constant
	int nonmagic = factorial<2>::value;
}
