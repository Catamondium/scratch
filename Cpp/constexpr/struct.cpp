// constexpr compile-time constant, signature f<x>::value
// https://en.wikipedia.org/wiki/Template_(C%2B%2B)#Generic_programming_features_in_other_languages
template<unsigned int N>
struct factorial {
	enum { value = N*factorial<N - 1>::value };
	//alternatively enum { value = (N != 0) ? N*factorial<N-1>::value : 1; }
};

template<>
struct factorial<0> {
	enum { value = 1 };
};

int main() {
	constexpr unsigned int magic = factorial<5>::value; // compile-time calculated constant
	constexpr unsigned int magic2 = factorial<10>::value; // compile-time calculated constant
	unsigned int nonmagic = factorial<2>::value;
}
