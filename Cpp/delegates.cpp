#include <iostream>
#include <functional>

/* C# style 'delegate' class replication.
 * 'delegate int bincalc(int x, int y)'
 * invokes a given objects' given method on call.
 * also capable of invoking, in order, any N (class, method)
 * added to it.
 */

class A {
	public:
		A(int z) : z(z) {};
		int operator()(int x, int y) {
			std::cout << "instance call z: " << z << std::endl;
			return z;
		};
	private:
		int z;
};

// particular case
template<class T>
class bincalc {
	public:
		using binfunctor = std::function<int(int, int)>;
		bincalc(T& target, binfunctor f) : f(f), target(target) {};
		void operator()(int x, int y) // a multi-delegate can't return
		{
			f(x, y);
		};
	private:
		binfunctor& f;
		T& target;
};

int main() {
	A obj(55);
	bincalc delegate(obj, &A::operator(int, int));
}
