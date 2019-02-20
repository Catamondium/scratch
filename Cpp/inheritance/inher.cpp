#include <iostream>
#include <string>

using namespace std;

struct myString { // naturally public class
	string str;
	myString(string in = ""): str(in) {}
};

struct myInt {
	public:
	int num;
	myInt(int in = 0): num(in) {}
};

class Derived: public myString, public myInt {
	float flt;
	public:
	Derived(float inF = 0, int inI = 0, string inS = ""):
		// Call supers and other constructor inits
		flt(inF), myInt(inI), myString(inS) {}
	// Allow 'friend' access to private/protected members
	friend ostream& operator<<(ostream&, Derived);
};

ostream& operator<<(ostream& stream, Derived a) {
	char buf[200];
	sprintf(buf, "Derived(%f, %d, %s)", a.flt, a.num, a.str.c_str());
	stream << buf;

	return stream;
}

int main() {
	Derived obj;
	cout << obj << endl;

	Derived obj_2 = {.5, 200, "Ass"};
	cout << obj_2 << endl;
}
