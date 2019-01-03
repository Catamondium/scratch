#include <iostream>
#include <array>
#define SEPARATOR "\n==========="

using namespace std;

template<typename T, typename B>
void f(T s, B m) {
	cout << s << m << endl;
}

template<class T>
class Entity {
	T store;
	public:
	Entity (T val): store(val) {}
	void func(T);
	void show();
};


template<class T>
void Entity<T>::show() {
	cout << store << endl;
}

template<class T>
void Entity<T>::func(T append) {
	f(store, append);
}

template<> // Specialisation
void Entity<int>::func(int append) {
	cout << store + append << endl;
}

void section(string name) {
	cout << name << SEPARATOR << endl;
}

int main() {
	section("Deductions");
	f<float, char>(1.1, 'b'); // Type explicit, 1.1b
	f(15, 'a'); // Type deduced, 15a

	Entity<string> obj ("obj"); // Type explicit
	obj.show();
	obj.func("Appended"); // objAppended

	Entity<float> flt (5.5); // Type deduced
	flt.show();
	flt.func(50); // 5.550

	section("\nSpecialism");
	Entity i = 200; // Class type deduced, assignment declaration
	i.func(90); // 290, specialised Entity::func<int> call

	section("\nSpecial init");
	/* Array style initialisation leveraging the = declaration,
	 * can be nested for composed objects
	 */
	array<Entity<int>, 4> arr = {300, 400, 500, 600};
	for(Entity<int> &example : arr) {
		example.show();
	}
}
