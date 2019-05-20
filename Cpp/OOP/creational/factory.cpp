#include <iostream>
#include <vector>
#include <memory>
#include <cstdlib>
#include <ctime>

using namespace std;

struct Animal
{
    virtual string type() = 0;
    virtual ~Animal(){};
};

struct Cat : public Animal
{
    string type() { return "Cat"; };
};

struct Dog : public Animal
{
    string type() { return "Dog"; };
};

using uanim_ptr = unique_ptr<Animal>;
using sanim_ptr = shared_ptr<Animal>;
uanim_ptr makeAnimal()
{
    int r = std::rand();
    if (r % 2 == 0)
        return make_unique<Cat>();
    else
        return make_unique<Dog>();
}

int main()
{
    std::srand(std::time(nullptr));

    vector<sanim_ptr> animals;
    for (int i = 0; i < 5; ++i)
        animals.push_back(makeAnimal());

    for (auto animal : animals)
        cout << animal->type() << endl;
}