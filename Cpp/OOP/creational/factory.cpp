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
    string type() override { return "Cat"; };
};

struct Dog : public Animal
{
    string type() override { return "Dog"; };
};

unique_ptr<Animal> makeAnimal()
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

    vector<shared_ptr<Animal>> animals;
    for (int i = 0; i < 5; ++i)
        animals.push_back(makeAnimal());

    for (auto animal : animals)
        cout << animal->type() << endl;
}