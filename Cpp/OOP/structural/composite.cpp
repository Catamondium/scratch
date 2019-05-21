#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <memory>
#include <cstdlib>
#include <ctime>

using namespace std;

struct Animal
{
    virtual string print() = 0;
};
using uanim_ptr = unique_ptr<Animal>;
using sanim_ptr = shared_ptr<Animal>;

// Components/leaves
struct Cat : public Animal
{
    string print() override { return "Cat"; };
};

struct Dog : public Animal
{
    string print() override { return "Dog"; };
};

// Composite
struct Herd : public Animal
{
    void group()
    {
        partition(animals.begin(), animals.end(), [](sanim_ptr elem) {
            auto herd = dynamic_pointer_cast<Herd>(elem);
            return (herd) ? false : true; // frontload leaves
        });
    };

    string pprint(string indent, bool last, bool root)
    {
        constexpr string_view joint = "└──";
        constexpr string_view terminator = "|-";
        constexpr string_view wall = "| ";
        group();

        stringstream ss;
        ss << indent;
        if (last)
        {
            if (!root)
                ss << joint;
            indent += "  ";
        }
        else
        {
            ss << joint;
            indent += wall;
        }
        ss << "Herd" << endl;

        for (int i = 0; i < animals.size(); ++i)
        {
            sanim_ptr elem = animals[i];
            auto herd = dynamic_pointer_cast<Herd>(elem);
            if (herd)
            { // branch
                ss << herd->pprint(indent, i == animals.size() - 1, false);
            }
            else
            { // leaf
                ss << indent;
                ss << terminator;
                ss << elem->print() << endl;
            }
        }

        string str = ss.str();
        if (root && str.back() == '\n')
            str.pop_back();
        return str;
    };

    // Uniform handling
    string print() override
    {
        return pprint("", true, true);
    };

    void push(sanim_ptr animal)
    {
        animals.push_back(animal);
    }

private:
    vector<sanim_ptr> animals;
};

uanim_ptr makeAnimal(int depth)
{
    int r = std::rand();
    r %= (depth < 4) ? 3 : 2;
    if (r == 0)
    {
        return make_unique<Cat>();
    }
    else if (r == 1)
    {
        return make_unique<Dog>();
    }
    else
    {
        unique_ptr<Herd> h = make_unique<Herd>();
        int num = 1 + std::rand() % 5;
        for (int i = 0; i < num; ++i)
        {
            h->push(makeAnimal(depth + 1));
        }
        return h;
    }
}

int main()
{
    std::srand(std::time(nullptr));

    vector<sanim_ptr> animals;
    for (int i = 0; i < 5; ++i)
        animals.push_back(makeAnimal(1));

    for (auto animal : animals)
        cout << animal->print() << endl;
    // Herd counts as Animal
}