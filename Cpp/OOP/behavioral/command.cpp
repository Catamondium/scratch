#include <iostream>
#include <algorithm>
#include <memory>
#include <cctype>
using namespace std;

// Encapsulates an action, storable
struct Command
{
    virtual void print() = 0;
    virtual void execute() = 0;
};

class Add final : public Command
{
    int left, right;

public:
    Add(int left, int right) : left(left), right(right){};

    void execute() override
    {
        cout << left + right << endl;
    }

    void print() override
    {
        cout << left << " + " << right << endl;
    }
};

class Mul final : public Command
{
    int left, right;

public:
    Mul(int left, int right) : left(left), right(right){};
    void execute() override
    {
        cout << left * right << endl;
    }

    void print() override
    {
        cout << left << " * " << right << endl;
    }
};

// Not relevant, convenient
constexpr string_view operations = "+*";
enum struct Operation
{
    add = '+',
    mul = '*'
};

unique_ptr<Command> opFactory()
{
    int left, right;
    char c;
    Operation op;

    cout << "Operation(* or +)? ";
    cin >> c;
    if (any_of(operations.cbegin(), operations.cend(), [c](auto a) { return a == c; }))
        op = static_cast<Operation>(c);
    else
    {
        cout << "Operation " << c << " doesn't exist" << endl;
        exit(1);
    }

    cout << "Left operand? ";
    cin >> left;
    cout << "Right operand? ";
    cin >> right;

    switch (op)
    {
    case Operation::add:
        return make_unique<Add>(left, right);
    case Operation::mul:
        return make_unique<Mul>(left, right);
    default:
        cout << "Control failure" << endl;
        exit(1);
    }
}

bool yesno()
{
    char c;
    cin >> c;
    c = tolower(c);
    return (c == 'y') ? true : false;
}

int main()
{
    unique_ptr<Command> reciever = opFactory();
    reciever->print();
    cout << "Continue? ";
    if (yesno())
        reciever->execute();
}