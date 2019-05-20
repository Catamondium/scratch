#include <iostream>
#include <memory>
#include <vector>
#include <cassert>
using namespace std;

// structure of modelled algorithm
struct Fibber
{
    virtual int calculate(int num) = 0;
};

// Interchangeable method
struct IterFibber final : public Fibber
{
    int calculate(int num) override
    {
        int x = 1;
        int acc = 0;
        for (int i = 0; i < num; ++i)
        {
            int tmp = x;
            x = acc;
            acc += tmp;
        }

        return acc;
    }
};

// Interchangeable methods
struct RecurFibber final : public Fibber
{
    int calculate(int num) override
    {
        if (num == 0)
            return 0;
        if (num == 1)
            return 1;
        else
            return calculate(num - 1) + calculate(num - 2);
    }
};

// Not relevant, but makes main easier
template <class T>
unique_ptr<Fibber> stratFactory()
{
    return make_unique<T>();
}

int main()
{
    unique_ptr<Fibber> fib;
    vector<int> iterfibs;
    vector<int> recurfibs;

    constexpr int testi = 8;

    fib = stratFactory<IterFibber>();
    for (int i = 0; i < testi; ++i)
    {
        int result = fib->calculate(i);
        iterfibs.push_back(result);
        cout << result << ", ";
    }
    cout << endl;

    fib = stratFactory<RecurFibber>();
    for (int i = 0; i < testi; ++i)
    {
        int result = fib->calculate(i);
        recurfibs.push_back(result);
        cout << result << ", ";
    }
    cout << endl;

    assert(iterfibs == recurfibs);
}