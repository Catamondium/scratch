#include <iostream>

struct counter
{
    int count = 0;

    template <class T>
    void call(T a)
    {
        count++;
    }
};

template <class... Ts>
constexpr int count(Ts... args)
{
    counter c;
    // 'args' substituted for each arg
    (c.call(args), ...);
    return c.count;
}

template <class... Ts>
void func(Ts... args)
{
    // binary left-fold
    (std::cout << ... << args);
    std::cout << std::endl;
}

int main()
{
    func("a", 1, 2, 3);
    std::cout << count(1, 2, 3, "4") << std::endl;
}