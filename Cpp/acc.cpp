#include <iostream>
#include <sstream>

struct acc
{
    std::string sofar;
    acc(const char *sofar) : sofar(sofar){};
    acc(std::string sofar = "") : sofar(sofar){};
};

acc operator""_acc(const char *str, std::size_t) noexcept
{
    return {str};
}

struct tmptype
{
    operator acc() { return acc(); };
};

// For chaining property, must recieve and return 'acc'
template <class T>
acc operator%(acc cur, T other)
{
    std::stringstream strm;
    strm << other;
    return cur.sofar + strm.str();
}

int main()
{
    tmptype a; // converted by operator% ? yes
    acc newacc = a % 222 % "Asss" % " " % 21.01;
    std::cout << newacc.sofar << std::endl;
}