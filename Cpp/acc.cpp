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
    acc newacc = "myacc "_acc % 222 % "Asss" % " " % 21.01;
    std::cout << newacc.sofar << std::endl;
}