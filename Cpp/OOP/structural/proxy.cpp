#include <iostream>
#include <memory>
using namespace std;

struct Vehicle
{
    virtual void drive() = 0;
};

struct Car : Vehicle
{
    void drive() override
    {
        cout << "Driving car" << endl;
    }
};

struct Bike : Vehicle
{
    void drive() override
    {
        cout << "Driving bike" << endl;
    }
};

// Access control proxy
template <class Impl>
struct AgeProxy : Vehicle
{
    int age;
    unique_ptr<Vehicle> vehicle = make_unique<Impl>();
    AgeProxy(int age) : age(age){};

    // Transparently delegates, depending on rights
    void drive() final
    {
        if (age < 16)
            cout << "Underage!" << endl;
        else
            vehicle->drive();
    };
};

int main()
{
    Car normalcar;
    AgeProxy<Car> agecar(7);
    AgeProxy<Bike> agebike(18);

    normalcar.drive();
    agecar.drive();
    agebike.drive();
}