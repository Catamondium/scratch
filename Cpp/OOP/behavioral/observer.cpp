#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>
using namespace std;

// Callee
struct Observer
{
    virtual void update() = 0;
};

// Caller
struct Publisher // or subject
{
    virtual void attach(Observer *) = 0;
    virtual void detach(Observer *) = 0;
    virtual void notify() = 0; // callback
};

class Button final : public Publisher
{
    vector<Observer *> observers;

public:
    void press()
    {
        cout << endl
             << "Button press:\t@" << this << endl;
        notify();
    }

    void attach(Observer *o) override
    {
        observers.push_back(o);
    };

    void detach(Observer *o) override
    {
        observers.erase(
            std::remove(observers.begin(), observers.end(), o),
            observers.end());
    };

    void notify() override
    {
        for (Observer *o : observers)
        {
            o->update();
        }
    };
};

class Submitter final : public Observer
{
    int presses = 0;

public:
    void update() override
    {
        cout << "Submitter:\t@" << this << endl;
        cout << "\tPresses: " << ++presses << endl;
    };
};

int main()
{
    using uobs_ptr = unique_ptr<Observer>;
    Button butt;

    uobs_ptr sub1 = make_unique<Submitter>();
    uobs_ptr sub2 = make_unique<Submitter>();
    butt.attach(sub1.get());
    butt.attach(sub2.get());

    butt.press();
    butt.press();

    uobs_ptr sub3 = make_unique<Submitter>();
    butt.attach(sub3.get());

    butt.press();
}