#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>
using namespace std;

// Callee
struct Observer // or subscriber
{
    virtual void update() = 0;
};

// Caller
struct Subject // or publisher
{
    virtual void attach(shared_ptr<Observer>) = 0;
    virtual void detach(shared_ptr<Observer>) = 0;
    virtual void notify() = 0; // callback
};

class Button final : public Subject
{
    vector<weak_ptr<Observer>> observers;

public:
    void press()
    {
        cout << endl
             << "Button press:\t@" << this << endl;
        notify();
    }

    void attach(shared_ptr<Observer> observer) override
    {
        observers.push_back(observer);
    };

    void detach(shared_ptr<Observer> observer) override
    {
        observers.erase(
            remove_if(
                observers.begin(),
                observers.end(),
                [&](const weak_ptr<Observer> &wptr) {
                    return wptr.expired() || wptr.lock() == observer;
                }),
            observers.end());
    };

    void notify() override
    {
        for (auto wptr : observers)
        {
            if (!wptr.expired())
            {
                auto observer = wptr.lock();
                observer->update();
            }
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
    Button butt;

    vector<shared_ptr<Observer>> observers;
    for (int i = 0; i < 5; ++i)
    {
        auto shptr = make_shared<Submitter>();
        observers.push_back(shptr);
        butt.attach(shptr);
    }
    butt.press();

    {
        auto shptr = make_shared<Submitter>();
        cout << "Scoped: " << shptr.get() << endl;
        butt.attach(shptr);
        butt.press();
    }
    butt.press();
}