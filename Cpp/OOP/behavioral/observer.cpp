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
using sobs_ptr = shared_ptr<Observer>; // reference counted
using wobs_ptr = weak_ptr<Observer>;   // non-owning based on shared

// Caller
struct Subject // or publisher
{
    virtual void attach(sobs_ptr) = 0;
    virtual void detach(sobs_ptr) = 0;
    virtual void notify() = 0; // callback
};

class Button final : public Subject
{
    vector<wobs_ptr> observers;

public:
    void press()
    {
        cout << endl
             << "Button press:\t@" << this << endl;
        notify();
    }

    void attach(sobs_ptr observer) override
    {
        observers.push_back(observer);
    };

    void detach(sobs_ptr observer) override
    {
        observers.erase(
            remove_if(
                observers.begin(),
                observers.end(),
                [&](const wobs_ptr &wptr) {
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

    vector<sobs_ptr> observers;
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