#include <iostream>
#include <memory>
using namespace std;

// One we're translating to, 'adaptee'
struct External
{
    virtual void fun() = 0;
};

// Our interface, 'target'
struct Internal
{
    virtual void call() = 0;
};

struct Cextern : public External
{
    void fun() override
    {
        cout << "Cextern" << endl;
    }
};

struct Clocal : public Internal
{
    void call() override
    {
        cout << "Clocal" << endl;
    }
};

// Internal -> External adaptor
struct Adapter final : public Internal
{
    // not sure which ptr is appropriate
    shared_ptr<External> ext;
    Adapter(shared_ptr<External> ext) : ext(ext){};
    void call()
    {
        cout << "ADAPTED" << endl;
        // Delegates to adaptee, w/ possible translations
        ext->fun();
    }
};

int main()
{
    shared_ptr<External> ext = make_shared<Cextern>();
    ext->fun(); // example raw external call

    unique_ptr<Internal> local = make_unique<Clocal>();
    local->call(); // example raw local call

    unique_ptr<Internal> adaptor = make_unique<Adapter>(ext);
    // adaptor->call()->{ext->fun()}
    adaptor->call();
}