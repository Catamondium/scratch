#include <iostream>
#include <vector>
using namespace std;

struct Document // 'complex' class
{
    string title;
    string body;
    vector<string> tags;
    Document(string title, string body, vector<string> tags)
        : title(title), body(body), tags(tags){};
};

ostream &operator<<(ostream &os, Document d)
{
    os << "Title: " << d.title << '\n';
    os << "[";
    for (auto it = d.tags.cbegin(); it != d.tags.cend(); ++it)
    {
        os << *it;
        if (it != d.tags.cend() - 1)
            os << ", ";
    }
    os << "]\n";

    os << d.body;

    return os;
}

template <class T>
struct Builder // abstract builder
{
    virtual T build() = 0;
    virtual ~Builder<T>(){};
};

class DocBuilder final : public Builder<Document>
{
    string title;
    string body;
    vector<string> tags;

public:
    void settitle(string ntitle)
    {
        title = ntitle;
    };

    void addline(string line)
    {
        body += line + '\n';
    };

    void addtag(string tag)
    {
        tags.push_back(tag);
    };

    Document build() override
    {
        return Document(title, body, tags);
    };
};

int main()
{
    DocBuilder makedoc;
    makedoc.settitle("Test document");

    makedoc.addline("Test first line");
    makedoc.addline("Test second line");

    makedoc.addtag("TEST");
    makedoc.addtag("ATTEMPT");
    makedoc.addtag("DOC");

    Document doc = makedoc.build();
    cout << doc << endl;
}