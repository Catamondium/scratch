#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

#define TXT "outfile"
#define BIN "binfile"

constexpr char DEL = '\t';

enum Colour
{
    red,
    green,
    blue
};

string tostring(Colour c)
{
    switch (c)
    {
    case red:
        return "RED";
    case green:
        return "GRN";
    case blue:
        return "BLU";
    }
}

// Trivially copyable (memcpy-able)
struct S
{
    Colour col;
    int val;
};

string tostring(S s)
{
    return tostring(s.col) + DEL + std::to_string(s.val);
}

// text serialization
namespace text
{
void write(string file, vector<S> v)
{
    ofstream ofile(file, ios::trunc);
    if (ofile.is_open())
    {
        for (S a : v)
            ofile << tostring(a) << "\n";
    }
}

vector<S> read(string file)
{
    vector<S> out;

    string line;
    ifstream ifile(file);
    if (ifile.is_open())
    {
        while (getline(ifile, line))
        {
            Colour c;
            int v;

            auto delim = line.find(DEL);
            if (delim == string::npos)
                continue;

            auto tokCol = line.substr(0, delim);
            if (tokCol == "RED")
                c = red;
            else if (tokCol == "GRN")
                c = green;
            else if (tokCol == "BLU")
                c = blue;
            else
                continue;

            auto tokVal = line.substr(delim);
            if (tokVal == "")
                continue;

            v = stoi(tokVal);
            out.push_back({c, v});
        }
    }

    return out;
}
} // namespace text

namespace bin
{
void write(string file, vector<S> v)
{
    ofstream ofile(file, ios::binary | ios::trunc);
    if (ofile.is_open())
    {
        for (S a : v)
            ofile.write((char *)&a, sizeof(a));
    }
}

vector<S> read(string file)
{
    vector<S> out;

    ifstream ifile(file, ios::binary);
    if (ifile.is_open())
    {
        while (true)
        {
            S s;
            ifile.read(reinterpret_cast<char *>(&s), sizeof(s));

            if (!ifile)
                break;

            out.push_back(s);
        }
    }

    return out;
}
} // namespace bin

int main()
{
    {
        vector<S> out = {{red, 2},
                         {green, 5},
                         {blue, 10000}};

        cout << "WRITE " << TXT << endl;
        text::write(TXT, out);

        cout << "READ " << TXT << "\n------" << endl;
        vector<S> t_in = text::read(TXT);
        for (S a : t_in)
            cout << tostring(a) << endl;
        cout << endl;
    }

    {
        vector<S> out = {{blue, 20},
                         {green, 500},
                         {red, 10000}};

        cout << "WRITE " << BIN
             << ", sizeof(S): " << sizeof(S) << endl;
        bin::write(BIN, out);

        cout << "READ " << BIN << "\n------" << endl;
        vector<S> t_in = bin::read(BIN);
        for (S a : t_in)
            cout << tostring(a) << endl;
    }
}