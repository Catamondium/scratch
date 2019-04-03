#include <iostream>
#include <fstream>
#include <string>
#include <vector>
using namespace std;

constexpr char TXT[] = "outfile";
constexpr char BIN[] = "binfile";

constexpr char DELIM = '\t';

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
    default:
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
    return tostring(s.col) + DELIM + std::to_string(s.val);
}

// text serialization
namespace text
{
void write(string file, vector<S> v)
{
    ofstream ofile(file, ios::trunc);
    if (ofile.is_open())
    {
        for (auto &&a : v)
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

            auto pos = line.find(DELIM);
            if (pos == string::npos)
                continue;

            auto tokCol = line.substr(0, pos);
            if (tokCol == "RED")
                c = red;
            else if (tokCol == "GRN")
                c = green;
            else if (tokCol == "BLU")
                c = blue;
            else
                continue;

            auto tokVal = line.substr(pos);
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
        for (auto &&a : v)
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
        for (auto &&a : t_in)
            cout << tostring(a) << endl;
        cout << endl;
    }

    {
        vector<S> out = {{red, 20},
                         {green, 50},
                         {blue, 1000}};

        cout << "WRITE " << BIN << endl;
        bin::write(BIN, out);

        cout << "READ " << BIN << "\n------" << endl;
        vector<S> t_in = bin::read(BIN);
        for (auto &&a : t_in)
            cout << tostring(a) << endl;
    }
}