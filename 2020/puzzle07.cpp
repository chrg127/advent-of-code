#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <set>

inline std::string get_ss(std::stringstream &ss) { std::string tmp; ss >> tmp; return tmp; }

namespace Part1 {

using Luggage = std::unordered_map<std::string, std::vector<std::string>>;

std::vector<std::string> get_bags(std::string &s)
{
    std::vector<std::string> res;
    auto start = std::find_if(s.begin(), s.end(), [](char x) { return isdigit(x); });
    while (start != s.end()) {
        auto end = std::find_if(start+1, s.end(), [](char x) { return isdigit(x); });
        auto tmp = *(end-1) == ' ' ? end-2 : end-1;
        while (!isspace(*--tmp));
        res.emplace_back(start+2, tmp);
        start = end;
    }
    return res;
}

void find_outer(const std::string &bag, Luggage &lug, std::set<std::string> &s)
{
    for (auto &x : lug[bag]) {
        find_outer(x, lug, s);
        s.insert(x);
    }
}

std::size_t count_outer(const std::string &bag, Luggage &lug)
{
    std::set<std::string> bagset;
    find_outer(bag, lug, bagset);
    return bagset.size();
}

void main()
{
    std::ifstream infile("input7.txt");
    Luggage lug;
    for (std::string line; std::getline(infile, line); ) {
        std::stringstream ss(line);
        std::string tmp = get_ss(ss);
        std::string bag = tmp + " " + get_ss(ss);
        auto t = lug[bag];
        auto bags = get_bags(line);
        for (auto &x : bags) {
            auto &bagp = lug[x];
            bagp.push_back(bag);
        }
    }
    std::cout << count_outer("shiny gold", lug) << '\n';
}

} // namespace Part1

namespace Part2 {

struct Bag {
    int n;
    std::string name;
};
using Luggage = std::unordered_map<std::string, std::vector<Bag>>;

std::vector<Bag> get_bags(std::string &s)
{
    std::vector<Bag> res;
    auto start = std::find_if(s.begin(), s.end(), [](char x) { return isdigit(x); });
    while (start != s.end()) {
        auto end = std::find_if(start+1, s.end(), [](char x) { return isdigit(x); });
        auto tmp = *(end-1) == ' ' ? end-2 : end-1;
        while (!isspace(*--tmp));
        int num = *start - '0';
        res.push_back({ *start - '0', std::string(start+2, tmp) });
        start = end;
    }
    return res;
}

std::size_t count_bags(const std::string &bag, Luggage &lug)
{
    std::size_t sum = 0;
    for (auto &b : lug[bag]) sum += b.n * count_bags(b.name, lug);
    return sum+1;
}

void main()
{
    std::ifstream infile("input7.txt");
    Luggage lug;
    for (std::string line; std::getline(infile, line); ) {
        std::stringstream ss(line);
        auto tmp = get_ss(ss);
        auto bag = tmp + " " + get_ss(ss);
        lug[bag] = get_bags(line);
    }
    std::cout << count_bags("shiny gold", lug)-1 << '\n';
}

} // namespace Part2

int main()
{
    Part2::main();
    return 0;
}
