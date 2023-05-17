#include <iostream>
#include <fstream>
#include <vector>
#include <bitset>
#include <cstdint>
#include <sstream>
#include <unordered_map>
#include <cmath>
#include <string_view>

inline std::string getstr(std::stringstream &ss) { std::string tmp; ss >> tmp; return tmp; }

namespace Part1 {
void main()
{
    std::unordered_map<uint64_t, uint64_t> mem;
    std::ifstream infile("input14.txt");
    uint64_t mask, inverse;
    for (std::string line; std::getline(infile, line); ) {
        if (line[1] == 'a') {
            int i = 35;
            mask = inverse = 0;
            for (auto c : std::string_view(line).substr(7)) {
                mask    |= (uint64_t) (c == 'X' ? 0 : c - '0') << i;
                inverse |= (uint64_t) (c != 'X') << i--;
            }
            inverse = ~inverse;
        } else {
            uint64_t addr = std::stoi(std::string(line.begin() + 4, line.begin() + line.find(']')));
            std::stringstream ss(line); getstr(ss); getstr(ss); // skip '='
            uint64_t data = std::stoi(getstr(ss));
            mem[addr] = (data & inverse) | mask;
        }
    }
    uint64_t sum = 0;
    for (auto x : mem)
        sum += x.second;
    std::cout << sum << '\n';
}
}

namespace Part2 {

std::vector<uint64_t> bit_combs(std::vector<int> positions)
{
    std::vector<uint64_t> res;
    uint64_t largest = pow(2, positions.size());
    for (uint64_t i = 0; i < largest; i++) {
        uint64_t num = 0;
        uint64_t bit = i;
        for (int i = positions.size()-1; i >= 0; bit >>= 1, i--)
            num |= (bit & 1) << positions[i];
        res.push_back(num);
    }
    return res;
}

void main()
{
    std::ifstream infile("input14.txt");
    std::unordered_map<uint64_t, uint64_t> mem;
    std::vector<uint64_t> combs;
    uint64_t mask, inverse;
    for (std::string line; std::getline(infile, line); ) {
        if (line[1] == 'a') {
            int i = 35;
            std::vector<int> positions;
            mask = inverse = 0;
            for (auto c : std::string_view(line).substr(7)) {
                if (c == 'X')
                    positions.push_back(i);
                mask |= (uint64_t) (c == 'X' ? 0 : c - '0') << i;
                inverse |= (uint64_t) (c == 'X') << i;
                i--;
            }
            inverse = ~inverse;
            combs = bit_combs(positions);
        } else {
            uint64_t addr = std::stoi(std::string(line.begin() + 4, line.begin() + line.find(']')));
            std::stringstream ss(line); getstr(ss); getstr(ss); // skip '='
            uint64_t data = std::stoi(getstr(ss));
            for (auto x : combs) {
                uint64_t real_addr = ((addr | mask) & inverse) | x;
                mem[real_addr] = data;
            }
        }
    }
    uint64_t sum = 0;
    for (auto x : mem)
        sum += x.second;
    std::cout << sum << '\n';
}
}

int main()
{
    Part2::main();
}
