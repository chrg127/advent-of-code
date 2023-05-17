#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <sstream>
#include <initializer_list>
#include <memory>

struct Rule {
    enum class Tag { CHAR, RULES, } tag;
    union {
        char c;
        struct {
            bool has_or;
            int rules[5];
            int rules2[5];
            int size;
            int size2;
        };
    } u;
};
using RuleTab = std::unordered_map<int, Rule>;

inline std::string getstr(std::stringstream &ss) { std::string tmp; ss >> tmp; return tmp; }

bool match(const std::string &s, int &ip, RuleTab &tab, int rn)
{
    std::cout << "i=" << ip << " rule " << rn << '\n';
    Rule rule = tab[rn];
    if (rule.tag == Rule::Tag::CHAR) {
        bool res = s[ip] == rule.u.c;
        std::cout << "i=" << ip << " rule " << rn << (res ? " matched" : " failed") << '\n';
        ip++;
        return res;
    }
    int last_ip = ip;
    bool matched = false;
    for (int i = 0; i < rule.u.size; i++)
        if (matched = match(s, ip, tab, rule.u.rules[i]), !matched)
            break;
    if (!matched && rule.u.has_or) {
        ip = last_ip;
        for (int i = 0; i < rule.u.size2; i++)
            if (matched = match(s, ip, tab, rule.u.rules2[i]), !matched)
                break;
    }
    std::cout << "i=" << ip << " rule " << rn << (matched ? " matched" : " failed") << '\n';
    return matched;
}

bool match_rule11(const std::string &s, int &ip, RuleTab &tab)
{
    std::cout << "i=" << ip << " rule 11" << '\n';
    if (!match(s, ip, tab, 42)) {
        std::cout << "i=" << ip << " rule 11 failed" << '\n';
        return false;
    }
    int tmp = ip;
    if (match(s, ip, tab, 31) && ip == s.size()) {
        std::cout << "i=" << ip << " rule 11 matched" << '\n';
        return true;
    }
    ip = tmp;
    if (!match_rule11(s, ip, tab)) {
        std::cout << "i=" << ip << " rule 11 failed" << '\n';
        return false;
    }
    if (match(s, ip, tab, 31) && ip == s.size()) {
        std::cout << "i=" << ip << " rule 11 matched" << '\n';
        return true;
    }
    std::cout << "i=" << ip << " rule 11 failed" << '\n';
    return false;
}

bool match_rule8(const std::string &s, int &ip, RuleTab &tab)
{
    std::cout << "i=" << ip << " rule 8" << '\n';
    if (!match(s, ip, tab, 42)) {
        std::cout << "i=" << ip << " rule 8 failed" << '\n';
        return false;
    }
    int tmp = ip;
    if (match_rule11(s, ip, tab)) {
        std::cout << "i=" << ip << " rule 8 matched" << '\n';
        return true;
    }
    ip = tmp;
    bool res = match_rule8(s, ip, tab);
    std::cout << "i=" << ip << " rule 8" << (res ? " matched" : " failed") << '\n';
    return res;
}

std::pair<Rule, int> parse_rule(const std::string &line)
{
    Rule r;
    std::stringstream ss(line);
    int rule_num = std::stoi(getstr(ss));

    int pos;
    if (pos = line.find('"'), pos != line.npos) {
        r.tag = Rule::Tag::CHAR;
        r.u.c = line[pos+1];
        return { r, rule_num };
    }

    std::string word;
    int (*arrp)[5] = &r.u.rules;
    int *sp = &r.u.size;
    *sp = 0;
    r.tag = Rule::Tag::RULES;
    r.u.has_or = false;
    while (word = getstr(ss), word != "") {
        if (word == "|") {
            r.u.has_or = true;
            arrp = &r.u.rules2;
            sp   = &r.u.size2;
            *sp  = 0;
        } else
            (*arrp)[(*sp)++] = std::stoi(word);
    }
    return { r, rule_num };
}

int main()
{
    RuleTab tab;
    std::ifstream infile("input19.txt");
    int nmes = 0;
    for (std::string line; std::getline(infile, line); ) {
        if (line.find(':') != line.npos) {
            auto p = parse_rule(line);
            tab[p.second] = p.first;
        } else if (line != "") {
            int i = 0;
            bool matched = match_rule8(line, i, tab);
            if (matched && i == line.size())
                std::cout << "matched " << line << '\n';
            else
                std::cout << "didn't match " << line << '\n';
            nmes += (matched && i == line.size());
        }
    }
    std::cout << nmes << '\n';
}

