import re
import functools
import sys

lines = [line.strip() for line in open("input16.txt")]
head_lines = list()
fields = [s for s in filter(lambda x: x != "" and x != "your ticket:" and x != "nearby tickets:" and ":" in x, lines)]
field_nums = [[int(x) for x in (filter(lambda x: x != '', re.split("[^0-9]", s)))] for s in fields]
field_names = [f.split(":")[0] for f in fields]
numlists = list([[int(x) for x in s.split(',')] for s in filter(lambda x: x != "" and x[0].isdigit(), lines)])
mynums = numlists[0]
numlists.pop(0)

def valid_field(x, f): return x >= f[0] and x <= f[1] or x >= f[2] and x <= f[3]

def valid_forall(x, fields):
    for f in fields:
        if valid_field(x, f): return True
    return False

# check if all nums are valid for a field
def valid_col_field(lists, index, field):
    for l in lists:
        if not valid_field(l[index], field): return False
    return True

def check(lists, index, fields):
    lf = list(filter(lambda f: valid_col_field(lists, index, f[0]), [(x,i) for i,x in enumerate(fields)]))
    res = (len(lf), [])
    for f in lf:
        res[1].append(f[1])
    return res

def part1():
    errors = []
    for nums in numlists:
        for n in nums:
            if not valid_forall(n, field_nums):
                errors.append(n)
                break
    print(functools.reduce(lambda x,y: x+y, errors))

def part2():
    valid_lists = list(filter(lambda x: len(list(filter(lambda n: not valid_forall(n, field_nums), x))) == 0, numlists))
    tab = {}
    visited = []
    visited_i = []
    for i in range(0, len(valid_lists[0])):
        tmp = check(valid_lists, i, field_nums)
        tab[tmp[0]] = (i, tmp[1])
    for i in range(1, len(tab)+1):
        tmp = tab[i]
        field_index = list(set(tmp[1]) - set(visited))[0]
        visited.append(field_index)
        visited_i.append(tmp[0])
    res = 1
    for i in range(0, len(visited)):
        if "departure" not in field_names[visited[i]]: continue
        res *= mynums[visited_i[i]]
    print(res)

part2()
