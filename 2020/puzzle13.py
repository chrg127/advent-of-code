lines = [line.strip() for line in open("input13.txt")]
start = int(lines[0])
# part 1
res = min(list(map(lambda x: (x, x-int(lines[0])%x), map(lambda x: int(x), filter(lambda x: x != "x", lines[1].split(","))))), key=lambda x: x[1])
print("part 1: " + str(res[0] * res[1]))

# part 2
def fix(n, i): return 0 if i == 0 else n - i % n
l = sorted(map(lambda x: (int(x[0]), fix(int(x[0]), x[1])), filter(lambda x: x[0] != "x", [(x,i) for i,x in enumerate(lines[1].split(","))])), key=lambda x: x[0], reverse=True)
res = l[0][1]
n = 1
for i in range(0, len(l)-1):
    n *= l[i][0]
    res = next(filter(lambda x: x % l[i+1][0] == l[i+1][1], map(lambda x: res + n * x, range(0, 100000000))), -1)
print("part 2: " + str(res))
