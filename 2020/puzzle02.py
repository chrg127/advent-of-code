import re
import functools

def process1(line):
    st, end, ch, passw = re.split(": | |-", line)
    cnt = len(passw)-len(list(filter(lambda x: x != ch, passw)))
    return 1 if cnt >= int(st) and cnt <= int(end) else 0
def process2(line):
    st, end, ch, passw = re.split(": | |-", line)
    return int(passw[int(st)-1] == ch) ^ int(passw[int(end)-1] == ch)
print(functools.reduce(lambda x,y: x+y, [process1(line.strip()) for line in open("input2.txt", "r")]))
