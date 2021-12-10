#!/usr/bin/env python3
from itertools import permutations
with open('data.txt') as f:
    data = [[x.split() for x in l.split('|')] for l in f.read().split('\n')]

M = {
    0 : "abcefg", 1 : "cf", 2 : "acdeg", 3 : "acdfg", 4 : "bcdf",
    5 : "abdfg", 6 : "abdefg", 7 : "acf", 8 : "abcdefg", 9 : "abcdfg"
}
MR = {s:str(k) for k,s in M.items()}

print(sum(len(y) in map(len, [M[1],M[4],M[7],M[8]])  for _, ys in data for y in ys))

all_maps = [{a:b for a,b in zip(p, "abcdefg")} for p in permutations("abcdefg")]

def make_num(m, xs):
    return int(''.join(MR[''.join(sorted(m[a] for a in x))] for x in xs))

output = 0
for xs, ys in data:
    for m in all_maps:
        try:
            constraint = make_num(m, xs)
            output += make_num(m, ys)
            break
        except:
            continue
print(output)