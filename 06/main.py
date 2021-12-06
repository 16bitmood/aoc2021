#!/usr/bin/env python3
from collections import Counter,defaultdict
with open('data.txt') as f:
    xs = [int(x) for x in f.read().split(',') if x != '0']
    xs = defaultdict(int, dict(Counter(xs)))

def next_day(xs):
    ys = defaultdict(int, {(d-1): c  for d,c in xs.items() if d != 0})
    ys[6] += xs[0]
    ys[8] += xs[0]
    return ys

[xs := next_day(xs) for _ in range(80)]
print(sum(xs.values()))

[xs := next_day(xs) for _ in range(256-80)]
print(sum(xs.values()))