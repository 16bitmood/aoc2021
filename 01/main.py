#!/usr/bin/env python3
with open("data.txt") as f:
    xs = [int(x) for x in f.read().split('\n') if len(x) > 0]

print("Part 1:", sum(y > x for x,y in zip(xs, xs[1:])))
print("Part 2:", sum(y > x for x,y in zip(xs, xs[3:])))