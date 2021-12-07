#!/usr/bin/env python3
with open('data.txt') as f:
    xs = [int(x) for x in f.read().split(',')]

xs.sort()
print(sum(abs(x - xs[len(xs)//2]) for x in xs))
print(min(sum(abs((x-xs[i])*(abs(x-xs[i])+1))//2 for x in xs) for i in range(len(xs))))