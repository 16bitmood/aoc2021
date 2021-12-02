#!/usr/bin/env python3
from functools import reduce

with open('data.txt') as f:
    data = [x.split() for x in f.read().split('\n') if x != '']

position = sum(int(xy[1]) for xy in data if xy[0] == 'forward')
depth = sum(int(xy[1]) if xy[0] == 'down' else -int(xy[1]) for xy in data if xy[0] != 'forward')
print(position*depth)

position, depth, aim = 0, 0, 0
for cmd, x in data:
    if cmd == 'down':
        aim += int(x)
    elif cmd == 'up':
        aim -= int(x)
    else:
        position += int(x)
        depth += aim*int(x)

print(position*depth) 