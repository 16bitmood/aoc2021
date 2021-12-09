#!/usr/bin/env python3
from collections import defaultdict
with open('data.txt') as f:
    data = [[(x,None) for x in map(int,l)] for l in f.read().split('\n')]
    h,w = len(data), len(data[0])

data_idxs = lambda : ((i,j) for i in range(h) for j in range(w))

mov = [(0,1),(1,0),(0,-1),(-1,0)]
mov_idxs  = lambda i,j: ((i+p,j+q) for p,q in mov if i+p in range(h) and j+q in range(w))

# Part 1
is_lowest = lambda i,j: all(data[i][j][0] < data[a][b][0] for a,b in mov_idxs(i,j))
print(sum(data[i][j][0] + 1 for i,j in data_idxs() if is_lowest(i,j)))

# Part 2
def bfs_at(i,j,m):
    data[i][j] = (data[i][j][0], m)
    for a,b in mov_idxs(i, j):
        x, y = data[i][j], data[a][b]
        if x[0] < y[0] and y[1] is None and y[0] != 9:
            bfs_at(a, b, m)

num_sinks = 0
for i,j in data_idxs():
    if is_lowest(i,j) and data[i][j][1] is None:
        bfs_at(i,j, num_sinks)
        num_sinks += 1

xs = defaultdict(int)
for i,j in data_idxs():
    if data[i][j][1] is not None:
        xs[data[i][j][1]] += 1

a,b,c = sorted(xs.values())[-3:]
print(a*b*c)