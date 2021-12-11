#!/usr/bin/env python3
with open('data.txt') as f:
    data = [list(map(int,x)) for x in f.read().split('\n')]
    h,w = len(data), len(data[0])
    data_size = h*w

def adjacent(m, i, j):
    return set((i+x, y+j) for x in [0,1,-1] for y in [0,1,-1] 
            if (x,y) != (0,0) and i+x in range(h) and j+y in range(w))

def step(m):
    flashes = 0
    to_flash = set()

    for i in range(h):
        for j in range(w):
            m[i][j] += 1
            if m[i][j] > 9:
                to_flash.add((i,j))

    already_flashed = set()

    while len(to_flash) > 0:
        to_flash_next = set()

        for i,j in to_flash - already_flashed:
            already_flashed.add((i,j))
            flashes += 1
            m[i][j] = 0

            for k,l in adjacent(m,i,j) - already_flashed:
                m[k][l] += 1
                if m[k][l] > 9:
                    to_flash_next.add((k,l))

        to_flash = to_flash_next

    for i,j in already_flashed:
        m[i][j] = 0
    
    return flashes

data_c = [[data[i][j] for j in range(h)] for i in range(w)]
print(sum(step(data_c) for _ in range(100)))

i = 1
while step(data) != data_size:
    i += 1
print(i)