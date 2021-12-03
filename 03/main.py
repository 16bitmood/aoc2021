#!/usr/bin/env python3
with open('data.txt') as f:
    xs = f.read().split('\n')
    n = len(xs[0])

most_common = lambda i,xs: '0' if sum(x[i] == '0' for x in xs) > len(xs)//2 else '1'
least_common = lambda i,xs: '0' if most_common(i,xs) == '1' else '1'

gamma = int(''.join(most_common(i, xs) for i in range(n)), 2)
epsilon = int(''.join(least_common(i, xs) for i in range(n)), 2)

print(gamma * epsilon)

i,ys = 0, xs.copy()
while len(ys) != 1:
    ys = [y for y in ys if y[i] == most_common(i, ys)]
    i += 1

j,zs = 0, xs.copy()
while len(zs) != 1:
    zs = [z for z in zs if z[j] == least_common(j, zs)]
    j += 1

print(int(ys[0],2)* int(zs[0],2))