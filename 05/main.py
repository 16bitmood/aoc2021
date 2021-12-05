#!/usr/bin/env python3
with open('data.txt') as f:
    xs = (x.split('->') for x in f.read().split('\n') if x != '')
    xs = [[list(map(int, y.split(','))) for y in x] for x in xs]
    m_x = max(max(s[0], e[0]) for s, e  in xs)
    m_y = max(max(s[1], e[1]) for s, e  in xs)

def points_in(s, e):
    x1,y1 = s
    x2,y2 = e
    t = max(abs(x1-x2), abs(y1-y2))
    k1 = 0 if x1 == x2 else (1 if x1 < x2 else -1)
    k2 = 0 if y1 == y2 else (1 if y1 < y2 else -1)
    return ((x1+ k1*i, y1 + k2*i) for i in range(t+1))

m1 = [[0 for _ in range(m_y + 1)] for _ in range(m_x + 1)]
m2 = [[0 for _ in range(m_y + 1)] for _ in range(m_x + 1)]
for s,e in xs:
    if s[0] == e[0] or s[1] == e[1]:
        for x,y in points_in(s, e):
            m1[x][y] += 1
            m2[x][y] += 1
    else:
        for x,y in points_in(s,e):
            m2[x][y] += 1

print(sum(x >= 2 for row in m1 for x in row))
print(sum(x >= 2 for row in m2 for x in row))