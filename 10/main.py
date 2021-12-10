#!/usr/bin/env python3
from functools import reduce
with open('data.txt') as f:
    xss = f.read().split('\n')

points1 = { ')':3, ']':57, '}':1197, '>':25137 }
points2 = { '(':1, '[':2, '{':3, '<':4 }

p1, p2 = 0, []
for xs in xss:
    stack, incomplete = [], True
    for x in xs:
        if x in '([{<':
            stack.append(x)
        elif stack[-1]+x in ['()', '[]', '{}', '<>']:
            stack.pop()
        else:
            p1 += points1[x]
            incomplete = False
            break

    if incomplete:
        ps = map(lambda x: points2[x], stack[::-1])
        p2.append(reduce(lambda a,x: 5*a + x, ps))

print(p1)
print(sorted(p2)[len(p2)//2])