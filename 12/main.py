#!/usr/bin/env python3
from collections import defaultdict
with open('data.txt') as f:
    data = [x.split('-') for x in f.read().split('\n')]
    graph = defaultdict(list)
    for x,y in data:
        graph[x].append(y)
        graph[y].append(x)

def solve(graph, permitted):
    all_paths = set()
    curr_path = []

    def get_paths(node = 'start'):
        curr_path.append(node)

        if node == "end":
            all_paths.add('-'.join(curr_path))
            curr_path.pop()
            return

        for n in graph[node]:
            if n.islower() and (curr_path.count(n) + 1 > permitted(n)):
                continue
            get_paths(n)

        curr_path.pop()

    get_paths()
    return all_paths
    
def permitted(x):
    if x.islower():
        return 1
    return float('inf')

def permit_twice(n):
    def f(x):
        if x == n:
            return 2
        elif x.islower():
            return 1
        else:
            return float('inf')
    return f

print(len(solve(graph, permitted)))

paths = set()
for n in graph.keys():
    if n.islower() and n not in ('start', 'end'):
        paths |= solve(graph, permit_twice(n))
print(len(paths))