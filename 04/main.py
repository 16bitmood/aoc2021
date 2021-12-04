#!/usr/bin/env python3
with open('data.txt') as f:
    nums, *boards = f.read().split('\n\n')
    nums = map(int, nums.split(','))
    boards = [[[(int(x), False) for x in r.split()] for r in b.split('\n')] for b in boards]

def mark(board, num):
    for i in range(5):
        for j in range(5):
            if board[i][j][0] == num:
                board[i][j] = (num, True)

def have_won(board):
    any_row_full = any(all(board[i][j][1] for j in range(5)) for i in range(5)) 
    any_col_full = any(all(board[i][j][1] for i in range(5)) for j in range(5)) 
    if any_row_full or any_col_full:
        return sum(sum(x for x,b in row if not b) for row in board)

def part1():
    for n in nums:
        for b in boards:
            mark(b, n)
            s = have_won(b)
            if s is not None:
                return n*s

def part2():
    to_remove = []
    for n in nums:
        for i in to_remove[::-1]:
            del boards[i]
        to_remove = []

        if len(boards) == 1:
            mark(boards[0], n)
            s = have_won(boards[0])
            if s is not None:
                return n*s
            continue

        for i,b in enumerate(boards):
            mark(b, n)
            if have_won(b) is not None:
                to_remove += [i]

print(part1())
print(part2())