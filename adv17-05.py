#!/usr/bin/env python

import sys

part = 2

jumpv = []
for line in sys.stdin:
    jumpv.append(int(line))

pc = 0
count = 0
while True:
    count += 1
    offset = jumpv[pc]
    if part == 1:
        jumpv[pc] += 1
    else:
        jumpv[pc] += 1 if offset < 3 else -1
    pc += offset
    if pc < 0 or pc >= len(jumpv):
        break

print count
