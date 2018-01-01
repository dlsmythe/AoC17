#!/usr/bin/env python

# I got this off stack-overflow so I could check whether my knot-hash was working properly.
# I had to clean it up a little...

import sys
from time import time


def reverse(text, repeat):
    knot = list(range(256))
    pos = 0
    skip = 0
    for isntevenused in range(repeat):
         for i in text:
            temp = []
            for j in range(i):
                temp.append(knot[(pos+j) % 256])
            for j in range(i):
                knot[(pos+i-1-j) % 256] = temp[j]
            pos += skip + i
            skip += 1
    return knot


def dense(knot):
    dense = [0]*16
    for i in range(16):
        dense[i] = knot[16*i]
        for m in range(1, 16):
            dense[i] ^= knot[16*i+m]
    return dense


def kh(densearg):
    knothash = ''
    for i in densearg:
        if len(hex(i)[2:]) == 2:
            knothash += hex(i)[2:]
        else:
            knothash += '0' + hex(i)[2:]
    return knothash


def knot_hash(str):
    text2 = []
    for i in range(len(str)):
        text2.append(ord(str[i]))
    text2 += [17, 31, 73, 47, 23]
    sparce = reverse(text2, 64)
    denseval = dense(sparce)
    knothash = kh(denseval)
    return knothash

inp = sys.argv[1] if len(sys.argv) > 1 else sys.stdin.readline().strip()
print knot_hash(inp)
# start = time()

# inp = '63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24'
# text = [63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24]
# text2 = []

# for i in range(len(inp)):
#     text2.append(ord(inp[i]))
# text2 += [17, 31, 73, 47, 23]

# knot = reverse(text, 1)
# sparce = reverse(text2, 64)

# dense = dense(sparce)
# knothash = kh(dense)

# print('Part One: ' + str(knot[0]*knot[1]))
# print('Part Two: ' + knothash)
# print('Completed in ' + str(time() - start) + ' seconds.')
