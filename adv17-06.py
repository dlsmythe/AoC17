#!/usr/bin/env python

import sys

bankhist = {}

# This is the puzzle input. The answer to part 1 is 4074 and part 2 is 2793.
#banks = [ 11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11 ]

# This is the example input.  The answer to part 1 should be 5 and part 2 is 4.
banks = [ 0, 2, 7, 0 ]

def checkhist():
    h = '{0}'.format(banks[0])
    for n in banks[1:]:
        h += '-{0}'.format(n)
    if h in bankhist:
        print 'cycle after {0} iterations'.format(cycle_number)
        print 'cycle length is {0}'.format(cycle_number-bankhist[h])
        sys.exit(0)
    bankhist[h] = cycle_number

cycle_number = 0
while True:
    cycle_number += 1
    bmax = 0
    for i in range(len(banks)):
        n = banks[i]
        if n > bmax:
            bmax = n
            bmaxidx = i
    banks[bmaxidx] = 0
    while bmax > 0:
        bmaxidx += 1
        if bmaxidx == len(banks):
            bmaxidx = 0
        banks[bmaxidx] += 1
        bmax -= 1
    checkhist()
