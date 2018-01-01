#!/usr/bin/env python

import sys
import string

def read_steps():
    steps = []
    for line in sys.stdin:
        sv = line.strip().split(',')
        for s in sv:
            if s[0] == 'x':
                v = s[1:].split('/')
                step = 'X'
                pos1 = int(v[0])
                pos2 = int(v[1])
                steps.append({'step': 'x', 'first': pos1, 'second': pos2})
            elif s[0] == 'p':
                v = s[1:].split('/')
                step = 'P'
                pos1 = ord(v[0])
                pos2 = ord(v[1])
                steps.append({'step': 'p', 'first': v[0], 'second': v[1]})
            elif s[0] == 's':
                step = 'S'
                pos1 = int(s[1:])
                pos2 = 0
                steps.append({'step': 's', 'count': int(s[1:])})
            else:
                print 'bogus step: {0}'.format(s)
                sys.exit(1)
            print 'add_step({0},{1},{2})'.format(step,pos1,pos2)
    return steps

steps = read_steps()
print 'there are {0} steps'.format(len(steps))

positions = list('abcdefghijklmnop')
# positions = list('abcde')
# steps = [{'step': 's', 'count': 1}, {'step': 'x', 'first': 3, 'second': 4}, {'step': 'p', 'first': 'e', 'second': 'b'}]

itercount =0
#itermax = 1000*1000*1000
itermax = 1
while itercount < itermax:
    itercount += 1
    stepcount = 0
    for s in steps:
        print '{0}: STEP_{1}'.format(stepcount, s['step'].upper())
        stepcount += 1
        if s['step'] == 's':
            pos = 0 - s['count']
            positions = positions[pos:] + positions[:pos]
        if s['step'] == 'x':
            t = positions[s['first']]
            positions[s['first']] = positions[s['second']]
            positions[s['second']] = t
        if s['step'] == 'p':
            p1 = string.join(positions, '').find(s['first'])
            p2 = string.join(positions, '').find(s['second'])
            t = positions[p1]
            positions[p1] = positions[p2]
            positions[p2] = t
    if (itercount % 1000000) == 0:
        print '{0}: {1}'.format(itercount, string.join(positions, ''))

print 'final positions: {0}'.format(string.join(positions, ''))
