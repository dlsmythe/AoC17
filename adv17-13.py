#!/usr/bin/env python

from __future__ import print_function
import sys
import re

verbose = True

max_depth = 0
def read_input():
    global max_depth
    pat = re.compile(r'(\d+):\s*(\d+)')
    i = {}
    for line in sys.stdin:
        m = re.match(pat, line.strip())
        if not m:
            print('bad line')
            sys.exit(1)
        depth = int(m.group(1))
        rng = int(m.group(2))
        if depth > max_depth:
            max_depth = depth
        i[depth] = { 'range': rng, 'scanpos': 0, 'incr': 1 }
    return i

def prt_state(state,pos):
    for i in range(max_depth+1):
        if i in state:
            scanpos = state[i]['scanpos']
            r = state[i]['range']
            layer = ''
            for j in range(r):
                if j == 0 and i == pos:
                    layer += '('
                layer += 'S' if j == scanpos else '_'
                if j == 0 and i == pos:
                    layer += ')'
            print('{0:3} {1}'.format(i, layer))
        else:
            print('{0:3} {1}'.format(i, '(.)' if i == pos else '...'))

def advance_scanners(state):
    for i in range(max_depth+1):
        if i in state:
            scanpos = state[i]['scanpos'] + state[i]['incr']
            if scanpos >= state[i]['range']:
                scanpos -= 2
                state[i]['incr'] = -state[i]['incr']
            elif scanpos == -1:
                scanpos = 1
                state[i]['incr'] = -state[i]['incr']
            state[i]['scanpos'] = scanpos

def check_caught(pos, state):
    if pos in state and state[pos]['scanpos'] == 0:
        if verbose:
            print('caught at depth {0} range {1}'.format(pos, state[pos]['range']))
        return pos * state[pos]['range']
    return 0

def do_trip(s, delay):
    severity = 0
    # if delay > 0:
    #     print('start delay {0}'.format(delay))
    for i in range(delay):
        advance_scanners(s)
    # if delay > 0:
    #     print('end delay')
    for i in range(max_depth+1):
        if verbose:
            print('Step {0}:'.format(i))
            prt_state(s,i)
        severity += check_caught(i,s)
        advance_scanners(s)
    if verbose:
        print('trip severity: {0}'.format(severity))
    return severity
    
print("reading input")
inp = read_input()

rlow = 35744
rhigh = 35745
print("trying range({0},{1})".format(rlow, rhigh))
for i in range(rlow,rhigh):
    sev = do_trip(inp.copy(),i)
    if sev == 0:
        print('smallest delay: {0}'.format(i))
        sys.exit(0)
    if verbose:
        print('delay {0}: severity {1}'.format(i, sev))
    if (i%10) == 0:
        print('.', end='')
        sys.stdout.flush()
print('try a bigger delay')
