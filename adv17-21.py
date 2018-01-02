#!/usr/bin/env python

import sys
import re

verbose = False

Part = 2
if Part == 1:
    num_iterations = 5
else:
    num_iterations = 18

def rotpat(p):
    if len(p) == 5:
        s = p[3]+p[0]+'/'+p[4]+p[1]
    elif len(p) == 11:
        s = p[8]+p[4]+p[0]+'/'+p[9]+p[5]+p[1]+'/'+p[10]+p[6]+p[2]
    else:
        print 'rot: can only rotate 2X2 or 3X3 patterns: {0}'.format(p)
        sys.exit(1)
    return s

def flipvertaxis(p):
    if len(p) == 5:
        s = p[1]+p[0]+'/'+p[4]+p[3]
    elif len(p) == 11:
        s = p[2]+p[1]+p[0]+'/'+p[6]+p[5]+p[4]+'/'+p[10]+p[9]+p[8]
    else:
        print 'fv: can only flip 2X2 or 3X3 patterns: {0}'.format(p)
        sys.exit(1)
    return s

def fliphorizaxis(p):
    if len(p) == 5:
        s = p[3:5]+'/'+p[0:2]
    elif len(p) == 11:
        s = p[8:11]+'/'+p[4:7]+'/'+p[0:3]
    else:
        print 'fh: can only flip 2X2 or 3X3 patterns: {0}'.format(p)
        sys.exit(1)
    return s

rules = {}
def add_rule(pat, replac):
    global rules

    rules[pat] = replac
    p = rotpat(pat)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac

    p = flipvertaxis(pat)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac

    p = fliphorizaxis(pat)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    p = rotpat(p)
    rules[p] = replac
    
def read_rules():
    nr = 0
    pat2 = re.compile(r'^(..)/(..) => (...)/(...)/(...)')
    pat3 = re.compile(r'^(...)/(...)/(...) => (....)/(....)/(....)/(....)')
    for line in sys.stdin:
        nr += 1
        l = line.strip()
        m = re.match(pat2, l)
        if m:
            add_rule(m.group(1)+'/'+m.group(2), [m.group(3), m.group(4), m.group(5), ''])
            continue
        m = re.match(pat3, l)
        if m:
            add_rule(m.group(1)+'/'+m.group(2)+'/'+m.group(3), [m.group(4), m.group(5), m.group(6), m.group(7)])
            continue
        print 'bad rule: {0}'.format(l)
        sys.exit(1)
    print 'read {0} rules'.format(nr)

def match_rule(pat):
    if not pat in rules:
        print 'no match: {0}'.format(pat)
        sys.exit(1)
    return rules[pat]

def one_iteration(g):
    if (len(g[0]) % 2) == 0:
        pat_width = 2
    elif (len(g[0]) % 3) == 0:
        pat_width = 3
    else:
        print 'grid is neither divisible by 2 or 3'
        sys.exit(1)
    ng = []
    for row in range(0,len(g[0]),pat_width):
        nr0 = ''
        nr1 = ''
        nr2 = ''
        nr3 = ''
        for col in range(0,len(g[0]),pat_width):
            if pat_width == 2:
                r0,r1,r2,r3 = match_rule(g[row][col:col+2]+'/'+g[row+1][col:col+2])
            else:
                r0,r1,r2,r3 = match_rule(g[row][col:col+3]+'/'+g[row+1][col:col+3]+'/'+g[row+2][col:col+3])
            nr0 += r0
            nr1 += r1
            nr2 += r2
            nr3 += r3
        ng.append(nr0)
        ng.append(nr1)
        ng.append(nr2)
        if pat_width == 3:
            ng.append(nr3)
    return ng

def print_grid(g):
    for row in g:
        print '  {0}'.format(row)

def main():
    read_rules()
    print 'there are {0} rules in total'.format(len(rules))

    grid = [ '.#.', '..#', '###' ]
    for i in range(num_iterations):
        if verbose:
            print '\niteration {0}:'.format(i)
            print_grid(grid)
        grid = one_iteration(grid)

    count = 0
    for r in grid:
        count += r.count('#')
    print 'there are {0} pixels still on'.format(count)
    
if __name__ == '__main__':
    main()
    sys.exit(0)
