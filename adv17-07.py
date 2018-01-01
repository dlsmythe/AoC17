#!/usr/bin/env python

# NB: abortive first-attempt

import sys
import string
import re

procs = {}

def fix_subprocs():
    for pk in procs.keys():
        if procs[pk]['subprocs']:
            sb = []
            for subp in procs[pk]['subprocs']:
                sb.append(procs[subp])
            procs[pk]['subprocs'] = sb

def parent(proc):
    for p in procs.keys():
        subprocs = procs[p]['subprocs']
        if subprocs and proc['name'] in map(lambda n:n['name'],subprocs):
            return p
    return None

def calc_weights(node):
    if node['subprocs']:
        for subp in node['subprocs']:
            node['totalweight'] += calc_weights(subp)
    return node['totalweight']

def fix_tower(node):
    print 'fix_tower({0})'.format(node)
    if not node['subprocs']:
        return
    n = None
    mx = 0
    mn = 99999999999999
    allsame = True
    for i in range(len(node['subprocs'])):
        if node['subprocs'][i]['totalweight'] > mx:
            mx = node['subprocs'][i]['totalweight']
            mxidx = i
        if node['subprocs'][i]['totalweight'] < mn:
            mn = node['subprocs'][i]['totalweight']
            mnidx = i
        if i > 0 and node['subprocs'][i]['totalweight'] != node['subprocs'][i-1]['totalweight']:
            allsame = False
    adjust = node['subprocs'][mxidx]['totalweight'] - node['subprocs'][mnidx]['totalweight']
    if not allsame:
        if node['subprocs'][mxidx]['subprocs']:
            fix_tower(node['subprocs'][mxidx])
        else:
            print 'adjustment is subtract {1} from {0}'.format(node['subprocs'][mxidx]['name'],adjust)
            sys.exit(0)
    
for line in sys.stdin:
    line = line.strip()
    pat = re.compile(r'(\S+) [(](\d+)[)]')
    m = re.match(pat, line, 0)
    if not m:
        print 'malformed line: {0}'.format(line)
        sys.exit(1)
    procname = m.group(1)
    weight = int(m.group(2))
    #print 'proc {0} weight {1}'.format(procname, weight)
    n = string.find(line, '->')
    if n > 0:
        subprocs = string.split(line[n+2:], ',')
        for i in range(len(subprocs)):
            subprocs[i] = string.strip(subprocs[i])
    else:
        subprocs = None
    procs[procname] = { 'name': procname, 'weight': weight, 'subprocs': subprocs, 'totalweight': weight}

fix_subprocs()
    
for p in procs.keys():
    if not parent(procs[p]):
        root = procs[p]
        print 'root is {0}'.format(p)
        # sys.exit(0)
        break

calc_weights(root)
fix_tower(root)
