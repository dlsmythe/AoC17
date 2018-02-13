#!/usr/bin/env python

import sys
import re
import string
from collections import deque

proctab = {}
rootprocname = None

def parent(procname):
    global rootprocname
    for pn in proctab.keys():
        p = proctab[pn]
        if p['subprocs']:
            if procname in p['subprocs']:
                return p['name']
    rootprocname = procname
    print "Root proc is {}".format(rootprocname)
    return None

def read_procs():
    pat = re.compile(r'^(\S+)\s+[(](\d+)[)].*')
    for line in sys.stdin:
        #print 'line: {0}'.format(line.strip())
        ma = re.match(pat, line)
        if ma:
            procname = ma.group(1)
            weight = int(ma.group(2))
            sidx = string.find(line, '->')
            subs = None
            if sidx > 0:
                sss = ''
                tsubs = string.split(line[sidx+2:].strip(), ',')
                subs = []
                for s in tsubs:
                    subs.append(s.strip())
                    sss += ' {}'.format(s.strip())
                sss = '({})'.format(sss.strip())
            else:
                sss = 'NIL'
            proctab[procname] = { 'name': procname, 'weight': weight, 'subweight': 0, 'subprocs': subs }
            # print "Read proc {} w: {} sw: {} sb: {}".format(
            #     proctab[procname]['name'],
            #     proctab[procname]['weight'],
            #     proctab[procname]['subweight'], sss)

    for pn in proctab.keys():
        proctab[pn]['parentname'] = parent(pn)
        
def add_nodes_top_down(outq):
    open_set = deque([])
    closed_set = set()
    outq.append(rootprocname)
    open_set.append(rootprocname)
    proctab[rootprocname]['level'] = 0
    while len(open_set) > 0:
        parentname = open_set.popleft()
        subprocs = proctab[parentname]['subprocs']
        if subprocs:
            for child in subprocs:
                proctab[child]['level'] = proctab[parentname]['level'] + 1
                if child in closed_set:
                    continue
                if child not in open_set:
	            # print "Adding child {}".format(child)
                    open_set.append(child)
                    outq.append(child)
        closed_set.add(parentname)

def dump_procs_work(pn):
    proc = proctab[pn]
    print '{0}{1} w: {2} sw: {3} total: {4}'.format('  ' * proc['level'], pn,
                                                    proc['weight'], proc['subweight'],
                                                    proc['weight'] + proc['subweight'])
    subprocs = proc['subprocs']
    if subprocs:
        for child in subprocs:
            dump_procs_work(child)

def dump_procs():
    print '====================================='
    dump_procs_work(rootprocname)
    print '====================================='
    
def calc_weights():
    pq = deque([])
    add_nodes_top_down(pq)
    while len(pq) > 0:
        pn = pq.pop()
        # print "CALC-WEIGHTS({})".format(pn)
        if proctab[pn]['subprocs']:
            for subp in proctab[pn]['subprocs']:
                proctab[pn]['subweight'] += proctab[subp]['weight'] + proctab[subp]['subweight']

read_procs()
calc_weights()
#dump_procs()

# find unbalanced tower
print '== finding bad tower  ==================================='
prevmx = 0
prevmn = 0
basenodename = rootprocname
while basenodename:
    bnode = proctab[basenodename]
    basenodename = None
    if bnode['subprocs']:
        mn = 99999999999
        mx = 0
        allsame = True
        prev = None
        for i in range(len(bnode['subprocs'])):
            pn = bnode['subprocs'][i]
            p = proctab[pn]
            weight = p['weight'] + p['subweight']
            if weight > mx:
                mx = weight
                mxidx = i
                mxp = p
            if weight < mn:
                mn = weight
                mnidx = i
                mnp = p
            if prev and (p['weight']+p['subweight']) != (prev['weight']+prev['subweight']):
                allsame = False
            prev = p
        if not allsame:
            print 'proc {0} tower {1} ({2}) weight is high:'.format(bnode['name'],
                                                                    mxidx, bnode['subprocs'][mxidx])
            for i in range(len(bnode['subprocs'])):
                procname = bnode['subprocs'][i]
                proc = proctab[procname]
                weight = proc['weight'] + proc['subweight'] 
                print '  {0}: {1}'.format(procname, weight)
                if i == mxidx:
                    basenodename = procname
                    prevmx = mx
                    prevmn = mn
        else:
            print '{0} must be reduced by {1} to {2}'.format(bnode['name'], prevmx - prevmn,
                                                             bnode['weight']-(prevmx - prevmn))
