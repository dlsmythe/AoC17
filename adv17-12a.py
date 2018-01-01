#!/usr/bin/env python

import sys
import string
import re
from collections import deque

progtab = {}

def read_input():
    progs = []
    pat = re.compile(r'^(\d+)\s+<->\s+(.+)')
    lineno = 0
    for line in sys.stdin:
        lineno += 1
        m = re.match(pat, line.strip())
        if not m:
            print 'bad line at {0}'.format(lineno)
            sys.exit(1)
        prog = {}
        prog['id'] = int(m.group(1))
        v = string.split(m.group(2),',')
        for i in range(len(v)):
            v[i] = int(v[i])
        prog['neighbors'] = v
        progs.append(prog)
    return progs

# ==========================================
    
class BFS:
    def __init__(self):
        self.open_set = deque([])
        self.closed_set = set()
        self.verbose = False

    def children_of(self, id):
        print 'have to override this one'
        sys.exit(1)
        
    def next_best(self):
        return self.open_set.popleft()

    def add_children_to_frontier(self, parent):
        if self.verbose:
            print 'adding children of {0}'.format(parent)
        for child in self.children_of(parent):
            if self.verbose:
                print '  processing child {0}'.format(child)
            if child in self.closed_set:
                if self.verbose:
                    print '   already processed'
                continue
            if child in self.open_set:
                if self.verbose:
                    print '   already in open_set'
            else:
                self.open_set.append(child)

    def visit_all(self, startnode, func, funcarg):
        self.open_set.append(startnode)
        while len(self.open_set) > 0:
            node = self.next_best()
            if self.verbose:
                print 'VISITING node {0}'.format(node)
            func(node, funcarg)
            # here is the test for completion
            self.add_children_to_frontier(node)
            self.closed_set.add(node)

class ProgBFS(BFS):

    def __init__(self, nodetab):
        BFS.__init__(self)
        self.nodes = nodetab

    def children_of(self, id):
        return self.nodes[id]['neighbors']
    
# ==========================================
def prtnode(n,v):
    #print 'visiting {0}'.format(n)
    v.add(n)

proglist = read_input()
print 'read {0} programs'.format(len(proglist))

#print 'progs:'
for p in proglist:
    # s = '{0}'.format(p['neighbors'][0])
    # for n in p['neighbors'][1:]:
    #     s += ', {0}'.format(n)
    # print '{0} <-> {1}'.format(p['id'],  s)
    progtab[p['id']] = p
allp = set(progtab.keys())
print 'there are {0} programs'.format(len(allp))
    
sleft = allp
numgroups = 0
while len(sleft) > 0:
    numgroups += 1
    srch = ProgBFS(progtab)
    #srch.verbose = True
    s1=set()
    goal = sleft.pop()
    sleft.add(goal)
    srch.visit_all(goal, prtnode, s1)
    print 'node {0} can reach {1} others'.format(goal, len(s1)-1)
    sleft -= s1
print 'There are {0} groups'.format(numgroups)
