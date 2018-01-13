#!/usr/bin/env python

import sys
import re
import string

pipes = {}
pipelines = []

num_pipes = 0
def add_pipe(e1, e2):
    global num_pipes
    p = { 'id': num_pipes, 'e1': e1, 'e2': e2 }
    num_pipes += 1
    pipes[p['id']] = p

def read_input():
    pat = re.compile(r'(\d+)/(\d+)')
    for line in sys.stdin:
        m = re.match(pat, line)
        if not m:
            print 'bogus input line: {0}'.format(line.strip())
            sys.exit(1)
        add_pipe(int(m.group(1)), int(m.group(2)))

# each call starts with the unused end of each pipeline in the list
# and finishes it using the available pipes.
def make_pipelines(must_match, pipeline, avail_set):
#    print 'mp: matching {0} pipeline {1} avail: {2}'.format(must_match, pipeline, avail_set)
    found_one = False
    for pid in avail_set:
        p = pipes[pid]
        if p['e1'] == must_match or p['e2'] == must_match:
            found_one = True
            if p['e1'] == must_match:
                other_end = p['e2']
            else:
                other_end = p['e1']
            plc = list(pipeline)
            plc.append(pid)
            avail_set.remove(pid)
            make_pipelines(other_end, plc, avail_set)
            avail_set.add(pid)
    if not found_one:
        strength = 0
        plstr = ''
        must_match = 0
        for pid in pipeline:
            p = pipes[pid]
            if p['e1'] == must_match:
                left = p['e1']
                right = p['e2']
            else:
                left = p['e2']
                right = p['e1']
            must_match = right
            plstr += ' {0}:{1}<{2}>'.format(left,right,pid)
            strength += left + right
        print 'strength {1} pipeline {0}'.format(plstr, strength)
        pipelines.append({'pipes': pipeline, 'str': plstr, 'strength': strength})

def main():
    read_input()

    make_pipelines(0, [], set(range(num_pipes)))
    longest = 0
    for pl in pipelines:
        if len(pl['pipes']) > longest:
            longest = len(pl['pipes'])
    strongest = 0
    for pl in pipelines:
        if len(pl['pipes']) == longest and pl['strength'] > strongest:
            strongest = pl['strength']
    print 'longest is length {0} strength {1}'.format(longest, strongest)
    
if __name__ == '__main__':
    main()
    sys.exit(0)
