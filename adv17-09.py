#!/usr/bin/env python

import sys
import string

verbose = False

def read_input():
    input = []
    in_garbage = False
    char_count = 0
    garbage_count = 0
    while True:
        c = sys.stdin.read(1)
        if '' == c:
            break
        char_count += 1
        if '\n' == c:
            continue
        if c == '!':
            sys.stdin.read(1)
            char_count += 1
            continue
        if in_garbage:
            if c == '>':
                in_garbage = False
            else:
                garbage_count += 1
            continue
        if c == '<':
            in_garbage = True
            continue
        input.append(c)
    print 'garbage_count: {0}'.format(garbage_count)
    return input

score = 0
groupnum = 0
pos = 0
buf = ''
def read_group(depth):
    global score, groupnum, pos, buf

    groupnum += 1
    this_group = groupnum
    if verbose:
        print "read_group[{0}]({1}) buf[{2}]='{3}' score={4}".format(this_group, depth, pos, buf[pos], score)

    subgroups = 0
    while pos < len(buf):
        if verbose:
            print "  group[{0}]({1}) buf[{2}]='{3}' score={4}".format(this_group, depth, pos, buf[pos], score)
        c = buf[pos]
        pos += 1
        if c == '{':
            read_group(depth+1)
            subgroups += 1
        elif c == ',':
            pass
        elif c == '\n':
            pass
        elif c == '}':
            if verbose:
                print 'group[{0}] has {1} subgroups'.format(this_group, subgroups)
            score += depth
            break
        else:
            print "unexpected character at pos {0}: '{1}'".format(pos-1, c)
            sys.exit(1)

    if verbose:
        print 'read_group[{0}]() score = {1}'.format(this_group, score)

buf = read_input()
#print string.join(chars,'')

read_group(0)
print 'score is {0}'.format(score)
