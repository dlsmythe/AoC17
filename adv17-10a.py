#!/usr/bin/env python

import sys
import string
import array

def read_lengths():
    str = sys.stdin.readline().strip()
    i = []
    while str != '':
        pos = string.find(str, ',')
        if pos > 0:
            s = str[0:pos]
            str = str[pos+1:]
        else:
            s = str
            str = ''
        i.append(int(s))
    return i

def reverse_n_at(buf, n, pos):
#    print 'reverse_n_at({0},{1},{2})'.format(buf, n, pos)
    l = len(buf)
    tmp = buf+buf
    s = tmp[pos:pos+n]
    a = array.array('i', tmp[pos:pos+n])
    a.reverse()
    tmp[pos:pos+n] = a.tolist()
    for i in range(n):
        tmp[(pos+i) % l] = tmp[pos+i]
    return tmp[0:l]

lengths = read_lengths()
input_len = 256
input = [x for x in range(input_len)]

skip=0
pos = 0
for l in lengths:
    input=reverse_n_at(input, l, pos)
    print ' {0}'.format(input)
    pos += l+skip
    pos %= len(input)
    skip += 1

print '{0} * {1} = {2}'.format(input[0], input[1], input[0]*input[1])
