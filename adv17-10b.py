#!/usr/bin/env python

import sys
import string
import array

def reverse_n_at(buf, n, pos):
    l = len(buf)
    tmp = buf+buf
    s = tmp[pos:pos+n]
    a = array.array('i', tmp[pos:pos+n])
    a.reverse()
    tmp[pos:pos+n] = a.tolist()
    for i in range(n):
        tmp[(pos+i) % l] = tmp[pos+i]
    return tmp[0:l]

# This is a round of the 'knot hash'
def dump_round(rnum, inp):
    print "round {}:".format(rnum)
    for i in range(16):
        s = ''
        for j in range(16):
            s += " {}".format(inp[i*16+j])
        print s
    print ''

skip=0
pos = 0
def do_round(lengths, input):
    global skip, pos
    for l in lengths:
        input=reverse_n_at(input, l, pos)
        pos += l+skip
        pos %= len(input)
        skip += 1
    return input

def knot_hash(str):
    global skip, pos

    # Input string becomes the 'lengths' array
    lens = []
    # s = ''
    for i in range(len(str)): 
        lens.append(ord(str[i]))
        # s += " {}".format(ord(str[i]))
    lens += [ 17, 31, 73, 47, 23 ]
    # s += ' 17 31 73 47 23'
    # print 'lens: {}'.format(lens)

    # Do 64 rounds of knot-hash on the canonical input using the given input lengths
    skip = 0
    pos = 0
    inp = range(256)
    for rnum in range(64):
        # dump_round(rnum, inp)
        inp = do_round(lens, inp)

    # Convert the 256-byte hashed input to 16-byte dense hash
    dense_hash = []
    for i in range(0,256,16):
        val = inp[i]
        for j in range(i+1,i+16):
            val ^= inp[j]
        dense_hash.append(val)

    strhash = ''
    for i in range(16):
        strhash += '{:02x}'.format(dense_hash[i])

    return strhash

print knot_hash(sys.argv[1] if len(sys.argv) > 1 else sys.stdin.readline().strip())
