#!/usr/bin/env python

from __future__ import print_function
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
    for i in range(len(str)): 
        lens.append(ord(str[i]))
    lens += [ 17, 31, 73, 47, 23 ]

    # Do 64 rounds of knot-hash on the canonical input using the given input lengths
    skip = 0
    pos = 0
    inp = range(256)
    for rnum in range(64):
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

    return strhash, dense_hash

class BlockSim():

    def __init__(self, keystr):
        self.bstrs = []
        total = 0
        for i in range(128):
            h,dh = knot_hash('{0}-{1}'.format(keystr, i))
            b = bin(int(h,16))[2:]
            # print('{0}-{1} -> {2} -> {3} = {4}'.format(inp, i, h, b, b.count('1')))
            total += b.count('1')
            self.bstrs.append('{:0128b}'.format(int(h,16)))
        print('there are {0} total 1 bits'.format(total))

        self.regions = {}
        self.regiontab = []
        for row in range(128):
            self.regiontab.append(array.array('i', [0 for i in range(128)]))

        self.last_region_num = 0
        self.current_region = None
    
    def next_region(self):
        self.last_region_num += 1
        return self.last_region_num

    def add_member(self, row, col):
        # print('add_member({0},{1})'.format(row,col))
        if not self.current_region in self.regions:
            self.regions[self.current_region] = { 'members': [] }
        self.regions[self.current_region]['members'].append([row,col])
        self.regiontab[row][col] = self.current_region

    def move_members(self, fromreg=None, toreg=None):
        # print('move_members(from {0}, to {1})'.format(fromreg,toreg))
        for x,y in self.regions[fromreg]['members']:
            self.regions[toreg]['members'].append([x,y])
            self.regiontab[x][y] = toreg
        self.regions[fromreg]['members'] = []
 
    def find_regions(self):
        for row in range(128):
            # print('row {0}:'.format(row))
            self.current_region = None
            for col in range(128):
                # print('col {0}:'.format(col))
                if self.bstrs[row][col] == '0':
                    self.current_region = None
                    continue
                if not self.current_region:
                    if row == 0:
                        self.current_region = self.next_region()
                    else:
                        if self.regiontab[row-1][col] > 0:
                            self.current_region = self.regiontab[row-1][col]
                        else:
                            self.current_region = self.next_region()
                else:
                    # check if we are merging with an existing region
                    if row > 0 and self.regiontab[row-1][col] > 0 and self.regiontab[row-1][col] != self.current_region:
                        self.move_members(fromreg=self.current_region, toreg=self.regiontab[row-1][col])
                        self.current_region = self.regiontab[row-1][col]
                self.add_member(row, col)
        total = 0
        for k in self.regions.keys():
            if len(self.regions[k]['members']) > 0:
                total += 1
        print('There are {0} regions'.format(total))

def main():
    inp = sys.argv[1] if len(sys.argv) > 1 else sys.stdin.readline().strip()
    sim = BlockSim(inp)
    sim.find_regions()
    
if __name__ == '__main__':
    main()
