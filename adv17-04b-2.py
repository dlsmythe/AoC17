#!/usr/bin/env python

# NB: this is the right way to do it. Inspiration courtesy M. Beede

import sys
import string

valid_phrases = 0
for line in sys.stdin:
    wv = line.strip().split()

    # check for duplicates
    if len(wv) != len(set(wv)):
        continue

    # check for permutations
    wvs = []
    for w in wv:
        wvs.append(string.join(sorted(list(w)),''))
    if len(wv) != len(set(wvs)):
        continue
        
    valid_phrases += 1

print '{0}'.format(valid_phrases)

