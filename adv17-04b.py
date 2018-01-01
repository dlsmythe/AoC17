#!/usr/bin/env python

from __future__ import print_function
import sys
import string

print_failures = False
print_valids = False

# returns array of permutations of w
def allperms(w):
    if len(w) == 1:
        return [w]
    retd = {}
    wv = list(w)
    for i in range(len(wv)):
        pwv = allperms(w[0:i]+w[i+1:])
        for pw in pwv: 
            retd[w[i]+pw] = 1
    return retd.keys()

valid_phrases = 0
for line in sys.stdin:
    wv = line.strip().split()
    if len(wv) == 0:
        continue

    # check for duplicates
    if len(wv) != len(set(wv)):
        continue

    # check for permutations
    reject = False
    pv = []
    for i in range(len(wv)):
        pv.append(allperms(wv[i]))
    for i in range(len(wv)):
        if reject:
            break
        for j in range(i+1,len(wv)):
            if wv[i] in pv[j]:
                reject = True
                break
    if reject:
        if print_failures:
            print(line.strip())
    else:
        valid_phrases += 1
        if print_valids:
            print(line.strip())

print('{0}'.format(valid_phrases))
