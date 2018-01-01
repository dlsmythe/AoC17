#!/usr/bin/env python

import sys

valid_phrases = 0
for line in sys.stdin:
    wv = line.strip().split()
    if len(wv) != len(set(wv)):
        continue
    valid_phrases += 1

print '{0}'.format(valid_phrases)

