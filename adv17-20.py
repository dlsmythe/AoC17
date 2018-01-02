#!/usr/bin/env python

import sys
import re

particles = []
def read_particles():
    global particles
    pnum = 0
    pat = re.compile(r'p=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>, v=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>, a=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>')
    for line in sys.stdin:
        m = re.match(pat, line)
        if not m:
            print 'particle {0} is garbled'.format(pnum)
            sys.exit(1)
        p = { 'num': pnum,
              'pos': (int(m.group(1)),int(m.group(2)),int(m.group(3))),
              'vel': (int(m.group(4)),int(m.group(5)),int(m.group(6))),
              'acc': (int(m.group(7)),int(m.group(8)),int(m.group(9))) }
        pnum += 1
        particles.append(p)

def mdist(pos):
    return sum(map(abs, pos))

def time_step():
    s = []
    for p in particles:
        nacc = (p['acc'][0],p['acc'][1],p['acc'][2])
        nvel = (p['vel'][0]+p['acc'][0],p['vel'][1]+p['acc'][1],p['vel'][2]+p['acc'][2])
        npos = (p['pos'][0]+nvel[0],p['pos'][1]+nvel[1],p['pos'][2]+nvel[2])
        s.append({'num':p['num'], 'pos': npos, 'vel': nvel, 'acc': nacc})
    return s

def remove_collisions(v):
    dead_set = set()
    seen = {}
    for p in v:
        if p['pos'] in seen:
            dead_set.add(seen[p['pos']])
            dead_set.add(p['num'])
        seen[p['pos']] = p['num']
    s = []
    for p in v:
        if p['num'] in dead_set:
            continue
        s.append(p)
    return s

read_particles()
# for p in particles:
#     print '{0}: p=<{1}> v=<{2}> a=<{3}>'.format(p['num'], p['pos'], p['vel'], p['acc'])

max_acc_mdist = 0
min_acc_mdist = 999999999999999999999999
max_acc_pnum = None
min_acc_pnum = None
max_vel_pnum = None
min_vel_pnum = None
max_vel = 0
min_vel = 99999999999999999999999999
max_pos_pnum = None
min_pos_pnum = None
max_pos = 0
min_pos = 99999999999999999999999999
for p in particles:
    md = sum(map(abs, p['acc']))
    if md > max_acc_mdist:
        max_acc_mdist = md
        max_acc_pnum = p['num']
    if md < min_acc_mdist:
        min_acc_mdist = md
        min_acc_pnum = p['num']
    md = sum(map(abs, p['vel']))
    if md > max_vel:
        max_vel = md
        max_vel_pnum = p['num']
    if md < min_vel:
        min_vel = md
        min_vel_pnum = p['num']
    md = sum(map(abs, p['pos']))
    if md > max_pos:
        max_pos = md
        max_pos_pnum = p['num']
    if md < min_pos:
        min_pos = md
        min_pos_pnum = p['num']
v1 = mdist(particles[min_vel_pnum]['vel'])
min_vel2_pnum = None
min_vel2 = 99999999999999999999999999
for p in particles:
    md = mdist(p['vel'])
    if md <= v1:
        continue
    if md < min_vel2:
        min_vel2 = md
        min_vel2_pnum = p['num']
print 'fastest accelerating particle is {0} with md of {1}'.format(max_acc_pnum, max_acc_mdist)
print 'slowest accelerating particle is {0} with acc of {1} and vel of {2}'.format(min_acc_pnum, particles[min_acc_pnum]['acc'], particles[min_acc_pnum]['vel'])

v2 = mdist(particles[min_vel2_pnum]['vel'])
print 'fastest particle is {0} with vel of {1}'.format(max_vel_pnum, particles[max_vel_pnum]['vel'])
print 'slowest particle is {0} with vel of {1} (md {2})'.format(min_vel_pnum, particles[min_vel_pnum]['vel'], v1)
print 'second slowest particle is {0} with vel of {1} (md {2})'.format(min_vel2_pnum, particles[min_vel2_pnum]['vel'], v2)

print 'furthest particle is {0} with pos of {1}'.format(max_pos_pnum, particles[max_pos_pnum]['pos'])
print 'nearest  particle is {0} with pos of {1}'.format(min_pos_pnum, particles[min_pos_pnum]['pos'])

# The longest the simulation could take is if the second slowest particle (p2) were at
# the maximum distance, and the slowest (p1) was opposite the origin of it going away
# from the origin. In time 2t, p2 would have reached where p1 started from. At this point,
# p1 can be no more than 2t*v1 further away, which is (by definition) < 2t*v2.  As this
# situation progresses, in the reference frame of p1 and p2, it looks to them as if p1 is
# approaching p2 at velocity v2-v1.  Since they start at distance 2*dist(p2), then the time
# equation is t*(v2-v1) = 2*x2, or t = 2*X2 / (v2-v1)

v1 = mdist(particles[min_vel_pnum]['vel'])
v2 = mdist(particles[min_vel2_pnum]['vel'])

max_time = 2 * mdist(particles[max_pos_pnum]['pos']) / (v2-v1)
print 'max time is {0}'.format(max_time)

print 'Starting with {0} particles'.format(len(particles))
t = 0
while t < max_time:
    particles = remove_collisions(time_step())
    t += 1
print 'Finished with {0} particles remaining'.format(len(particles))
    
