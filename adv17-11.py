#!/usr/bin/env python

import sys
import math
from collections import deque

hex_width = 5.0

def coords_for_direction(tile,dir):
    if dir == 'n':
        return tile['x'], tile['y'] + hex_width*math.cos(math.radians(30))
    if dir == 'ne':
        x = tile['x'] + (hex_width*.75)
        y = tile['y'] + (hex_width/2)*math.cos(math.radians(30))
        return x,y
    if dir == 'se':
        x = tile['x'] + (hex_width*.75)
        y = tile['y'] - (hex_width/2)*math.cos(math.radians(30))
        return x,y
    if dir == 's':
        return tile['x'], tile['y'] - hex_width*math.cos(math.radians(30))
    if dir == 'sw':
        x = tile['x'] - (hex_width*.75)
        y = tile['y'] - (hex_width/2)*math.cos(math.radians(30))
        return x,y
    if dir == 'nw':
        x = tile['x'] - (hex_width*.75)
        y = tile['y'] + (hex_width/2)*math.cos(math.radians(30))
        return x,y
    print 'bad dir {0}'.format(dir)
    sys.exit(1)

def read_input():
    ret = []
    pos = 0
    while True:
        step = ''
        while True:
            c = sys.stdin.read(1)
            if c == '':
                break
            pos += 1
            if c in 'nsew':
                step += c
            else:
                break
        if step == '':
            break
        #print "[{0}] step: '{1}'".format(pos, step)
        ret.append(step)
        # if step == 'n':
        #     ret.append(0)
        # elif step == 'ne':
        #     ret.append(1)
        # elif step == 'se':
        #     ret.append(2)
        # elif step == 's':
        #     ret.append(3)
        # elif step == 'sw':
        #     ret.append(4)
        # elif step == 'nw':
        #     ret.append(5)
    return ret

def reverse_direction(dir):
    if dir == 'n':
        return 's'
    if dir == 'ne':
        return 'sw'
    if dir == 'se':
        return 'nw'
    if dir == 's':
        return 'n'
    if dir == 'sw':
        return 'ne'
    if dir == 'nw':
        return 'se'
    print 'bad dir {0}'.format(dir)
    sys.exit(1)
    
tile_array = []
tile_hash = {}

def dist_between(x0,y0,x1,y1):
    return math.sqrt(math.pow(x0-x1,2.0) + math.pow(y0-y1,2.0))

def dist_between_nodes(src,dst):
    return dist_between(src['x'],src['y'],dst['x'],dst['y'])

def loc_hash_str(x,y):
    return '{0:08.2f}_{1:08.2f}'.format(round(x,2),round(y,2))

close_enough = .5
def find_tile(x,y):
    h = loc_hash_str(x,y)
    if h in tile_hash:
        return tile_hash[h]
    # for t in tile_array:
    #     xdelta = math.fabs(t['x'] - x)
    #     ydelta = math.fabs(t['y'] - y)
    #     if xdelta < close_enough and ydelta < close_enough:
    #         # print 'find_tile({0},{1}) = {2}'.format(x,y,t['id'])
    #         return t
    #print 'find_tile({0},{1}) NOT FOUND'.format(x,y)
    return None

def print_tile(tile):
    n = ''
    for dir in ['n','ne','se','s','sw','nw']:
        if tile[dir]:
            n += ' {0:2s}: {1:4d}'.format(dir,tile[dir]['id'])
    if tile['id'] == 0:
        dist = 0.0
    else:
        dist = dist_between(tile_array[0]['x'],tile_array[0]['y'],tile['x'],tile['y'])
    print '{0:4}: ({1:08.2f}, {2:08.2f}) dist: {3:6.2f} {4}'.format(tile['id'], tile['x'], tile['y'], dist, n)
    
tile_id = 0
def new_tile(x,y):
    global tile_id
    tile = find_tile(x,y)
    if tile:
        return tile
    tile = {}
    tile['id'] = tile_id
    tile_id += 1
    tile['x'] = x
    tile['y'] = y
    tile['n'] = None
    tile['ne'] = None
    tile['se'] = None
    tile['s'] = None
    tile['sw'] = None
    tile['nw'] = None
    tile['seen'] = False

    # hook up any already-existing neighbors
    for dir in ['n','ne','se','s','sw','nw']:
        if tile[dir]:
            continue
        nx,ny = coords_for_direction(tile, dir)
        ntile = find_tile(nx,ny)
        if ntile:
            tile[dir] = ntile
            ntile[reverse_direction(dir)] = tile
            # print 'to the {0:2} is tile {1}'.format(dir, ntile['id'])

    tile_array.append(tile)
    tile_hash[loc_hash_str(x,y)] = tile
    # print 'NEW TILE'
    # print_tile(tile)
            
    return tile

def new_tile_in_dir(tile,dir):
    # print 'new_tile_in_dir({0},{1})'.format(tile['id'],dir)
    x,y = coords_for_direction(tile, dir)
    return new_tile(x,y)


def clear_visit_flags():
    for t in tile_array:
        t['seen'] = False
        t['dist'] = len(tile_array)+1

paths = []
def visit_tile(dst, tile, path):
    global paths
    if tile['seen']:
        return 
    tile['seen'] = True
    if tile['id'] == dst:
        print 'possible path: {0}'.format(path+dst)
        paths.append(path+dst)
    else:
        for dir in ['n','ne','se','s','sw','nw']:
            if tile[dir]:
                visit_tile(dst, tile, path+tile['id'])
    
def path_to(tile, dst):
    clear_visit_flags()
    for dir in ['n','ne','se','s','sw','nw']:
        if tile[dir]:
            visit_tile(dst, t[dir], path+tile['id'])
# ==========================================

# Add the tile we start on
tile = new_tile(0.0, 0.0)

# for dir in ['n','ne','se','s','sw','nw']:
#     x,y = coords_for_direction(tile,dir)
#     print '{0:2} {1},{2}'.format(dir, x,y)
#sys.exit(0)

child_steps = read_input()
#print 'input steps: {0}'.format(child_steps)
print 'input {0}  steps'.format(len(child_steps))

# first, walk the child's steps to populate the known tiles
for step in child_steps:
    tile = new_tile_in_dir(tile, step)
dest_tile_id = tile['id']
print 'child ended on tile {0}'.format(dest_tile_id)

# for t in tile_array:
#     print_tile(t)

# implement A*:
#  g(n) = dist_between_nodes(0, n)
#  h(n) = dist_between_nodes(n, dst)
