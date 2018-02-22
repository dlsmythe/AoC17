#!/usr/bin/env python

import sys
import math
from collections import deque
import pygame

Part = 1

BLACK = (  0,   0,   0)
WHITE = (255, 255, 255)
BLUE =  (  0,   0, 255)
GREEN = (  0, 255,   0)
RED =   (255,   0,   0)

fg_color = RED
bg_color = BLACK

SCR_H = 8000
SCR_W = 6000

# pygame.display.list_modes()
# [(2560, 1600), (2048, 1152), (1920, 1440), (1920, 1200), (1920, 1080), (1856, 1392),
#  (1792, 1344), (1736, 934), (1680, 1050), (1600, 1200), (1600, 900), (1440, 900),
#  (1400, 1050), (1366, 768), (1360, 768), (1280, 1024), (1280, 960), (1280, 800),
#  (1280, 768), (1280, 720), (1024, 768), (848, 480), (800, 600), (640, 480)]
size = [1280, 1024]

hex_width = 40.0

# these are in game-coords, not screen-coords
scr_base_x = 0
scr_base_y = 0
scr_magnify = 1.0

def cvt_to_scr(coord):
    (x,y) = coord
    x = int(x * scr_magnify)
    y = int(y * scr_magnify)
    x -= scr_base_x
    y -= scr_base_y
    x += size[0]/2
    y = size[1]/2 - y
    return [x,y]

cosd = (hex_width/2)*math.cos(math.radians(30))
sind = (hex_width/2)*math.sin(math.radians(30))

TILE_HEIGHT = 2*cosd
TILE_WIDTH = hex_width

once=True
def tile_coords(cx, cy):
    global cosd, sind, once

    c_nw = [cx - sind, cy + cosd]
    c_ne = [cx + sind, cy + cosd]
    c_e  = [cx + (hex_width/2), cy]
    c_se = [cx + sind, cy - cosd]
    c_sw = [cx - sind, cy - cosd]
    c_w  = [cx - (hex_width/2), cy]

    if not once:
        once = True
        print 'hex: {0} -> {1}'.format([ c_nw,c_ne,c_e,c_se,c_sw,c_w ],
                                       [ cvt_to_scr(c_nw), cvt_to_scr(c_ne), cvt_to_scr(c_e),
                                         cvt_to_scr(c_se), cvt_to_scr(c_sw), cvt_to_scr(c_w) ])
    return [ cvt_to_scr(c_nw), cvt_to_scr(c_ne), cvt_to_scr(c_e), cvt_to_scr(c_se), cvt_to_scr(c_sw), cvt_to_scr(c_w) ]

def draw_tile(t):
    global font, scr_magnify
    
    coords = tile_coords(t['x'], t['y'])
    pygame.draw.polygon(screen, t['color'], coords, 2 if scr_magnify > .3 else 0)
  
    if scr_magnify > .3:
        font.set_bold(True)
        label = font.render("{0}".format(t['id']), True, fg_color, bg_color)
        rect = label.get_rect()
        rect.center = cvt_to_scr([t['x'], t['y']])
        screen.blit(label, rect)  

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
        if not step in ['n','s','ne','nw','se','sw']:
            print "[{0}] bad step: '{1}'".format(pos, step)
            sys.exit(1)
        #print "[{0}] step: '{1}'".format(pos, step)
        ret.append(step)
        pos += 1

    # Print back out what we read so we can check whether we read it correctly.
    if False:
        with open('regnerated_input_path.txt', 'w') as f:
            sep = ''
            for s in ret:
                f.write('{0}{1}'.format(sep,s))
                sep = ','
            f.write('\n')

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

def dist_between_nodes_byindex(src,dst):
    return dist_between_nodes(tile_array[src], tile_array[dst])

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
            n += ' {0:2s}: {1:4d}'.format(dir,tile[dir])
    if tile['id'] == 0:
        dist = 0.0
    else:
        dist = dist_between(tile_array[0]['x'],tile_array[0]['y'],tile['x'],tile['y'])
    print '{0:4}: ({1:08.2f}, {2:08.2f}) dist: {3:6.2f} {4}'.format(tile['id'], tile['x'], tile['y'], dist, n)
    
tile_id = 0
def tile_at(x,y):
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
    tile['color'] = BLUE

    # hook up any already-existing neighbors
    for dir in ['n','ne','se','s','sw','nw']:
        if tile[dir]:
            continue
        nx,ny = coords_for_direction(tile, dir)
        ntile = find_tile(nx,ny)
        if ntile:
            tile[dir] = ntile['id']
            ntile[reverse_direction(dir)] = tile['id']
            # print 'to the {0:2} is tile {1}'.format(dir, ntile['id'])

    tile_array.append(tile)
    tile_hash[loc_hash_str(x,y)] = tile
    # print 'NEW TILE'
    # print_tile(tile)
            
    return tile

def tile_in_dir(tile,dir):
    # print 'tile_in_dir({0},{1})'.format(tile['id'],dir)
    x,y = coords_for_direction(tile, dir)
    return tile_at(x,y)
        
# ==========================================

def children_of(tileid):
    ret = []
    for dir in ['n','ne','se','s','sw','nw']:
        if tile_array[tileid][dir]:
            ret.append(tile_array[tileid][dir])
    return ret
    
class BFS:
    def __init__(self, verbose=False):
        self.open_set = deque([])
        self.closed_set = set()
        self.verbose = verbose
        self.max_path_len = 0
        self.max_path_tileid = 0
        self.num_nodes_visited = 0
        
    def next_best(self):
        return self.open_set.popleft()

    def add_children_to_frontier(self, parent):
        for child in children_of(parent):
            # if self.verbose:
            #     print 'processing child {0}'.format(child)
            if child in self.closed_set:
                # if self.verbose:
                #     print ' already processed'
                continue
            if child not in self.open_set:
                # child.level = parent.level + 1
                # set child's path to current path + parent
                tile_array[child]['path'] = tile_array[parent]['path']+[parent]
                # if self.verbose:
                #     print ' adding child {0} to open_set with path {1}'.format(child, tile_array[child]['path'])
                self.open_set.append(child)
                sfo = steps_from_origin(child)
                if sfo > self.max_path_len:
                    self.max_path_len = sfo
                    self.max_path_tileid = child

    def search(self, goalnode):
        self.goal_node = goalnode
        self.open_set.append(0)
        tile_array[0]['path'] = []
        while len(self.open_set) > 0:
            node = self.next_best()
            self.num_nodes_visited += 1
            if self.verbose:
                print 'VISITING node {0}'.format(node)
            # here is the test for completion
            if node == self.goal_node and not Part == 2:
                return tile_array[node]['path'] + [node]
            self.add_children_to_frontier(node)
            self.closed_set.add(node)
        return None

class TileBFS(BFS):

    def __init__(self, verbose=False):
        BFS.__init__(self, verbose)

    def next_best(self):
        min_node = None
        min_val = len(tile_array)+1
        if self.verbose and len(self.open_set) > 1:
            print 'open_set {0}'.format(self.open_set)
        for node in self.open_set:
            len_to_here = len(tile_array[node]['path'])
            if self.verbose and len(self.open_set) > 1:
                print 'frontier node {0} len_to_here: {1} min_val: {2} min_node: {3}'.format(node, len_to_here, min_val, min_node)
            if len_to_here < min_val:
                    min_val = len_to_here
                    min_node = node
        if self.verbose and len(self.open_set) > 1:
            print 'selecting {0} as best frontier node with len {1}'.format(min_node, min_val)
        self.open_set.remove(min_node)
        return min_node
    
# ==========================================
    
KEY_UP = 273
KEY_DOWN = 274
KEY_RIGHT = 275
KEY_LEFT = 276
KEY_KPUP = 264
KEY_KPRIGHT = 262
KEY_KPDOWN = 258
KEY_KPLEFT = 260
KEY_KPCENTER = 261
KEY_KPMINUS = 269
KEY_KPPLUS = 270
KEY_Q = 113

def pan(keycode):
    global scr_base_x, scr_base_y, scr_magnify
    pan_amount = (size[0]/4) + min(size[0]/4,(size[0]/16) * (1. / math.sqrt(scr_magnify)))
    if keycode == KEY_RIGHT or keycode == KEY_KPRIGHT:
        print 'KEY_RIGHT'
        scr_base_x += pan_amount
    elif keycode == KEY_LEFT or keycode == KEY_KPLEFT:
        print 'KEY_LEFT'
        scr_base_x -= pan_amount
    elif keycode == KEY_UP or keycode == KEY_KPUP:
        print 'KEY_UP'
        scr_base_y += pan_amount
    elif keycode == KEY_DOWN or keycode == KEY_KPDOWN:
        print 'KEY_DOWN'
        scr_base_y -= pan_amount
    elif keycode == KEY_KPCENTER:
        scr_base_x = 0
        scr_base_y = 0
        scr_magnify = 1.0
    elif keycode == KEY_KPPLUS:
        scr_magnify *= 1.1
    elif keycode == KEY_KPMINUS:
        if scr_magnify > .01:
            scr_magnify *= .9

# =================================================================

origin_dist = {}
# Start at some tile, and work one tile-location at a time towards tile 0.
# Keep note of tile locations calculated so far.
# When you get to tile 0 or a previously hashed value, return up the list
# of locations back towards the destination and add them all to the hash.
# Return the final result.
def steps_from_origin(tileid):
#    print 'steps_from_origin({0})'.format(tileid)
    if 0 == tileid:
        return 0
    dest_x = tile_array[tileid]['x']
    dest_y = tile_array[tileid]['y']
    deltas = {}
    t0 = tile_array[0]
    for dir in ['n','ne','se','s','sw','nw']:
        deltas[dir] = coords_for_direction(t0, dir)
    steps = []
    cur_x = dest_x
    cur_y = dest_y
    found_hashed = None
    ns = 0
    while math.fabs(cur_x) > close_enough or math.fabs(cur_y) > close_enough:
        # See if we knew the rest of the distance from here
        h = loc_hash_str(cur_x, cur_y)
#        print 'checking location {0}'.format(h)
        if h in origin_dist:
#            print 'HIT! dist is {0} steps'.format(origin_dist[h])
            ns = origin_dist[h]
            break
        steps.append(h)
        
        # for the tile-center in each direction, find which of thos positions is
        # closest to the destination
        neighbor = {}
        mindist = 9999999999
        minpos = None
        mindir = None
        for dir in ['n','ne','se','s','sw','nw']:
            npos = deltas[dir]
            nx = cur_x+npos[0]
            ny = cur_y+npos[1]
            d = dist_between(0.0, 0.0, nx, ny)
            # print '({0},{1}) {2} dist {3}'.format(nx, ny, dir, d)
            if d < mindist:
                mindist = d
                mindir = dir
                minpos_x = nx
                minpos_y = ny
        cur_x = minpos_x
        cur_y = minpos_y
        # print 'went {0} to ({1},{2})'.format(mindir, minpos_x, minpos_y)

    while len(steps) > 0:
        ns += 1
        h = steps.pop()
#        print 'remembering steps from {0} is {1}'.format(h, ns)
        origin_dist[h] = ns
#    print 'steps from tileid {0} at ({1},{2}) is {3}'.format(tileid, dest_x, dest_y, ns)
    return ns
    
# =================================================================

pygame.init()
screen = pygame.display.set_mode(size)

pygame.display.set_caption("hex tile display for AoC2017-11")

font = pygame.font.Font(None, int(hex_width * .4))

# ==========================================

child_steps = read_input()
print 'The child took {0}  steps'.format(len(child_steps))

# First, add the tile we start on.
tile = tile_at(0.0, 0.0)

# Then, walk the child's steps to populate the known tiles.
for step in child_steps:
    tile = tile_in_dir(tile, step)
dest_tile_id = tile['id']
print 'child ended on tile {0} at ({1},{2}) (first tile is labeled 0)'.format(dest_tile_id, tile['x'], tile['y'])
print 'There are {0} unique tiles.'.format(len(tile_array))
print 'child ended {0} steps from the origin'.format(steps_from_origin(dest_tile_id))

# Find the shortest path on tiles the child already stepped on
srch = TileBFS(verbose=False)
path = srch.search(dest_tile_id)
print 'visited {0}/{1} nodes'.format(srch.num_nodes_visited, len(tile_array))
print 'Longest distance from the start was {0} at node {1}'.format(srch.max_path_len, srch.max_path_tileid)
if path:
    print 'shortest path length: {0} (includes the end-points)'.format(len(path))

# save the shortest path
with open('output_path.txt', 'w') as f:
    f.write(str(path))

# Color the tiles chosen for the shortest path in GREEN, others in BLUE
for id in path:
    t = tile_array[id]
    if t:
        t['color'] = GREEN
    else:
        print 'tile {0} on path but not in tile_array?'.format(id)

# Loop until the user clicks the close button or types 'q'.
done = False
clock = pygame.time.Clock()
 
while not done:
 
    # This limits the while loop to a max of 10 times per second.
    # Leave this out and we will use all CPU we can.
    clock.tick(10)
     
    for event in pygame.event.get(): # User did something
        if event.type == pygame.QUIT: # If user clicked close
            done=True # Flag that we are done so we exit this loop

        # key.__dict__: {'scancode': 113, 'key': 276, 'mod': 0}
        elif event.type == pygame.KEYUP:
            # print 'key {0}'.format(event.__dict__)
            if event.__dict__['key'] == KEY_Q:
                done = True
            else:
                pan(event.__dict__['key'])

    # Clear the screen and set the screen background
    screen.fill(BLACK)

    for t in tile_array:
        draw_tile(t)
    
    # Go ahead and update the screen with what we've drawn.
    # This MUST happen after all the other drawing commands.
    pygame.display.flip()
 
# Be IDLE friendly
pygame.quit()
