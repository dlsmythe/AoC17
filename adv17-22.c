#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <ctype.h>
#include <time.h>
#include <err.h>
#include <assert.h>
#include <sys/queue.h>
#include <limits.h>

#define PART 2

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

int verbose = 0;
int num_bursts = 70;

typedef enum {
    CELL_CLEAN,
    CELL_INFECTED,
    CELL_WEAKENED,
    CELL_FLAGGED
} cell_state_t;

char state_char[4] = { '.', '#', 'W', 'F' };
char *state_name[4] = { "CLEAN", "INFECTED", "WEAKENED", "FLAGGED" };

cell_state_t next_state[4] = { CELL_WEAKENED, CELL_FLAGGED, CELL_INFECTED, CELL_CLEAN };

struct cell_s {
    STAILQ_ENTRY(cell_s) list;
    int x;
    int y;
    cell_state_t state;
};

STAILQ_HEAD(map_s, cell_s) netmap = STAILQ_HEAD_INITIALIZER(netmap);

struct cell_s *cell_at(int x, int y)
{
    struct cell_s *c;
    STAILQ_FOREACH(c, &netmap, list) {
	if (c->x == x  &&  c->y == y) {
	    return c;
	}
    }
    return NULL;
}

cell_state_t cell_state(int x, int y)
{
    struct cell_s *c;
    c = cell_at(x,y);
    return c ? c->state : CELL_CLEAN;
}

int cell_is_infected(int x, int y)
{
    return cell_state(x,y) == CELL_INFECTED;
}

struct cell_s *add_cell(int x, int y, cell_state_t state)
{
    struct cell_s *c = malloc(sizeof *c);
    assert(c);
//    printf("add_cell(%d,%d,%d)\n", x, y, state);
    c->x = x;
    c->y = y;
    c->state = state;
    STAILQ_INSERT_TAIL(&netmap, c, list);
    return c;
}

void set_cell(int x, int y, cell_state_t state)
{
    struct cell_s *c;
    c = cell_at(x,y);
    if (c) {
	c->state = state;
    } else {
	add_cell(x, y, state);
    }
}

void read_map(char *fname)
{
    FILE *fp;
    fp = fopen(fname, "r");
    if (!fp) {
	err(1, "fopen(%s)", fname);
    }
    char buf[300];
    int width = 0, numrows = 0;
    int y = 0;
    while (fgets(buf, sizeof buf, fp) != NULL) {
	int len = strlen(buf);
	if (buf[len-1] == '\n') {
	    buf[--len] = '\0';
	}
	numrows++;
	if (width == 0) {
	    width = len;
	    if (1 != (len%2)) {
		errx(1, "grid must be of odd width");
	    }
	    y = width/2;
	}
	int x = -width/2;
	int i;
	for (i = 0; i < len; i++) {
	    add_cell(x++, y, buf[i] == '#' ? CELL_INFECTED : CELL_CLEAN);
	}
	y--;
    }
    fclose(fp);
    if (numrows != width) {
	errx(1, "grid must be square");
    }
}

typedef enum direct_e {
    DIR_UP,
    DIR_RIGHT,
    DIR_DOWN,
    DIR_LEFT,

    DIR_LAST
} direct_t;

direct_t opposite_dir[4] = { DIR_DOWN, DIR_LEFT, DIR_UP, DIR_RIGHT };

void print_map(direct_t cur_dir, int show_x, int show_y)
{
    struct cell_s *c;
    int min_x = INT_MAX, min_y = INT_MAX, max_x = -INT_MAX, max_y = -INT_MAX;
    
    STAILQ_FOREACH(c, &netmap, list) {
	if (c->x < min_x) {
	    min_x = c->x;
	}
	if (c->y < min_y) {
	    min_y = c->y;
	}
	if (c->x > max_x) {
	    max_x = c->x;
	}
	if (c->y > max_y) {
	    max_y = c->y;
	}
    }
    if (show_x < min_x) {
	min_x = show_x;
    }
    if (show_y < min_y) {
	min_y = show_y;
    }
    if (show_x > max_x) {
	max_x = show_x;
    }
    if (show_y > max_y) {
	max_y = show_y;
    }
//    printf("limits: (%d,%d) (%d,%d)\n", min_x, min_y, max_x, max_y);
    int x, y;
    for (y = max_y; y >= min_y; y--) {
	for (x = min_x; x <= max_x; x++) {
	    struct cell_s *c = cell_at(x,y);
	    printf("%c%c%c",
		   (x == show_x  &&  y == show_y) ? '[':' ',
		   c ? state_char[c->state] : '.',
		   (x == show_x  &&  y == show_y) ? ']':' ');
	}
	printf("\n");
    }
}

char *dir_name(direct_t d)
{
    switch (d) {
    case DIR_UP:	return "UP";
    case DIR_RIGHT:	return "RIGHT";
    case DIR_DOWN:	return "DOWN";
    case DIR_LEFT:	return "LEFT";
    default:
	errx(1, "bad dir");
    }
}

// dir_off[cur_dir] -> (x,y) offsets of next cell
struct coord_s {
    int x, y;
} dir_off[DIR_LAST] = {
    { 0, 1 },
    { 1, 0 },
    { 0, -1 },
    { -1, 0 }
};

// turn_dir[direction-you-are-facing][direction-you-want-to-turn] = direction-you-will-be-facing
direct_t turn_dir[4][4] = {
    { }, // UP
    { DIR_RIGHT, DIR_DOWN, DIR_LEFT, DIR_UP }, // turning RIGHT
    { }, // DOWN
    { DIR_LEFT, DIR_UP, DIR_RIGHT, DIR_DOWN } // turning LEFT
};

int step_count;
int infect_count;

void go_forward(direct_t cur_dir, int *x_p, int *y_p)
{
    *x_p += dir_off[cur_dir].x;
    *y_p += dir_off[cur_dir].y;
}

direct_t turn(direct_t desired_turn, direct_t cur_dir, int x, int y)
{
    assert(desired_turn == DIR_LEFT || desired_turn == DIR_RIGHT);
    return turn_dir[desired_turn][cur_dir];
}

direct_t next_cell(direct_t cur_dir, int *x_p, int *y_p)
{
    cell_state_t cur_state = cell_state(*x_p, *y_p), new_state;
    if (verbose) printf("Facing %s. ", dir_name(cur_dir));
#if PART == 1
    cur_dir = turn(cur_state == CELL_INFECTED ? DIR_RIGHT : DIR_LEFT, cur_dir, *x_p, *y_p);
    new_state = cur_state == CELL_INFECTED ? CELL_CLEAN : CELL_INFECTED;
#else
    switch (cur_state) {
    case CELL_CLEAN:
	cur_dir = turn(DIR_LEFT, cur_dir, *x_p, *y_p);
	break;
    case CELL_INFECTED:
	cur_dir = turn(DIR_RIGHT, cur_dir, *x_p, *y_p);
	break;
    case CELL_FLAGGED:
	cur_dir = opposite_dir[cur_dir];
	break;
    case CELL_WEAKENED:
	break;
    }
    new_state = next_state[cur_state];
#endif
    if (verbose) printf("cell (%d,%d) is %s. Turning %s.\n", *x_p, *y_p, state_name[cur_state], dir_name(cur_dir));
    set_cell(*x_p, *y_p, new_state);
    infect_count += new_state == CELL_INFECTED;
    go_forward(cur_dir, x_p, y_p);
    step_count++;
    return cur_dir;
}

void walk_map(void)
{
    int x = 0, y = 0, i;
    direct_t d = DIR_UP;

    struct timespec t0, t;
    double t0secs, now;
    clock_gettime(CLOCK_REALTIME, &t0);
    t0secs = t0.tv_sec + (t0.tv_nsec / 1000000000.0);
    printf("initial map, facing UP:\n");
    print_map(d, x, y);
    for (i = 0; i < num_bursts; i++) {
	if (verbose) printf("%s from (%d,%d)\n", dir_name(d), x, y);
	d = next_cell(d, &x, &y);
	if (verbose > 1) print_map(d, x, y);
#define PRINT_ITER_VAL 10000
	if ((i%PRINT_ITER_VAL) == 0) {
	    clock_gettime(CLOCK_REALTIME, &t);
	    now = t.tv_sec + (t.tv_nsec / 1000000000.0);
	    printf("%d %lf microsecs/iteration\n", i, (1000000*(now-t0secs))/PRINT_ITER_VAL);
	}
    }
    if (verbose) {
	printf("final direction is %s:\n", dir_name(d));
	print_map(d, x, y);
    }
}

int main(int argc, char **argv)
{
    int c;
    char *filename = "adv17-22.input";
    
    while ((c = getopt(argc, argv, "vf:n:")) != EOF) {
	switch (c) {
	case 'f':
	    filename = strdup(optarg);
	    break;
	case 'n':
	    num_bursts = strtol(optarg, NULL, 0);
	    break;
	case 'v':
	    verbose++;
	    break;
	}
    }

    read_map(filename);
    walk_map();
    printf("Bursts caused %d infections\n", infect_count);

    return 0;
}
