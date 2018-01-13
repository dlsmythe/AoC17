#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <ctype.h>
#include <err.h>
#include <assert.h>
#include <sys/stat.h>
#include <fcntl.h>

char found_letters[100];
int num_found_letters;
int verbose = 0;

int numrows = 0;
struct row_s {
    char *data;
    int len;
};
struct row_s map[1000];

void add_row(char *s, int n)
{
    map[numrows].len = n;
    map[numrows].data = strdup(s);
    numrows++;
}

char val_at(int row, int col)
{
    if (row < 0) {
	return ' ';
    }
    if (col < 0) {
	return ' ';
    }
    if (row >= numrows) {
	return ' ';
    }
    if (col >= map[row].len) {
	return ' ';
    }
    return map[row].data[col];
}

void read_map(char *fname)
{
    FILE *fp;
    fp = fopen(fname, "r");
    if (!fp) {
	err(1, "fopen(%s)", fname);
    }
    char buf[300];
    while (fgets(buf, sizeof buf, fp) != NULL) {
	int len = strlen(buf);
	if (buf[len-1] == '\n') {
	    buf[--len] = '\0';
	}
	add_row(buf, len);
    }
}

typedef enum direct_e {
    DIR_UP,
    DIR_RIGHT,
    DIR_DOWN,
    DIR_LEFT,

    DIR_LAST
} direct_t;

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
    int row, col;
} dir_off[DIR_LAST] = {
    { -1, 0 },
    { 0, 1 },
    { 1, 0 },
    { 0, -1 }
};

direct_t turn_dir[4][4] = {
    { }, // UP
    { DIR_RIGHT, DIR_DOWN, DIR_LEFT, DIR_UP }, // turning RIGHT
    { }, // DOWN
    { DIR_LEFT, DIR_UP, DIR_RIGHT, DIR_DOWN } // turning LEFT
};

int step_count;

int cell_in_dir(direct_t cur_dir, int cur_row, int cur_col, int *next_row_p, int *next_col_p)
{
    int r, c;
    if (!next_row_p) {
	next_row_p = &r;
	next_col_p = &c;
    }
    *next_row_p = cur_row + dir_off[cur_dir].row;
    *next_col_p = cur_col + dir_off[cur_dir].col;
    return val_at(*next_row_p, *next_col_p);
}

int go_forward(direct_t cur_dir, int *row_p, int *col_p)
{
    int n_row, n_col;
    char c = cell_in_dir(cur_dir, *row_p, *col_p, &n_row, &n_col);

    if (' ' == c) {
	return 0;
    }
    if (isalpha(c)) {
	found_letters[num_found_letters++] = c;
    }

    *row_p = n_row;
    *col_p = n_col;
    return 1;
}

int turn(direct_t desired_turn, direct_t *cur_dir_p, int row, int col)
{
    assert(desired_turn == DIR_LEFT || desired_turn == DIR_RIGHT);
    direct_t d = turn_dir[desired_turn][*cur_dir_p];
    char c = cell_in_dir(d, row, col, NULL, NULL);

    if (' ' == c) {
	return 0;
    }

    *cur_dir_p = d;
    return 1;
}

direct_t next_cell(direct_t cur_dir, int *row_p, int *col_p)
{
    if (go_forward(cur_dir, row_p, col_p)) {
	step_count++;
    } else {
	if (verbose) printf("Can't go fwd from (%d,%d)\n", *row_p, *col_p);
	if (!turn(DIR_LEFT, &cur_dir, *row_p, *col_p)) {
	    if (verbose) printf("Can't turn left from (%d,%d)\n", *row_p, *col_p);
	    if (!turn(DIR_RIGHT, &cur_dir, *row_p, *col_p)) {
		if (verbose) printf("Can't turn right from (%d,%d) - THE END!\n", *row_p, *col_p);
		return DIR_LAST;
	    }
	}
    }
    return cur_dir;
}

void walk_map(void)
{
    int row=0, col;
    char c;
    // Find the start
    for(col = 0; (c = val_at(row, col)) == ' '; col++) {
    }
    if (verbose) printf("Start is at (R%d,C%d)\n", row, col);
    direct_t d = DIR_DOWN;
    step_count = 1;
    while (DIR_LAST != d) {
	if (verbose) printf("%s from (%d,%d)\n", dir_name(d), row, col);
	d = next_cell(d, &row, &col);
    }
    printf("end is at (%d,%d) after %d steps. Found chars: %s\n", row, col, step_count, found_letters);
}

int main(int argc, char **argv)
{
    int c;
    char *filename = "adv17-19.input";
    
    while ((c = getopt(argc, argv, "vf:")) != EOF) {
	switch (c) {
	case 'f':
	    filename = strdup(optarg);
	    break;
	case 'v':
	    verbose = 1;
	    break;
	}
    }

    read_map(filename);
    walk_map();
    
    return 0;
}
