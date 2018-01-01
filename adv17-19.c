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
    //printf("map[%3d]: len=%3d data='%s'\n", numrows, map[numrows].len, map[numrows].data);
    numrows++;
}

char val_at(int row, int col)
{
    if (row < 0) {
//	printf("smallrow\n");
	return ' ';
    }
    if (col < 0) {
//	printf("smallcol\n");
	return ' ';
    }
    if (row >= numrows) {
//	printf("bigrow\n");
	return ' ';
    }
    if (col >= map[row].len) {
//	printf("bigcol\n");
	return ' ';
    }
//    printf("val_at(%d,%d) = %c\n", row, col, map[row].data[col]);
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

direct_t next_cell(direct_t cur_dir, int *row_p, int *col_p)
{
    struct coord_s u, r, d, l;
    char c_u, c_r, c_d, c_l;
    u.row = *row_p + dir_off[DIR_UP].row;
    u.col = *col_p + dir_off[DIR_UP].col;
    r.row = *row_p + dir_off[DIR_RIGHT].row;
    r.col = *col_p + dir_off[DIR_RIGHT].col;
    d.row = *row_p + dir_off[DIR_DOWN].row;
    d.col = *col_p + dir_off[DIR_DOWN].col;
    l.row = *row_p + dir_off[DIR_LEFT].row;
    l.col = *col_p + dir_off[DIR_LEFT].col;
    c_u = val_at(u.row, u.col);
    c_r = val_at(r.row, r.col);
    c_d = val_at(d.row, d.col);
    c_l = val_at(l.row, l.col);
    switch (cur_dir) {
    default:
	errx(1, "bad dir");
    case DIR_UP:
	printf("next char UP is '%c' at (%d,%d)\n", c_u, u.row, u.col);
	if (isalpha(c_u)) {
	    found_letters[num_found_letters++] = c_u;
	    (*row_p)--;
	    return DIR_UP;
	}
	if ('-' == c_u  ||  '|' == c_u  ||  '+' == c_u) {
	    (*row_p)--;
	    return DIR_UP;
	}
	// have to turn or quit
	if (c_l == ' ' &&  c_u == ' ' &&  c_r == ' ') {
	    // reached the end
	    break;
	}
	if (c_l == '-'  &&  c_r == '-') {
	    errx(1, "multiple directional choices going %s from (%d,%d)", dir_name(cur_dir), *row_p, *col_p);
	}
	if (c_l == '-') {
	    (*col_p)--;
	    return DIR_LEFT;
	}
	(*col_p)++;
	return DIR_RIGHT;

    case DIR_RIGHT:
	printf("next char RIGHT is '%c' at (%d,%d)\n", c_r, r.row, r.col);
	if (isalpha(c_r)) {
	    found_letters[num_found_letters++] = c_r;
	    (*col_p)++;
	    return DIR_RIGHT;
	}
	if ('-' == c_r  ||  '|' == c_r) {
	    (*col_p)++;
	    return DIR_RIGHT;
	}
	// have to turn or quit
	if (c_d == ' ' &&  c_u == ' ' &&  c_r == ' ') {
	    // reached the end
	    break;
	}
	if (c_u == '-'  &&  c_d == '-') {
	    errx(1, "multiple directional choices going %s from (%d,%d)", dir_name(cur_dir), *row_p, *col_p);
	}
	if (c_u != ' ') {
	    (*row_p)--;
	    return DIR_UP;
	}
	(*row_p)++;
	return DIR_DOWN;

    case DIR_DOWN:
	printf("next char DOWN is '%c' at (%d,%d)\n", c_d, d.row, d.col);
	if (isalpha(c_d)) {
	    found_letters[num_found_letters++] = c_d;
	    (*row_p)++;
	    return DIR_DOWN;
	}
	if ('-' == c_d  ||  '|' == c_d) {
	    (*row_p)++;
	    return DIR_DOWN;
	}
	// have to turn or quit
	if (c_d == ' ' &&  c_l == ' ' &&  c_r == ' ') {
	    // reached the end
	    break;
	}
	if (c_l == '-'  &&  c_r == '-') {
	    errx(1, "multiple directional choices going %s from (%d,%d)", dir_name(cur_dir), *row_p, *col_p);
	}
	if (c_l != ' ') {
	    (*col_p)--;
	    return DIR_LEFT;
	}
	(*col_p)++;
	return DIR_RIGHT;

    case DIR_LEFT:
	printf("next char LEFT is '%c' at (%d,%d)\n", c_l, l.row, l.col);
	if (isalpha(c_l)) {
	    found_letters[num_found_letters++] = c_l;
	    (*col_p)--;
	    return DIR_LEFT;
	}
	if ('-' == c_l  ||  '|' == c_l) {
	    (*col_p)--;
	    return DIR_LEFT;
	}
	// have to turn or quit
	if (c_d == ' ' &&  c_l == ' ' &&  c_u == ' ') {
	    // reached the end
	    break;
	}
	if (c_d == '-'  &&  c_u == '-') {
	    errx(1, "multiple directional choices going %s from (%d,%d)", dir_name(cur_dir), *row_p, *col_p);
	}
	if (c_d != ' ') {
	    (*row_p)++;
	    return DIR_DOWN;
	}
	(*row_p)--;
	return DIR_UP;
    }
    return DIR_LAST;
}

void walk_map(void)
{
    int row=0, col;
    char c;
    // Find the start
    for(col = 0; (c = val_at(row, col)) == ' '; col++) {
    }
    printf("Start is at (R%d,C%d)\n", row, col);
    direct_t d = DIR_DOWN;
    while (DIR_LAST != d) {
	printf("%s from (%d,%d)\n", dir_name(d), row, col);
	d = next_cell(d, &row, &col);
    }
    printf("end is at (%d,%d) chars: %s\n", row, col, found_letters);
}

int main(int argc, char **argv)
{
    int c;
    char *filename = "adv17-19.input";
    
    while ((c = getopt(argc, argv, "lf:t")) != EOF) {
	switch (c) {
	case 'f':
	    filename = strdup(optarg);
	    break;
	}
    }

    read_map(filename);
    walk_map();
    
    return 0;
}
