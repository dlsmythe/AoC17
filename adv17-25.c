#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <getopt.h>
#include <sys/queue.h>
#include <assert.h>
#include <ctype.h>

char *infname = "adv17-25.input";
int verbose = 0;

void usage(void)
{
    errx(1, "usage: %s [-S] [input-file]", program_invocation_short_name);
}

char *sample_blueprint[] = {
    "Begin in state A.",
    "Perform a diagnostic checksum after 6 steps.",
    "",
    "In state A:",
    "  If the current value is 0:",
    "    - Write the value 1.",
    "    - Move one slot to the right.",
    "    - Continue with state B.",
    "  If the current value is 1:",
    "    - Write the value 0.",
    "    - Move one slot to the left.",
    "    - Continue with state B.",
    "",
    "In state B:",
    "  If the current value is 0:",
    "    - Write the value 1.",
    "    - Move one slot to the left.",
    "    - Continue with state A.",
    "  If the current value is 1:",
    "    - Write the value 1.",
    "    - Move one slot to the right.",
    "    - Continue with state A.",
    NULL
};

typedef struct state_desc_s {
    struct {
	int write_val;
	int move_incr;
	int next_state;
    } val[2];
} state_desc_t;

state_desc_t states[26];
int start_state = -1;
int max_steps = 0;
int cur_state = -1;
int cur_value = -1;
int cur_line = -1;
void process_blueprint_line(char *line)
{
    assert(line);
    while (isspace(*line)) {
	line++;
    }
    cur_line++;
    if (!line[0]) {
	return;
    }
    if (!strncmp(line, "Begin in state ", 15)) {
	start_state = line[15] - 'A';
	assert(start_state >= 0);
	assert(start_state < 26);
	return;
    }
    if (!strncmp(line, "Perform a diagnostic checksum after ", 36)) {
	max_steps = strtol(line+36, NULL, 0);
	assert(max_steps > 0);
	return;
    }
    if (!strncmp(line, "In state ", 9)) {
	cur_state = line[9] - 'A';
	assert(cur_state >= 0);
	assert(cur_state < 26);
	return;
    }
    if (!strncmp(line, "If the current value is ", 24)) {
	int val = line[24] - '0';
	assert(val == 0 || val == 1);
	cur_value = val;
	return;
    }
    if (!strncmp(line, "- Write the value ", 18)) {
	int val = line[18] - '0';
	assert(val == 0 || val == 1);
	assert(cur_state >= 0);
	assert(cur_state < 26);
	assert(cur_value == 0 || cur_value == 1);
	states[cur_state].val[cur_value].write_val = val;
	return;
    }
    if (!strncmp(line, "- Move one slot to the ", 23)) {
	int dir = line[23];
	assert(dir == 'r' || dir == 'l');
	assert(cur_state >= 0);
	assert(cur_state < 26);
	assert(cur_value == 0 || cur_value == 1);
	states[cur_state].val[cur_value].move_incr = dir == 'r' ? 1 : -1;
	return;
    }
    if (!strncmp(line, "- Continue with state ", 22)) {
	int next_state = line[22] - 'A';
	assert(next_state >= 0);
	assert(next_state < 26);
	assert(cur_state >= 0);
	assert(cur_state < 26);
	assert(cur_value == 0 || cur_value == 1);
	states[cur_state].val[cur_value].next_state = next_state;
	return;
    }
    errx(1, "%d: bogus input line: %s", cur_line, line);
}

void read_sample_blueprint(void)
{
    char **line;
    for (line = sample_blueprint; *line; line++) {
	process_blueprint_line(*line);
    }
}

void read_blueprint(char *fname)
{
    FILE *fp = fopen(fname, "r");
    if (!fp) {
	err(1, "fopen(%s)", fname);
    }
    char buf[100];
    while (fgets(buf, sizeof buf, fp) != NULL) {
	char *p = strchr(buf, '\n');
	if (p) {
	    *p = '\0';
	}
	process_blueprint_line(buf);
    }
    fclose(fp);
}

void print_blueprint(void)
{
    int s;

    printf("Begin in state %c.\n", start_state + 'A');
    printf("Perform a diagnostic checksum after %d steps.\n", max_steps);
    for (s = 0; s < 26; s++) {
	if (0 == states[s].val[0].move_incr) {
	    continue;
	}
	printf("\nIn state %c:\n", s+'A');
	printf("  If the current value is 0:\n");
	printf("    - Write the value %d.\n", states[s].val[0].write_val);
	printf("    - Move one slot to the %s.\n", states[s].val[0].move_incr == 1 ? "right":"left");
	printf("    - Continue with state %c.\n", states[s].val[0].next_state + 'A');
	printf("  If the current value is 1:\n");
	printf("    - Write the value %d.\n", states[s].val[1].write_val);
	printf("    - Move one slot to the %s.\n", states[s].val[1].move_incr == 1 ? "right":"left");
	printf("    - Continue with state %c.\n", states[s].val[1].next_state + 'A');
    }
}

TAILQ_HEAD(tapelist_s, cell_s) tapelist = TAILQ_HEAD_INITIALIZER(tapelist);

typedef struct cell_s {
    TAILQ_ENTRY(cell_s) list;
    int pos;
    int val;
} cell_t;

cell_t *new_cell(int pos, int val)
{
    cell_t *c = calloc(1, sizeof *c);
    assert(c);
    c->pos = pos;
    c->val = val;
    return c;
}

cell_t *cell_at(int pos)
{
    cell_t *c, *prev = NULL;
    TAILQ_FOREACH(c, &tapelist, list) {
	if (pos == c->pos) {
	    return c;
	}
	if (pos > c->pos) {
	    prev = c;
	}
    }
    c = new_cell(pos, 0);
    if (prev) {
//	    printf("inserting pos,val (%d,%d) after (%d,%d)\n", pos, val, prev->pos, prev->val);
	TAILQ_INSERT_AFTER(&tapelist, prev, c, list);
    } else {
//	    printf("prepending pos,val (%d,%d)\n", pos, val);
	TAILQ_INSERT_HEAD(&tapelist, c, list);
    }
    return c;
}

int diagnostic_checksum(void)
{
    int s = 0;
    cell_t *c;
    TAILQ_FOREACH(c, &tapelist, list) {
	s += c->val;
    }
    return s;
}

#define CELLS_PER_LINE 0
void print_tape(int curpos)
{
    cell_t *c = TAILQ_FIRST(&tapelist);
    if (!c) {
	return;
    }
    int colcnt = 0, pos = c->pos;
    while (c) {
#if CELLS_PER_LINE != 0
	if ((colcnt%CELLS_PER_LINE) == 0) {
	    printf("  %d: ", c->pos);
	}
#endif
	printf("%c%c%c", 
	       c->pos == curpos?'[':' ',
	       c->val + '0',
	       c->pos == curpos?']':' ');
	++colcnt;
#if CELLS_PER_LINE != 0
	if ((++colcnt%CELLS_PER_LINE) == 0) {
	    printf("\n");
	    colcnt = 0;
	}
#endif
	c = TAILQ_NEXT(c, list);
	if (c) {
	    while (++pos < c->pos) {
#if CELLS_PER_LINE != 0
		if ((colcnt%CELLS_PER_LINE) == 0) {
		    printf("  %d: ", c->pos);
		}
#endif
		printf("%c0%c", pos == curpos?'[':' ', pos == curpos?']':' ');
#if CELLS_PER_LINE != 0
		if ((++colcnt%CELLS_PER_LINE) == 0) {
		    printf("\n");
		    colcnt = 0;
		}
#endif
	    }
	}
    }
    if (colcnt) {
	printf("\n");
    }
}

int main(int argc, char **argv)
{
    int c;
    int print_bp = 0;

    while ((c = getopt(argc, argv, "Spvi:")) != EOF) {
	switch (c) {
	default:
	    usage();
	case 'S':
	    infname = NULL;
	    read_sample_blueprint();
	    break;
	case 'p':
	    print_bp = 1;
	    break;
	case 'v':
	    verbose++;
	    break;
	case 'i':
	    cell_at(strtol(optarg, NULL, 0));
	    break;
	}
    }

    if (argv[optind]) {
	infname = argv[optind];
    }
    if (infname) {
	read_blueprint(infname);
    }
    
    if (print_bp) {
	print_blueprint();
    }
    
    int pos = 0, step_count;
    cur_state = start_state;
    for (step_count = 0; step_count < max_steps; step_count++) {
	cell_t *cur_cell = cell_at(pos);
	if (verbose > 1) {
	    printf("TIME: %d about to run state %c:\n", step_count, cur_state + 'A');
	}
	if (verbose) {
	    print_tape(pos);
	}
	if (0 == states[cur_state].val[0].move_incr) {
	    errx(1, "bogus state: %c", cur_state + 'A');
	}

	int nstate = states[cur_state].val[cur_cell->val].next_state;
	pos += states[cur_state].val[cur_cell->val].move_incr;
	cur_cell->val = states[cur_state].val[cur_cell->val].write_val;
	cur_state = nstate;
    }

    printf("diagnostic checksum: %d\n", diagnostic_checksum());
    
    return 0;
}
