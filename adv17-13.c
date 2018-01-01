#define _GNU_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <getopt.h>
#include <sys/queue.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

int verbose = 0;

int max_depth = 0;

struct layer_s {
    STAILQ_ENTRY(layer_s) list;
    int depth;
    int range;
    int incr;
    int scanpos;
};

STAILQ_HEAD(lyrlist, layer_s) input_list = STAILQ_HEAD_INITIALIZER(input_list);

void advance_scanners(struct lyrlist *state);
void prt_state(struct lyrlist *state, int pos);

struct layer_s node_heap[100000];
int last_free = 0;

struct layer_s *node_alloc(int d, int r, int s, int incr)
{
    int i, pos;
    for (i = 0, pos = last_free; i < LENGTHOF(node_heap); i++, pos++) {
	if (pos >= LENGTHOF(node_heap)) {
	    pos = 0;
	}
	if (node_heap[pos].range == 0) {
	    node_heap[pos].range = r;
	    node_heap[pos].depth = d;
	    node_heap[pos].incr = incr;
	    node_heap[pos].scanpos = s;
	    last_free = pos+1;
	    return &node_heap[pos];
	}
    }
    errx(1, "heap exhausted");
    return NULL; // NOTREACHED
}

void node_free(struct layer_s *node)
{
    memset(node, 0, sizeof *node);
}

struct layer_s *find_in_list(struct lyrlist *lyrs, int idx)
{
    struct layer_s *l;
    STAILQ_FOREACH(l, lyrs, list) {
	if (l->depth == idx) {
	    return l;
	}
    }
    return NULL;
}

void free_list(struct lyrlist *lyrs)
{
    struct layer_s *l;
    while (!STAILQ_EMPTY(lyrs)) {
	l = STAILQ_FIRST(lyrs);
	STAILQ_REMOVE_HEAD(lyrs, list);
	node_free(l);
    }
}

void copy_list(struct lyrlist *lyrs, struct lyrlist *newhead)
{
    struct layer_s *ol, *nl;
    STAILQ_INIT(newhead);
    STAILQ_FOREACH(ol, lyrs, list) {
	nl = node_alloc(ol->depth, ol->range, ol->scanpos, ol->incr);
	STAILQ_INSERT_TAIL(newhead, nl, list);
    }
}

// ====================

// Keep the last 100 picoseconds of data so we don't have to keep regenerating it
int max_to_keep = 100;
int max_kept = 0;

struct fw_state {
    STAILQ_ENTRY(fw_state) list;
    struct lyrlist layers;
    int simtime;
};

STAILQ_HEAD(timelist, fw_state) simtimelist = STAILQ_HEAD_INITIALIZER(simtimelist);

void prune_simlist(void)
{
    while (max_kept > max_to_keep) {
	struct fw_state *s;
	s = STAILQ_FIRST(&simtimelist);
	STAILQ_REMOVE_HEAD(&simtimelist, list);
	if (verbose > 1) {
	    printf("Deleted state at time %d\n", s->simtime);
	}
	free_list(&s->layers);
	memset(s, 0, sizeof *s); // superstition
	free(s);
	max_kept--;
    }
}

void add_fw_state(struct fw_state *s)
{
    if (verbose > 1) {
	printf("Added state at time %d\n", s->simtime);
    }
    STAILQ_INSERT_TAIL(&simtimelist, s, list);
    max_kept++;
    prune_simlist();
}

struct lyrlist *fw_state_at_time(int t)
{
    struct fw_state *s, *prev = NULL;
    int max_seen = -1;
    STAILQ_FOREACH(s, &simtimelist, list) {
	if (s->simtime == t) {
	    return &s->layers;
	}
	if (s->simtime > max_seen) {
	    max_seen = s->simtime;
	    prev = s;
	}
    }
    int i;
    for (i = max_seen+1; i <= t; i++) {
	s = calloc(1, sizeof *s);
	s->simtime = i;
	if (0 == i) {
	    copy_list(&input_list, &s->layers);
	} else {
	    copy_list(&prev->layers, &s->layers);
	    advance_scanners(&s->layers);
	}
	add_fw_state(s);
	prev = s;
    }
    
    return &s->layers;
}

//================================================

void read_input(struct lyrlist *newhead)
{
    char buf[100];

    STAILQ_INIT(newhead);
    while (fgets(buf, sizeof buf, stdin) != NULL) {
	struct layer_s *l;
	int d, r;
	char *p;
	p = strchr(buf, '\n');
	if (p) {
	    *p = 0;
	}
	sscanf(buf, "%d:%d", &d, &r);
	l = node_alloc(d, r, 0, 1);
	STAILQ_INSERT_TAIL(newhead, l, list);
        if (d > max_depth) {
            max_depth = d;
	}
    }
}

void prt_state(struct lyrlist *state, int pos)
{
    struct layer_s *l;
    int i = 0, j;
    printf("State(pos %d)\n", pos);
    STAILQ_FOREACH(l, state, list) {
	while (i < l->depth) {
	    printf("%3d %s\n", i, i == pos ? "(.)" : "...");
	    i++;
	}
	char layer[80];
	int x=0;
	for (j = 0; j < l->range; j++) {
	    x += sprintf(&layer[x], "%s%c%s",
			 (j == 0 && i == pos) ? "(":"",
			 (j == l->scanpos) ? 'S':'_',
			 (j == 0 && i == pos) ? ")":"");
	}
	printf("%3d %s\n", l->depth, layer);
	i++;
    }
    while (i < max_depth) {
	printf("%3d %s\n", i, i == pos ? "(.)" : "...");
	i++;
    }
}

void advance_scanners(struct lyrlist *state)
{
    struct layer_s *l;
    STAILQ_FOREACH(l, state, list) {
	l->scanpos += l->incr;
	if (l->scanpos >= l->range) {
	    l->scanpos -= 2;
	    l->incr = -l->incr;
	} else if (l->scanpos == -1) {
	    l->scanpos = 1;
	    l->incr = -l->incr;
	}
    }
}

int check_caught(int pos, struct lyrlist *state, int *severity)
{
    struct layer_s *l;
    l = find_in_list(state, pos);
    if (l  &&  l->scanpos == 0) {
        if (verbose) {
            printf("caught at depth %d range %d\n", pos, l->range);
	}
	*severity = pos * l->range;
	return 1;
    }
    return 0;
}	

int do_trip(int delay, int *caught)
{
    int i, severity = 0;
    *caught = 0;
    for (i = 0; i <= max_depth; i++) {
	struct lyrlist *state = fw_state_at_time(i+delay);
        if (verbose) {
            printf("Time %d step %d:\n", i+delay, i);
            prt_state(state,i);
	}
	int sev;
        if (check_caught(i,state, &sev)) {
	    *caught = 1;
	    severity += sev;
	}
    }
    if (verbose) {
        printf("trip severity: %d\n", severity);
    }
    return severity;
}

int main(int argc, char **argv)
{
    int c;
    int rlow = 0;
    int rhigh = rlow+1;

    while ((c = getopt(argc, argv, "vl:h:")) != EOF) {
	switch (c) {
	case 'v':
	    verbose = 1;
	    break;
	case 'l':
	    rlow = strtoul(optarg, NULL, 0);
	    break;
	case 'h':
	    rhigh = strtoul(optarg, NULL, 0);
	    break;
	}
    }

    printf("reading input\n");
    read_input(&input_list);

//    prt_state(input_list, 0);

    printf("trying range[%d,%d)\n", rlow, rhigh);
    int delay, caught = 0, col = 0;
    for (delay = rlow; delay < rhigh; delay++) {
	int sev = do_trip(delay, &caught);
	if (!caught  &&  sev == 0) {
	    printf("smallest delay: %d\n", delay);
	    exit(0);
	}
	if (verbose) {
	    printf("delay %d: severity %d\n", delay, sev);
	}
	if ((delay%100) == 0) {
	    putchar('.');
	    fflush(stdout);
	    col++;
	    if (col == 80) {
		printf("\n%d ", delay);
		col = 0;
	    }
	}
    }
    printf("try a bigger delay\n");

    exit(1);
}
