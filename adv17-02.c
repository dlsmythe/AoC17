#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <sys/queue.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

char *infname;

struct line_s {
    STAILQ_ENTRY(line_s) list;
    int numvals;
    long vals[20];
};
STAILQ_HEAD(linelist, line_s) input_list = STAILQ_HEAD_INITIALIZER(input_list);

void parsvals(char *instr, long vals[], int maxlen, int *numvals)
{
    int len = 0;
    char *p = instr, *p2;
    while (1) {
	errno = 0;
	p2 = NULL;
	long val = strtol(p, &p2, 0);
	if (errno) {
	    err(1, "bad input: '%s'", instr);
	}
	if (p2 == p) {
	    break;
	}
	p = p2;
	if (len == maxlen) {
	    err(1, "too many values - maxlen is %d: '%s'", maxlen, instr);
	}
	vals[len++] = val;
    }
    *numvals = len;
}

int main(int argc, char **argv)
{
    FILE *fp;
    char buf[1000];

    infname = argv[1];
    if (argc == 1) {
	infname = "adv17-02.input";
    } else {
	infname = argv[argc-1];
    }
    if (!strcmp(infname, "-")) {
	fp = stdin;
    } else {
	fp = fopen(infname, "r");
	if (!fp) {
	    err(1, "%s", infname);
	}
    }

    while (1) {
	memset(buf, 0, sizeof buf);
	if (fgets(buf, sizeof buf, fp) == NULL) {
	    break;
	}
	struct line_s *l = (struct line_s*)calloc(1, sizeof(struct line_s));
	assert(l);
	parsvals(buf, l->vals, LENGTHOF(l->vals), &l->numvals);
	STAILQ_INSERT_TAIL(&input_list, l, list);
    }

    long cksum[2] = { 0, 0 };
    struct line_s *l;
    STAILQ_FOREACH(l, &input_list, list) {
	int i, top = 0, bottom = -1;
	long minval = LONG_MAX, maxval = LONG_MIN;
	for (i = 0; i < l->numvals; i++) {
	    if (l->vals[i] > maxval) {
		maxval = l->vals[i];
	    }
	    if (l->vals[i] < minval) {
		minval = l->vals[i];
	    }

	    if (l->vals[i] == 0) {
		continue;
	    }

	    int j;
	    for (j = 0; j < l->numvals  &&  -1 == bottom; j++) {
		if (i == j) {
		    continue;
		}
		if (l->vals[j] == 0) {
		    continue;
		}
		if ((l->vals[i] % l->vals[j]) == 0) {
		    top = i;
		    bottom = j;
		} else {
		    // NB: This test is unnecessary - it would be performed
		    //      eventually in any case.  This is a just an optimization.
		    if ((l->vals[j] % l->vals[i]) == 0) {
			top = j;
			bottom = i;
		    }
		}
	    }
	}
	cksum[0] += maxval - minval;
	cksum[1] += l->vals[top] / l->vals[bottom];
    }
    printf("part 1 cksum is %ld\n", cksum[0]);
    printf("part 2 cksum is %ld\n", cksum[1]);
    return 0;
}
