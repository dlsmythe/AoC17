#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <assert.h>
#include <sys/queue.h>

#define PART 2
#if PART==1
# define ENDVAL 2017
#else
# define ENDVAL 50000000
#endif
#define EXAMPLE 0
#if EXAMPLE
int stepsize = 3;
#else
int stepsize = 369;
#endif

struct node_s {
    CIRCLEQ_ENTRY(node_s) list;
    int value;
};

CIRCLEQ_HEAD(cq_s, node_s) cbuf = CIRCLEQ_HEAD_INITIALIZER(cbuf);

struct node_s *new_node(int val)
{
    struct node_s *node;
    node = calloc(1, sizeof *node);
    assert(node);
    node->value = val;
    return node;
}

struct node_s *insert_after(struct node_s *n, int val)
{
    struct node_s *node = new_node(val);
    CIRCLEQ_INSERT_AFTER(&cbuf, n, node, list);
    return node;
}

void print_state(struct node_s *nzero, int val, int curval)
{
    int i;
    struct node_s *cur = nzero;
    for (i = 0; i < val; i++) {
	printf("%s%d%s ", cur->value==curval? "(":"", cur->value, cur->value==curval? ")":"");
	cur = CIRCLEQ_LOOP_NEXT(&cbuf, cur, list);
    }
    puts("");
}

int main(int argc, char **argv)
{
    int val;
    struct node_s *cur, *nzero;

    nzero = cur = new_node(0);
    CIRCLEQ_INSERT_HEAD(&cbuf, cur, list);
    for (val = 1; val <= ENDVAL; val++) {
	if ((val % 100000)==0) {
	    printf("%d\n", val);
	}
	//print_state(nzero, val, cur->value);
	int i;
	for (i = 0; i < stepsize; i++) {
	    cur = CIRCLEQ_LOOP_NEXT(&cbuf, cur, list);
	}
	cur = insert_after(cur, val);
    }
#if PART==1
    cur = nzero;
    for (val = 0; val < ENDVAL; val++) {
	cur = CIRCLEQ_LOOP_NEXT(&cbuf, cur, list);
	if (cur->value == ENDVAL) {
	    printf("Number after %d is %d\n", ENDVAL, CIRCLEQ_LOOP_NEXT(&cbuf, cur, list)->value);
	    break;
	}
	if (cur->value == 0) {
	    break;
	}
    }
#else
    printf("Number after 0 is %d\n", CIRCLEQ_LOOP_NEXT(&cbuf, nzero, list)->value);
#endif
    return 0;
}
