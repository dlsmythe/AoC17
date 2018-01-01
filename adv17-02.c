#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <ctype.h>
#include <limits.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

#define PART1 1
#define PART2 0

char *infname;

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
    infname = "adv17-2.in";
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

  long cksum = 0;
  while (1) {
    memset(buf, 0, sizeof buf);
    if (fgets(buf, sizeof buf, fp) == NULL) {
      break;
    }
    long vals[20];
    int numvals = 0, i;
    parsvals(buf, vals, LENGTHOF(vals), &numvals);
#if PART1
    long minval = LONG_MAX, maxval = LONG_MIN;
#endif
#if PART2
    long result = 0;
#endif
    for (i = 0; i < numvals; i++) {
#if PART1
      if (vals[i] > maxval) {
	maxval = vals[i];
      }
      if (vals[i] < minval) {
	minval = vals[i];
      }
#endif
#if PART2
      if (vals[i] == 0) {
	continue;
      }
      int j;
      for (j = 0; j < numvals; j++) {
	if (i == j) {
	  continue;
	}
	if (vals[j] == 0) {
	  continue;
	}
	if ((vals[i] % vals[j]) == 0) {
	  result = vals[i] / vals[j];
	  break;
	}
	// NB: This test is unnecessary - it would be performed
	//      eventually in any case.  This is a just an optimization.
	if ((vals[j] % vals[i]) == 0) {
	  result = vals[j] / vals[i];
	  break;
	}
      }
      if (result) {
	break;
      }
#endif
    }
#if PART1
    cksum += maxval - minval;
#endif
#if PART2
    cksum += result;
#endif
  }
  printf("cksum is %ld\n", cksum);
  return 0;
}
