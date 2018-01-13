#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <err.h>

int a = 1;

int main(int argc, char **argv)
{
    int b = 67;
    int c = b;
    long h = 0;

    if (a != 0) {
	b *= 100;
	b -= -100000;
	c = b - -17000;
    }
    int mulcnt = 0;
    printf("initial values: b=%d c=%d\n", b, c);
    while (1) {
	printf("b=%d c=%d h=%ld\n", b, c, h);
	int f = 1;
	int d;
	for (d = 2; d != b; d++) {
	    int e;
	    for (e = 2; e != b; e++) {
		int v = d*e;
		mulcnt++;
		if (a  &&  v > b) {
		    break;
		}
		if (v == b) {
		    f = 0;
		    goto COUNTIT;
		}
	    }
	}
    COUNTIT:
	if (0 == f) {
	    h++;
	}
	if (b == c) {
	    break;
	}
	b -= -17;
    }
    printf("mulcnt %d\n", mulcnt);
    printf("Final value of h is %ld\n", h);
    return 0;
}
