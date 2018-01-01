#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <err.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include <nacl/crypto_hash.h>

typedef struct step_s {
    enum { STEP_X, STEP_S, STEP_P } step;
    uint32_t pos1, pos2;
    uint32_t dummy; // align 16
} step_t;

int numsteps = 0, maxsteps = 0;
step_t *steplist = NULL;
void add_step(int step, int p1, int p2)
{
//    printf("add_step(%c,%d,%d)\n", "XSP"[step], p1, p2);
    if (numsteps >= maxsteps) {
	maxsteps += 10000;
	step_t *newlist = realloc(steplist, maxsteps*sizeof(step_t));
	assert(newlist);
	steplist = newlist;
    }
    steplist[numsteps].step = step;
    steplist[numsteps].pos1 = p1;
    steplist[numsteps].pos2 = p2;
    numsteps++;
}

void read_steps(char *filename)
{
    struct stat s;
    char *buf;

    int fd = open(filename, O_RDONLY);
    if (-1 == fd) {
	err(1, "open(%s)", filename);
    }
    if (-1 == fstat(fd, &s)) {
	err(1, "fstat(%s)", filename);
    }
    buf = malloc(s.st_size+1);
    assert(buf);
    buf[s.st_size+1] = '\0';
    if (read(fd, buf, s.st_size) != s.st_size) {
	err(1, "read(%s)", filename);
    }
    close(fd);

    char *p = buf;
    int step, p1, p2;
    while (p  &&  (p - buf) < s.st_size) {
	switch (*p) {
	case 'x':
	{
	    char *q = strchr(p, '/');
	    step = STEP_X;
	    p1 = atoi(p+1);
	    p2 = atoi(q+1);
	}
	    break;
	case 's':
	    step = STEP_S;
	    p1 = atoi(p+1);
	    p2 = 0;
	    assert(p1 < 16);
	    break;
	case 'p':
	    step = STEP_P;
	    p1 = p[1];
	    p2 = p[3];
	    break;
	default:
	    errx(1, "bogus step");
	}
	add_step(step, p1, p2);
	p = strchr(p, ',');
	if (p) {
	    p++;
	}
    }
}

#define CHECK_DUPS 0
#if CHECK_DUPS
uint32_t hashes[10000];
char strs[10000][17];
void addhash(char *s, int i)
{
    uint8_t buf[crypto_hash_BYTES];
    crypto_hash(buf, (unsigned char*)s, strlen(s));
    hashes[i] = *(uint32_t*)buf;
    strcpy(strs[i], s);
    printf("%d: %s\n", i, s);
}
void checkhashes(void)
{
    int i, j, skips=0;
    for (i = 0; i < numsteps-1; i++) {
	for (j = i+1; j < numsteps; j++) {
	    if (hashes[i] == hashes[j]) {
//	    if (!strcmp(strs[i], strs[j])) {
		printf("same state at steps %d and %d: %s %s\n", i, j, strs[i], strs[j]);
		skips += j-i;
	    }
	}
    }
    printf("total skips: %d\n", skips);
}
#else
# define addhash(...)
# define checkhashes(...)
#endif

int main(int argc, char **argv)
{
    read_steps("adv17-16.input");
    printf("there are %d steps\n", numsteps);
    int i, it, itercount = 1000 * 1000 * 1000;
    itercount = 1000;
    static char positions[17] = "abcdefghijklmnop";
    for (it = 1; it <= itercount; it++) {
	if ((it % 10000) == 0) {
	    printf("."); fflush(stdout);
	}
	for (i = 0; i < numsteps; i++) {
//	    addhash(positions,i);
	    int pos1 = steplist[i].pos1;
	    int pos2 = steplist[i].pos2;
	    switch (steplist[i].step) {
	    default:
		errx(1, "corrupted steplist");
	    case STEP_S:
		//printf("%d: STEP_S\n", i);
		{
		    char buf[15];
		    memmove(buf, positions+16-pos1, pos1);
		    memmove(positions+pos1, positions, 16-pos1);
		    memmove(positions, buf, pos1);
		}
		break;
	    case STEP_X:
		//printf("%d: STEP_X\n", i);
		{
		    char c = positions[pos1];
		    positions[pos1] = positions[pos2];
		    positions[pos2] = c;
		}
		break;
	    case STEP_P:
		//printf("%d: STEP_P\n", i);
		{
		    int j, p1=-1, p2=-1;
		    for (j = 0; j < 16; j++) {
			if (positions[j] == pos1) {
			    p1 = j;
			    if (p2>=0) break;
			}
			if (positions[j] == pos2) {
			    p2 = j;
			    if (p1>=0) break;
			}
		    }
		    positions[p1] = pos2;
		    positions[p2] = pos1;
		}
		break;
	    }
	}
	addhash(positions,it);
    }
    checkhashes();
    printf("final positions: %s\n", positions);
    return 0;
}
