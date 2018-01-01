#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define PART 2
#define EXAMPLE 0
uint64_t gen_a(void)
{
#if EXAMPLE
    static uint64_t previous = 65; // example
#else
    static uint64_t previous = 618;
#endif
    uint64_t value;
    do {
	value = (previous * 16807) % 2147483647;
	previous = value;
    } while (
#if PART==1
	0
#else
	(value & 0x3) != 0
#endif
	);	
    return value;
}

uint64_t gen_b(void)
{
#if EXAMPLE
    static uint64_t previous = 8921; // example
#else
    static uint64_t previous = 814;
#endif
    uint64_t value;
    do {
	value = (previous * 48271) % 2147483647;
	previous = value;
    } while (
#if PART==1
	0
#else
	(value & 0x7) != 0
#endif
	);	
    return value;
}

int main(int argc, char **argv)
{
    int i, score = 0;
#if PART==1
    const int max_iterations = 40 * 1000 * 1000;
#else
    const int max_iterations = 5 * 1000 * 1000;
#endif
    for (i=0; i < max_iterations; i++) {
	uint64_t a = gen_a();
	uint64_t b = gen_b();
	//printf("%10lu  %10lu\n", a, b);
	if ((a&0xffff) == (b&0xffff)) {
	    score++;
	}
    }
    printf("score %d\n", score);
    return 0;
}
