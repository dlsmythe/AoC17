#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <err.h>

// Center is shell 0
// for each shell n > 0:
//   there are 8n numbers in it.
//   each side has 2n+1 numbers
// The manhattan distance is:
//   number of steps to the center of a side + n
// Calculate a "side index" to calculate first term above:
//   shell_base is the lowest number in the shell (just above LR corner)
//       -- its value is (sum+1)
//   SI = (number - shell_base) % (2n + 1)
// Dist to center of side is ABS(SI - (n-1))

int find_dist(int number)
{
  if (number == 1)
    return 0;

  int sum = 1, n = 1;

  // Find the shell:
  while (1) {
    if (number <= sum + (n*8)) {
      break;
    }
    sum += n++ * 8;
  }

  // now, sum+1 is the shell_base and n is the shell number

  int shell_index = number - (sum+1);
  int side_index = shell_index % (2*n);
  
#if 0
  int shell_length = n * 8;
  printf("Number is %d\n", number);
  printf("Shell is %d, shell_length is %d, sum is %d\n", n, shell_length, sum);
  printf("next shell_base is %d\n", sum + 1 + shell_length);
  printf("sides are length %d\n", 2*n + 1);
  int c0, c1, c2, c3;
  c0 = sum + 2*n;
  c1 = sum + 4*n;
  c2 = sum + 6*n;
  c3 = sum + 8*n;
  printf("corners are %d, %d, %d, %d\n", c0, c1, c2, c3);
  printf("centers are %d, %d, %d, %d\n",
	 sum + n, sum + 3*n, sum + 5*n, sum + 7*n);
  printf("centers are also %d, %d, %d, %d\n", c0-n, c1-n, c2-n, c3-n);
  printf("side is %d\n", shell_index / (2*n));
  printf("shell index: %d\n", shell_index);
  printf("side_index is %d\n", side_index);
  printf("dist to center of side: %d\n", abs(side_index - (n-1)));
#endif

  return n + abs(side_index - (n-1));
}

int main(int argc, char **argv)
{
  int num;
  if (argc == 1) {
    num = 368078;
  } else {
    num = strtol(argv[argc-1], NULL, 0);
  }
  if (num < 1) {
    errx(1, "number must be positive");
  }
  printf("Dist(%d) = %d\n", num, find_dist(num));
  return 0;
}
