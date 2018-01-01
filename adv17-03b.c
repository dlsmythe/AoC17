#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

// Used to map "square number" to grid coords.
// Origin is 0,0.
// Spiral starts at 1,0 and continues CCW.
struct {
  int x;
  int y;
  int sum;
} table[1+8+16+32+40+48];

enum direction_t {
  EAST,
  NORTH,
  WEST,
  SOUTH,
  LASTDIR = SOUTH
};

char *dirname[4] = { "EAST", "NORTH", "WEST", "SOUTH" };

struct {
  int dx;
  int dy;
} dirincr[4] = {
  { 1, 0 }, // EAST
  { 0, 1 }, // NORTH
  { -1, 0}, // WEST
  { 0, -1} // SOUTH
};

int cell_index_with_coords(int x, int y)
{
  int i;
  for (i = 0; i < LENGTHOF(table); i++) {
    if (table[i].x == x  &&  table[i].y == y) {
      return i;
    }
  }
  errx(1, "(%d,%d) is not in the table", x, y);
  return -1; // NOTREACHED
}

void init_map(void)
{
  // First, determine the coordinates of each cell of the spiral
  int x=1, y=0, shell_base, shell = 1;
  for (shell_base = 1; shell_base+(8*shell) < LENGTHOF(table); shell_base += 8*shell++) {
    enum direction_t direction = NORTH;
    int i, shell_max = 8*shell;
    for (i = 0; i < shell_max; i++) {
      table[shell_base+i].x = x;
      table[shell_base+i].y = y;
      if (((i+1) % (2*shell)) == 0  &&  i != (shell_max-1)) { // hit a corner?
	direction = ((int)direction + 1) % 4;
      }
      x += dirincr[(int)direction].dx;
      y += dirincr[(int)direction].dy;
    }
  }
}

void usage(void)
{
  fprintf(stderr, "usage: %s <n>\n", "adv17-3b");
  exit(1);
}

int main(int argc, char **argv)
{
  if (argc != 2) {
    usage();
  }
  int target_value = strtoul(argv[1], NULL, 0);
  
  init_map();

  // Now walk the spiral and fill in the sums
  int i;
  table[0].sum = 1;
  for (i = 1; i < LENGTHOF(table); i++) {
    int x = table[i].x;
    int y = table[i].y;
    table[i].sum = 0;
    table[i].sum += table[cell_index_with_coords(x+1, y)].sum;
    table[i].sum += table[cell_index_with_coords(x+1, y+1)].sum;
    table[i].sum += table[cell_index_with_coords(x,   y+1)].sum;
    table[i].sum += table[cell_index_with_coords(x-1, y+1)].sum;
    table[i].sum += table[cell_index_with_coords(x-1, y)].sum;
    table[i].sum += table[cell_index_with_coords(x-1, y-1)].sum;
    table[i].sum += table[cell_index_with_coords(x,   y-1)].sum;
    table[i].sum += table[cell_index_with_coords(x+1, y-1)].sum;
    printf("%3d: (%d,%d) = %d\n", i, x, y, table[i].sum);

    if (table[i].sum > target_value) {
      printf("Next largest sum is %d at index %d\n", table[i].sum, i);
      exit(0);
    }
  }
  err(1, "not found at index %d.  Try a bigger table size?", i);

  return 0;
}
