#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <string.h>

char *infilename = "adv17-01.input";

int main(int argc, char **argv)
{
  char inbuf[3000];

  FILE *infp;
  infp = fopen(infilename, "r");
  if (NULL == infp) {
    err(1, "reading %s", infilename);
  }
  size_t inbuflen;
  inbuflen = fread(inbuf, 1, sizeof inbuf, infp);
  if (ferror(infp)) {
    err(1, "reading %s", infilename);
  }
  fclose(infp);

  while (inbuf[inbuflen-1] == '\n') {
    inbuflen--;
  }

#if 0
# define OTHER(i) ((i+1)%inbuflen)
#else
# define OTHER(i) ((i+(inbuflen/2))%inbuflen)
#endif

  int i;
  int sum = 0;
  for (i = 0; i < inbuflen; i++) {
    if (inbuf[i] == inbuf[OTHER(i)]) {
      sum += inbuf[i] - '0';
    }
  }
  printf("%d\n", sum);
  return 0;
}
