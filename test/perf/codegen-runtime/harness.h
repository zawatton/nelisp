/* Full-width checksums make wrong values fail, including high bits. */
#include <stdio.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdlib.h>
static I ck_main(I n, I r);
int main(void) {
  I n = 0, r = 1;
  if (scanf("%" SCNd64 " %" SCNd64, &n, &r) != 2) return 81;
  printf("%016" PRIx64 "\n", (U)ck_main(n, r));
  return 0;
}
