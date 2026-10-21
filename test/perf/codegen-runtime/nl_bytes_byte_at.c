#include "runtime.c"
#include "harness.h"
static I ck_main(I n, I r) {
  unsigned char data[32]; U s = 0;
  for (I i=0; i<32; i++) data[i] = i*7+3;
  for (I j=0; j<r; j++) for (I i=0; i<n; i++) s += cr_nl_bytes_byte_at((I)(uintptr_t)data, i&31);
  return (I)s;
}
