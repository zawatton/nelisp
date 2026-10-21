#include "runtime.c"
#include "harness.h"
static I ck_main(I n, I r) {
  unsigned char data[32]; U s = 0;
  for (I i=0; i<32; i++) data[i] = i*7+3;
  for (I j=0; j<r; j++) for (I i=0; i<n; i++) s += cr_nelisp_fnv1a_step4(2166136261, (I)(uintptr_t)data, 0, 16+(i&3), 4294967295);
  return (I)s;
}
