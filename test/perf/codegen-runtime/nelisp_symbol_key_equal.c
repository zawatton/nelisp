#include "runtime.c"
#include "harness.h"
static I ck_main(I n, I r) {
  _Alignas(8) unsigned char p[256] = {0}, data[32]; U s = 0;
  for (I i=0; i<32; i++) data[i] = 65+(i&15);
  for (I i=0; i<8; i++) { unsigned char *a=p+i*32; a[0]=4; *(I*)(a+8)=42; *(I*)(a+16)=(I)(uintptr_t)data; *(I*)(a+24)=16; }
  p[64]=16; p[96]=16; p[128]=16; p[160]=16; *(I*)(p+168)=43;
  p[192]=5; p[224]=5; *(I*)(p+240)=(I)(uintptr_t)(data+16);
  for (I j=0; j<r; j++) for (I i=0; i<n; i++) { I a=(I)(uintptr_t)(p+(i&3)*64); s += cr_nelisp_symbol_key_equal(a,a+32); }
  return (I)s;
}
