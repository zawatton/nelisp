#include "runtime.c"
#include "harness.h"
#define CR_TOP ((I *)(uintptr_t)402653184)
#define CR_REGISTRY ((I *)(uintptr_t)402653192)
#define CR_REGION ((unsigned char *)(uintptr_t)402653312)
I cr_nl_root_frame_slow(I env, I n) { (void)env; (void)n; exit(82); }
static I ck_main(I n, I r) {
  if (mmap((void *)(uintptr_t)402653184,8192,3,50,-1,0) == MAP_FAILED) exit(83);
  U s = 0;
  for (I j=0; j<r; j++) for (I i=0; i<n; i++) {
    I slots=i&7; *CR_TOP=(I)(uintptr_t)CR_REGION; *CR_REGISTRY=0;
    I p=cr_nl_root_frame(0,slots); s += p-(I)(uintptr_t)CR_REGION;
    for (I k=0; k<slots; k++) s += *(I*)(uintptr_t)(p+k*8);
  }
  return (I)s;
}
