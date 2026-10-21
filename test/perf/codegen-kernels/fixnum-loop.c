/* fixnum-loop: sum and compare in a counted loop.  */
#include "harness.h"

#ifdef REPR_GNU
#define XFIXNUM(x) ((long) (x) >> 2)
#define make_fixnum(n) (((uintptr_t) (n) << 2) + 2)
#else
#define XFIXNUM(x) ((long) (x) >> 2)
#define make_fixnum(n) (((uintptr_t) (n) << 2) | 1)
#endif

CK_KERNEL uintptr_t
ck_kernel (uintptr_t nw)
{
  long lim = XFIXNUM (nw), s = 0;
  for (long i = 0; i < lim; i++)
    {
      s += i;
      if (s >= 1000003)
        s -= 1000003;
    }
  return make_fixnum (s);
}

static long
ck_main (long n)
{
  return XFIXNUM (ck_kernel (make_fixnum (n)));
}
