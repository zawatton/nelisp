/* vector-sum: sum the fixnum elements of a simple vector; a non-fixnum
   element gives -1 (CHECK_FIXNUM in GNU's C).  */
#include "harness.h"

#ifdef REPR_GNU
#define FIXNUMP(x) (((x) & 3) == 2)
#define XFIXNUM(x) ((long) (x) >> 2)
struct Lisp_Vector { long size; uintptr_t contents[]; };

CK_KERNEL long
ck_kernel (uintptr_t v)
{
  struct Lisp_Vector *xv = (struct Lisp_Vector *) (v - 5);
  long n = xv->size, s = 0;  /* ASIZE */
  for (long i = 0; i < n; i++)
    {
      uintptr_t e = xv->contents[i];  /* AREF */
      if (!FIXNUMP (e))
        return -1;
      s += XFIXNUM (e);
    }
  return s;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  struct Lisp_Vector *xv = ck_mmap (8 * m + 64);
  for (long k = 0; k < m; k++)
    xv->contents[k] = ((uintptr_t) (k & 1023) << 2) + 2;
  xv->size = n;
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel ((uintptr_t) xv + 5);
  return res;
}
#else
#define FIXNUMP(x) (((x) & 3) == 1)
#define XFIXNUM(x) ((long) (x) >> 2)

CK_KERNEL long
ck_kernel (uintptr_t v)
{
  uintptr_t nv = *(uintptr_t *) (v + 8);
  const uintptr_t *d = *(uintptr_t **) nv;
  long n = *(long *) (nv + 16), s = 0;
  for (long i = 0; i < n; i++)
    {
      uintptr_t e = d[i];
      if (!FIXNUMP (e))
        return -1;
      s += XFIXNUM (e);
    }
  return s;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  unsigned char *v = ck_mmap (64);
  uintptr_t *nv = ck_mmap (64);
  uintptr_t *data = ck_mmap (8 * m + 64);
  for (long k = 0; k < m; k++)
    data[k] = ((uintptr_t) (k & 1023) << 2) | 1;
  v[0] = 8;
  *(uintptr_t **) (v + 8) = nv;
  nv[0] = (uintptr_t) data;
  nv[1] = m;
  nv[2] = n;
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel ((uintptr_t) v);
  return res;
}
#endif
