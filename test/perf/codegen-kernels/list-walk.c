/* list-walk: length of a proper list.  */
#include "harness.h"

#ifdef REPR_GNU
#define CONSP(x) ((((x) - 3) & 7) == 0)
#define XCDR(x) (*(uintptr_t *) ((x) - 3 + 8))

CK_KERNEL long
ck_kernel (uintptr_t tail)
{
  long n = 0;
  while (CONSP (tail))
    {
      tail = XCDR (tail);
      n++;
    }
  return n;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  uintptr_t *cells = ck_mmap (16 * m + 64);
  for (long k = 0; k < m; k++)
    {
      cells[2 * k] = ((uintptr_t) k << 2) + 2;
      cells[2 * k + 1] = k + 1 < m ? (uintptr_t) &cells[2 * (k + 1)] + 3 : 0;
    }
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel (n == 0 ? 0 : (uintptr_t) &cells[2 * (m - n)] + 3);
  return res;
}
#else
CK_KERNEL long
ck_kernel (uintptr_t w)
{
  long n = 0;
  while ((w & 1) == 0 && *(unsigned char *) w == 7)
    {
      w = *(uintptr_t *) (*(uintptr_t *) (w + 8) + 8);
      n++;
    }
  return n;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  unsigned char *boxes = ck_mmap (32 * m + 64);
  unsigned char *cells = ck_mmap (32 * m + 64);
  for (long k = 0; k < m; k++)
    {
      uintptr_t next = k + 1 < m ? (uintptr_t) (boxes + 32 * (k + 1)) : 3;
      boxes[32 * k] = 7;
      *(uintptr_t *) (boxes + 32 * k + 8) = (uintptr_t) (cells + 32 * k);
      *(uintptr_t *) (cells + 32 * k) = ((uintptr_t) k << 2) | 1;
      *(uintptr_t *) (cells + 32 * k + 8) = next;
    }
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel (n == 0 ? 3 : (uintptr_t) (boxes + 32 * (m - n)));
  return res;
}
#endif
