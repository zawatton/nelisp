/* byte-scan: find a byte in a unibyte string (memchr-style loop).  */
#include "harness.h"

#ifdef REPR_GNU
struct Lisp_String { long size, size_byte; void *intervals; unsigned char *data; };

CK_KERNEL long
ck_kernel (uintptr_t s, long c)
{
  struct Lisp_String *xs = (struct Lisp_String *) (s - 4);
  const unsigned char *p = xs->data;
  long n = xs->size_byte < 0 ? xs->size : xs->size_byte;  /* SBYTES */
  long i = 0;
  while (i < n && p[i] != c)
    i++;
  return i;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  struct Lisp_String *xs = ck_mmap (64);
  unsigned char *data = ck_mmap (m + 64);
  for (long i = 0; i < m; i++)
    data[i] = 97;
  xs->size = n;
  xs->size_byte = -1;
  xs->data = data;
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel ((uintptr_t) xs + 4, 122);
  return res;
}
#else
CK_KERNEL long
ck_kernel (uintptr_t s, long c)
{
  const unsigned char *p = *(unsigned char **) (s + 16);
  long n = *(long *) (s + 24);
  long i = 0;
  while (i < n && p[i] != c)
    i++;
  return i;
}

static long
ck_main (long n, long r)
{
  long m = 100000;
  unsigned char *s = ck_mmap (64);
  unsigned char *data = ck_mmap (m + 64);
  for (long i = 0; i < m; i++)
    data[i] = 97;
  s[0] = 5;
  *(long *) (s + 8) = m + 64;
  *(unsigned char **) (s + 16) = data;
  *(long *) (s + 24) = n;
  long res = 0;
  for (long j = 0; j < r; j++)
    res = ck_kernel ((uintptr_t) s, 122);
  return res;
}
#endif
