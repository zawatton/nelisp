/* class-table: a byte through a 256-entry table per character, as the
   regex fastmap skip (regex-emacs.c re_search_2) does.  */
#include "harness.h"

#ifdef REPR_GNU
struct Lisp_String { long size, size_byte; void *intervals; unsigned char *data; };

CK_KERNEL long
ck_kernel (uintptr_t s, const unsigned char *table)
{
  struct Lisp_String *xs = (struct Lisp_String *) (s - 4);
  const unsigned char *p = xs->data;
  long n = xs->size_byte < 0 ? xs->size : xs->size_byte;
  long i = 0;
  while (i < n && !table[p[i]])
    i++;
  return i;
}

static long
ck_main (long n)
{
  long m = 100000;
  struct Lisp_String *xs = ck_mmap (64);
  unsigned char *table = ck_mmap (256);
  unsigned char *data = ck_mmap (m + 64);
  for (long i = 0; i < m; i++)
    data[i] = 97;
  table[122] = 1;
  xs->size = n;
  xs->size_byte = -1;
  xs->data = data;
  return ck_kernel ((uintptr_t) xs + 4, table);
}
#else
CK_KERNEL long
ck_kernel (uintptr_t s, const unsigned char *table)
{
  const unsigned char *p = *(unsigned char **) (s + 16);
  long n = *(long *) (s + 24);
  long i = 0;
  while (i < n && !table[p[i]])
    i++;
  return i;
}

static long
ck_main (long n)
{
  long m = 100000;
  unsigned char *s = ck_mmap (64);
  unsigned char *table = ck_mmap (256);
  unsigned char *data = ck_mmap (m + 64);
  for (long i = 0; i < m; i++)
    data[i] = 97;
  table[122] = 1;
  s[0] = 5;
  *(long *) (s + 8) = m + 64;
  *(unsigned char **) (s + 16) = data;
  *(long *) (s + 24) = n;
  return ck_kernel ((uintptr_t) s, table);
}
#endif
