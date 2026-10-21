/* Shared harness for the codegen kernels (compiled with gcc -O2).
   Reads "N [R]" from stdin, runs ck_main (N, R), which builds the data and
   runs the kernel R times, and exits with (result & 127), as
   compile-kernel.el does for the NeLisp versions.
   REPR_GNU lays the data out as GNU Emacs 31.1 does (lisp.h: LSB tags,
   Lisp_Int0 = 2, Lisp_String = 4, Lisp_Vectorlike = 5, Lisp_Cons = 3,
   Qnil = 0); REPR_NL uses NeLisp's word heap (lisp/nelisp-sexp-layout.el).
   Kernels are noipa so GCC compiles them for unknown arguments, as the
   NeLisp compiler does.  */
#include <stdint.h>
#include <unistd.h>
#include <sys/mman.h>

#define CK_KERNEL __attribute__ ((noipa))

static void *
ck_mmap (long size)
{
  return mmap (0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS,
               -1, 0);
}

/* The K-th (0 or 1) decimal number of BUF[0, GOT); a missing second
   number is the repeat count 1.  */
static long
ck_parse (const char *buf, long got, int k)
{
  long n = 0;
  int field = -1, in = 0, found = 0;
  for (long i = 0; i < got; i++)
    if (buf[i] >= '0' && buf[i] <= '9')
      {
        if (!in)
          field++, in = 1;
        if (field == k)
          n = n * 10 + (buf[i] - '0'), found = 1;
      }
    else
      in = 0;
  return found ? n : k == 1 ? 1 : 0;
}

static long ck_main (long n, long r);

int
main (void)
{
  char buf[64];
  long got = read (0, buf, 64);
  return ck_main (ck_parse (buf, got, 0), ck_parse (buf, got, 1)) & 127;
}
