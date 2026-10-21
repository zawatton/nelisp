/* Shared harness for the codegen kernels (compiled with gcc -O2).
   Reads the iteration count N from stdin, runs ck_main (N) and exits with
   (result & 127), as compile-kernel.el does for the NeLisp versions.
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

static long
ck_read_n (void)
{
  char buf[64];
  long got = read (0, buf, 64), n = 0;
  for (long i = 0; i < got; i++)
    if (buf[i] >= '0' && buf[i] <= '9')
      n = n * 10 + (buf[i] - '0');
  return n;
}

static long ck_main (long n);

int
main (void)
{
  return ck_main (ck_read_n ()) & 127;
}
