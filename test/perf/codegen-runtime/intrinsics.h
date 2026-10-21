/* C rendering of the raw AOT dialect, not GNU's object representation.
   Keep the tagged-immediate semantics of the AOT compiler. */
#include <stdint.h>
#include <stddef.h>
#include <string.h>
typedef int64_t I;
typedef uint64_t U;
static inline I cr_tag(I v)
{
  return (v & 1) ? ((v & 3) == 1 ? 2 : v == 3 ? 0 : 1)
    : *(uint8_t *)(uintptr_t)v;
}
static inline I cr_streq(I a, I b)
{
  /* emit-string-eq-core's string/symbol payloads: data +16, len +24. */
  I an = *(I *)(uintptr_t)(a + 24), bn = *(I *)(uintptr_t)(b + 24);
  if (an != bn) return 0;
  const uint8_t *ap = *(const uint8_t **)(uintptr_t)(a + 16);
  const uint8_t *bp = *(const uint8_t **)(uintptr_t)(b + 16);
  for (I i = 0; i < an; ++i) if (ap[i] != bp[i]) return 0;
  return 1;
}
