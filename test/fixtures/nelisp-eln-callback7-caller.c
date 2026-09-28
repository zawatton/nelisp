/* SysV callers for the unary and seven-word AOT callback entries. */
#include <stdint.h>

typedef uint64_t (*callback7_t)(uint64_t, uint64_t, uint64_t, uint64_t,
                                uint64_t, uint64_t, uint64_t);
typedef uint64_t (*callback1_t)(uint64_t);
typedef uint64_t (*root_mark_t)(uint64_t, uint64_t);

static uint64_t
root_mark(const uint64_t *descriptor)
{
  root_mark_t read_top = (root_mark_t) (uintptr_t) descriptor[9];
  uint64_t out_slot = descriptor[11];
  read_top(descriptor[10], out_slot);
  return ((const uint64_t *) (uintptr_t) out_slot)[1];
}

uint64_t
nelisp_eln_callback7_call(const uint64_t *descriptor)
{
  callback7_t callback = (callback7_t) (uintptr_t) descriptor[0];
  uint64_t before = root_mark(descriptor);
  uint64_t result = callback(descriptor[1], descriptor[2], descriptor[3],
                             descriptor[4], descriptor[5], descriptor[6],
                             descriptor[7]);
  uint64_t after = root_mark(descriptor);
  ((uint64_t *) descriptor)[8] = result;
  ((uint64_t *) descriptor)[12] = before;
  ((uint64_t *) descriptor)[13] = after;
  return result;
}

uint64_t
nelisp_eln_callback1_call(const uint64_t *descriptor)
{
  callback1_t callback = (callback1_t) (uintptr_t) descriptor[0];
  uint64_t result = callback(descriptor[1]);
  ((uint64_t *) descriptor)[8] = result;
  return result;
}
