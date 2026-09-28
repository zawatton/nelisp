/* Test-only raw callback adapter for the GNU .eln callback smoke.
   This fixture is single-threaded; context nesting is LIFO and does not
   claim process or worker-thread safety. */
#include <stdint.h>

typedef intptr_t Lisp_Object;
typedef int64_t (*gateway_fn)(uint64_t, uint64_t, uint64_t,
                              int64_t, int64_t, uint64_t);

struct sexp_slot {
  uint64_t tag;
  uint64_t a;
  uint64_t b;
  uint64_t c;
};

struct callback_context {
  gateway_fn gateway;
  uint64_t env;
  uint64_t function_slot;
  struct sexp_slot *args;
  struct sexp_slot *out;
  int64_t argc;
  int64_t status;
};

#define CONTEXT_LIMIT 8
static struct callback_context contexts[CONTEXT_LIMIT];
static uint64_t context_depth;

uint64_t nelisp_p5_callback_context_push(uint64_t gateway, uint64_t env,
                                         uint64_t function_slot,
                                         uint64_t args, uint64_t out,
                                         int64_t argc) {
  struct callback_context *ctx;
  if (context_depth >= CONTEXT_LIMIT || gateway == 0 || env == 0 ||
      function_slot == 0 || args == 0 || out == 0 || argc < 0 || argc > 2)
    return 0;
  ctx = &contexts[context_depth];
  ctx->gateway = (gateway_fn)(uintptr_t)gateway;
  ctx->env = env;
  ctx->function_slot = function_slot;
  ctx->args = (struct sexp_slot *)(uintptr_t)args;
  ctx->out = (struct sexp_slot *)(uintptr_t)out;
  ctx->argc = argc;
  ctx->status = -1;
  context_depth++;
  return context_depth;
}

uint64_t nelisp_p5_callback_context_status(uint64_t token) {
  if (token == 0 || token != context_depth)
    return UINT64_MAX;
  return (uint64_t)contexts[token - 1].status;
}

uint64_t nelisp_p5_callback_context_pop(uint64_t token) {
  struct callback_context *ctx;
  if (token == 0 || token != context_depth)
    return 0;
  context_depth--;
  ctx = &contexts[context_depth];
  ctx->gateway = 0;
  ctx->env = 0;
  ctx->function_slot = 0;
  ctx->args = 0;
  ctx->out = 0;
  ctx->argc = 0;
  ctx->status = -1;
  return 1;
}

/* This exact adapter handles only GNU Lisp fixnums and only for a fixture.
   The runtime call itself goes through the checked-root gateway. */
Lisp_Object nelisp_eln_random_callback(Lisp_Object limit) {
  struct callback_context *ctx;
  int64_t value;
  int64_t rc;
  if (context_depth == 0)
    return limit;
  ctx = &contexts[context_depth - 1];
  if ((((uint64_t)limit) & 3u) != 2u) {
    ctx->status = 2;
    return limit;
  }
  value = (int64_t)(limit >> 2);
  ctx->args[0].tag = 2;
  ctx->args[0].a = (uint64_t)value;
  ctx->args[0].b = 0;
  ctx->args[0].c = 0;
  rc = ctx->gateway(ctx->env, ctx->function_slot,
                    (uint64_t)(uintptr_t)ctx->args, 0, ctx->argc,
                    (uint64_t)(uintptr_t)ctx->out);
  ctx->status = rc;
  if (rc != 0 || ctx->out->tag != 2)
    return limit;
  return (Lisp_Object)((ctx->out->a << 2) | 2u);
}
