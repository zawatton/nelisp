/* Measurement instrumentation, adapted from regex-native-client.c at a1caa92c9.
   START authenticates preparation GC; END includes automatic workload GC. */
#include <emacs-module.h>
#include <valgrind/callgrind.h>
int plugin_is_GPL_compatible;
static emacs_value request(emacs_env *env, ptrdiff_t n, emacs_value *a, void *p) {
  (void)n; (void)p;
  int which = env->extract_integer(env, a[0]);
  if (which == 0) {
    CALLGRIND_DUMP_STATS_AT("PCF-START");
    CALLGRIND_ZERO_STATS;
  } else if (which == 1) CALLGRIND_DUMP_STATS_AT("PCF-END");
  else CALLGRIND_DUMP_STATS_AT("PCF-BOOT");
  return env->intern(env, "nil");
}
int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value args[] = {env->intern(env, "pcf-ir-request"),
    env->make_function(env, 1, 1, request, "Callgrind START/zero, END, BOOT.", NULL)};
  env->funcall(env, env->intern(env, "fset"), 2, args);
  return 0;
}
