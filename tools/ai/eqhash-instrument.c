/* Measurement-only client request; no NeLisp runtime primitive is added.
   Build from the repository root:
   cc -shared -fPIC -DEQHASH_GNU tools/ai/eqhash-instrument.c -o target/eqhash/instrument.so
   cc -c -O2 -fno-pie -fno-stack-protector -fno-asynchronous-unwind-tables tools/ai/eqhash-instrument.c -o target/eqhash/instrument.o
   objcopy --only-section=.text -O binary target/eqhash/instrument.o target/eqhash/instrument.bin
 */
#include <valgrind/callgrind.h>

__attribute__((noinline)) int eqhash_profile_begin(void)
{
    CALLGRIND_START_INSTRUMENTATION;
    return 0;
}

#ifdef EQHASH_GNU
#include <emacs-module.h>

int plugin_is_GPL_compatible;

static emacs_value profile_start(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data)
{
    (void)nargs;
    (void)args;
    (void)data;
    eqhash_profile_begin();
    return env->intern(env, "nil");
}

int emacs_module_init(struct emacs_runtime *runtime)
{
    emacs_env *env = runtime->get_environment(runtime);
    emacs_value args[] = {
        env->intern(env, "eqhash-profile-start"),
        env->make_function(env, 0, 0, profile_start,
                           "Start Callgrind instrumentation.", NULL)
    };
    env->funcall(env, env->intern(env, "fset"), 2, args);
    return 0;
}
#endif
