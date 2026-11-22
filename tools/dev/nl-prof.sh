#!/usr/bin/env bash
# nl-prof.sh FORM-FILE [--prefix P]... [--functions F...] [--runs N] [--warmup N] [--top K] [--setup FILE]
#            [--log-slots N] [--bin PATH] [--dump FILE] [--cpus LIST] [--timeout SECS]
# Profile the forms in FORM-FILE in the emacs-lib runtime image booted from a flat dump (0.03 s boot).
# One run -> calls, inclusive/exclusive ns, ns/call, top caller->callee edges.  See tools/dev/README.md.
# Env: NL_PROF_BIN (default REPO/target/nelisp), NL_PROF_DUMP (default $NEMACS_RUNTIME_DUMP or
# REPO/build/nemacs-runtime.dump), NELISP_HOME, NL_PROF_CPUS (default 9,25).
set -u
here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../.." && pwd)
usage() { sed -n '2,8p' "$0" >&2; exit 2; }
[ $# -ge 1 ] || usage
form=$1; shift
[ -f "$form" ] || { echo "nl-prof: no such form file: $form" >&2; exit 2; }
bin=${NL_PROF_BIN:-$repo/target/nelisp}
dump=${NL_PROF_DUMP:-${NEMACS_RUNTIME_DUMP:-$repo/build/nemacs-runtime.dump}}
cpus=${NL_PROF_CPUS:-9,25}
tmo=300
runs=100; warm=2; top=30; setup=""; slots=""
prefixes=""; funcs=""
q() { printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'; }
mode=""
while [ $# -gt 0 ]; do
  case $1 in
    --prefix) prefixes="$prefixes \"$(q "$2")\""; shift 2; mode="" ;;
    --functions) mode=f; shift ;;
    --runs) runs=$2; shift 2; mode="" ;;
    --warmup) warm=$2; shift 2; mode="" ;;
    --top) top=$2; shift 2; mode="" ;;
    --setup) setup=$2; shift 2; mode="" ;;
    --log-slots) slots=$2; shift 2; mode="" ;;
    --bin) bin=$2; shift 2; mode="" ;;
    --dump) dump=$2; shift 2; mode="" ;;
    --cpus) cpus=$2; shift 2; mode="" ;;
    --timeout) tmo=$2; shift 2; mode="" ;;
    -*) echo "nl-prof: unknown option $1" >&2; usage ;;
    *) if [ "$mode" = f ]; then
         case $1 in *[[:space:]\(\)\'\"\\\;]*) echo "nl-prof: bad function name: $1" >&2; exit 2;; esac
         funcs="$funcs $1"; shift
       else echo "nl-prof: unexpected argument $1" >&2; usage; fi ;;
  esac
done
[ -x "$bin" ] || { echo "nl-prof: binary not found: $bin (set NL_PROF_BIN)" >&2; exit 2; }
[ -f "$dump" ] || { echo "nl-prof: dump not found: $dump (set NL_PROF_DUMP; bake: mk/emacs-lib.mk bake-runtime-dump)" >&2; exit 2; }
abs() { case $1 in /*) printf '%s' "$1";; *) printf '%s/%s' "$PWD" "$1";; esac; }
opts="(:runs $runs :warmup $warm :top $top :prefixes ($prefixes) :functions ($funcs)"
[ -n "$setup" ] && opts="$opts :setup-file \"$(q "$(abs "$setup")")\""
[ -n "$slots" ] && opts="$opts :log-slots $slots"
opts="$opts)"
ev="(progn (nemacs-runtime-dump-reinit) (load \"$(q "$here/nl-prof.el")\" nil t t) (nl-prof-main \"$(q "$(abs "$form")")\" '$opts))"
# ulimit cap, timeout, closed stdin, pinned CPUs (never raise the cap; do not use CPUs of other agents)
ulimit -v 8388608
exec timeout "$tmo" taskset -c "$cpus" "$bin" --cold-load-from "$dump" --eval "$ev" < /dev/null
