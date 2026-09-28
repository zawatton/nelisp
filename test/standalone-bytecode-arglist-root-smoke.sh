#!/usr/bin/env bash
set -euo pipefail

binary=${1:-target/nelisp}
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

actual=$("$binary" --eval '(progn
  (garbage-collect)
  (let* ((direct (ash 1 2))
         (gc-while-evaluating-second-arg
          (ash (progn (garbage-collect) 3)
               (progn (garbage-collect) 4)))
         (gc-between-three-args
          (list (progn (garbage-collect) 1)
                (progn (garbage-collect) 2)
                (progn (garbage-collect) 3)))
         (gc-before-next-call (progn (garbage-collect) (ash 1 2))))
    (list direct gc-while-evaluating-second-arg
          gc-between-three-args gc-before-next-call)))')
actual_result=${actual##*$'\n'}
if [[ "$actual_result" != '(4 48 (1 2 3) 4)' ]]; then
  echo "standalone-bytecode-arglist-root-smoke: incomplete argument list after GC: $actual_result" >&2
  exit 1
fi

echo "standalone-bytecode-arglist-root-smoke: PASS (ash arity 2 and list arity 3 survive forced GC)"

host_gc_arg=$(emacs --batch -Q --eval \
  '(prin1 (ash 1 (progn (garbage-collect) -8)))' 2>/dev/null)
standalone_gc_arg=$("$binary" --eval \
  '(ash 1 (progn (garbage-collect) -8))')
standalone_gc_arg=${standalone_gc_arg##*$'\n'}
if [[ "$host_gc_arg" != 0 || "$standalone_gc_arg" != "$host_gc_arg" ]]; then
  echo "standalone-bytecode-arglist-root-smoke: Host/standalone GC arg mismatch: host=$host_gc_arg standalone=$standalone_gc_arg" >&2
  exit 1
fi
echo "standalone-bytecode-arglist-root-smoke: PASS (Host/standalone two-arg GC parity)"
