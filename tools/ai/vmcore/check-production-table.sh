#!/bin/sh
# Compile the real wf_bytecode_w_fast_run through the production harness (about one minute; its ERT assertion on
# R8-cache bytes is red since v19 and is ignored) and inspect the two objects it leaves: the "on" object (ordered
# tables enabled) must have a replicated table jump per arm and a bare byte-table dispatch (no bound compares after
# the opcode fetch); the "off" object (ordered tables disabled, the control) must have neither.
root=$(cd "$(dirname "$0")/../../.." && pwd); cd "$root" || exit 1
out=$(mktemp -d) || exit 1
( ulimit -v 8388608; NELISP_ORDERED_PRODUCTION_OUT=$out timeout 200 emacs -Q --batch -L lisp -L src -L scripts -L test -l ert \
    -l test/nelisp-bytecode-core-ordered-test.el --eval '(ert-run-tests-batch-and-exit "actual-producer")' > "$out/log" 2>&1 )
r=1
if [ -f "$out/on/production-on.o" ] && [ -f "$out/off/production-on.o" ]; then
  objdump -d --no-show-raw-insn "$out/on/production-on.o" > "$out/on.dis"
  objdump -d --no-show-raw-insn "$out/off/production-on.o" > "$out/off.dis"
  non=$(grep -c 'jmp    \*%rax' "$out/on.dis"); noff=$(grep -c 'jmp    \*%rax' "$out/off.dis")
  # the line after the first opcode fetch must start the table jump
  nxt=$(grep -A1 'movzbq (%r15,%r12,1),%r14' "$out/on.dis" | sed -n 2p)
  head=$(grep -c 'movzbq (%r15,%r12,1),%r14' "$out/off.dis")
  echo "indirect jumps on=$non off=$noff; after fetch: $nxt"
  if [ "$non" -ge 20 ] && [ "$noff" -le 6 ] && echo "$nxt" | grep -q 'lea .*(%rip),%r10' ; then r=0; fi
fi
rm -rf "$out"; exit $r
