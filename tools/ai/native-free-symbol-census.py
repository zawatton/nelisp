#!/usr/bin/env python3
"""Find native functions that compiled a free symbol as a dynamic lookup.

The AOT compiler gives every ordinary native defun hidden boundary slots
(`nelisp-aot-compiler--object-hidden-boundary-symbols': out mirror frames
name_slot ...).  When those exist, a free symbol in value position does not
fail the build: `nelisp-aot-compiler--parse-value' lowers it to

  (seq (sexp-write-symbol-lit name_slot "NAME")
       (extern-call nelisp_env_lookup_value mirror frames name_slot out)
       out)

and the hidden slots are uninitialized in runtime helpers.  A misplaced
paren that moves a `let*' variable out of scope therefore builds cleanly and
crashes at run time (S4.2 `bf_env_capture_roots_w', final2: SIGSEGV writing
the symbol into name_slot = 0x301).

The emitted pattern is distinctive: the name bytes are pushed on the stack,
`mov %rsp,%rdi' passes them to `nl_alloc_symbol', and the next call is
`nelisp_env_lookup_value'.  Runtime sources had no such site before S4.2.
Only function names and counts are printed.

  native-free-symbol-census.py count BIN [BIN ...]
      sites per binary; later binaries are compared with the first
  native-free-symbol-census.py check BIN --max N
      exit 0 iff BIN has at most N sites
"""
import re
import subprocess
import sys

FUNC = re.compile(r'^[0-9a-f]+ <([^>]+)>:$')
CALL = re.compile(r'\scall\s+[0-9a-f]+ <([^>+]+)>')


def scan(binary):
    """Return {function: sites} for the free-symbol lookup pattern."""
    proc = subprocess.Popen(["objdump", "-d", "--no-show-raw-insn", binary],
                            stdout=subprocess.PIPE, text=True, errors="replace")
    hits = {}
    fn = None
    stack_name = False  # `mov %rsp,%rdi' since the last call
    pending = False     # previous call: nl_alloc_symbol of stack bytes
    for line in proc.stdout:
        m = FUNC.match(line)
        if m:
            fn, stack_name, pending = m.group(1), False, False
            continue
        if 'mov    %rsp,%rdi' in line:
            stack_name = True
            continue
        c = CALL.search(line)
        if c:
            target = c.group(1)
            if pending and target == 'nelisp_env_lookup_value':
                hits[fn] = hits.get(fn, 0) + 1
            pending = target == 'nl_alloc_symbol' and stack_name
            stack_name = False
    if proc.wait() != 0:
        raise SystemExit(f"objdump failed on {binary}")
    return hits


def describe(hits, limit=20):
    names = sorted(hits)
    more = f" (+{len(names) - limit} more)" if len(names) > limit else ""
    return " ".join(names[:limit]) + more


def main(argv):
    if len(argv) >= 2 and argv[0] == "count":
        results = [scan(b) for b in argv[1:]]
        for binary, hits in zip(argv[1:], results):
            print(f"FREE-SYMBOL {binary}: functions={len(hits)} "
                  f"sites={sum(hits.values())} {describe(hits)}".rstrip())
        for binary, hits in zip(argv[2:], results[1:]):
            added = {k: v for k, v in hits.items() if k not in results[0]}
            removed = {k: v for k, v in results[0].items() if k not in hits}
            print(f"FREE-SYMBOL {binary} vs {argv[1]}: "
                  f"added=[{describe(added)}] removed=[{describe(removed)}]")
        return 0
    if len(argv) == 4 and argv[0] == "check" and argv[2] == "--max":
        hits = scan(argv[1])
        sites = sum(hits.values())
        verdict = "PASS" if sites <= int(argv[3]) else "FAIL"
        print(f"FREE-SYMBOL {verdict} {argv[1]}: sites={sites} max={argv[3]} "
              f"{describe(hits)}".rstrip())
        return 0 if verdict == "PASS" else 1
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
