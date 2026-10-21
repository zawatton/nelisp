#!/usr/bin/env python3
"""Where does one evaluation of FORM allocate?  A bounded allocation-site map.

  callpath-alloc-sites.py BINARY MODE FORM [--control FORM] [--setup FILE] [--depth N] [--top K]

Runs BINARY under gdb (8 GiB cap) with MODE (interpreter: NELISP_FX_DISABLE=1;
vm or bytecode: the FX VM enabled; bytecode needs FORM's functions loaded by
--setup).  A function whose body is FORM is called twice to warm caches; the
third call runs between `nelisp--debug-switch' 24 and 25, which gate the
allocator's diagnostic counters, and every `nl_alloc_bytes' call made while
the gate is open is backtraced.  The same run with FORM replaced by the
control (nil by default) gives the printed counts: FORM's run minus the
control's, per caller chain (frames 1..N), so the gate's own calls cancel
out.  For VM or bytecode code, compare a fixture loop with its empty loop,
e.g. FORM (pcf-funcall 3) with --control (pcf-while 3).

Use it to find a call path's per-call allocation sites before changing it,
and to confirm a change removed them (callpath-perf-s63.py alloc measures
the per-call counts against GNU).
"""
import argparse
import collections
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("binary")
    ap.add_argument("mode", choices=("interpreter", "vm", "bytecode"))
    ap.add_argument("form")
    ap.add_argument("--control", default="nil", help="the control FORM (default nil)")
    ap.add_argument("--setup", help="file loaded first (fixture functions)")
    ap.add_argument("--depth", type=int, default=6)
    ap.add_argument("--top", type=int, default=12)
    args = ap.parse_args()
    tmp = Path(tempfile.mkdtemp(prefix="alloc-sites-", dir=ROOT / "target/tmp"))
    runs = {}
    for name, form in (("form", args.form), ("control", args.control)):
        stacks, rc = trace(tmp, name, form, args)
        runs[name] = collections.Counter(" < ".join(s[1:args.depth + 1]) for s in stacks
                                         if "bf_debug_switch_w" not in s)
        print(f"{name}: allocations={sum(runs[name].values())} gdb rc={rc}")
    diff = runs["form"] - runs["control"]
    print(f"FORM minus control: {sum(diff.values())} allocations")
    for chain, n in diff.most_common(args.top):
        print(f"{n:4d}  {chain[:220]}")


def trace(tmp, name, form, args):
    probe = tmp / f"{name}.el"
    probe.write_text(";;; -*- lexical-binding: t -*-\n"
                     + (f"(load {json_str(args.setup)} nil t)\n" if args.setup else "")
                     + f"(defun alloc-sites--form () {form})\n"
                     "(defun alloc-sites--run () (nelisp--debug-switch 24) (alloc-sites--form)"
                     " (nelisp--debug-switch 25))\n"
                     "(alloc-sites--form) (alloc-sites--form) (alloc-sites--run)\n"
                     "(kill-emacs 0)\n")
    script = tmp / "trace.gdb"
    # A hardware watchpoint on the gate arms the allocation breakpoint only
    # while the gate is open: a conditional breakpoint stopped on every
    # allocation of the setup (VM compilation) and took minutes.
    script.write_text("set pagination off\nset print frame-arguments none\nset confirm off\n"
                      "handle SIGSEGV nostop noprint pass\nhandle SIGPIPE nostop noprint pass\n"
                      "watch *(long*)&nl_alloc_diag\ncommands 1\nsilent\n"
                      "if *(long*)&nl_alloc_diag != 0\nenable 2\nelse\ndisable 2\nend\ncont\nend\n"
                      "break nl_alloc_bytes\ndisable 2\n"
                      f"commands 2\nsilent\nbt {args.depth + 1}\ncont\nend\nrun\n")
    env = dict(os.environ, PCF_MODE=args.mode)
    env.pop("NELISP_FX_DISABLE", None)
    if args.mode == "interpreter":
        env["NELISP_FX_DISABLE"] = "1"
    proc = subprocess.run(["bash", "-c", 'ulimit -v 8388608; exec "$@"', "_", "gdb", "-q", "-batch", "-x", str(script),
                           "--args", str(Path(args.binary).resolve()), "--load", str(probe)],
                          cwd=ROOT, env=env, capture_output=True, text=True, timeout=600)
    stacks, cur = [], []
    for line in proc.stdout.splitlines():
        m = re.match(r"#(\d+)\s+(?:0x[0-9a-f]+ in )?(\S+)", line)
        if not m:
            continue
        if m.group(1) == "0" and cur:
            stacks.append(cur)
            cur = []
        cur.append(m.group(2))
    if cur:
        stacks.append(cur)
    return stacks, proc.returncode


def json_str(path):
    return '"' + str(Path(path).resolve()).replace("\\", "\\\\").replace('"', '\\"') + '"'


if __name__ == "__main__":
    main()
