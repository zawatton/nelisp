#!/usr/bin/env python3
"""Word-heap S6.3 on the call path: evidence for ledger stage S4.

  callpath-perf-s63.py census CENSUS.tsv BINARY STAGE
      Record the box census that a fresh build of the current source wrote
      with NELISP_BOX_CENSUS=CENSUS.tsv (BINARY is that build, STAGE the
      source snapshot it was built from) as target/callpath-perf/s63/census.json.
      `callpath-perf-check.py s63census' recounts it over SCOPE:
      nl_root_reserve sites (S4.1) and the clone_into, wf_copy32 and *_ptr
      materialisers (S4.2).
  callpath-perf-s63.py alloc BINARY [GNU]
      Allocations per call (S4.3) of the wall-clock floors fixture's
      pcf-funcall, pcf-builtin and pcf-apply over its empty loop pcf-while,
      in each mode.  NeLisp counts every allocation with the allocator's
      diagnostic counters (`nelisp--debug-switch' 24 enables and resets
      them, 25 disables them; elements 10, 11 and 12 of its result are the
      bucket, linear and bump allocations).  GNU 31.1 counts objects with
      `memory-use-counts' on the same fixture; its "vm" row repeats the
      interpreter, as in the wall-clock criteria.  A loop that allocates a
      vector per iteration is the control that the counters see
      allocations.  Writes target/callpath-perf/s63/alloc.json.

SCOPE is gc-port's CALL_PATH boundary (tools/ai/word-heap-stage-check.py
on eval/gc-port-20261021 at 9d22b894b) with every wf_bytecode_* function,
plus the call-path functions that had no root site when that boundary was
drawn: eval_sub's argument evaluation, specbind/unbind_to, the function-cell
read, and the Ffuncall entry and record_in_backtrace activations.  GNU
funcall_subr, funcall_lambda and exec_byte_code keep arguments in a stack
argv and register no roots, so such a call allocates nothing.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf/s63"
SCOPE = re.compile(
    r"^(nelisp_apply|nl_apply|bf_apply|nl_ali_|nl_funcall|nl_eic_|nl_n3_eval|nl_n4_frame_eval|m5_fx_|nl_fx_"
    r"|nl_native_frame|wf_bytecode_|bf_n2_call_lisp|bf_n2_direct_arity"
    r"|nl_eval_arg_list|nl_sb_|nelisp_env_lookup_function|nl_n4_(apply|direct_apply|named_apply|push|finish))")
# Every head that reserves 32-byte Sexp root slots (the build census counts
# them all); a WORD frame, `nl_root_frame', is the replacement, not debt.
ROOTS = ("nl_root_reserve", "nl_root_reserve_checked", "wf_bytecode_w_roots",
         "wf_bytecode_call_root_frame", "m5_fx_root_block")
MATERIALISERS = ("clone_into", "wf_copy32", "nl_cons_car_ptr", "nl_cons_cdr_ptr", "record-slot-ref-ptr",
                 "vector-ref-ptr", "nl_vector_slot_ptr", "nl_record_slot_ptr")
OPS = ("pcf-funcall", "pcf-builtin", "pcf-apply")
CONTROL = ("s63-plain", "s63-vector")
MODES = ("interpreter", "vm", "bytecode")
N = 20000
GNU = "/usr/local/bin/emacs"


def load_tool(name):
    spec = importlib.util.spec_from_file_location(name.replace("-", "_").removesuffix(".py"), ROOT / "tools/ai" / name)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def source_identity(root=ROOT):
    return load_tool("callpath-perf-measure.py").source_identity(root)


def census_counts(tsv):
    """Debt over SCOPE, read the way box-census.py reads a census."""
    units = load_tool("box-census.py").load(tsv)
    counts = {"units": len(units), "scope_defuns": 0, "roots": 0, "materialisers": 0, "per_defun": {}}
    scoped = set()
    for rows in units.values():
        for fn, idiom, n in rows:
            if not SCOPE.match(fn):
                continue
            scoped.add(fn)
            kind = "roots" if idiom in ROOTS else "materialisers" if idiom in MATERIALISERS else None
            if kind:
                counts[kind] += n
                row = counts["per_defun"].setdefault(fn, {})
                row[idiom] = row.get(idiom, 0) + n
    counts["scope_defuns"] = len(scoped)
    return counts


def census(tsv, binary, stage):
    OUT.mkdir(parents=True, exist_ok=True)
    report = {"schema": 1, "source": source_identity(), "stage_source": source_identity(stage),
              "census": pin(tsv), "binary": pin(binary), "producer": pin(__file__),
              "scope": SCOPE.pattern, "counts": census_counts(tsv)}
    (OUT / "census.json").write_text(json.dumps(report, indent=1) + "\n")
    c = report["counts"]
    print(f"census units={c['units']} scope_defuns={c['scope_defuns']} roots={c['roots']} "
          f"materialisers={c['materialisers']} stage_matches={report['stage_source'] == report['source']}")


CONTROLS = r'''
(defun s63-plain (n) (let ((i 0) (acc nil)) (while (< i n) (setq acc i) (setq i (1+ i))) acc))
(defun s63-vector (n) (let ((i 0) (acc nil)) (while (< i n) (setq acc (make-vector 3 i)) (setq i (1+ i))) acc))
'''


def nelisp_form(n):
    ops = " ".join(("pcf-while",) + OPS + CONTROL)
    return ("(progn " + CONTROLS +
            "(defun s63-count () (let ((s (nelisp--debug-switch 0))) (+ (nth 10 s) (nth 11 s) (nth 12 s)))) "
            "(let ((n %d) (ops '(%s)) (res nil)) "
            "(dolist (op ops) (funcall op 2000)) "
            "(nelisp--debug-switch 24) "
            "(dolist (op ops) (let ((a (s63-count))) (funcall op n) "
            "(setq res (cons (format \"%%s=%%d\" op (- (s63-count) a)) res)))) "
            "(nelisp--debug-switch 25) "
            "(princ (format \"S63-ALLOC %%s\\n\" (mapconcat (function identity) (nreverse res) \" \")))))"
            % (n, ops))


def gnu_form(n):
    ops = " ".join(("pcf-while",) + OPS + CONTROL)
    # memory-use-counts: conses floats vector-cells symbols string-chars intervals strings.
    return ("(progn " + CONTROLS +
            "(let ((n %d) (ops '(%s)) (res nil)) "
            "(dolist (op ops) (funcall op 2000)) "
            "(dolist (op ops) (let ((a (memory-use-counts))) (funcall op n) "
            "(let ((b (memory-use-counts)) (d nil)) "
            "(while b (setq d (cons (- (car b) (car a)) d) a (cdr a) b (cdr b))) "
            "(setq res (cons (format \"%%s=%%s\" op (mapconcat (function number-to-string) (nreverse d) \",\")) res))))) "
            "(princ (format \"S63-ALLOC %%s\\n\" (mapconcat (function identity) (nreverse res) \" \")))))"
            % (n, ops))


def run(engine, binary, mode, setup, form):
    env = dict(os.environ, PCF_MODE=mode, LC_ALL="C", TMPDIR=str(ROOT / "target/tmp"),
               XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
    env.pop("NELISP_FX_DISABLE", None)
    if mode == "interpreter" and engine == "nelisp":
        env["NELISP_FX_DISABLE"] = "1"
    argv = [str(binary)] + (["-Q", "--batch"] if engine == "gnu" else []) + ["--load", str(setup), "--eval", form]
    proc = subprocess.run(["bash", "-c", 'ulimit -v 8388608; exec "$@"', "_"] + argv,
                          cwd=ROOT, env=env, capture_output=True, timeout=900)
    match = re.search(rb"S63-ALLOC (.*)\n", proc.stdout)
    raw = dict(item.split("=", 1) for item in match.group(1).decode().split()) if match else {}
    return {"returncode": proc.returncode, "raw": raw, "stderr_tail": proc.stderr[-300:].decode(errors="replace")}


def per_call(raw, engine, n):
    """Allocations (NeLisp) or objects (GNU: every count but vector cells) per iteration over pcf-while."""
    def count(op):
        if engine == "nelisp":
            return int(raw[op])
        c = [int(x) for x in raw[op].split(",")]
        return c[0] + c[1] + c[3] + c[5] + c[6]
    rows = {op: (count(op) - count("pcf-while")) / n for op in OPS}
    rows["control"] = (count("s63-vector") - count("s63-plain")) / n
    if engine == "gnu":
        cells = lambda op: int(raw[op].split(",")[2])
        rows["vector_cells"] = {op: cells(op) - cells("pcf-while") for op in OPS}
        # GNU counts a vector in vector cells, not as an object.
        rows["control"] = (cells("s63-vector") - cells("s63-plain")) / n
    return rows


def alloc(binary, gnu=GNU):
    OUT.mkdir(parents=True, exist_ok=True)
    setup, fixture_pins = load_tool("callpath-perf-wall.py").fixture("floors")
    report = {"schema": 1, "source": source_identity(), "binary": pin(binary), "gnu": pin(gnu),
              "producer": pin(__file__), "fixture": fixture_pins, "n": N, "ops": OPS, "modes": {}}
    for mode in MODES:
        row = {}
        for engine, path, form in (("nelisp", binary, nelisp_form(N)), ("gnu", gnu, gnu_form(N))):
            result = run(engine, path, mode, setup, form)
            result["per_call"] = per_call(result["raw"], engine, N) if result["returncode"] == 0 else None
            row[engine] = result
        report["modes"][mode] = row
        print(mode, {e: row[e]["per_call"] for e in ("nelisp", "gnu")}, flush=True)
    if report["source"] != source_identity():
        raise SystemExit("source changed during the allocation probe")
    (OUT / "alloc.json").write_text(json.dumps(report, indent=1) + "\n")


if __name__ == "__main__":
    if len(sys.argv) == 5 and sys.argv[1] == "census":
        census(*sys.argv[2:5])
    elif len(sys.argv) in (3, 4) and sys.argv[1] == "alloc":
        alloc(*sys.argv[2:])
    else:
        raise SystemExit(__doc__)
