#!/usr/bin/env python3
"""Pinned S5 runtime code quality study.

study: census exact runtime bodies compiled by NeLisp and gcc -O2; also
       require measured dynamic Ir and attribution for all ten functions.
build NAME: build one executable fixture (no timing).
wall NAME: seven paired alternating rounds; wall ratio <= 1.5 is the gate.
ir NAME: N-minus-zero callgrind diagnostic (never a speed gate).
endtoend: real candidate/base/GNU bytecode workloads; candidate/base <= 1.03.
selfcheck: one deliberately wrong result must be rejected.

Sources are pinned in test/perf/codegen-runtime, never read from a live
callpath tree. CODEGEN_EMACS, CODEGEN_CC, CODEGEN_CPU, CODEGEN_RUNTIME_OUT,
CODEGEN_RUNTIME_BASE, CODEGEN_RUNTIME_BIN and CODEGEN_RUNTIME_GNU
override checkout-relative defaults. Build/test children have an 8 GiB cap.
"""
import argparse
import bisect
import collections
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import shutil
import statistics
import subprocess
import sys
import time

ROOT = Path(os.environ.get("CODEGEN_RUNTIME_ROOT", Path(__file__).resolve().parents[2]))
SRC = Path(os.environ.get("CODEGEN_RUNTIME_FIXTURES", ROOT / "test/perf/codegen-runtime"))
OUT = Path(os.environ.get("CODEGEN_RUNTIME_OUT", ROOT / "target/codegen/s5/runtime")).resolve()
PINS = json.loads((SRC / "pins.json").read_text())
NAMES = tuple(PINS["names"])
HELPERS = tuple(PINS["helpers"])
N = 10000
MISSING = {
    "nl_alloc_bytes": "needs representative free-list pop/run-carve arena states and verified resets",
    "nelisp_mirror_lookup_entry": "needs rooted key, symbol-cell and hashed-mirror graph fixtures",
    "nelisp_eval_call_w": "needs real evaluator/scope boundary state for both entry paths",
    "nl_gc_sweep_merge_range": "needs live/free/marked arena metadata and independently checked sweep mutations",
    "wf_bytecode_execute_frame": "full frame, handlers, bindings and heap-view dependencies are not independently validated",
}
LIMIT = 8388608 * 1024


def bounded():
    resource.setrlimit(resource.RLIMIT_AS, (LIMIT, LIMIT))


def run(args, **kwargs):
    return subprocess.run([str(x) for x in args], capture_output=True,
                          text=True, check=True, preexec_fn=bounded, **kwargs)


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(name, value):
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / name).write_text(json.dumps(value, indent=2) + "\n")


def cc():
    return os.environ.get("CODEGEN_CC", "gcc-14" if shutil.which("gcc-14") else "gcc")


def emacs_reference():
    command = os.environ.get("CODEGEN_EMACS", os.environ.get("CODEGEN_RUNTIME_GNU", "emacs"))
    binary = shutil.which(command)
    if binary is None:
        raise ValueError("GNU Emacs reference is unavailable: " + command)
    version = run([binary, "--version"]).stdout.splitlines()[0]
    if not re.match(r"^GNU Emacs 31\.1(?:\s|$)", version):
        raise ValueError("GNU Emacs 31.1 reference required, got: " + version)
    save("gnu-reference.json", {"binary": str(Path(binary).resolve()), "sha256": sha(binary), "version": version})
    return binary


def forms(path):
    """Read the deliberately restricted, fully evaluated pinned AOT dialect."""
    tokens = re.findall(r';[^\n]*|"(?:[^"\\]|\\.)*"|[()]|[^\s()]+', path.read_text())
    tokens = iter(t for t in tokens if not t.startswith(";"))

    def read(t):
        if t == "(":
            value = []
            for token in tokens:
                if token == ")":
                    return value
                value.append(read(token))
            raise ValueError("unterminated source")
        if t == ")":
            raise ValueError("unmatched close")
        if re.fullmatch(r"-?\d+", t):
            return int(t)
        if t.startswith('"'):
            raise ValueError("unexpected string in pinned runtime body")
        return t
    return [read(t) for t in tokens]


def cname(s):
    # Prefixing also keeps C keywords (inline, float, ...) legal.
    return "cr_" + re.sub(r"[^a-zA-Z0-9_]", lambda m: "_%02x" % ord(m[0]), s)


def vname(s):
    # Lisp's value and function namespaces are separate. C's are not.
    return "cr_var_" + cname(s)[3:]


def unique_forms(source):
    result, seen = [], {}
    for form in source:
        name = form[1]
        if name in seen:
            if seen[name] != form:
                raise ValueError("conflicting pinned providers for " + name)
            continue
        seen[name] = form
        result.append(form)
    return result


class Lower:
    """Mechanical C expression lowering; unknown calls retain an extern ABI.

    All lexical forms become GNU C statement expressions. Argument evaluation
    is explicitly left to right; neither C's unspecified call-argument order
    nor signed overflow may change the pinned dialect's behavior.
    """
    def __init__(self, source, target=None):
        source = unique_forms(source)
        self.source = source
        for f in source:
            if f[2] == "nil":
                f[2] = []
        self.target = target
        self.defs = {f[1]: f for f in source}
        self.externs = {}
        self.data = set()
        self.serial = 0

    def fresh(self):
        self.serial += 1
        return "cg_tmp_%d" % self.serial

    def sequence(self, xs):
        return "({ " + " ".join(self.expr(x) + ";" for x in xs[:-1]) + " " + self.expr(xs[-1] if xs else 0) + "; })"

    def operands(self, xs, finish):
        ts = [self.fresh() for _ in xs]
        return "({ " + " ".join("I %s = %s;" % (t, self.expr(x)) for t, x in zip(ts, xs)) + " " + finish(ts) + "; })"

    def expr(self, x):
        if isinstance(x, int):
            return "((I)UINT64_C(%d))" % (x % (1 << 64))
        if isinstance(x, str):
            if x.startswith("'"):
                symbol = "literal_" + x[1:]
                self.externs.setdefault(symbol, 0)
                return cname(symbol) + "()"
            return "0" if x == "nil" else "1" if x == "t" else vname(x)
        if not x:
            return "0"
        op, a = x[0], x[1:]
        if op in ("seq", "progn"):
            return self.sequence(a)
        if op == "cond":
            result = "0"
            for clause in reversed(a):
                result = "(%s ? %s : %s)" % (self.expr(clause[0]), self.sequence(clause[1:]), result)
            return result
        if op == "if":
            # The compiler accepts more than one else form, as Elisp does.
            return "(%s ? %s : %s)" % (self.expr(a[0]), self.expr(a[1]), self.sequence(a[2:]))
        if op in ("let", "let*"):
            if op == "let*":
                decl = " ".join("I %s = %s;" % (vname(b[0]), self.expr(b[1])) for b in a[0])
            else:
                ts = [self.fresh() for _ in a[0]]
                decl = " ".join("I %s = %s;" % (t, self.expr(b[1])) for t, b in zip(ts, a[0]))
                decl += " " + " ".join("I %s = %s;" % (vname(b[0]), t) for t, b in zip(ts, a[0]))
            return "({ %s %s; })" % (decl, self.sequence(a[1:]))
        if op == "setq":
            if len(a) % 2:
                raise ValueError("odd setq")
            return "(" + ", ".join("(%s = %s)" % (vname(a[i]), self.expr(a[i + 1])) for i in range(0, len(a), 2)) + ")"
        if op == "while":
            return "({ while (%s) { %s; } (I)0; })" % (self.expr(a[0]), self.sequence(a[1:]))
        if op in ("and", "or"):
            # These source forms are predicates: codegen emits canonical 0/1.
            return "(" + (" && " if op == "and" else " || ").join("(%s != 0)" % self.expr(v) for v in a) + ")"
        if op == "not":
            return "(!%s)" % self.expr(a[0])
        if op in ("+", "-", "*", "logand", "logior", "logxor", "=", "/=", "<", "<=", ">", ">=", "/", "mod", "shl", "shr", "sar"):
            operator = {"logand": "&", "logior": "|", "logxor": "^", "=": "==", "/=": "!=", "mod": "%", "shl": "<<", "shr": ">>", "sar": ">>"}.get(op, op)
            if op in ("shl", "shr", "sar"):
                def shift(ts):
                    left = "(U)" + ts[0] if op != "sar" else ts[0]
                    return "(I)(%s %s (%s & 63))" % (left, operator, ts[1])
                return self.operands(a, shift)
            unsigned = op in ("+", "-", "*", "shl", "shr")
            return self.operands(a, lambda ts: "(I)(" + (operator if len(ts) == 1 else "") + (" %s " % operator).join("(U)%s" % t if unsigned else t for t in ts) + ")")
        if op == "lognot":
            return "(~%s)" % self.expr(a[0])
        if op == "data-addr":
            self.data.add(a[0])
            return "((I)(uintptr_t)%s)" % cname(a[0])
        if re.fullmatch(r"ptr-read-u(8|16|32|64)", op):
            bits = op.rsplit("u", 1)[1]
            return self.operands(a, lambda ts: "(I)*(uint%s_t *)(uintptr_t)((U)%s + (U)%s)" % (bits, *ts))
        if re.fullmatch(r"ptr-write-u(8|16|32|64)", op):
            bits = op.rsplit("u", 1)[1]
            return self.operands(a, lambda ts: "(*(uint%s_t *)(uintptr_t)((U)%s + (U)%s) = %s, (I)1)" % (bits, *ts))
        if op in ("record-slot-count", "vector-len"):
            return self.operands(a, lambda ts: "(*(I *)(uintptr_t)cg_compact_payload(%s) >> 8)" % ts[0])
        if op in ("heap-sexp-tag-word", "heap-sexp-payload"):
            op = "nl_word_tag64" if op == "heap-sexp-tag-word" else "nl_word_payload"
        if op == "sexp-int-unwrap":
            return self.operands(a, lambda ts: "(%s & 1) ? (%s >> 2) : *(I *)(uintptr_t)(%s + 8)" % (ts[0], ts[0], ts[0]))
        if op == "sexp-int-make":
            return self.operands(a, lambda ts: "(*(uint8_t *)(uintptr_t)%s = 2, *(I *)(uintptr_t)(%s + 8) = %s, %s)" % (ts[0], ts[0], ts[1], ts[0]))
        if op == "atomic-fetch-add":
            return self.operands(a, lambda ts: "__atomic_fetch_add((I *)(uintptr_t)%s, %s, __ATOMIC_SEQ_CST)" % tuple(ts))
        if op == "alloc-bytes":
            op = "nl_alloc_bytes"
        if op == "sexp-tag":
            return "cg_tag(%s)" % self.expr(a[0])
        if op == "str-eq":
            return self.operands(a, lambda ts: "cg_streq(%s)" % ", ".join(ts))
        if op == "syscall-direct":
            return self.operands(a, lambda ts: "(I)syscall(%s)" % ", ".join(ts))
        if op == "extern-call":
            op, a = a[0], a[1:]
        if op in ("quote", "lambda", "function"):
            raise ValueError("unsupported source construct %s" % op)
        if op not in self.defs:
            old = self.externs.setdefault(op, len(a))
            if old != len(a):
                raise ValueError("inconsistent extern arity %s" % op)
        return self.operands(a, lambda ts: "%s(%s)" % (cname(op), ", ".join(ts)))

    def translate(self):
        bodies = []
        for f in self.source:
            attr = "__attribute__((noipa)) " if (f[1] == self.target or self.target is None) else "static "
            bodies.append("%sI %s(%s) { return %s; }" % (attr, cname(f[1]), ", ".join("I " + vname(p) for p in f[2]) or "void", self.sequence(f[3:])))
        decls = []
        for f in self.source:
            attr = "" if (f[1] == self.target or self.target is None) else "static "
            decls.append("%sI %s(%s);" % (attr, cname(f[1]), ", ".join("I" for _ in f[2]) or "void"))
        for name, arity in sorted(self.externs.items()):
            decls.append("extern I %s(%s);" % (cname(name), ", ".join("I" for _ in range(arity)) or "void"))
        decls += ["extern unsigned char %s[];" % cname(s) for s in sorted(self.data)]
        return (SRC / "intrinsics.h").read_text() + "\n" + "\n".join(decls + bodies) + "\n"


def generated(target=None):
    return OUT / "generated" / (target or "study")


def generate(target=None):
    OUT.mkdir(parents=True, exist_ok=True)
    source = forms(SRC / "runtime.nl.el")
    specs = json.loads((SRC / "fixtures.json").read_text())
    if target:
        wanted = specs.get(target, {}).get("functions", [])
        if (SRC / "dependencies.nl.el").exists():
            source += forms(SRC / "dependencies.nl.el")
        if (SRC / "heap-fields.nl.el").exists():
            source += forms(SRC / "heap-fields.nl.el")
        source = [f for f in source if f[1] in wanted]
    if specs.get(target, {}).get("lower_driver"):
        source += forms(SRC / (target + ".nl.el"))
    if specs.get(target, {}).get("auto_cold"):
        source += forms(OUT / (target + ".cold.nl.el"))
    addresses = specs.get(target, {}).get("data_addresses", {})
    def fix(x):
        if not isinstance(x, list):
            return x
        if x and x[0] == "data-addr" and x[1] in addresses:
            return addresses[x[1]]
        return [fix(y) for y in x]
    source = [fix(f) for f in source]
    lower = Lower(source, target)
    gen = generated(target)
    gen.mkdir(parents=True, exist_ok=True)
    cpath = gen / "runtime.c"
    cpath.write_text(lower.translate())
    save(str(cpath.parent.relative_to(OUT) / "lowering.json"), {"source_sha256": sha(SRC / "runtime.nl.el"), "generated_c_sha256": sha(cpath),
                          "externs": lower.externs, "data": sorted(lower.data), "revision": PINS["revision"],
                          "target": target or "study",
                          "dependencies_sha256": sha(SRC / "dependencies.nl.el") if (SRC / "dependencies.nl.el").exists() else None,
                          "heap_fields_sha256": sha(SRC / "heap-fields.nl.el") if (SRC / "heap-fields.nl.el").exists() else None,
                          "vector_convention": specs.get(target, {}).get("vector_convention", "compact-word" if target is None else "legacy-view"),
                          "fidelity": "Mechanical lowering preserves pinned branches and data flow; raw stores return sentinel1 and x86 shifts mask count63. Genuine pinned heap-word providers supply semantic field access. Seven GNU full-width goldens validate only documented representative paths. Unexercised external boundaries, including standard-output symbol allocation, trap; broader runtime parity is unestablished."})
    return lower


def classify(ins):
    op = ins.split()[0]
    rest = ins[len(op):]
    if op in ("push", "pop"):
        return "pushpop"
    if op in ("ret", "leave") or re.match(r"(sub|add)\s+rsp,", ins) or re.match(r"mov\s+(rbp,rsp|rsp,rbp)", ins):
        return "frame"
    if op == "call":
        return "call"
    if op.startswith("set"):
        return "boolmat"
    if op.startswith("j") or op in ("cmp", "test"):
        return "branch"
    if re.search(r"\[(rbp|rsp)[^\]]*\]", rest):
        return "stack"
    if op.startswith("mov") and re.search(r",\s*(0x[0-9a-f]+|-?\d+)$", rest):
        return "const"
    if op.startswith("mov") and "[" not in rest:
        return "move"
    return "mem" if "[" in rest else "alu"


def census(binary, names, raw_symbols=None, inline_data=None):
    result = {}
    # Objdump's symbol-delimited output, kept private. No address tables or
    # assembly are printed; symbol size avoids counting the next function.
    for name, symbol in names.items():
        if raw_symbols is not None:
            spec = raw_symbols[symbol]
            lo, end = spec["offset"], spec["offset"] + spec["size"]
            spans = sorted((a, n) for a, n in (inline_data or []) if a < end and a + n > lo)
            ranges, cursor = [], lo
            for a, n in spans:
                if n <= 0 or a < cursor or a + n > end:
                    raise ValueError("Invalid compiler inline-data extent for " + name)
                ranges.append((cursor, a))
                cursor = a + n
            ranges.append((cursor, end))
            # Restart decoding after every exact data span. Filtering decoded
            # addresses afterwards is insufficient: data can shift boundaries.
            commands = [["objdump", "-D", "-b", "binary", "-m", "i386:x86-64", "-M", "intel", "--no-show-raw-insn",
                         "--start-address=" + str(a), "--stop-address=" + str(b), binary]
                        for a, b in ranges if a < b]
        else:
            spans = []
            commands = [["objdump", "-d", "-M", "intel", "--no-show-raw-insn", "--disassemble=" + symbol, binary]]
        out = "\n".join(run(args).stdout for args in commands)
        ins = []
        for line in out.splitlines():
            m = re.match(r"\s*[0-9a-f]+:\s+(.*)$", line)
            if m:
                text = re.sub(r"\s+#.*$", "", m[1].strip())
                # Alignment padding belongs to placement, not the body.
                if text and not re.match(r"(?:[a-z]+\s+)*nop|data16|cs\s+nop", text):
                    ins.append(text)
        result[name] = {"instructions": len(ins), "categories": dict(collections.Counter(map(classify, ins))),
                        "inline_data_spans": len(spans), "inline_data_bytes": sum(n for _, n in spans)}
    return result


def static_study():
    lower = generate()
    run([cc(), "-O2", "-fwrapv", "-fno-strict-aliasing", "-c", generated() / "runtime.c", "-o", generated() / "runtime.gcc.o"])
    emacs = emacs_reference()
    proc = run([emacs, "-Q", "--batch", "-L", ROOT / "lisp", "-L", ROOT / "src", "-L", ROOT / "scripts",
                "-l", SRC / "compile.el", "--eval", "(codegen-runtime-unit)"] , cwd=ROOT,
               env=dict(os.environ, CODEGEN_RUNTIME_OUT=str(OUT)))
    (OUT / "compile.log").write_text(proc.stdout + proc.stderr)
    gcc = {}
    source = forms(SRC / "runtime.nl.el")
    for name in NAMES:
        static = OUT / "census"
        static.mkdir(parents=True, exist_ok=True)
        cfile = static / (name + ".c")
        obj = static / (name + ".gcc.o")
        cfile.write_text(Lower(source, name).translate())
        run([cc(), "-O2", "-fwrapv", "-fno-strict-aliasing", "-c", cfile, "-o", obj])
        gcc.update(census(obj, {name: cname(name)}))
    nl = census(OUT / "runtime.nl.text", {n: n for n in NAMES}, json.loads((OUT / "runtime.nl.symbols.json").read_text()),
                json.loads((OUT / "runtime.nl.inline-data.json").read_text()))
    result = {"revision": PINS["revision"], "source_sha256": sha(SRC / "runtime.nl.el"),
              "gcc": gcc, "nl": nl, "externs": lower.externs,
              "compiler_sha256": sha(ROOT / "lisp/nelisp-aot-compiler.el"),
              "gcc_version": run([cc(), "--version"]).stdout.splitlines()[0]}
    save("census.json", result)
    return result


def prepare_cold(name):
    specs = json.loads((SRC / "fixtures.json").read_text())
    spec = specs[name]
    if not spec.get("auto_cold"):
        return
    wanted = spec["functions"]
    source = forms(SRC / "runtime.nl.el") + forms(SRC / "dependencies.nl.el")
    if (SRC / "heap-fields.nl.el").exists():
        source += forms(SRC / "heap-fields.nl.el")
    source = [f for f in source if f[1] in wanted] + forms(SRC / (name + ".nl.el"))
    lower = Lower(source, name)
    lower.translate()
    rows, mapping = [], {}
    externs = dict(lower.externs)
    externs.update(spec.get("native_cold", {}))
    for index, (symbol, arity) in enumerate(sorted(externs.items())):
        code = 100 + index
        if code > 250:
            raise ValueError("too many unresolved cold boundaries")
        mapping[code] = symbol
        params = " ".join("p%d" % i for i in range(arity))
        rows.append("(defun %s (%s) (syscall-direct 60 %d 0 0 0 0 0))" % (symbol, params, code))
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / (name + ".cold.nl.el")).write_text("\n".join(rows) + "\n")
    save(name + ".cold-map.json", mapping)


def fixture(name):
    if not (SRC / (name + ".nl.el")).exists():
        reason = MISSING.get(name, "no representative runnable fixture yet")
        save(name + ".missing.json", {"name": name, "pass": False, "reason": reason})
        raise ValueError(name + ": " + reason + "; criterion remains FAIL")
    prepare_cold(name)
    generate(name)
    emacs = emacs_reference()
    proc = run([emacs, "-Q", "--batch", "-L", ROOT / "lisp", "-L", ROOT / "src", "-L", ROOT / "scripts",
                "-l", SRC / "compile.el", "--eval", '(codegen-runtime-executable "%s")' % name], cwd=ROOT,
               env=dict(os.environ, CODEGEN_RUNTIME_OUT=str(OUT)))
    (OUT / (name + ".compile.log")).write_text(proc.stdout + proc.stderr)
    run([cc(), "-O2", "-fwrapv", "-fno-strict-aliasing", "-ffunction-sections", "-fdata-sections",
         "-Wl,--gc-sections", "-I", SRC, "-I", generated(name), SRC / (name + ".c"), "-o", OUT / (name + ".gcc")])
    bins = {"nl": OUT / (name + ".nl"), "gcc": OUT / (name + ".gcc")}
    # Representative cases must match independent complete 64-bit goldens,
    # not merely low exit bits; validate.el runs the same inputs in GNU.
    verify = run([emacs, "-Q", "--batch", "-l", SRC / "validate.el", "--eval",
                  '(codegen-runtime-validate "%s" "%s" "%s")' % (name, bins["nl"], bins["gcc"])])
    (OUT / (name + ".validation.log")).write_text(verify.stdout + verify.stderr)
    return bins


def frequency(cpu):
    try:
        return int(Path("/sys/devices/system/cpu/cpu%s/cpufreq/scaling_cur_freq" % cpu).read_text())
    except (ValueError, OSError):
        return 0


def timed(exe, n, repeats, cpu):
    freq = frequency(cpu)
    t = time.perf_counter()
    proc = subprocess.run(["taskset", "-c", cpu, str(exe)], input=f"{n:08d} {repeats}\n", capture_output=True,
                          text=True, preexec_fn=bounded)
    dt = time.perf_counter() - t
    if proc.returncode:
        raise ValueError("fixture exited %s: %s" % (proc.returncode, proc.stderr[:120]))
    return {"seconds": dt, "khz": freq, "output": proc.stdout.strip()}


def agree(rows):
    return len({r["output"] for r in rows}) == 1


def wall(name):
    bins = fixture(name)
    cpu = os.environ.get("CODEGEN_CPU", "9")
    repeats = 1
    while True:
        sample = timed(bins["gcc"], N, repeats, cpu)
        if sample["seconds"] >= 0.5:
            break
        repeats *= max(2, int(0.6 / max(sample["seconds"], .001)))
    for exe in bins.values():
        timed(exe, N, repeats, cpu)
        timed(exe, 0, repeats, cpu)
    rounds = []
    for i in range(7):
        row = {}
        for variant in (tuple(bins) if i % 2 == 0 else tuple(reversed(bins))):
            tn = timed(bins[variant], N, repeats, cpu)
            tz = timed(bins[variant], 0, repeats, cpu)
            ns = (tn["seconds"] - tz["seconds"]) * 1e9 / (N * repeats)
            if ns <= 0:
                raise ValueError("non-positive own wall cost; measurement invalid")
            row[variant] = {"n": tn, "zero": tz, "ns_per_call": ns}
        row["ratio"] = row["nl"]["ns_per_call"] / row["gcc"]["ns_per_call"]
        row["results_agree"] = agree([row[v]["n"] for v in bins]) and agree([row[v]["zero"] for v in bins])
        rounds.append(row)
    ratio = statistics.median(r["ratio"] for r in rounds)
    ok = ratio <= 1.5 and all(r["results_agree"] for r in rounds)
    result = {"name": name, "cpu": cpu, "n": N, "repeats": repeats, "rounds": rounds,
              "source_revision": PINS["revision"], "source_sha256": sha(SRC / "runtime.nl.el"),
              "compiler_sha256": sha(ROOT / "lisp/nelisp-aot-compiler.el"),
              "raw_tco_enabled": os.environ.get("NELISP_TCO") != "0",
              "fixture_input_sha256": {f: sha(SRC / f) for f in
                  ("runtime.nl.el", "pins.json", "dependencies.nl.el", "heap-fields.nl.el",
                   "fixtures.json", "compile.el", "harness.h", "harness.nl.el", "intrinsics.h",
                   name + ".c", name + ".nl.el") if (SRC / f).exists()},
              "generated_c_sha256": sha(generated(name) / "runtime.c"),
              "median_paired_ratio": ratio, "pass": ok, "sha256": {v: sha(p) for v, p in bins.items()}}
    save(name + ".wall.json", result)
    print("%s: wall NL/gcc -O2 x%.4f, cap 1.5, seven pairs: %s" % (name, ratio, "PASS" if ok else "FAIL"))
    return ok


def heavy_run():
    override = os.environ.get("CODEGEN_HEAVY_RUN")
    if override:
        return override
    common = Path(run(["git", "rev-parse", "--git-common-dir"], cwd=ROOT).stdout.strip())
    if not common.is_absolute():
        common = ROOT / common
    return common.resolve().parent.parents[1] / "bin/heavy-run.sh"


def ir(name):
    bins = fixture(name)
    count = int(os.environ.get("CODEGEN_RUNTIME_IR_N", "1000" if name == "wf_bytecode_w_fast_run" else str(N)))
    result = {"name": name, "diagnostic_only": True, "n": count, "variants": {}, "source_sha256": sha(SRC / "runtime.nl.el"),
              "compiler_sha256": sha(ROOT / "lisp/nelisp-aot-compiler.el"),
              "raw_tco_enabled": os.environ.get("NELISP_TCO") != "0"}
    for variant, exe in bins.items():
        counts, outputs, owners = [], [], []
        for n in (count, 0):
            dest = OUT / (name + "." + variant + ".%d.cg" % n)
            proc = run([heavy_run(), "taskset", "-c", "0-7", "valgrind", "--tool=callgrind", "--callgrind-out-file=" + str(dest), exe], input=f"{n:08d} 1\n")
            outputs.append(proc.stdout.strip())
            m = re.search(r"^totals: (\d+)", dest.read_text(), re.M)
            if not m:
                raise ValueError("missing callgrind totals")
            counts.append(int(m[1]))
            owners.append(profile(dest, exe, capture=True))
        symbol = name if variant == "nl" else cname(name)
        self_delta = owners[0]["all_self_ir"].get(symbol, 0) - owners[1]["all_self_ir"].get(symbol, 0)
        call_delta = owners[0]["all_calls"].get(symbol, 0) - owners[1]["all_calls"].get(symbol, 0)
        result["variants"][variant] = {"ir_per_call": (counts[0] - counts[1]) / count,
                                      "cost_scope": "semantic invocation including deterministic reset/checksum loop, N minus zero",
                                      "target_self_ir_per_invocation": self_delta / count,
                                      "observed_target_calls": call_delta,
                                      "target_self_ir_per_observed_call": self_delta / call_delta if call_delta > 0 else None,
                                      "ownership": "out-of-line" if call_delta > 0 else "inlined or unobserved out-of-line",
                                      "outputs": outputs, "sha256": sha(exe)}
    result["results_agree"] = result["variants"]["nl"]["outputs"] == result["variants"]["gcc"]["outputs"]
    if not all(v["ir_per_call"] > 0 for v in result["variants"].values()):
        raise ValueError("non-positive dynamic Ir delta")
    save(name + ".ir.json", result)
    print("%s: diagnostic Ir/call NL %.2f GCC %.2f; results %s" % (name, result["variants"]["nl"]["ir_per_call"], result["variants"]["gcc"]["ir_per_call"], result["results_agree"]))
    return result["results_agree"]


def study():
    c = static_study()
    absent = [n for n in NAMES if not (OUT / (n + ".ir.json")).exists()]
    zero = [n for n in NAMES if not c["nl"][n]["instructions"] or not c["gcc"][n]["instructions"]]
    ranking = []
    profile_path = OUT / "profile.json"
    measured = json.loads(profile_path.read_text()) if profile_path.exists() else None
    for n in NAMES:
        nl, gcc = c["nl"][n], c["gcc"][n]
        cats = {k: nl["categories"].get(k, 0) - gcc["categories"].get(k, 0)
                for k in set(nl["categories"]) | set(gcc["categories"])}
        row = {"name": n, "static_excess": nl["instructions"] - gcc["instructions"],
               "category_excess": cats,
               "mechanisms": {"stack_and_pushpop": cats.get("stack", 0) + cats.get("pushpop", 0),
                              "branch_and_bool_materialization": cats.get("branch", 0) + cats.get("boolmat", 0),
                              "constant_materialization": cats.get("const", 0)}}
        if measured:
            p = measured["functions"][n]
            row["runtime_profile"] = p
            if p["calls"] == 0:
                row["ownership_note"] = "No out-of-line calls observed; work may be inlined in measured callers or absent from this workload."
            elif p["self_ir"] == 0:
                row["ownership_note"] = "Measured zero incremental self cost after independent control subtraction."
            else:
                row["ownership_note"] = "Measured out-of-line owner; raw and incremental cost/calls are recorded separately."
            # This heuristic ranks investigation only; static counts are not
            # dynamic call costs and never establish a speed criterion.
            row["estimated_global_fraction"] = p["share"] * max(0, 1 - gcc["instructions"] / nl["instructions"]) if nl["instructions"] else 0
        ranking.append(row)
    ranking.sort(key=lambda r: r.get("estimated_global_fraction", r["static_excess"]), reverse=True)
    dynamic = {}
    for n in NAMES:
        path = OUT / (n + ".ir.json")
        if path.exists():
            dynamic[n] = json.loads(path.read_text())
    valid_dynamic = [n for n, row in dynamic.items() if row.get("results_agree") and
                     row.get("source_sha256") == c["source_sha256"] and
                     all(v["ir_per_call"] > 0 for v in row["variants"].values())]
    all_profiled = (measured is not None
                    and re.fullmatch(r"[0-9a-f]{64}", measured.get("binary_sha256", ""))
                    and re.fullmatch(r"[0-9a-f]{64}", measured.get("callgrind_sha256", ""))
                    and measured.get("total_self_ir", 0) > 0
                    and all(n in measured.get("functions", {}) for n in NAMES))
    ok = not zero and len(valid_dynamic) == len(NAMES) and all_profiled
    result = {"pass": bool(ok), "source_revision": PINS["revision"], "ranking": ranking,
              "dynamic_fixture_results": dynamic, "missing_valid_dynamic": sorted(set(NAMES) - set(valid_dynamic)),
              "profile_binary_sha256": measured["binary_sha256"] if measured else None,
              "ranking_basis": "runtime self-Ir share times static count reduction estimate" if measured else "static excess only; global gain unmeasured",
              "interpretation": "stack/pushpop: expression/parameter registers; branch/boolmat: predicate fusion; const: address/constant propagation. GCC recursion-to-loop is relevant to FNV. Static counts alone cannot establish dynamic savings."}
    save("attribution.json", result)
    print("S5 study: %d/%d static bodies compiled from %s; %d/%d valid dynamic fixtures" % (len(NAMES) - len(zero), len(NAMES), PINS["revision"][:9], len(valid_dynamic), len(NAMES)))
    print("Largest static gaps: " + "; ".join("%s +%d" % (r["name"], r["static_excess"]) for r in ranking[:3]))
    if not ok:
        print("FAIL: complete dynamic fixture coverage and real-runtime shares are required; see attribution.json")
    else:
        print("PASS (Ir remains diagnostic; speed gates use wall time)")
    return bool(ok)


def selfcheck():
    bins = fixture("nl_bytes_byte_at")
    mutant = OUT / "result-mutant.py"
    mutant.write_text("#!/usr/bin/env python3\nimport pathlib, subprocess, sys\np = subprocess.run([str(pathlib.Path(__file__).with_name('nl_bytes_byte_at.gcc'))], input=sys.stdin.read(), capture_output=True, text=True, check=True)\nprint('%016x' % (int(p.stdout.strip(),16) ^ 1))\n")
    mutant.chmod(0o755)
    proc = subprocess.run([emacs_reference(), "-Q", "--batch", "-l", str(SRC / "validate.el"),
                           "--eval", '(codegen-runtime-validate "nl_bytes_byte_at" "%s" "%s")' % (bins["nl"], mutant)],
                          capture_output=True, text=True, preexec_fn=bounded)
    ok = proc.returncode != 0 and "expected 0000000000000000" in proc.stderr and "0000000000000001" in proc.stderr
    (OUT / "negative-control.log").write_text(proc.stdout + proc.stderr)
    save("negative-control.json", {"pass": ok, "returncode": proc.returncode,
                                   "mutation": "one bit of the actual executable checksum",
                                   "positive": "both executables previously passed all 7 GNU goldens"})
    print("full-width result checker: actual one-bit executable mutation rejected: " + ("PASS" if ok else "FAIL"))
    return ok


def profile(path, binary, control=None, capture=False):
    """Read real-runtime Callgrind self Ir/call and shares, without timing."""
    aliases, self_ir, calls = {}, collections.Counter(), collections.Counter()
    symbols = []
    for line in run(["nm", "-n", "--defined-only", binary]).stdout.splitlines():
        fields = line.split()
        if len(fields) == 3 and fields[1] in "Tt":
            symbols.append((int(fields[0], 16), fields[2]))
    addresses = [a for a, _ in symbols]
    sections = run(["objdump", "-h", binary]).stdout
    text_section = re.search(r"^\s*\d+\s+\.text\s+([0-9a-f]+)\s+([0-9a-f]+)", sections, re.M)
    if not text_section:
        raise ValueError("profile binary has no .text section")
    text_lo = int(text_section[2], 16)
    text_hi = text_lo + int(text_section[1], 16)
    def resolve(value):
        base = re.sub(r"'\d+$", "", value)
        if base.startswith("0x"):
            address = int(base, 16)
            index = bisect.bisect_right(addresses, address) - 1
            return symbols[index][1] if index >= 0 and text_lo <= address < text_hi else base
        return base
    current, callee, pending = None, None, False
    positions = 1
    ir_column = 0
    def name(spec):
        m = re.match(r"\((\d+)\)(?:\s+(.*))?", spec)
        if not m:
            return resolve(spec.strip())
        if m[2]:
            aliases[m[1]] = resolve(m[2].strip())
        return aliases.get(m[1], "?")
    for line in path.read_text(errors="replace").splitlines():
        if line.startswith("positions:"):
            positions = len(line.split()) - 1
        elif line.startswith("events:"):
            ir_column = line.split()[1:].index("Ir")
        elif line.startswith("fn="):
            current, pending = name(line[3:]), False
        elif line.startswith("cfn="):
            callee = name(line[4:])
        elif line.startswith("calls="):
            calls[callee] += int(line[6:].split()[0])
            pending = True
        elif line and line[0] in "0123456789+-*" and current:
            fields = line.split()
            if pending:
                pending = False
            elif len(fields) > positions + ir_column:
                self_ir[current] += int(fields[positions + ir_column])
    total = sum(self_ir.values())
    result = {"callgrind_sha256": sha(path), "binary_sha256": sha(binary), "total_self_ir": total,
              "all_self_ir": dict(self_ir), "all_calls": dict(calls),
              "functions": {n: {"self_ir": self_ir[n], "calls": calls[n],
                                "ir_per_call": self_ir[n] / calls[n] if calls[n] else None,
                                "share": self_ir[n] / total if total else 0} for n in NAMES}}
    if control:
        zero = profile(control, binary, capture=True)
        result["control_sha256"] = sha(control)
        result["raw_total_self_ir"] = total
        result["total_self_ir"] -= zero["total_self_ir"]
        result["measurement"] = "workload plus following GC, minus independent empty loop plus GC"
        if result["total_self_ir"] <= 0:
            raise ValueError("non-positive profile Ir delta")
        deltas = {n: result["all_self_ir"].get(n, 0) - zero["all_self_ir"].get(n, 0)
                  for n in set(result["all_self_ir"]) | set(zero["all_self_ir"])}
        result["top_incremental_owners"] = sorted(
            ({"name": n, "self_ir_delta": v, "share": v / result["total_self_ir"]}
             for n, v in deltas.items() if v > 0), key=lambda r: r["self_ir_delta"], reverse=True)[:20]
        for n in NAMES:
            p, z = result["functions"][n], zero["functions"][n]
            p["raw_self_ir"] = p["self_ir"]
            p["raw_share"] = p["share"]
            p["control_calls"] = z["calls"]
            p["incremental_calls"] = p["calls"] - z["calls"]
            p["self_ir"] -= z["self_ir"]
            p["incremental_ir_per_workload_call"] = p["self_ir"] / p["calls"] if p["calls"] > 0 else None
            p["share"] = p["self_ir"] / result["total_self_ir"]
    if capture:
        return result
    save("profile.json", result)
    print("real-runtime profile: %d/%d named functions reached; %s" % (sum(result["functions"][n]["calls"] > 0 for n in NAMES), len(NAMES), result.get("measurement", "raw interval")))
    return result["total_self_ir"] > 0


def runtime_run(binary, name, n, gnu=False):
    cpu = os.environ.get("CODEGEN_CPU", "9")
    env = dict(os.environ, CODEGEN_RUNTIME_BYTECODE=str(SRC / "bytecode.eld"),
               CODEGEN_RUNTIME_WORKLOAD=name, CODEGEN_RUNTIME_N=str(n))
    freq = frequency(cpu)
    args = ["taskset", "-c", cpu, binary]
    args += ["-Q", "--batch", "-l"] if gnu else ["--load"]
    args.append(SRC / "bytecode-workload.el")
    proc = run(args, env=env, cwd=ROOT)
    found = re.findall(r"^S5-WORK (\S+) (\d+) (\d+) ([0-9.]+)$", proc.stdout, re.M)
    if len(found) != 1 or found[0][0] != name or int(found[0][1]) != n:
        raise ValueError("missing or malformed runtime completion marker")
    value = int(found[0][2])
    expected = n if name == "while" else n + 2
    seconds = float(found[0][3])
    if value != expected or seconds <= 0:
        raise ValueError("invalid runtime result or non-positive wall time")
    return {"seconds": seconds, "khz": freq, "answer": value}


def endtoend():
    bins = {"candidate": Path(os.environ.get("CODEGEN_RUNTIME_BIN", ROOT / "target/codegen/lv-src/target/final/nelisp-align")),
            "base": Path(os.environ.get("CODEGEN_RUNTIME_BASE", ROOT / "target/codegen/alignment/e9/nelisp-dyn")),
            "gnu": Path(shutil.which(os.environ.get("CODEGEN_RUNTIME_GNU", "emacs")) or "emacs")}
    for label, binary in bins.items():
        if not binary.exists():
            raise ValueError("%s runtime binary missing: %s" % (label, binary))
    if sha(bins["candidate"]) == sha(bins["base"]):
        raise ValueError("candidate and baseline binaries are identical")
    result = {"fixture_revision": PINS["revision"], "real_runtime_versions": {}, "workloads": {},
              "sha256": {v: sha(b) for v, b in bins.items()}, "bytecode_sha256": sha(SRC / "bytecode.eld")}
    for label, binary in bins.items():
        result["real_runtime_versions"][label] = run([binary, "--version"]).stdout.strip()
    if "GNU Emacs 31.1" not in result["real_runtime_versions"]["gnu"]:
        raise ValueError("end-to-end golden must be GNU Emacs 31.1")
    for name in ("while", "funcall"):
        n = 10000
        while True:
            warm = runtime_run(bins["gnu"], name, n, True)
            if warm["seconds"] >= .25:
                break
            n *= max(2, int(.3 / warm["seconds"]))
        for label, binary in bins.items():
            runtime_run(binary, name, n, label == "gnu")
        rounds = []
        for i in range(7):
            order = tuple(bins) if i % 2 == 0 else tuple(reversed(bins))
            row = {label: runtime_run(bins[label], name, n, label == "gnu") for label in order}
            row["base_ratio"] = row["candidate"]["seconds"] / row["base"]["seconds"]
            row["gnu_ratio"] = row["candidate"]["seconds"] / row["gnu"]["seconds"]
            rounds.append(row)
        b = statistics.median(r["base_ratio"] for r in rounds)
        g = statistics.median(r["gnu_ratio"] for r in rounds)
        result["workloads"][name] = {"n": n, "rounds": rounds, "median_base_ratio": b,
                                     "median_gnu_ratio": g, "pass": b <= 1.03}
        save("endtoend.json", result)
        print("bytecode %s: candidate/base x%.4f (cap 1.03), candidate/GNU x%.4f" % (name, b, g))
    # Existing differential checks remain part of this end-to-end criterion.
    result["differential"] = {}
    for stage in ("1", "loader", "2", "apply", "const", "unit", "word", "overflow", "signal"):
        proc = subprocess.run([sys.executable, str(ROOT / "tools/ai/codegen-check.py"), "lispvalue", stage],
                              capture_output=True, text=True, preexec_fn=bounded, cwd=ROOT)
        (OUT / ("differential-" + stage + ".log")).write_text(proc.stdout + proc.stderr)
        result["differential"][stage] = {"returncode": proc.returncode, "pass": proc.returncode == 0}
    ok = all(x["pass"] for x in result["workloads"].values()) and all(x["pass"] for x in result["differential"].values())
    result["pass"] = ok
    save("endtoend.json", result)
    print("endtoend: " + ("PASS" if ok else "FAIL"))
    return ok


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", choices=("study", "build", "wall", "ir", "endtoend", "selfcheck", "generate", "profile"))
    parser.add_argument("name", nargs="?", choices=NAMES)
    parser.add_argument("--callgrind", type=Path)
    parser.add_argument("--profile-binary", type=Path)
    parser.add_argument("--control-callgrind", type=Path)
    args = parser.parse_args()
    if shutil.disk_usage(ROOT).free < 50 * 1024**3:
        raise ValueError("less than 50 GiB free disk")
    if args.command in ("build", "wall", "ir") and args.name is None:
        parser.error("this command requires NAME")
    if args.command == "study":
        ok = study()
    elif args.command == "build":
        fixture(args.name)
        print(args.name + ": built and independently validated")
        ok = True
    elif args.command == "generate":
        generate()
        print("generated C for %d pinned functions" % (len(NAMES) + len(HELPERS)))
        ok = True
    elif args.command == "wall":
        ok = wall(args.name)
    elif args.command == "ir":
        ok = ir(args.name)
    elif args.command == "selfcheck":
        ok = selfcheck()
    elif args.command == "profile":
        if not args.callgrind or not args.profile_binary:
            parser.error("profile requires --callgrind PATH --profile-binary BINARY")
        ok = profile(args.callgrind, args.profile_binary, args.control_callgrind)
    else:
        ok = endtoend()
    return 0 if ok else 1


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        if isinstance(error, subprocess.CalledProcessError):
            OUT.mkdir(parents=True, exist_ok=True)
            (OUT / "failure.log").write_text((error.stdout or "") + (error.stderr or ""))
            print("FAIL: subprocess exited %s; details %s" % (error.returncode, OUT / "failure.log"))
        else:
            print("FAIL: " + str(error))
        sys.exit(1)
