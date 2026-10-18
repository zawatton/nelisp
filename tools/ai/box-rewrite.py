#!/usr/bin/env python3
"""Doc 215 word-transport rewrite engine.  Dry-run mode only so far.

Input: the unit-source dump a build writes with NELISP_BOX_DUMP=FILE (one
JSON line per unit, the same source the census sees: after source patches,
before the inliner).  A unit dumped twice keeps its last line.

  box-rewrite.py report DUMP [--examples N] [--json OUT]

Classification (Doc 215, "Mechanical rewrite"):

out-parameter: a defun parameter P is CONVERTIBLE when every use of P is
  the destination of a known writer (WRITERS below), or P forwarded into the
  out position of another convertible parameter.  Fixpoint over the call
  graph, starting optimistic.  Any other use (a read, raw ptr-write, a
  return, arithmetic) disqualifies it; the reason is kept.

transport site: a local bound to (alloc-bytes 32 8), or to a *_ptr
  materialiser, or an inline *_ptr call.  Each use of the value is
    result  - destination of a writer / convertible out position
    tag     - (sexp-tag V), (sexp-int-unwrap V)       -> word tag / decode
    src     - value argument of a setter or constructor -> the word
    walk    - container argument of a cons/record/vector accessor
              -> word accessor (immediate answers 0)
    shim    - argument of any other function           -> nl_val_load shim
    manual  - raw memory access, arithmetic, return, unknown
  A site is mechanical when every use is result/tag/src/walk, shim-only when
  the rest are shim, otherwise manual.
Manual overlays (judgment required, never rewritten by the tool):
    R2/R3 - the defun reserves root-stack slots (nl_root_reserve /
            nl_root_reserve_checked); R3 when it is a binder
    R4    - a parameter that callers pass as a *_ptr view or scratch box
            and the callee reads after another lookup (approximated: the
            parameter is read after a call that may allocate)
    worker- the defun runs on Doc 199 workers only (nl_thread_* family)
Families come from tools/ai/box-census.py.
"""
import importlib.util, json, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location("census", os.path.join(HERE, "box-census.py"))
census = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(census)
family = census.family

# writer -> index of its out argument (0-based among the call's arguments)
WRITERS = {
    "wf_write_int": 0, "wf_write_nil": 0, "wf_write_t": 0, "wf_write_int_checked": 0,
    "sexp-write-nil": 0, "sexp-write-t": 0, "sexp-int-make": 0,
    "nl_write_canonical_nil": 0, "nl_write_canonical_t": 0, "nl_write_nil_slot": 0,
    "nl_write_float_bits": 0, "nl_env_write_nil_slot": 0,
    "nl_sexp_clone_into": 1, "wf_copy32": 0, "nl_sci_store_imm": 1, "nl_val_load": 1,
    "nelisp_cons_construct": 2, "cons-make": 2, "cons-make-with-clone": 2,
    "vector-make": 1, "record-slot-ref": 2, "vector-ref": 2,
    "nl_alloc_str": 2, "nl_alloc_symbol": 2, "nl_alloc_str_pos": 2, "nl_alloc_symbol_pos": 2,
    "nl_alloc_symbol_plain": 2, "nl_alloc_symbol_named": 3, "nl_alloc_symbol_plain_named": 2,
}
# setter/constructor -> argument indexes that take a VALUE (word-aware)
# Only consumers that reach nl_val_clone_into (which stores an immediate WORD
# as it is).  wf_copy32, nl_sexp_clone_into, cons-make and cell-make copy or
# read a 32-byte Sexp and are NOT word-aware: a WORD there needs the shim.
SRC = {
    "record-slot-set": (2,), "vector-slot-set": (2,), "nl_record_set_slot": (2,),
    "nl_vector_set_slot": (2,), "nl_consbox_set_car": (1,), "nl_consbox_set_cdr": (1,),
    "cons-set-car": (1,), "cons-set-cdr": (1,), "nelisp_cons_construct": (0, 1),
    "nl_val_clone_into": (0,), "nl_alloc_cell": (0,), "nl_alloc_cell_init": (1,),
}
# accessor -> index of its container argument
WALK = {
    "nl_cons_car_ptr": 0, "nl_cons_cdr_ptr": 0, "nl_cons_car_word": 0, "nl_cons_cdr_word": 0,
    "record-slot-ref-ptr": 0, "vector-ref-ptr": 0, "nl_record_slot_ptr": 0,
    "nl_vector_slot_ptr": 0, "record-slot-count": 0, "vector-len": 0,
    "record-slot-ref": 0, "vector-ref": 0, "nl_vector_slot_word": 0, "nl_record_slot_word": 0,
}
TAG = {"sexp-tag", "sexp-int-unwrap", "nl_val_tag"}
PTR = {"nl_cons_car_ptr", "nl_cons_cdr_ptr", "record-slot-ref-ptr", "vector-ref-ptr",
       "nl_record_slot_ptr", "nl_vector_slot_ptr"}
ROOT = {"nl_root_reserve", "nl_root_reserve_checked"}
BINDER = re.compile(census.FAMILIES[3][1])
WORKER = re.compile(r"^nl_thread_")
SPECIAL = {"quote", "data-addr", "$str", "$vec"}


def is_call(x):
    return isinstance(x, list) and x and isinstance(x[0], str) and x[0] not in ("$str", "$vec")


def call_args(x):
    """(head, args) with extern-call unwrapped."""
    if x[0] == "extern-call" and len(x) > 1 and isinstance(x[1], str):
        return x[1], x[2:]
    return x[0], x[1:]


def load(path):
    units = {}
    for line in open(path, encoding="utf-8"):
        d = json.loads(line)
        units[d["unit"]] = d["source"]
    defuns = {}
    def collect(unit, x):
        if is_call(x):
            if x[0] == "defun" and len(x) >= 3 and isinstance(x[1], str):
                params = [p for p in (x[2] if isinstance(x[2], list) else []) if isinstance(p, str)]
                defuns[x[1]] = {"unit": unit, "params": params, "body": x[3:]}
                return
            for y in x[1:]:
                collect(unit, y)
    for unit, src in units.items():
        collect(unit, src)
    return defuns


def occurrences(body, var):
    """Yield (parent, index) for every occurrence of symbol VAR in BODY."""
    stack = [(None, None, b) for b in body]
    def walk(parent, idx, x):
        if x == var:
            yield parent, idx
        elif is_call(x) and x[0] not in SPECIAL:
            for i, y in enumerate(x):
                if i == 0 and isinstance(y, str):
                    continue
                yield from walk(x, i, y)
    for p, i, b in stack:
        yield from walk(p, i, b)


WORD_IN = set()   # (defun, param-index) that already accept a WORD (fixpoint)
ARITY = {}        # defun -> number of params


def use_kind(parent, idx, convertible):
    """Classify one use of a transport value at PARENT[IDX]."""
    if parent is None:
        return "manual", "returned"
    if not is_call(parent):
        return "manual", "in data"
    head, args = call_args(parent)
    pos = idx - (len(parent) - len(args))
    if head in ("let", "let*"):
        return "manual", "rebound"
    if head == "setq":
        return "manual", "aliased by setq"
    if head in ("=", "/=") and len(args) == 2 and 0 in args:
        return "tag", "null test"
    if head == "ptr-read-u64" and pos == 0 and len(args) == 2 and args[1] in (0, 8):
        return ("tag", "tag read") if args[1] == 0 else ("tag", "payload read")
    if head in ("if", "and", "or", "seq", "progn", "when", "unless", "cond"):
        return "manual", "value used as %s operand" % head
    if WRITERS.get(head) == pos or (head, pos) in convertible:
        return "result", head
    if head in TAG and pos == 0:
        return "tag", head
    if pos in SRC.get(head, ()):
        return "src", head
    if WALK.get(head) == pos:
        return "walk", head
    if (head, pos) in WORD_IN:
        return "arg", head
    if head.startswith("ptr-") or head in ("+", "-", "=", "/=", "<", ">", "logand", "logior", "sar", "shl"):
        return "manual", head
    return "shim", head


def out_params(defuns):
    """Fixpoint: set of (defun, param-index) whose param is a convertible out."""
    cand = {(n, i) for n, d in defuns.items() for i in range(len(d["params"]))}
    reasons = {}
    changed = True
    while changed:
        changed = False
        for n, i in sorted(cand):
            p = defuns[n]["params"][i]
            ok, writes = True, 0
            for parent, idx in occurrences(defuns[n]["body"], p):
                kind, why = use_kind(parent, idx, cand)
                if kind == "result":
                    writes += 1
                else:
                    ok = False; reasons[(n, i)] = "%s (%s)" % (kind, why); break
            if not ok or writes == 0:
                cand.discard((n, i)); changed = True
                reasons.setdefault((n, i), "never written")
    return cand, reasons


def word_in_params(defuns, convertible):
    """Fixpoint: (defun, i) whose param i would accept a WORD unchanged in
    meaning: every use is a tag test, a value to a setter, a container to an
    accessor, a null test, or forwarded to another such param."""
    cand = {(n, i) for n, d in defuns.items() for i in range(len(d["params"]))}
    WORD_IN.clear(); WORD_IN.update(cand)
    changed = True
    while changed:
        changed = False
        for n, i in sorted(cand):
            p = defuns[n]["params"][i]
            uses = list(occurrences(defuns[n]["body"], p))
            ok = bool(uses) and all(use_kind(par, idx, convertible)[0] in ("tag", "src", "walk", "arg")
                                    for par, idx in uses)
            if not ok:
                cand.discard((n, i)); WORD_IN.discard((n, i)); changed = True
    return cand


def sites(defuns, convertible):
    """Yield (defun, idiom, cls, reasons, example) for every transport site."""
    for n, d in defuns.items():
        body = d["body"]
        flat = json.dumps(body)
        roots = any(r in flat for r in ROOT)
        overlay = None
        if roots:
            overlay = "R3" if BINDER.match(n) else "R2"
        elif WORKER.match(n):
            overlay = "worker"
        bound = set()
        def visit(x):
            if not is_call(x) or x[0] in SPECIAL:
                return
            if x[0] in ("let", "let*") and len(x) > 1 and isinstance(x[1], list):
                binds = x[1]
                for k, b in enumerate(binds):
                    if isinstance(b, list) and len(b) == 2 and isinstance(b[0], str):
                        var, init = b
                        idiom = None
                        if is_call(init):
                            h, a = call_args(init)
                            if h == "alloc-bytes" and a == [32, 8]:
                                idiom = "alloc32"
                            elif h in PTR:
                                idiom = "ptr:" + h
                        if idiom:
                            bound.add(id(init))
                            scope = [bb[1] for bb in binds[k + 1:] if isinstance(bb, list) and len(bb) == 2] \
                                if x[0] == "let*" else []
                            scope += x[2:]
                            kinds = [use_kind(p, i, convertible) for p, i in occurrences(scope, var)]
                            yield_site(n, idiom, kinds, json.dumps(b)[:90])
                    if isinstance(b, list) and len(b) == 2 and is_call(b[1]):
                        visit(b[1])
            for i, y in enumerate(x):
                if is_call(y):
                    h, _ = call_args(y)
                    if h in PTR and id(y) not in bound:
                        kinds = [use_kind(x, i, convertible)]
                        yield_site(n, "ptr:" + h, kinds, json.dumps(x)[:90])
                    visit(y)
        out = []
        def yield_site(fn, idiom, kinds, ex):
            cls = classify(kinds)
            why = sorted({w for k, w in kinds if k in ("manual", "shim")})
            if overlay and cls != "mechanical":
                cls = "manual"; why = [overlay] + why
            out.append((fn, idiom, cls, why, ex))
        for b in body:
            visit(b)
        yield from out


def r4_sites(defuns, convertible):
    """R4: a call that passes a NON-local box into a convertible out position
    (a caller parameter, a fixed address, (+ env N), data-addr): converting the
    callee to return a WORD would stop updating a box someone else reads."""
    out = []
    for n, d in defuns.items():
        params = set(d["params"])
        def visit(x):
            if not is_call(x) or x[0] in SPECIAL:
                return
            h, args = call_args(x)
            for pos, a in enumerate(args):
                if (h, pos) in convertible or WRITERS.get(h) == pos:
                    why = None
                    if isinstance(a, str) and a in params:
                        # Forwarding the caller's own convertible out is the
                        # chain the fixpoint already accepted, not R4.
                        if (n, d["params"].index(a)) not in convertible:
                            why = "out box is caller parameter %s" % a
                    elif isinstance(a, int) and a >= 268435456:
                        why = "out box is fixed address %d" % a
                    elif is_call(a) and a[0] in ("+", "data-addr"):
                        why = "out box is %s" % json.dumps(a)[:40]
                    if why and (h, pos) in convertible:
                        out.append((n, "out:" + h, "manual", ["R4", why], json.dumps(x)[:90]))
            for y in x[1:]:
                visit(y)
        for b in d["body"]:
            visit(b)
    return out


def source_index(root):
    """defun name -> source file (first textual definition)."""
    idx = {}
    files = [os.path.join(root, "scripts", "nelisp-standalone-build.el")]
    for d in ("lisp", "scripts", "src"):
        dd = os.path.join(root, d)
        files += sorted(os.path.join(dd, f) for f in os.listdir(dd) if f.endswith(".el"))
    pat = re.compile(r"\(defun ([A-Za-z0-9_]+)[\s(]")
    for f in files:
        for m in pat.finditer(open(f, encoding="utf-8").read()):
            idx.setdefault(m.group(1), os.path.relpath(f, root))
    return idx


def manual_queue(path, root, out_path):
    defuns = load(path)
    conv, _ = out_params(defuns)
    word_in_params(defuns, conv)
    src = source_index(root)
    q = []
    rows = [r for r in sites(defuns, conv) if r[2] == "manual"] + r4_sites(defuns, conv)
    for fn, idiom, cls, why, ex in rows:
        fam = family(fn)
        if fam in ("bytecode", "vm-call"):
            continue
        if why and why[0] in ("R2", "R3", "R4", "worker"):
            c = why[0]
        elif any(w.startswith("ptr-") for w in why):
            c = "raw"
        else:
            c = "unknown"
        q.append({"unit": defuns[fn]["unit"], "file": src.get(fn, "generated (build-time copy, see scripts/nelisp-standalone-build.el)"), "defun": fn,
                  "family": fam, "idiom": idiom, "class": c,
                  "reason": "; ".join(w for w in why if w not in ("R2", "R3", "R4", "worker"))[:120],
                  "site": ex})
    q.sort(key=lambda e: (e["file"], e["defun"], e["idiom"]))
    with open(out_path, "w") as f:
        json.dump(q, f, indent=1)
    by = {}
    for e in q:
        by[(e["family"], e["class"])] = by.get((e["family"], e["class"]), 0) + 1
    print("manual queue: %d sites -> %s" % (len(q), out_path))
    for k, v in sorted(by.items()):
        print("  %-10s %-8s %d" % (k[0], k[1], v))


def classify(kinds):
    ks = {k for k, _ in kinds}
    if not kinds:
        return "manual"          # bound and never used: dead box, still judgment
    if ks <= {"result", "tag", "src", "walk", "arg"}:
        return "mechanical"
    if ks <= {"result", "tag", "src", "walk", "arg", "shim"}:
        return "shim-only"
    return "manual"


def report(path, nex, json_out):
    defuns = load(path)
    conv, reasons = out_params(defuns)
    win = word_in_params(defuns, conv)
    conv_fams = {}
    for n, i in conv:
        f = family(n); conv_fams[f] = conv_fams.get(f, 0) + 1
    rows = list(sites(defuns, conv))
    table, examples, why = {}, {}, {}
    for fn, idiom, cls, w, ex in rows:
        fam = family(fn)
        idiom = "ptr" if idiom.startswith("ptr:") else idiom
        key = (fam, idiom)
        table.setdefault(key, {"mechanical": 0, "shim-only": 0, "manual": 0})
        table[key][cls] += 1
        examples.setdefault((fam, idiom, cls), []).append("%s: %s" % (fn, ex))
        for r in w:
            why[(fam, cls, r)] = why.get((fam, cls, r), 0) + 1
    print("defuns %d  convertible out-parameters %d  (%s)" % (
        len(defuns), len(conv), " ".join("%s=%d" % kv for kv in sorted(conv_fams.items()))))
    print("parameters that already accept a WORD: %d" % len(win))
    print("%-10s %-8s %10s %10s %8s" % ("family", "idiom", "mechanical", "shim-only", "manual"))
    tot = {"mechanical": 0, "shim-only": 0, "manual": 0}
    for (fam, idiom), c in sorted(table.items()):
        print("%-10s %-8s %10d %10d %8d" % (fam, idiom, c["mechanical"], c["shim-only"], c["manual"]))
        for k in tot:
            tot[k] += c[k]
    print("%-10s %-8s %10d %10d %8d" % ("TOTAL", "", tot["mechanical"], tot["shim-only"], tot["manual"]))
    print("\nmanual/shim reasons (family class reason count):")
    for (fam, cls, r), c in sorted(why.items(), key=lambda kv: -kv[1])[:40]:
        print("  %-10s %-9s %-40s %d" % (fam, cls, r[:40], c))
    if nex:
        print("\nexamples:")
        for (fam, idiom, cls), exs in sorted(examples.items()):
            for e in exs[:nex]:
                print("  [%s %s %s] %s" % (fam, idiom, cls, e))
    if json_out:
        with open(json_out, "w") as f:
            json.dump({"table": {"%s/%s" % k: v for k, v in table.items()},
                       "convertible_out": sorted("%s#%d" % k for k in conv),
                       "totals": tot}, f, indent=1)


if __name__ == "__main__":
    a = sys.argv[1:]
    if len(a) >= 3 and a[0] == "queue":
        root = a[a.index("--root") + 1] if "--root" in a else "."
        manual_queue(a[1], root, a[2])
    elif len(a) >= 2 and a[0] == "report":
        nex = int(a[a.index("--examples") + 1]) if "--examples" in a else 0
        jo = a[a.index("--json") + 1] if "--json" in a else None
        report(a[1], nex, jo)
    else:
        sys.exit(__doc__)
