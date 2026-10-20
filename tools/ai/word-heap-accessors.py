#!/usr/bin/env python3
"""Doc 216 S1: conservative raw-header read census and retained evidence.

The raw-header-read idiom counts ALL unmediated storage reads, a superset
of object-header reads. No name/offset-based guess exempts a pointer. Both
the canonical producers and post-patch/pre-inline linked sources are checked.
ISA encoder implementation is not an object consumer; its callers are.
This is source provenance, not an instruction-level pointer-taint oracle.

census [DUMP]       report canonical source and optional linked-source counts
check-source       reject any raw read outside the accessor module
controls           inject one raw read and demonstrate rejecting checks
check [--source-revision REV --linked DUMP]
                   recheck hash-bound S1.2 evidence; integration replay pins
                   historical source and requires the current linked census
check-historical   recheck committed S1 sources and raw evidence after S2+ edits
migrate            mechanical call-head/assembler-reference migration
"""
import hashlib
import io
import importlib.util
import json
from pathlib import Path
import re
import subprocess
import sys
import tarfile

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/word-heap-s1"
ACCESSORS = "lisp/nelisp-heap-accessors.el"
READS = {f"heap-read-{s}{n}": [f"ptr-read-{s}{n}"]
         for s in ("u", "s") for n in (8, 16, 32, 64)}
READS.update({f"heap-native-read-u{n}": [f"nelisp_ptr_read_u{n}"] for n in (8, 64)})
READS.update({f"heap-extern-read-u{n}": ["extern-call", f"nelisp_ptr_read_u{n}"]
              for n in (8, 64)})
RAW = {v[-1] for v in READS.values()}
FIELDS = {
    "heap-sexp-tag-byte": ("ptr-read-u8", 0),
    "heap-sexp-tag-word": ("ptr-read-u64", 0),
    "heap-sexp-payload": ("ptr-read-u64", 8),
    "heap-sexp-data": ("ptr-read-u64", 16),
    "heap-sexp-length": ("ptr-read-u64", 24),
}
ASM = {
    "nelisp-asm-x86_64-" + x for x in (
        "mov-reg-mem-disp8", "mov-reg-mem-rsp-disp", "movdqu-xmm-mem-disp8",
        "movsd-xmm-mem-disp8", "movzx-reg-byte-mem")
} | {"nelisp-asm-arm64-" + x for x in (
    "ldr-imm", "ldr-post-sp-16", "ldr-reg-reg", "ldrb-imm", "ldrw-reg-reg",
    "ldur-d-base-disp")}
ASM_ALIASES = {x: x.replace("nelisp-asm-", "nelisp-heap-accessors-") for x in ASM}
VERSION = "header-access-walk-v1"


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def lower(x):
    """The source census shares the compiler forms, never native addresses."""
    if not isinstance(x, list) or not x or x[0] in ("quote", "function", "$str", "$vec"):
        return x
    args = [lower(y) for y in x[1:]]
    if isinstance(x[0], str) and x[0] in FIELDS:
        op, offset = FIELDS[x[0]]
        return [op] + args + [offset]
    if isinstance(x[0], str) and x[0] in READS:
        return READS[x[0]] + args
    return [lower(x[0])] + args


def header_rows(source):
    """Visit binding initializers too; quotes and literal data are not code."""
    rows = {("-", VERSION): 0}

    def walk(x, fn="-"):
        if not isinstance(x, list) or not x:
            return
        head = x[0]
        if head in ("quote", "$str", "$vec"):
            return
        if head == "defun":
            for body in x[3:]:
                walk(body, x[1])
            return
        if head == "lambda":
            for body in x[2:]:
                walk(body, fn)
            return
        if head in ("let", "let*"):
            for binding in x[1] if isinstance(x[1], list) else []:
                if isinstance(binding, list):
                    for value in binding[1:]:
                        walk(value, fn)
            for body in x[2:]:
                walk(body, fn)
            return
        call = x[1] if head == "extern-call" and len(x) > 1 else head
        if isinstance(call, str) and (call in RAW or call in READS or call in FIELDS):
            key = (fn, "raw-header-read" if call in RAW else "header-accessor")
            rows[key] = rows.get(key, 0) + 1
        for y in x:
            walk(y, fn)

    walk(source)
    return rows


def tokens(text):
    """Elisp token spans, excluding comments, strings and character literals.

    Keeping original spans lets the refactor preserve all formatting. This
    lexical scan deliberately includes quoted DSL manifests in host sources.
    The build census separately distinguishes executed forms from Lisp data.
    """
    i = 0
    while i < len(text):
        c = text[i]
        if c.isspace():
            i += 1
        elif c == ";":
            end = text.find("\n", i)
            i = len(text) if end < 0 else end + 1
        elif c == '"':
            i += 1
            while i < len(text):
                if text[i] == "\\":
                    i += 2
                elif text[i] == '"':
                    i += 1
                    break
                else:
                    i += 1
        elif c == "?" and i + 1 < len(text) and not text[i + 1].isspace():
            i += 2
            if text[i - 1] == "\\":
                # Escaped character syntax may contain modifier prefixes.
                while i < len(text) and text[i] in "\\CMASHs-":
                    i += 1
                i += 1
        elif c in "()[]'`,":
            yield c, i, i + 1
            i += 1
        else:
            start = i
            while i < len(text) and not text[i].isspace() and text[i] not in "()[]'`,;\"":
                i += 1
            yield text[start:i], start, i


def producers():
    for directory in ("lisp", "src", "scripts", "packages"):
        for path in sorted((ROOT / directory).rglob("*.el")):
            rel = path.relative_to(ROOT).as_posix()
            if rel == ACCESSORS or path.name.startswith("nelisp-asm-") or "/test/" in rel:
                continue
            yield path


def dsl_positions(toks):
    """Quoted host manifests/templates are DSL; ordinary Lisp calls are not.

    An interpreted Lisp ptr-read-u64 call invokes the public raw-memory
    accessor. It does not emit a load from a Lisp WORD. Its implementation
    must still pass the linked-unit census. Never rename that public API or
    require a new runtime function merely to centralize native DSL loads.
    """
    stack, positions = [], set()
    for i, (name, start, end) in enumerate(toks):
        if name == "(":
            quoted, pattern = stack[-1] if stack else (False, False)
            # Recognizer data is not executed code. In particular the
            # authenticated native loader compares an ordinary Lisp memory
            # accessor form; changing that literal would break its pinning.
            pattern |= (not quoted and i + 1 < len(toks)
                        and toks[i + 1][0] in ("equal", "eq", "member", "memq", "assoc", "assq"))
            quoted |= (i > 0 and toks[i - 1][0] in ("'", "`")
                       and not (i > 1 and toks[i - 2][0] == "#"))
            quoted |= i + 1 < len(toks) and toks[i + 1][0] in ("quote", "backquote")
            stack.append((quoted and not pattern, pattern))
        elif name == ")":
            if stack:
                stack.pop()
        elif stack and stack[-1][0]:
            positions.add(start)
    return positions


def raw_sites(text):
    toks = list(tokens(text))
    dsl = dsl_positions(toks)
    result = []
    for i, (name, start, end) in enumerate(toks):
        if name in ASM:
            result.append((start, end, name, ASM_ALIASES[name]))
        elif (start in dsl and name in RAW and i and toks[i - 1][0] == "("
              and i + 1 < len(toks) and toks[i + 1][0] != "."):
            alias = next(k for k, v in READS.items() if v == [name])
            result.append((start, end, name, alias))
        elif (start in dsl and name == "extern-call" and i and toks[i - 1][0] == "("
              and i + 1 < len(toks) and toks[i + 1][0] in RAW):
            target = toks[i + 1][0]
            alias = next(k for k, v in READS.items() if v == [name, target])
            result.append((start, toks[i + 1][2], target, alias))
    return result


def source_counts(extra=None, revision=None):
    rows = []
    paths = [*producers(), *([Path(extra)] if extra else [])]
    need(ROOT / "scripts/nelisp-standalone-build.el" in paths
         and ROOT / "lisp/nelisp-aot-compiler.el" in paths, "missing canonical producers")
    for path in paths:
        if revision:
            historical = subprocess.run(["git", "show", f"{revision}:{path.relative_to(ROOT)}"],
                                        cwd=ROOT, capture_output=True)
            if historical.returncode:
                continue
            text = historical.stdout.decode()
        else:
            text = path.read_text()
        for start, end, name, alias in raw_sites(text):
            rows.append(dict(path=str(path.relative_to(ROOT)),
                             line=text.count("\n", 0, start) + 1, operation=name))
    return dict(files=len(paths), raw_header_read_upper_bound=len(rows), sites=rows)


def dump_units(path):
    return {row["unit"]: row["source"] for line in Path(path).open()
            if (row := json.loads(line))}


def dump_counts(path):
    rows = [r for source in dump_units(path).values() for r in header_rows(source).items()]
    return {name: sum(n for (_, idiom), n in rows if idiom == name)
            for name in ("raw-header-read", "header-accessor")}


def header_variables(path):
    """Promote simple semantic-accessor operands to logical fixed-field forms.

    This improves the semantic API but is NOT the zero-read proof: uncertain
    operands still use the explicit storage boundary and are counted there.
    The raw census exempts none based on this inference. Integer tag-looking
    comparisons alone are not a type proof (allocator flags use them too).
    """
    functions = {}

    def nodes(x):
        if isinstance(x, list) and x and x[0] not in ("quote", "function", "$str", "$vec"):
            yield x
            for y in x:
                yield from nodes(y)

    for source in dump_units(path).values():
        for fn in nodes(source):
            if fn[0] == "defun":
                functions[fn[1]] = (fn[2], list(nodes(fn[3:])))
    typed = {fn: set() for fn in functions}
    consumers = {"sexp-tag", "sexp-int-unwrap", "sexp-float-unwrap", "nl_val_tag",
                 "nl_word_tag64", "nl_word_payload", "wf_bytecode_w_tag", "wf_bytecode_w_int",
                 "nl_cons_car_word", "nl_cons_cdr_word", "nl_cons_car_ptr", "nl_cons_cdr_ptr",
                 "nl_vector_slot_word", "nl_record_slot_word", "record-slot-ref-ptr",
                 "vector-ref-ptr", "record-slot-count", "vector-len"}
    for fn, (_, body) in functions.items():
        params = functions[fn][0]
        if fn in consumers and params and isinstance(params[0], str):
            typed[fn].add(params[0])
        for x in body:
            if isinstance(x[0], str) and x[0] in consumers and len(x) > 1 and isinstance(x[1], str):
                typed[fn].add(x[1])
    changed = True
    while changed:
        before = sum(map(len, typed.values()))
        for fn, (_, body) in functions.items():
            for x in body:
                if isinstance(x[0], str) and x[0] in functions:
                    params = functions[x[0]][0]
                    for param, value in zip(params, x[1:]):
                        if param in typed[x[0]] and isinstance(value, str):
                            typed[fn].add(value)
                if x[0] in ("let", "let*") and isinstance(x[1], list):
                    for b in x[1]:
                        if (isinstance(b, list) and len(b) == 2
                                and isinstance(b[0], str) and isinstance(b[1], str)):
                            if b[0] in typed[fn] or b[1] in typed[fn]:
                                typed[fn].update(b)
        changed = before != sum(map(len, typed.values()))
    return typed


def check_source(extra=None, revision=None):
    result = source_counts(extra, revision)
    print(f"SOURCE raw-header-read upper bound={result['raw_header_read_upper_bound']}")
    need(result["raw_header_read_upper_bound"] == 0, "raw source read outside accessor module")


def controls():
    out = OUT / "controls"
    out.mkdir(parents=True, exist_ok=True)
    fixture = out / "injected.el"
    fixture.write_text("(defconst wh_injected_header_read\n  '(defun wh_injected (object) (ptr-read-u64 object 8)))\n")
    before = source_counts()["raw_header_read_upper_bound"]
    need(source_counts(fixture)["raw_header_read_upper_bound"] == before + 1,
         "source census missed injected raw header read")
    run = subprocess.run([sys.executable, str(Path(__file__).resolve()), "check-source", str(fixture)],
                         capture_output=True)
    (out / "source.out").write_bytes(run.stdout)
    (out / "source.err").write_bytes(run.stderr)
    need(run.returncode == 1, "injected raw source read passed check")
    # Include first binding initializers, explicit extern calls and nested
    # bases. Quoted data and variable bindings must not become phantom reads.
    sample = ["seq", ["defun", "wh_control", ["object"],
        ["let*", [["tag", ["ptr-read-u8", "object", 0]],
                  ["value", ["extern-call", "nelisp_ptr_read_u64", "object", 8]]],
            ["ptr-read-u64", ["ptr-read-u64", "object", 8], 0],
            ["function", ["lambda", [], ["ptr-read-u64", "object", 8]]],
            ["quote", ["ptr-read-u64", "data", 8]]]]]
    need(sum(n for (_, k), n in header_rows(sample).items() if k == "raw-header-read") == 5,
         "incomplete raw header walk")
    positive = ["seq", ["defun", "wh_control", ["object"], ["heap-read-u64", "object", 8]]]
    need(header_rows(positive).get(("wh_control", "header-accessor")) == 1,
         "accessor positive control failed")
    injected = json.loads(json.dumps(positive))
    injected[1].append(["ptr-read-u64", "object", 8])
    need(header_rows(injected).get(("wh_control", "raw-header-read")) == 1,
         "compiled-source mutation missed")
    need(not raw_sites("(defun public_call (p) (ptr-read-u64 p 8))"),
         "public Lisp memory accessor was mistaken for a native DSL load")
    need(not raw_sites("(equal form '(ptr-read-u8 address index))"),
         "recognizer literal was mistaken for emitted code")
    asm = out / "injected-asm.el"
    asm.write_text("(defun wh_emit (buf) (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi))\n")
    need(source_counts(asm)["raw_header_read_upper_bound"] == before + 1,
         "codegen census missed direct tag-byte emitter")
    dump = out / "injected.jsonl"
    units = dump_units(OUT / "dynamic/dump.jsonl")
    need(len(units) == 114, "incomplete linked-source control input")
    units["arena.o"].append(["defun", "wh_injected_header_read", ["object"],
                              ["ptr-read-u64", "object", 8]])
    dump.write_text("".join(json.dumps(dict(unit=u, source=s)) + "\n" for u, s in units.items()))
    census = out / "injected.tsv"
    tool = ROOT / "tools/ai/box-census.py"
    subprocess.run([sys.executable, str(tool), "from-dump", str(dump), str(census)], check=True)
    rejected = subprocess.run([sys.executable, str(tool), "check", str(census),
                               "--family", "all", "--idiom", "raw-header-read", "--max", "0"],
                              capture_output=True)
    (out / "linked.out").write_bytes(rejected.stdout)
    (out / "linked.err").write_bytes(rejected.stderr)
    need(rejected.returncode == 1 and b"raw-header-read 1 " in rejected.stdout,
         "standard build census accepted injected read")
    old = out / "uncertified.tsv"
    old.write_text("old.o\tf\tdefun\t1\n")
    stale = subprocess.run([sys.executable, str(tool), "check", str(old),
                            "--family", "all", "--idiom", "raw-header-read", "--max", "0"],
                           capture_output=True)
    need(stale.returncode != 0 and b"regenerate" in stale.stderr,
         "uncertified zero census accepted")
    old.write_text("")
    empty = subprocess.run([sys.executable, str(tool), "check", str(old),
                            "--family", "all", "--idiom", "raw-header-read", "--max", "0"],
                           capture_output=True)
    need(empty.returncode != 0, "empty census accepted")
    (out / "result.json").write_text(json.dumps(dict(
        source_delta=1, rejected_exit=run.returncode, traversal_raw_reads=5,
        accessor_positive=1, dump_injected_raw_reads=1, linked_rejected_exit=rejected.returncode,
        asm_delta=1, uncertified_rejected=True), indent=2) + "\n")
    print("PASS raw-read injection source=1 linked=1 codegen=1; check exits=1; uncertified scan rejected")


def migrate():
    # Mechanical refactor only. No string, comment, literal, offset, argument
    # order, ABI symbol, native registration or allocation size is rewritten.
    counts = {}
    typed = header_variables(OUT / "base/dump.jsonl")
    for path in producers():
        text = path.read_text()
        sites = raw_sites(text)
        if not sites:
            continue
        toks = list(tokens(text))
        at = {start: i for i, (_, start, _) in enumerate(toks)}
        stack, ranges, close = [], [], {}
        for i, (name, start, end) in enumerate(toks):
            if name == "(":
                stack.append(i)
            elif name == ")" and stack:
                opening = stack.pop()
                close[opening] = i
                if opening + 2 < len(toks) and toks[opening + 1][0] in ("defun", "cl-defun"):
                    ranges.append((toks[opening][1], end, toks[opening + 2][0]))
        edits = []
        for start, end, name, alias in sites:
            i = at[start]
            fn = max((r for r in ranges if r[0] <= start < r[1]), default=(0, 0, "-"))[2]
            if (name in ("ptr-read-u8", "ptr-read-u64") and i + 3 < len(toks)
                    and toks[i + 1][0] in typed.get(fn, set()) and toks[i + 3][0] == ")"):
                operand, offset = toks[i + 1][0], toks[i + 2][0]
                field = next((k for k, v in FIELDS.items() if v == (name, int(offset))), None) if offset.isdecimal() else None
                if field:
                    alias = field
                    edits.append((toks[i + 1][2], toks[i + 2][2], "", ""))
            if name in ASM and i and toks[i - 1][0] == "(":
                last = close.get(i - 1, 0) - 1
                if last > i and toks[last][0] in ("nelisp-sexp--offset-payload", "nelisp-sexp--offset-int-payload", "nelisp-sexp--offset-tag"):
                    if name.endswith("mov-reg-mem-disp8") and toks[last][0] != "nelisp-sexp--offset-tag":
                        alias = "nelisp-heap-accessors-x86_64-sexp-payload"
                    elif name == "nelisp-asm-arm64-ldr-imm" and toks[last][0] != "nelisp-sexp--offset-tag":
                        alias = "nelisp-heap-accessors-arm64-sexp-payload"
                    elif name == "nelisp-asm-arm64-ldrb-imm" and toks[last][0] == "nelisp-sexp--offset-tag":
                        alias = "nelisp-heap-accessors-arm64-sexp-tag"
                    if alias.endswith(("sexp-payload", "sexp-tag")):
                        edits.append((toks[last - 1][2], toks[last][2], "", ""))
            edits.append((start, end, name, alias))
        changed = text
        for start, end, name, alias in sorted(edits, reverse=True):
            changed = changed[:start] + alias + changed[end:]
        if any(name in ASM for _, _, name, _ in sites):
            # A compiler-only dependency, never injected into quoted DSL.
            marker = ";;; Code:"
            require = "(require 'nelisp-heap-accessors)"
            need(marker in changed, f"no Code marker: {path}")
            if require not in changed:
                changed = changed.replace(marker, marker + "\n\n" + require, 1)
        path.write_text(changed)
        counts[str(path.relative_to(ROOT))] = len(sites)
    print(json.dumps(counts, indent=2))


def historical_source_hashes(names):
    """Authenticate S1's frozen sources without rebinding any measured bytes.

    S4 deliberately changes the representation that S1 proved unchanged.
    Artifact paths still name the original on-disk evidence; source paths
    name exactly the committed S1 tree, never a caller-selected revision.
    """
    revision = "b51fe66f4bb7fbac31d4ce44796f1f074c85d17b"
    names = [name for name in names if not name.startswith("target/")]
    request = "".join(f"{revision}:{name}\n" for name in names).encode()
    run = subprocess.run(["git", "cat-file", "--batch"], input=request,
                         cwd=ROOT, capture_output=True, check=True)
    stream, result = io.BytesIO(run.stdout), {}
    for name in names:
        header = stream.readline().split()
        need(len(header) == 3 and header[1] == b"blob",
             f"S1 source absent from pinned revision: {name}")
        data = stream.read(int(header[2]))
        need(stream.read(1) == b"\n", "invalid git blob framing")
        result[name] = hashlib.sha256(data).hexdigest()
    need(not stream.read(), "unexpected S1 source blobs")
    return result


def check(revision=None, linked=None, historical=False):
    check_source()
    manifest = json.loads((OUT / "s1-evidence.json").read_text())
    need(manifest["schema"] == 1 and manifest["baseline_commit"] == "50a75cbcb",
         "wrong S1 baseline/schema")
    frozen = historical_source_hashes(manifest["hashes"]) if historical else {}
    if revision:
        need(linked is not None, "historical S1 replay requires a current linked dump")
        ancestor = subprocess.run(["git", "merge-base", "--is-ancestor", revision, "HEAD"], cwd=ROOT)
        need(ancestor.returncode == 0, "S1 source revision is not an ancestor of HEAD")
        paths = [name for name in manifest["hashes"] if not name.startswith("target/")]
        archive = subprocess.run(["git", "archive", revision, "--", *paths],
                                 cwd=ROOT, capture_output=True)
        need(archive.returncode == 0, "cannot read pinned S1 source revision")
        with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as members:
            for member in members:
                if member.isfile():
                    frozen[member.name] = hashlib.sha256(members.extractfile(member).read()).hexdigest()
        units = dump_units(linked)
        need(len(units) == 114, "incomplete current linked-source input")
        current = dump_counts(linked)
        need(current["raw-header-read"] == 0 and current["header-accessor"] > 0,
             "current linked source bypasses the accessor layer")
        for name in (ACCESSORS, "tools/ai/box-census.py", "tools/ai/word-heap-accessor-code.el"):
            need(sha(ROOT / name) == manifest["hashes"][name], f"changed S1 accessor contract: {name}")
    for name, expected in manifest["hashes"].items():
        actual = frozen.get(name) if (revision or historical) and not name.startswith("target/") else sha(ROOT / name)
        need(actual == expected, f"changed S1 evidence: {name}")
    need(manifest["facts"] == facts(historical=historical), "S1 measured facts changed")
    if revision:
        print(f"Retained accessor-only S1 source={revision}; current linked raw=0")
    print("PASS S1.2 raw=0 ABI-S6.2=0; 51 identical-code cases; 24 Ir profiles <=+0.5%")


def source_proof():
    before = dump_units(OUT / "base/dump.jsonl")
    after = {u: lower(s) for u, s in dump_units(OUT / "dynamic/dump.jsonl").items()}
    need(set(before) == set(after) and len(before) == 114, "incomplete source units")
    need(all(before[u] == after[u] for u in before if u != "driver.o"),
         "accessor lowering changes runtime source")
    # Driver embeds authenticated generated Lisp, including a compiler-only
    # emitter whose new accessor alias/dependency changes that literal. Do not
    # assert whole-ELF byte identity. Restrict differences to that exact blob
    # and its length constants, and retain their identities for review.
    def blob(source):
        rows = [x for x in source if isinstance(x, list) and len(x) >= 3
                and x[:2] == ["data-blob", "nl_repl_prelude_source_literal_data"]]
        need(len(rows) == 1 and rows[0][2][0] == "$str", "missing bootstrap literal")
        return rows[0][2][1]
    a, b = blob(before["driver.o"]), blob(after["driver.o"])
    delta = len(b) - len(a)
    differences = []
    literals = {}

    def compare(x, y, fn="-", path=()):
        if x == y:
            return
        if isinstance(x, str) and isinstance(y, str):
            marker = "(setq nelisp-standalone--backquote-file "
            need(x.startswith(marker) and y.startswith(marker) and min(len(x), len(y)) > 1000000
                 and len(y) - len(x) == delta, f"unexpected driver string change: {path}")
            key = hashlib.sha256(x.encode()).hexdigest()
            literals[key] = dict(before_bytes=len(x), after_bytes=len(y),
                                 after_sha256=hashlib.sha256(y.encode()).hexdigest())
            differences.append(dict(path=list(path), kind="authenticated bootstrap literal"))
        elif isinstance(x, list) and isinstance(y, list) and len(x) == len(y):
            if x and x[0] == "defun":
                fn = x[1]
            for i, (u, v) in enumerate(zip(x, y)):
                compare(u, v, fn, path + (i,))
        else:
            need(fn == "nl_repl_prelude_source" and isinstance(x, int)
                 and isinstance(y, int) and y - x == delta and x > 1000000,
                 f"driver code changed outside bootstrap length: {path}")
            differences.append(dict(path=list(path), kind="bootstrap length", before=x, after=y))
    compare(before["driver.o"], after["driver.o"])
    return dict(units=114, exact_normalized_units=113, driver_changes=differences,
                bootstrap_bytes_delta=delta,
                bootstrap_literals=literals,
                bootstrap_before_sha256=hashlib.sha256(a.encode()).hexdigest(),
                bootstrap_after_sha256=hashlib.sha256(b.encode()).hexdigest())


def abi_tool():
    spec = importlib.util.spec_from_file_location("abi_ir", ROOT / "tools/ai/abi-ir.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def facts(historical=False):
    result = dict(source=source_proof())
    for name in ("dynamic", "static"):
        dump = OUT / name / "dump.jsonl"
        counts = dump_counts(dump)
        need(len(dump_units(dump)) == 114 and counts["raw-header-read"] == 0
             and counts["header-accessor"] > 3000, f"incomplete/unsafe linked source: {name}")
        result[name] = counts
        checked = subprocess.run([str(ROOT / "tools/ai/abi-merge-gate.sh"), "store-word",
                                   str(OUT / name / "census.tsv"), "0"], capture_output=True)
        need(checked.returncode == 0, f"ABI S6.2 failed: {name}")
        raw = subprocess.run([sys.executable, str(ROOT / "tools/ai/box-census.py"),
                               "check", str(OUT / name / "census.tsv"), "--family", "all",
                               "--idiom", "raw-header-read", "--max", "0"], capture_output=True)
        need(raw.returncode == 0, f"uncertified/nonzero build census: {name}")
    code = json.loads((OUT / "accessor-code.json").read_text())
    need(len(code) == 3 and {(x["arch"], x["format"]) for x in code}
         == {("x86_64", "elf"), ("x86_64", "coff"), ("aarch64", "elf")}
         and all(x["cases"] == 17 and x["identical_units"] and x["changed_offset_rejected"]
                 and x["broken_lowerer_rejected"] for x in code), "missing actual code comparisons")
    result["code"] = code
    abi = abi_tool()
    candidate = OUT / "dynamic/nelisp-dynamic"
    baseline = OUT / "base/nelisp-dynamic"
    need(sha(candidate) != sha(baseline), "candidate reused as baseline")
    directory = OUT / "ir"
    identities = abi.identities(candidate, baseline)
    if historical:
        # Copies of the S1 evidence keep their original path labels. Compare
        # exactly the same file/environment hashes under those labels; never
        # rewrite the recorded manifest or accept a changed artifact.
        recorded = json.loads((directory / "measurement.json").read_text())["identities"]
        suffix = "/tools/ai/abi-ir.py"
        origins = [name[:-len(suffix)] for name in recorded if name.endswith(suffix)]
        need(len(origins) == 1, "ambiguous S1 measurement origin")
        prefix = str(ROOT) + "/"
        identities = {(origins[0] + "/" + name[len(prefix):]
                       if name.startswith(prefix) else name): digest
                      for name, digest in identities.items()}
        for key in ("candidate-path", "baseline-path"):
            value = identities[key]
            need(value.startswith(prefix), f"S1 executable outside evidence copy: {key}")
            identities[key] = origins[0] + "/" + value[len(prefix):]
    else:
        # Retained evidence may be copied into an integration worktree. Preserve
        # every measured hash and the runtime-environment identity; translate only
        # the checkout prefix before comparing the immutable provenance record.
        measured = json.loads((directory / "measurement.json").read_text())["identities"]
        suffix = "target/word-heap-s1/dynamic/nelisp-dynamic"
        measured_candidate = measured["candidate-path"]
        need(measured_candidate.endswith("/" + suffix), "unexpected S1 candidate provenance")
        measured_root = measured_candidate[:-len(suffix)].rstrip("/")
        def original_path(value):
            prefix = str(ROOT) + "/"
            return measured_root + "/" + value[len(prefix):] if value.startswith(prefix) else value
        identities = {original_path(k): original_path(v) if k in ("candidate-path", "baseline-path") else v
                      for k, v in identities.items()}
    abi.validate_record(directory, identities, abi.ACCESSOR_MODES)
    ir = {}
    for mode in abi.ACCESSOR_MODES:
        rows, outputs = [], set()
        for layout in abi.LAYOUTS:
            row = dict(layout=layout)
            for who, binary in (("base", baseline), ("cand", candidate)):
                stem = directory / f"{mode}-{who}-{layout}"
                row[who], row[who + "_gc"] = abi.profile_counts(stem.with_suffix(".cg"), binary)
                outputs.add(stem.with_suffix(".out").read_bytes())
            rows.append(row)
        bb, cc = sum(x["base"] for x in rows), sum(x["cand"] for x in rows)
        need(len(outputs) == 1 and cc * 1000 <= bb * 1005, f"S1 Ir cap/output: {mode}")
        ir[mode] = dict(samples=rows, baseline_mean=bb / 4, candidate_mean=cc / 4,
                        percent=100 * (cc / bb - 1))
    result["ir"] = ir
    return result


def seal():
    check_source()
    result = facts()
    paths = {path for path in producers()}
    paths.add(ROOT / ACCESSORS)
    paths.update((ROOT / "tools/ai" / name) for name in (
        "word-heap-accessors.py", "word-heap-accessor-code.el", "box-census.py", "abi-ir.py"))
    paths.update(ROOT / "test" / name for name in (
        "abi-ir-test.py", "nelisp-shadow-differential-cases.el"))
    for directory in ("base", "dynamic", "static"):
        for name in ("dump.jsonl", "census.tsv", "build.log"):
            paths.add(OUT / directory / name)
        binary = "nelisp-static" if directory == "static" else "nelisp-dynamic"
        paths.update(OUT / directory / (binary + suffix) for suffix in ("", ".cold", ".native-startup.el"))
    paths.update(OUT / name for name in ("accessor-code.json", "accessor-code.out", "accessor-code.err",
                                        "controls/result.json", "controls/linked.out", "controls/source.out",
                                        "census-before.json", "ir/measurement.json"))
    # S1.3 remains pending for coordinator-owned Wine. Retain the completed
    # Linux acceptance inputs and outputs alongside the S1.2 proof anyway.
    paths.update(OUT / name for name in (
        "linux-evidence.json", "final-checks-repaired-env.log",
        "parity.el", "gnu-parity.out", "gnu-parity.err",
        "dynamic-parity.out", "dynamic-parity.err", "static-parity.out", "static-parity.err",
        "mcp-smoke.py", "mcp.json", "mcp.out", "mcp.err", "mcp-smoke.log",
        "parens-new-compile.out", "parens-new-compile.err", "legacy-decoders.log"))
    paths.update((OUT / "legacy-decoders").glob("*"))
    paths.add(ROOT / "target/final-verify/checks.sh")
    paths.update((ROOT / "target/final-verify/checks-word-heap-s1-final").glob("*"))
    report = dict(schema=1, baseline_commit="50a75cbcb", facts=result,
                  hashes={str(p.relative_to(ROOT)): sha(p) for p in sorted(paths)})
    (OUT / "s1-evidence.json").write_text(json.dumps(report, indent=2) + "\n")
    print("sealed S1 evidence; raw=0, ABI conversion debt=0, code cases=51, Ir profiles=24")


def main(args):
    cmd = args[0]
    if cmd == "census":
        result = dict(source=source_counts())
        if len(args) > 1:
            result["linked"] = dump_counts(args[1])
        print(json.dumps(result, indent=2))
    elif cmd == "check-source":
        if len(args) > 2 and args[1] == "--revision":
            check_source(revision=args[2])
        else:
            check_source(args[1] if len(args) > 1 else None)
    elif cmd == "controls":
        controls()
    elif cmd == "migrate":
        migrate()
    elif cmd == "check":
        if len(args) == 5 and args[1] == "--source-revision" and args[3] == "--linked":
            check(revision=args[2], linked=Path(args[4]))
        else:
            need(len(args) == 1, "check [--source-revision REV --linked DUMP]")
            check()
    elif cmd == "check-historical":
        check(historical=True)
    elif cmd == "seal":
        seal()
    else:
        raise ValueError(__doc__)


if __name__ == "__main__":
    try:
        main(sys.argv[1:])
    except (ValueError, OSError, KeyError, TypeError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
