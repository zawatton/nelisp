#!/usr/bin/env python3
"""Validate one run's startup phase events (Doc 235 measurement protocol).

Usage:
  check_phase_events.py --wrapper RUN.wrapper.json --driver RUN.events.sexp
  check_phase_events.py --self-test

Exit 0 only if every required phase is present, the run ids agree, the clock
is monotonic everywhere and the timestamps are non-decreasing in order.
"""
import json
import re
import sys

WRAPPER_FIRST = ["SPAWN"]
BOOT_ORDER = ["BOOT_ENTRY", "BOOT_STATE_RESTORED", "BOOT_REINIT_COMPLETE"]
# In the benchmark harness the driver file is itself the first user form, and
# the helper/prelude/producer loading and native admission run inside it, so
# the first top-level form completes BEFORE the application state is ready.
# A deployed application with an already prepared image may differ; this
# order describes the measured harness, not normal application startup.
DRIVER_ORDER = ["FIRST_FORM_COMPLETE", "APPLICATION_STATE_READY"]
KERNEL_ORDER = ["KERNEL_BEGIN", "KERNEL_END", "RESULT_VALIDATED"]
WRAPPER_LAST = ["CHILD_EXIT", "WRAPPER_CLEANUP_COMPLETE"]
KIND_CODES = {1: "cold", 2: "source"}


# ---- minimal reader for the printed record -------------------------------
def parse_sexp(text):
    toks = re.findall(r'"(?:\\.|[^"\\])*"|[()\[\]]|[^\s()\[\]"]+', text)
    pos = 0

    def atom(t):
        if t.startswith('"'):
            return re.sub(r'\\(.)', r'\1', t[1:-1])
        if re.fullmatch(r'-?\d+', t):
            return int(t)
        if re.fullmatch(r'-?\d*\.\d+(e[+-]?\d+)?', t):
            return float(t)
        return None if t == "nil" else t

    def read():
        nonlocal pos
        t = toks[pos]
        pos += 1
        if t in "([":
            close = ")" if t == "(" else "]"
            out = []
            while toks[pos] != close:
                out.append(read())
            pos += 1
            return out
        return atom(t)

    val = read()
    return val


def record_from_text(text):
    """Parse one flushed record into a dict."""
    raw = parse_sexp(text.strip())
    rec = {}
    i = 0
    while i + 1 < len(raw):
        rec[raw[i].lstrip(":")] = raw[i + 1]
        i += 2
    return rec


# ---- validation -----------------------------------------------------------
def validate(wrapper, rec):
    """Return a list of error strings (empty means valid)."""
    errs = []
    run_ids = list(wrapper)
    if len(run_ids) != 1:
        return [f"wrapper sidecar must hold exactly one run id, got {run_ids}"]
    rid = run_ids[0]
    w = wrapper[rid]
    if rec.get("run-id") != rid:
        errs.append(f"run id mismatch: wrapper={rid!r} driver={rec.get('run-id')!r}")
    if w.get("clock") != "CLOCK_MONOTONIC":
        errs.append(f"wrapper clock is not monotonic: {w.get('clock')!r}")
    if rec.get("clock") != "monotonic":
        errs.append(f"driver clock is not monotonic: {rec.get('clock')!r}")
    boot = rec.get("boot")
    if not (isinstance(boot, list) and len(boot) == 4):
        errs.append("boot record missing or malformed")
        boot = [None] * 4
    elif boot[3] not in KIND_CODES and boot[3] not in ("cold", "source"):
        errs.append(f"unknown boot kind {boot[3]!r}")
    events = {}
    warm = {}
    for ev in rec.get("events") or []:
        name, ns = ev[0], ev[1]
        if name == "BENCH_WARMUP_BEGIN" or name == "BENCH_WARMUP_END":
            warm.setdefault(name, []).append(ns)
        elif name in events:
            errs.append(f"duplicate event {name}")
        else:
            events[name] = ns
    # Assemble the ordered chain of (name, ns); missing names are errors.
    chain = []

    def need(name, ns):
        if not isinstance(ns, int) or isinstance(ns, bool) or ns <= 0:
            errs.append(f"missing or invalid phase {name}")
        else:
            chain.append((name, ns))

    need("SPAWN", w.get("SPAWN"))
    for name, ns in zip(BOOT_ORDER, boot[:3]):
        need(name, ns)
    for name in DRIVER_ORDER:
        need(name, events.get(name))
    nwarm = rec.get("warmups")
    try:
        nwarm = int(nwarm) if nwarm is not None else 0
    except ValueError:
        nwarm = 0
    if nwarm > 0:
        b, e = warm.get("BENCH_WARMUP_BEGIN", []), warm.get("BENCH_WARMUP_END", [])
        if len(b) != nwarm or len(e) != nwarm:
            errs.append(f"expected {nwarm} warmup begin/end pairs, got {len(b)}/{len(e)}")
        else:
            for i in range(nwarm):
                chain.append((f"BENCH_WARMUP_{i}_BEGIN", b[i]))
                chain.append((f"BENCH_WARMUP_{i}_END", e[i]))
    for name in KERNEL_ORDER:
        need(name, events.get(name))
    need("CHILD_EXIT", w.get("CHILD_EXIT"))
    need("WRAPPER_CLEANUP_COMPLETE", w.get("WRAPPER_CLEANUP_COMPLETE"))
    for (an, a), (bn, b) in zip(chain, chain[1:]):
        if a > b:
            errs.append(f"out of order: {an}={a} > {bn}={b}")
    # Identity keys (Doc 235): process, image, and source.  A wrapper record
    # written before identities were recorded lacks these keys; when present
    # they must be non-empty and agree with the driver record.
    if "image_sha256" in w or "source_id" in w:
        if not w.get("source_id"):
            errs.append("missing source identity")
        if w.get("image_sha256") != rec.get("image-sha256"):
            errs.append("image identity mismatch between wrapper and driver")
    if not rec.get("pid"):
        errs.append("missing process identity")
    return errs


def main_check(wrapper_path, driver_path):
    wrapper = json.load(open(wrapper_path))
    text = open(driver_path).read().strip()
    if not text:
        print("FAIL: empty driver event file")
        return 1
    recs = [record_from_text(line) for line in text.splitlines() if line.strip()]
    rid = next(iter(wrapper), None)
    recs = [r for r in recs if r.get("run-id") == rid] or recs
    if len(recs) != 1:
        print(f"FAIL: expected one driver record, got {len(recs)}")
        return 1
    errs = validate(wrapper, recs[0])
    for e in errs:
        print("FAIL:", e)
    print("OK" if not errs else f"{len(errs)} error(s)")
    return 1 if errs else 0


# ---- self test ------------------------------------------------------------
def _synthetic():
    rid = "11111111-2222-3333-4444-555555555555"
    wrapper = {rid: {"SPAWN": 1000, "CHILD_EXIT": 9000,
                     "WRAPPER_CLEANUP_COMPLETE": 9500, "clock": "CLOCK_MONOTONIC",
                     "source_id": "src-tree", "image_sha256": "ab"}}
    text = ('(:run-id "%s" :clock monotonic :boot [1100 1200 1300 1] :pid 7 '
            ':image-sha256 "ab" :driver "/d.el" :warmups "2" :events ('
            '(FIRST_FORM_COMPLETE 1400 nil) (APPLICATION_STATE_READY 1500 nil) '
            '(BENCH_WARMUP_BEGIN 2000 0) (BENCH_WARMUP_END 3000 0) '
            '(BENCH_WARMUP_BEGIN 3100 1) (BENCH_WARMUP_END 4000 1) '
            '(KERNEL_BEGIN 5000 nil) (KERNEL_END 6000 nil) '
            '(RESULT_VALIDATED 7000 nil)))' % rid)
    return wrapper, text


def self_test():
    import copy
    ok = True

    def expect(label, wrapper, text, want_pass):
        nonlocal ok
        errs = validate(wrapper, record_from_text(text))
        passed = not errs
        status = "ok" if passed == want_pass else "WRONG"
        if passed != want_pass:
            ok = False
        print(f"self-test {label}: {'pass' if passed else 'fail'} (expected "
              f"{'pass' if want_pass else 'fail'}) [{status}]"
              + ("" if passed else f" -- {errs[0]}"))

    wrapper, text = _synthetic()
    expect("valid", wrapper, text, True)
    expect("missing-phase", wrapper, text.replace("(RESULT_VALIDATED 7000 nil)", ""), False)
    expect("out-of-order", wrapper, text.replace("(KERNEL_END 6000 nil)", "(KERNEL_END 4500 nil)"), False)
    bad = copy.deepcopy(wrapper)
    rid = next(iter(bad))
    bad = {"99999999-0000-0000-0000-000000000000": bad[rid]}
    expect("run-id-mismatch", bad, text, False)
    expect("wall-clock", wrapper, text.replace("monotonic", "wall"), False)
    nosrc = copy.deepcopy(wrapper)
    nosrc[rid]["source_id"] = ""
    expect("missing-source-id", nosrc, text, False)
    expect("image-mismatch", wrapper, text.replace(':image-sha256 "ab"', ':image-sha256 "cd"'), False)
    expect("missing-pid", wrapper, text.replace(":pid 7", ":pid nil"), False)
    return 0 if ok else 1


def main(argv):
    if argv[1:] == ["--self-test"]:
        return self_test()
    if len(argv) == 5 and argv[1] == "--wrapper" and argv[3] == "--driver":
        return main_check(argv[2], argv[4])
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
