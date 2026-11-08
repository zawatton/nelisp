#!/usr/bin/env python3
"""Static check: variant standalone-reader builds never overwrite the default.

Scans the Makefile, tools/*.sh and tools/ai/*.sh for calls that build the
standalone reader and enforces three rules (S6.1 of the CI speed ledger):

  R1  A build call carrying any NELISP_* assignment other than the benign
      ones (target, output, cache dir, jobs, job wrapper) is a variant build
      and must also set a non-default NELISP_STANDALONE_READER_OUTPUT, so it
      cannot replace target/nelisp or target/nelisp.cold.
  R2  Cache poisoning.  The unit cache partition name is computed by
      `nelisp-standalone--target-cache-dir' (scripts/nelisp-standalone-build.el)
      from exactly these environment variables: NELISP_READER_DYNAMIC (-dyn),
      NELISP_TCO (-tco unless "0") and NELISP_RUNTIME_RELOAD (-reload via
      `nelisp-standalone--runtime-reload-enabled-p'); the Windows arena base
      comes from `nelisp-standalone-arena-rewrite-windows-base', not from the
      environment.  Any other mode variable (for example NELISP_LINUX_ARENA_SIZE,
      NELISP_INLINE, NELISP_STANDALONE_COLD_IMAGE) is in neither the partition
      name nor the unit content key, so a build carrying it must also set its
      own NELISP_STANDALONE_UNIT_CACHE_DIR.
  R3  A direct default build (no mode variables, default output) that bypasses
      tools/nelisp-standalone-reader-build.sh is flagged: default builds go
      through `make standalone-reader' so an unchanged tree skips the build.

Calls are recognised by `-f nelisp-standalone-build-reader',
`-f nelisp-standalone-reader-*-test', `bash tools/nelisp-standalone-reader-build.sh'
(without --status) and `$(MAKE)|make standalone-reader'.  Known exceptions are
listed in ALLOWLIST with a reason; a stale entry fails the test.  Limitation:
variables exported on an earlier line or set through shell arrays are not
seen, so scripts that do that must be allowlisted.
"""
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Keep in step with the env handling of tools/nelisp-standalone-reader-build.sh
# (`env_included' and its denylist): that tool hashes every NELISP_* variable
# except GATE_MUTATION_*, CHECK_SKIP, FORCE_REBUILD, JOBS and JOB_WRAPPER.  This
# set is a different question -- which variables do NOT make a build a variant
# -- so TARGET, READER_OUTPUT and UNIT_CACHE_DIR are benign here although the
# stamp key does include them.
BENIGN = {
    "NELISP_STANDALONE_TARGET",
    "NELISP_STANDALONE_READER_OUTPUT",
    "NELISP_STANDALONE_UNIT_CACHE_DIR",
    "NELISP_STANDALONE_JOBS",
    "NELISP_STANDALONE_JOB_WRAPPER",
    # Selects only which Emacs runs the build, not what it produces.
    "NELISP_BUILD_EMACS",
}
# Read from `nelisp-standalone--target-cache-dir'; see the module docstring.
PARTITION_VARS = {"NELISP_READER_DYNAMIC", "NELISP_TCO", "NELISP_RUNTIME_RELOAD"}

# (relative path, Makefile target or "" for scripts) -> reason.
ALLOWLIST = {
    ("Makefile", "standalone-reader"):
        "the stamp-tool recipe itself",
    ("Makefile", "standalone-reader-status"):
        "read-only twin of standalone-reader: runs the stamp tool with "
        "--status and the same build command, never builds",
    ("Makefile", "standalone-reader-ffi-smoke"):
        "Windows branch: direct default build on a target where the dyn "
        "variant does not exist; Linux branch goes through the stamp tool",
    ("Makefile", "standalone-reader-tls-smoke-windows"):
        "Windows-only recipe (MSYS make boundary), default Windows build",
    ("tools/linux-standalone-reader-test.sh", ""):
        "embedded-source (NELISP_SRC) exit-code test; builds its own source "
        "into the default output by design",
    ("tools/macos-standalone-reader-test.sh", ""):
        "embedded-source (NELISP_SRC) exit-code test on macOS; same as the "
        "Linux one",
    ("tools/ai/integ-wh-build.sh", ""):
        "integration worktree builder: own output under target/integ, mode "
        "variables set through a shell array",
    ("tools/ai/nelisp-standalone-build-split.sh", ""):
        "split-build script: two-phase cold-image build of the default "
        "binary by design",
    ("tools/ai/windows-bringup-check.sh", ""):
        "Windows bring-up on MSYS2: deliberately rebuilds the default "
        "target/nelisp.exe in a cold-image mode (known cache-key gap)",
    ("tools/ai/win-smoke.sh", ""):
        "cross-built Windows PE under wine with its own output directory",
}
# Class exemptions by regex over the build call.
CLASS_ALLOW = [
    (re.compile(r"-f\s+nelisp-standalone-reader-[a-z0-9-]+-test\b"),
     "elisp `-f nelisp-standalone-reader-*-test' builders run their own "
     "in-Emacs build"),
]

BUILD_RE = re.compile(r"-f\s+nelisp-standalone-build-reader\b")
STAMP_RE = re.compile(r"\bbash\s+\S*tools/nelisp-standalone-reader-build\.sh\b")
MAKE_RE = re.compile(
    r"(\$\(MAKE\)|\$\{MAKE:-make\}|\bmake)\s+standalone-reader(?![\w-])")
TEST_BUILD_RE = re.compile(r"-f\s+nelisp-standalone-reader-[a-z0-9-]+-test\b")
ASSIGN_RE = re.compile(r"(?<![\w$])(NELISP_[A-Z0-9_]+)=(\S*)")
TARGET_RE = re.compile(r"^([A-Za-z0-9_.%/+-]+)\s*:(?!=)")
DEFAULT_OUT_RE = re.compile(
    r"^[\"']?(\$\{?REPO_ROOT\}?/|\./)?target/nelisp(\.exe)?[\"']?$")


def logical_lines(path):
    """Yield (lineno, makefile-target, text) with continuations joined."""
    target = ""
    buf, start = [], 0
    with open(path, encoding="utf-8", errors="replace") as fh:
        for no, raw in enumerate(fh, 1):
            line = raw.rstrip("\n")
            if not buf:
                start = no
                if path.endswith("Makefile"):
                    m = TARGET_RE.match(line)
                    if m and not line.startswith("\t"):
                        target = m.group(1)
            if line.endswith("\\"):
                buf.append(line[:-1])
                continue
            buf.append(line)
            yield start, target, " ".join(s.strip() for s in buf)
            buf = []
    if buf:
        yield start, target, " ".join(s.strip() for s in buf)


def is_comment_or_echo(text):
    t = text.lstrip("@- \t")
    return t.startswith("#") or t.startswith("echo") or t.startswith("printf")


def find_calls(path):
    rel = os.path.relpath(path, ROOT)
    for no, target, text in logical_lines(path):
        if is_comment_or_echo(text):
            continue
        direct = bool(BUILD_RE.search(text) or TEST_BUILD_RE.search(text))
        stamp = bool(STAMP_RE.search(text) and "--status" not in text)
        viamake = bool(MAKE_RE.search(text))
        if not (direct or stamp or viamake):
            continue
        yield {
            "file": rel, "line": no, "target": target, "text": text,
            "stamped": stamp or viamake,
            "assign": dict(ASSIGN_RE.findall(text)),
        }


def classify(call, allowlist=ALLOWLIST, class_allow=CLASS_ALLOW, used=None):
    """Return a list of violation strings for one call."""
    key = (call["file"], call["target"] if call["file"] == "Makefile" else "")
    if key in allowlist:
        if used is not None:
            used.add(key)
        return []
    for rx, _reason in class_allow:
        if rx.search(call["text"]):
            return []
    out = []
    assign = call["assign"]
    mode = {v for v in assign if v not in BENIGN}
    outval = assign.get("NELISP_STANDALONE_READER_OUTPUT")
    own_output = outval is not None and not DEFAULT_OUT_RE.match(outval)
    where = "%s:%d" % (call["file"], call["line"])
    if mode and not own_output:
        out.append("%s R1: mode %s without a non-default "
                   "NELISP_STANDALONE_READER_OUTPUT" % (where, sorted(mode)))
    unpartitioned = mode - PARTITION_VARS
    if unpartitioned and "NELISP_STANDALONE_UNIT_CACHE_DIR" not in assign:
        out.append("%s R2: %s are not in the unit-cache partition name and "
                   "need NELISP_STANDALONE_UNIT_CACHE_DIR" %
                   (where, sorted(unpartitioned)))
    if not call["stamped"] and not mode and not own_output:
        out.append("%s R3: direct default build bypasses "
                   "tools/nelisp-standalone-reader-build.sh" % where)
    return out


def scan():
    paths = [os.path.join(ROOT, "Makefile")]
    for sub in ("tools", os.path.join("tools", "ai")):
        d = os.path.join(ROOT, sub)
        paths += sorted(os.path.join(d, f) for f in os.listdir(d)
                        if f.endswith(".sh"))
    violations, used, ncalls = [], set(), 0
    for p in paths:
        for call in find_calls(p):
            ncalls += 1
            violations += classify(call, used=used)
    stale = [k for k in ALLOWLIST if k not in used]
    return ncalls, violations, stale


def negative_controls():
    """Synthetic call text: the first three must fail, the rest must pass."""
    def mk(text, stamped):
        return {"file": "synthetic.sh", "line": 1, "target": "",
                "text": text, "stamped": stamped,
                "assign": dict(ASSIGN_RE.findall(text))}
    bad = [
        ("mode var with default output",
         mk("NELISP_READER_DYNAMIC=1 emacs -f nelisp-standalone-build-reader",
            False), "R1"),
        ("non-partitioning mode var, own output, no private cache dir",
         mk("NELISP_LINUX_ARENA_SIZE=8 NELISP_STANDALONE_READER_OUTPUT="
            "target/v bash tools/nelisp-standalone-reader-build.sh --target "
            "x -- emacs -f nelisp-standalone-build-reader", True), "R2"),
        ("direct default build",
         mk("emacs --batch -f nelisp-standalone-build-reader", False), "R3"),
    ]
    good = [
        ("partitioning mode var with own output",
         mk("NELISP_READER_DYNAMIC=1 NELISP_STANDALONE_READER_OUTPUT="
            "target/nelisp-dyn make standalone-reader", True)),
        ("non-partitioning mode var with own output and cache dir",
         mk("NELISP_LINUX_ARENA_SIZE=8 NELISP_STANDALONE_READER_OUTPUT="
            "target/v NELISP_STANDALONE_UNIT_CACHE_DIR=target/c "
            "bash tools/nelisp-standalone-reader-build.sh --target x -- "
            "emacs -f nelisp-standalone-build-reader", True)),
        ("default build through the stamp recipe",
         mk("$(MAKE) standalone-reader NELISP_STANDALONE_TARGET=linux-x86_64",
            True)),
    ]
    errors = []
    for name, call, rule in bad:
        res = classify(call, allowlist={}, class_allow=[])
        if not any(" %s:" % rule in r for r in res):
            errors.append("negative control did not fail %s: %s" % (rule, name))
    for name, call in good:
        res = classify(call, allowlist={}, class_allow=[])
        if res:
            errors.append("positive control failed: %s -> %s" % (name, res))
    return errors


def main():
    errors = negative_controls()
    ncalls, violations, stale = scan()
    errors += violations
    errors += ["stale allowlist entry: %s" % (k,) for k in stale]
    for e in errors:
        print("FAIL: " + e)
    print("GATE-COUNT checked=%d findings=%d" % (ncalls, len(errors)))
    if errors:
        return 1
    print("PASS: %d build calls, 6 controls" % ncalls)
    return 0


if __name__ == "__main__":
    sys.exit(main())
