#!/usr/bin/env bash
# Fast runner controls. LIB supplies read-only inventory/stderr helpers when
# this script is used in an isolated lane rather than a complete checkout.
set -euo pipefail
test_dir=$(cd "$(dirname "$0")" && pwd)
python3 - "$test_dir/c-core-parity-smoke.sh" "${LIB:-$test_dir/../..}" <<'PY'
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time

RUNNER = Path(sys.argv[1])
HELPERS = Path(sys.argv[2]) / "test/nelisp-emacs-lib"
AREAS = ("x-gui", "display", "process", "buffer", "chars", "files", "other")
NAMES = ("alpha", "message", "gamma", "delta", "epsilon", "zeta", "eta")
checks = []

def check(label, condition):
    checks.append((label, bool(condition)))
    print(f"{'PASS' if condition else 'FAIL'} {label}", flush=True)
    if not condition:
        raise AssertionError(label)

STUB = r'''#!/usr/bin/env bash
set -eu
ulimit -c 0
role=host
if [ "${1:-}" = --cold-load-from ]; then
  role=nelisp
  [ -s "$2" ] || { echo 'image missing' >&2; exit 94; }
  if [ "${3:-}" = --eval ]; then
    python3 - "$2" "$4" <<'CHECK'
import json, re, sys
from pathlib import Path
if Path(sys.argv[1]).read_text() != 'fixture heap image\n':
    sys.exit(10)
marker = json.loads(re.search(r'princ ("(?:[^"\\]|\\.)*")', sys.argv[2])[1])
sys.stdout.write(marker + 't\n')
CHECK
    exit $?
  fi
  [ "${3:-}" = --load ] || { echo 'expected image-backed driver' >&2; exit 95; }
  driver=$4
  ! rg -q 'nemacs-bootstrap.el' "$driver" || { echo 'source fallback' >&2; exit 96; }
fi
if [ "${1:-}" = --eval ]; then
  [ "${FAKE_CASE:-pass}" != image_fail ] || exit 9
  [ "${FAKE_CASE:-pass}" != image_empty ] || { printf 't\n'; exit 0; }
  python3 - "$2" <<'BUILD'
import json, re, sys
from pathlib import Path
form = sys.argv[1]
path = json.loads(re.search(r'nelisp--arena-dump-image-stream ("[^"\n]*")', form)[1])
Path(path).write_text('fixture heap image\n')
marker = json.loads(re.search(r'princ ("(?:[^"\\]|\\.)*")', form)[1])
sys.stdout.write(marker + 't\n')
BUILD
  exit 0
fi
if [ -n "${C_CORE_PROBE_DIR:-}" ]; then
  python3 -c 'import json; from pathlib import Path; print("C-CORE-PROBES:" + json.dumps([s.split("\t")[0] for s in Path("tools/c-core-areas.tsv").read_text().splitlines()]))'
  exit 0
fi
area=${C_CORE_AREA:-}
phase=eval
for arg in "$@"; do
  case "$arg" in *precheck.el) phase=precheck;; esac
done
if [ "$phase" = precheck ]; then
  printf ";;; -*- lexical-binding: t; -*-\n(setq c-core-parity--staged-entries '(" > "$C_CORE_SELECTED_PROBES"
  awk -F '\t' -v area="$area" 'area=="" || $2==area {printf "(%s 1)", $1}' tools/c-core-areas.tsv >> "$C_CORE_SELECTED_PROBES"
  printf '))\n' >> "$C_CORE_SELECTED_PROBES"
  expected=$(awk -F '\t' -v area="$area" 'area=="" || $2==area {n++} END {print n+0}' tools/c-core-areas.tsv)
  printf 'P-EXPECTED|%s\n' "$expected"
  exit 0
fi
# Certification must load the very same staged file on both sides.
if [ "${FAKE_CERT:-0}" = 1 ]; then
  [ -n "${C_CORE_SELECTED_PROBES:-}" ] || { echo 'missing staged selection' >&2; exit 90; }
  [ -z "${C_CORE_AREA:-}" ] || { echo 'redundant runtime area filter' >&2; exit 92; }
  # Runtime selection comes solely from the staged file, as on real binaries.
  area=$(basename "$(dirname "$C_CORE_SELECTED_PROBES")")
  if [ "$role" = host ]; then
    found=0
    for arg in "$@"; do
      if [ "$arg" = "$C_CORE_SELECTED_PROBES" ] || [ "$arg" = "${C_CORE_SELECTED_PROBES#$FAKE_ROOT/}" ]; then found=1; fi
    done
    [ "$found" = 1 ] || { echo 'host did not load staged selection' >&2; exit 90; }
  else
    rg -q 'C_CORE_SELECTED_PROBES' "$driver" || exit 91
    head -n 1 "$driver" | rg -q 'lexical-binding: t' || exit 93
  fi
  sha256sum "$C_CORE_SELECTED_PROBES" | awk '{print $1}' > "$FAKE_ROOT/events/$role-$area.selection"
fi
date +%s%N > "$FAKE_ROOT/events/$role-${area:-unit}.start"
printf '%s\n' "$$" > "$FAKE_ROOT/events/$role-${area:-unit}.pid"
sleep 0.08
case "${FAKE_CASE:-pass}:$role:$area" in
  crash:host:process) kill -TERM "$$";;
  timeout:nelisp:chars|cancel:nelisp:x-gui)
    trap '' TERM
    sleep 60 &
    printf '%s %s\n' "$$" "$!" > "$FAKE_ROOT/timeout.pids"
    wait
    ;;
  mutate:nelisp:files) printf '; changed during run\n' >> packages/nelisp-emacs-foundation/src/fixture.el;;
  stderr:nelisp:buffer) printf 'unexpected diagnostic\n' >&2;;
esac
awk -F '\t' -v area="$area" 'area=="" || $2==area {print $1}' tools/c-core-areas.tsv |
while IFS= read -r name; do
  if [ "$role" = nelisp ] && ! rg -Fq "($name 1)" "$C_CORE_SELECTED_PROBES"; then continue; fi
  value=1
  if [ "${FAKE_CASE:-pass}:$role:$name" = mismatch:nelisp:delta ]; then value=changed; fi
  if [ "${FAKE_CASE:-pass}:$role:$name" = unterminated:nelisp:epsilon ]; then value='"unterminated'; fi
  if [ "${FAKE_CASE:-pass}:$name" = multiline:message ]; then
    printf 'P| %s | "first \"quoted\" line\nP-DONE\nlast"\n' "$name"
  else
    printf 'P| %s | %s\n' "$name" "$value"
  fi
  if [ "$name" = message ]; then
    if [ "${FAKE_CASE:-pass}:$role:$area" = effects:nelisp:display ]; then
      printf 'changed message side effect\n' >&2
    else
      printf 'message side effect\n' >&2
    fi
  fi
done
if [ "${FAKE_CASE:-pass}:$role:$area" = continuation:nelisp:files ]; then printf 'unexpected continuation\n'; fi
if [ "${FAKE_CASE:-pass}:$role:$area" != marker:nelisp:files ]; then printf 'P-DONE\n'; fi
if [ "$role" = nelisp ] && [ "${FAKE_CASE:-pass}" != marker ]; then printf 't\n'; fi
date +%s%N > "$FAKE_ROOT/events/$role-${area:-unit}.end"
'''

def main(root):
    test = root / "test/nelisp-emacs-lib"
    (test / "c-core-probes").mkdir(parents=True)
    (root / "tools").mkdir()
    (root / "build").mkdir()
    package = root / "packages/nelisp-emacs-foundation/src"
    package.mkdir(parents=True)
    (package / "fixture.el").write_text("; package identity\n")
    (root / "build/nemacs-bootstrap.el").write_text("; fixture bundle\n")
    (test / "c-core-parity-driver.el").write_text("; fixture driver\n")
    (test / "c-core-probes/fixture.el").write_text(
        "".join(f"({name} 1)\n" for name in NAMES))
    table = "".join(f"{name}\t{area}\n" for name, area in zip(NAMES, AREAS))
    (root / "tools/c-core-areas.tsv").write_text(table)
    (root / "build/c-core-census.tsv").write_text(
        "".join(f"{name}\tinterpreted\t1\t0\t0\n" for name in NAMES))
    for name in ("c-core-stderr.py", "c-core-inventory-verify.py"):
        shutil.copy2(HELPERS / name, test / name)
    # Only the temporary seven-name fixture changes the census size. The real
    # inventory verifier and stderr validator otherwise run without alteration.
    inventory = test / "c-core-inventory-verify.py"
    source = inventory.read_text()
    anchor = "EXPECTED_CENSUS_COUNT = 1460"
    if source.count(anchor) != 1:
        raise AssertionError("inventory census-size contract changed")
    inventory.write_text(source.replace(anchor, "EXPECTED_CENSUS_COUNT = 7"))
    script = test / RUNNER.name
    original = RUNNER.read_text()
    shutil.copy2(RUNNER, script)
    shutil.copy2(RUNNER.parents[2] / "tools/c-core-image.sh", root / "tools/c-core-image.sh")
    for role in ("host", "nelisp"):
        stub = root / role
        stub.write_text(STUB)
        stub.chmod(0o755)
    (root / "nelisp.cold").write_text("fixture cold image\n")
    out = root / "build/c-core-parity"
    events = root / "events"
    events.mkdir()

    def environment(case, args, jobs):
        env = os.environ.copy()
        for key in ("C_CORE_PROBE_DIR", "C_CORE_UNIT", "C_CORE_AREA",
                    "C_CORE_EXTRA", "C_CORE_SELECTED_PROBES", "C_CORE_PARITY_JOBS"):
            env.pop(key, None)
        env.update(NELISP_BIN=str(root / "nelisp"), EMACS=str(root / "host"),
                   C_CORE_INVENTORY_EMACS=str(root / "host"), FAKE_ROOT=str(root),
                   FAKE_CASE=case, FAKE_CERT=str(int(args == ("run",))))
        if jobs is not None:
            env["C_CORE_PARITY_JOBS"] = str(jobs)
        return env

    def invoke(case="pass", *args, jobs=None):
        if not args:
            args = ("run",)
        shutil.rmtree(events)
        events.mkdir()
        result = subprocess.run(["bash", str(script), *args], cwd=root,
                                env=environment(case, args, jobs), text=True,
                                capture_output=True, timeout=20)
        (root / "last.stdout").write_text(result.stdout)
        (root / "last.stderr").write_text(result.stderr)
        return result

    def receipt(area):
        return (out / f"{area}.result").read_text()

    def passes_except(excluded=()):
        return all(re.fullmatch(r"PASS covered=1/1 unknown=0\nIDENTITY [a-f0-9]{64}\n",
                                receipt(area)) for area in AREAS if area not in excluded)

    def isolated_failure(case, area, reason):
        result = invoke(case)
        text = receipt(area)
        print(f"CASE {case}: runner_exit={result.returncode}; {text.splitlines()[0]}")
        check(f"{case}: intended failure and six independent passes",
              result.returncode == 1 and text.startswith("FAIL ") and reason in text
              and passes_except((area,)))
        check(f"{case}: check rejects failed area",
              invoke("pass", "check", area).returncode != 0)

    result = invoke()
    if result.returncode:
        print(result.stdout + result.stderr, file=sys.stderr)
    check("all-pass: seven exact-format receipts and seven output lines",
          result.returncode == 0 and passes_except()
          and len(re.findall(r"^  [\w-]+: PASS ", result.stdout, re.M)) == 7)
    check("all-pass: separate host/standalone processes and identical selections",
          len({p.read_text().strip() for p in events.glob("*.pid")}) == 14
          and all((events / f"host-{area}.selection").read_bytes()
                  == (events / f"nelisp-{area}.selection").read_bytes() for area in AREAS)
          and all((out / area / "host.raw").is_file()
                  and (out / area / "nelisp.raw").is_file() for area in AREAS))
    # Exercise the actual generated precheck with GNU, rather than trusting
    # the fake precheck to reproduce its selected-file header correctly.
    selected = root / "build/cookie-selection.el"
    host = os.environ.get("EMACS", "emacs")
    env = os.environ.copy()
    env.update(C_CORE_UNIT="", C_CORE_AREA="x-gui",
               C_CORE_SELECTED_PROBES=str(selected))
    precheck = subprocess.run(
        [host, "-Q", "--batch", "-l", str(out / "x-gui/precheck.el")],
        cwd=root, env=env, text=True, capture_output=True, timeout=20)
    check("generated precheck: cookie, exact selection and count",
          precheck.returncode == 0 and not precheck.stderr
          and precheck.stdout == "P-EXPECTED|1\n"
          and selected.read_text().splitlines()[0] == ";;; -*- lexical-binding: t; -*-")
    command = [host, "-Q", "--batch", "-l", str(selected), "--eval",
               "(unless (equal c-core-parity--staged-entries '((alpha 1))) (kill-emacs 1))"]
    loaded = subprocess.run(command, cwd=root, text=True,
                            capture_output=True, timeout=20)
    check("generated selection: loads without warnings and preserves forms",
          loaded.returncode == 0 and not loaded.stdout and not loaded.stderr)
    selected.write_text(selected.read_text().split("\n", 1)[1])
    broken = subprocess.run(command, cwd=root, text=True,
                            capture_output=True, timeout=20)
    check("cookie negative control: GNU reports the original missing-cookie warning",
          broken.returncode == 0 and "lexical-binding" in broken.stderr
          and "Missing" in broken.stderr)
    intervals = [(int(p.read_text()), int(p.with_suffix(".end").read_text()))
                 for p in events.glob("*.start")]
    peak = max(sum(start <= instant < end for start, end in intervals)
               for instant, _ in intervals)
    check("default jobs: concurrent and bounded by four", 1 < peak <= 4)
    check("check AREA: all seven receipts remain valid",
          all(invoke("pass", "check", area).returncode == 0 for area in AREAS))
    result = invoke(jobs=1)
    intervals = [(int(p.read_text()), int(p.with_suffix(".end").read_text()))
                 for p in events.glob("*.start")]
    peak = max(sum(start <= instant < end for start, end in intervals)
               for instant, _ in intervals)
    check("jobs=1: serial certification succeeds", result.returncode == 0 and peak == 1)
    result = invoke("multiline")
    check("multiline strings: escaped quotes and embedded marker preserve exact coverage",
          result.returncode == 0 and passes_except())
    isolated_failure("unterminated", "chars", "NeLisp completion or probe count invalid")
    isolated_failure("continuation", "files", "NeLisp completion or probe count invalid")
    isolated_failure("mismatch", "buffer", "transcripts differ")
    isolated_failure("crash", "process", "host exited 143")
    isolated_failure("stderr", "buffer", "nelisp stderr validation failed")
    isolated_failure("effects", "display", "deterministic stderr side effects differ")
    isolated_failure("marker", "files", "NeLisp completion or probe count invalid")

    check("production caps: 120s host/precheck and 300s standalone certification",
          original.count('area_timeout 120 "$HOST"') == 2
          and original.count('area_timeout 300 "$BIN"') == 1)
    # Shorten only the /tmp copy for an actual deadline test, never production.
    script.write_text(original.replace('area_timeout 300 "$BIN"', 'area_timeout 1 "$BIN"'))
    isolated_failure("timeout", "chars", "NeLisp exited 124")
    pids = [int(p) for p in (root / "timeout.pids").read_text().split()]

    def running(pid):
        path = Path(f"/proc/{pid}/stat")
        try:
            # Killed descendants can briefly remain zombies until reaped by init.
            return path.read_text().split(") ", 1)[1].split()[0] != "Z"
        except FileNotFoundError:
            return False

    check("timeout: neither parent nor TERM-resistant descendant survives",
          not any(running(pid) for pid in pids))
    script.write_text(original)

    result = invoke("mutate")
    check("fingerprint mutation: all seven receipts fail for changed inputs",
          result.returncode == 1 and all(receipt(area).startswith(
              "FAIL inputs changed during run\nIDENTITY ") for area in AREAS))
    (package / "fixture.el").write_text("; package identity\n")
    result = invoke()
    check("fresh pass restored after mutation", result.returncode == 0 and passes_except())
    (root / "tools/c-core-areas.tsv").write_text(table + "alpha\tx-gui\n")
    result = invoke()
    print(f"CASE inventory: runner_exit={result.returncode}; {result.stderr.strip()}")
    check("inventory failure: stale PASS removed before gate, no workers launched",
          result.returncode == 1 and "canonical probe inventory disagree" in result.stderr
          and not list(out.glob("*.result")) and not list(events.iterdir()))
    (root / "tools/c-core-areas.tsv").write_text(table)
    result = invoke(jobs=0)
    check("invalid jobs: rejected without certification",
          result.returncode == 2 and "must be a positive integer" in result.stderr
          and not list(out.glob("*.result")))

    result = invoke("pass", "run", "--audit-area", "x-gui")
    if result.returncode:
        print(result.stdout + result.stderr, file=sys.stderr)
    check("audit: unchanged diagnostic success and no certification anywhere",
          result.returncode == 0 and "AUDIT area=x-gui expected=1" in result.stdout
          and not list(out.rglob("*.result")))
    result = invoke("mismatch", "run", "--audit-area", "buffer")
    check("audit mismatch: intended failure without a receipt",
          result.returncode == 1 and "transcripts differ" in result.stderr
          and not list((out / "audit").rglob("*.result")))
    result = invoke("pass", "run", "--unit", "fixture")
    unit = out / "units/fixture/unit.result"
    check("unit: unchanged receipt and check-unit behavior",
          result.returncode == 0 and unit.read_text().startswith("PASS fixture 7\nIDENTITY ")
          and invoke("pass", "check-unit", "fixture").returncode == 0)

    image_tool = root / "tools/c-core-image.sh"

    def image_command(action, case="pass"):
        return subprocess.run(["bash", str(image_tool), action], cwd=root,
                              env=environment(case, ("image",), None), text=True,
                              capture_output=True, timeout=10)

    image = Path(image_command("path").stdout.strip())
    image.unlink()
    check("image missing: path and existing unit receipt are rejected",
          image_command("path").returncode != 0
          and invoke("pass", "check-unit", "fixture").returncode != 0)
    for mode in ((), ("--audit-area", "x-gui"), ("--unit", "fixture")):
        result = invoke("image_fail", "run", *mode)
        check("failed image build: rejects " + str(mode),
              result.returncode == 1 and "cannot build current bundle heap image" in result.stderr
              and not list(out.glob("*.result"))
              and not list((root / "build/c-core-image").glob(".image-*.tmp")))
    check("image build without marker: rejected", image_command("build", "image_empty").returncode != 0)
    check("image rebuilt after failure", image_command("build").returncode == 0)
    image = Path(image_command("path").stdout.strip())
    before = image.stat().st_mtime_ns
    check("same identity: reuses published image",
          image_command("build").returncode == 0 and image.stat().st_mtime_ns == before)
    cold = root / "nelisp.cold"
    cold.write_text(cold.read_text() + "changed cold input\n")
    check("cold identity staleness: old image cannot be used",
          image_command("path").returncode != 0)
    rebuilt = image_command("build")
    check("new identity: atomic publication deletes stale image",
          rebuilt.returncode == 0 and not image.exists()
          and len(list((root / "build/c-core-image").glob("*.flat"))) == 1)

    # Interrupt the coordinator while its first worker has a live descendant.
    (root / "timeout.pids").unlink()
    proc = subprocess.Popen(["bash", str(script), "run"], cwd=root,
                            env=environment("cancel", ("run",), 1), text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    deadline = time.monotonic() + 5
    while not (root / "timeout.pids").exists() and time.monotonic() < deadline:
        time.sleep(0.02)
    try:
        pids = [int(p) for p in (root / "timeout.pids").read_text().split()]
        proc.terminate()
        stdout, stderr = proc.communicate(timeout=5)
        check("cancellation: coordinator and descendant groups exit, no area PASS",
              proc.returncode == 143 and not any(running(pid) for pid in pids)
              and not list(out.glob("*.result")))
    finally:
        if proc.poll() is None:
            proc.kill()
            proc.wait()
        if (root / "timeout.pids").exists():
            for pid in map(int, (root / "timeout.pids").read_text().split()):
                if running(pid):
                    os.kill(pid, 9)

try:
    with tempfile.TemporaryDirectory(prefix="c-core-parity-area-test-", dir="/tmp") as directory:
        main(Path(directory))
except Exception as error:
    print(f"FAIL test execution: {error}", file=sys.stderr)
    sys.exit(1)
finally:
    print(f"tests={len(checks)} passed={sum(ok for _, ok in checks)} "
          f"failed={sum(not ok for _, ok in checks)}", flush=True)
PY
