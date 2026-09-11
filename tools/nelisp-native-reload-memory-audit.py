#!/usr/bin/env python3
"""Separate GC-heap retention from retained native code mappings.

Republishing a native unit deliberately keeps the superseded generation mapped:
nothing in the runtime reports whether a call is still in flight through a
stable entry gate, so `nelisp-native-unit-reclaim' refuses to unmap a published
generation rather than guess.  That is a correctness decision, but it means RSS
after N republications contains two different things, and "RSS grew" on its own
cannot tell them apart.

This driver measures them separately in ONE persistent process:

  * the child reports `nelisp-native-unit-resources' and the addresses of every
    table it still holds after each round;
  * the harness samples /proc/<pid>/smaps and sums the Rss of exactly the
    regions containing those addresses.

The retained-native figure is therefore resident bytes in the unit's own
mappings, not a virtual-size upper bound -- a mapped page that was never
touched contributes nothing to RSS, so subtracting virtual size would
overstate it.  Whatever growth is left after removing those regions is the
GC heap plus ordinary allocator reservation, and is reported as a remainder
rather than labelled a leak: an allocator that keeps reusable arenas looks
exactly like a leak in RSS alone.
"""
import argparse
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

MARKER = re.compile(
    r"^ROUND (\d+) pid=(\d+) gen=(\d+) retained_bytes=(\d+) retired=(\d+) tables=(\S*)$")


def rss_kib(pid):
    try:
        with open(f"/proc/{pid}/status", encoding="ascii") as stream:
            for line in stream:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1])
    except (OSError, ValueError):
        return None
    return None


def smaps_regions(pid):
    """Return [(start, end, rss_kib)] for every mapping, or None."""
    regions = []
    try:
        with open(f"/proc/{pid}/smaps", encoding="ascii", errors="replace") as stream:
            start = end = None
            for line in stream:
                header = re.match(r"^([0-9a-f]+)-([0-9a-f]+) ", line)
                if header:
                    start = int(header.group(1), 16)
                    end = int(header.group(2), 16)
                elif line.startswith("Rss:") and start is not None:
                    regions.append((start, end, int(line.split()[1])))
                    start = end = None
    except (OSError, ValueError):
        return None
    # An empty list means the read raced the child's exit, not that a live
    # process has no mappings.  Reporting 0 resident KiB for that would look
    # like every retained mapping had just been released.
    return regions or None


def resident_kib_for(pid, addresses):
    """Sum the Rss of the distinct regions containing ADDRESSES."""
    if not addresses:
        return 0, 0
    regions = smaps_regions(pid)
    if regions is None:
        return None, None
    hit = {}
    for address in addresses:
        for start, end, rss in regions:
            if start <= address < end:
                hit[(start, end)] = rss
                break
    return sum(hit.values()), len(hit)


def build_driver(path, rounds, root, smoke_dir):
    path.write_text(f"""(progn
  (require 'nelisp-native-unit-development)
  (unless (and (fboundp 'ptr-call) (fboundp 'syscall-direct)
               (nelisp-native-load--raw-supported-p))
    (nelisp--write-stdout-bytes "SKIP: native raw runtime is unavailable\\n")
    (kill-emacs 0))
  (setq audit-root {json.dumps(root)})
  (setq audit-dir {json.dumps(smoke_dir)})
  (setq audit-unit nil)
  ;; Live Lisp state held across every publication, so the remainder figure
  ;; is not quietly measuring this fixture growing.
  (setq audit-retained (list "日本語" [17 29]))
  (dotimes (i {rounds})
    (let ((source (expand-file-name (format "round-%d.el" i) audit-dir)))
      (with-temp-file source
        (insert (format "(defun privatehelper (x) (+ x %d))\\n" (+ 1 i)))
        (insert "(defun publicscore (x) (* (privatehelper x) 2))\\n"))
      (let ((result (nelisp-native-unit-rebuild-and-reload
                     source audit-unit '("publicscore") audit-root)))
        (unless (eq (plist-get result :status) 'published)
          (error "round %d publication failed: %S" i result))
        (setq audit-unit (plist-get result :unit-id)))
      (garbage-collect)
      (unless (equal audit-retained (list "日本語" [17 29]))
        (error "retained state changed in round %d" i))
      (let* ((resources (nelisp-native-unit-resources))
             (status (nelisp-native-unit-status audit-unit))
             (tables (nelisp-native-unit-retained-addresses audit-unit))
             (pid (with-temp-buffer
                    (insert-file-contents "/proc/self/status")
                    ;; No regexp groups here on purpose: this driver is
                    ;; generated from Python, and one lost backslash silently
                    ;; turned the capture into a literal, reporting pid 0 for
                    ;; every round.  `emacs-pid' is not an alternative: it
                    ;; answers 0 in this runtime.
                    (let* ((txt (buffer-string))
                           (at (string-match "Pid:" txt))
                           (from (and at (+ at 4)))
                           (to (and from (string-match "\\n" txt from))))
                      (if (and from to)
                          (number-to-string
                           (string-to-number (substring txt from to)))
                        "0")))))
        ;; /proc/self/smaps truncates when read from inside this runtime
        ;; (10 regions in maps, 5 in smaps), so the resident figure is taken
        ;; by the harness from /proc/PID/smaps instead of computed here.
        (nelisp--write-stdout-bytes
         (format "ROUND %d pid=%s gen=%d retained_bytes=%d retired=%d tables=%s\\n"
                 i pid (plist-get status :generation)
                 (or (plist-get resources :retained-bytes) 0)
                 (or (plist-get resources :retired) 0)
                 (mapconcat #'number-to-string tables ","))))))
  (nelisp--write-stdout-bytes "AUDIT_DONE\\n")
  (kill-emacs 0))
""", encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", required=True)
    parser.add_argument("--root", default=str(Path(__file__).resolve().parents[1]))
    parser.add_argument("--rounds", type=int, default=8)
    parser.add_argument("--out", required=True)
    parser.add_argument("--timeout", type=float, default=900.0)
    args = parser.parse_args()

    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    work = out / "work"
    work.mkdir(exist_ok=True)
    driver = work / "audit.el"
    build_driver(driver, args.rounds, args.root, str(work))

    environment = dict(os.environ)
    environment["NELISP_NATIVE_SMOKE_DIR"] = str(work)
    environment["NELISP_SMOKE_ROOT"] = args.root

    # AI.md: "plain `target/nelisp --repl` does not preload the full artifact
    # compilation runtime needed for source reload" -- a bare --load leaves
    # `secure-hash' void and every publication fails in :phase :source.
    entry = work / "input.el"
    entry.write_text(
        f'(load {json.dumps(str(driver))})\n(exit)\n', encoding="utf-8")
    environment["NELISP_BIN"] = args.binary
    child = subprocess.Popen(
        [str(Path(args.root) / "tools/ai/nelisp-ai.sh"), "repl", "--no-prompt"],
        stdin=entry.open("r"), stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True, env=environment, cwd=args.root)

    samples = []
    baseline = None
    skipped = False
    done = False
    deadline = time.time() + args.timeout
    try:
        for line in child.stdout:
            line = line.rstrip("\n")
            if line.startswith("SKIP:"):
                skipped = True
                break
            if line == "AUDIT_DONE":
                done = True
                break
            match = MARKER.match(line)
            if not match:
                continue
            index, pid, generation, retained_bytes, retired, tables = match.groups()
            pid = int(pid)
            addresses = [int(value) for value in tables.split(",") if value]
            total = rss_kib(pid)
            native_kib, region_count = resident_kib_for(pid, addresses)
            sample = {
                "round": int(index),
                "pid": pid,
                "generation": int(generation),
                "reported_retained_bytes": int(retained_bytes),
                "retired_generations": int(retired),
                "rss_kib": total,
                "native_resident_kib": native_kib,
                "native_regions": region_count,
                "remainder_kib": (None if total is None or native_kib is None
                                  else total - native_kib),
            }
            samples.append(sample)
            if baseline is None:
                baseline = sample
            if time.time() > deadline:
                child.kill()
                break
        stderr = child.stderr.read()
    finally:
        if child.poll() is None:
            child.kill()
        child.wait()

    report = {
        "binary": args.binary,
        "rounds_requested": args.rounds,
        "rounds_observed": len(samples),
        "skipped": skipped,
        "completed": done,
        "exit_code": child.returncode,
        "stderr": stderr[-4000:],
        "samples": samples,
    }
    measured = [s for s in samples
                if s["rss_kib"] is not None and s["native_resident_kib"] is not None]
    report["rounds_measured"] = len(measured)
    report["rounds_unmeasured"] = len(samples) - len(measured)
    if len(measured) >= 2:
        first, last = measured[0], measured[-1]
        report["growth_from_round"] = first["round"]
        report["growth_to_round"] = last["round"]
        if True:
            report["rss_growth_kib"] = last["rss_kib"] - first["rss_kib"]
            report["native_growth_kib"] = (last["native_resident_kib"]
                                           - first["native_resident_kib"])
            report["remainder_growth_kib"] = (report["rss_growth_kib"]
                                              - report["native_growth_kib"])
    (out / "native-memory-audit.json").write_text(
        json.dumps(report, indent=1), encoding="utf-8")

    if skipped:
        print("native-reload-memory-audit: SKIP (native raw runtime unavailable)")
        print("GATE-COUNT checked=0 findings=0")
        return 0
    if not done or len(samples) != args.rounds:
        print(f"native-reload-memory-audit: FAIL (observed {len(samples)} of "
              f"{args.rounds} rounds, completed={done}, exit={child.returncode})")
        print(f"GATE-COUNT checked={len(samples)} findings=1")
        if stderr.strip():
            print(stderr.strip()[:2000])
        return 1
    print(f"rounds={len(samples)} "
          f"rss_growth_kib={report.get('rss_growth_kib')} "
          f"native_growth_kib={report.get('native_growth_kib')} "
          f"remainder_growth_kib={report.get('remainder_growth_kib')}")
    print(f"report: {out / 'native-memory-audit.json'}")
    print(f"GATE-COUNT checked={len(samples)} findings=0")
    return 0


if __name__ == "__main__":
    sys.exit(main())
