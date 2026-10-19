#!/usr/bin/env python3
"""Prepare just the regexp module, measure it, or check a recorded timing.

Source-first preparation uses the reader build's opcode admission policy and
keeps an existing standalone process/binary available for short repair loops.
Reports retain source, binary and image hashes; timing checks reject stale data.
Cache cost subtracts a control loop and retains both raw and control timings.
Matching cost is the complete public call, without subtracting loop overhead.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "lisp/nelisp-stdlib-regexp.el"
PROBE = ROOT / "tools/ai/regex-speed-probe.el"
PATTERNS = (r"\`\(.*\)\(\)\'", r"\`\(.*\)\(\.el\)\'",
            r"\`\(.*\)\(\.so\|\.elc\|\.elc\.gz\|\.el\|\.el\.gz\)\'", "file")


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def prepare(args):
    args.output.parent.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, REGEX_SPEED_INPUT=str(args.source.resolve()),
               REGEX_SPEED_OUTPUT=str(args.output.resolve()),
               REGEX_SPEED_WRAPPERS="1" if args.public_wrappers else "")
    expression = '''(let* ((path (getenv "REGEX_SPEED_INPUT"))
        (source (with-temp-buffer (insert-file-contents path) (buffer-string)))
        (result (nelisp-prelude-bytecode-transform source
                 "lisp/nelisp-stdlib-regexp.el" nil t)))
      (with-temp-file (getenv "REGEX_SPEED_OUTPUT")
        (insert (car result))
        (when (equal (getenv "REGEX_SPEED_WRAPPERS") "1")
          (require 'nelisp-standalone-build)
          (let ((text (nelisp-standalone--reader-repl-prelude-source))
                (position 0) forms)
            (condition-case nil
                (while t
                  (let* ((read-result (read-from-string text position))
                         (form (car read-result)))
                    (setq position (cdr read-result))
                    (when (and (memq (car-safe form) '(defun defmacro))
                               (memq (cadr form) '(string-match string-match-p match-data save-match-data)))
                      (push (concat (prin1-to-string form) "\\n") forms))))
              (end-of-file nil))
            (unless (= (length forms) 4) (error "Expected four public wrappers"))
            (insert (car (nelisp-prelude-bytecode-transform
                          (apply #'concat (nreverse forms)) "regex-public-wrappers.el" nil t))))))
      (princ (format "REGEX-BYTECODE %d\\n" (cadr result))))'''
    started = time.monotonic()
    result = subprocess.run([args.emacs, "-Q", "--batch", "-L", "scripts", "-L", "lisp", "-L", "src",
                             "-l", "nelisp-prelude-bytecode", "--eval", expression],
                            cwd=ROOT, env=env, capture_output=True, text=True,
                            timeout=60)
    if result.returncode or not re.search(r"REGEX-BYTECODE [1-9][0-9]*", result.stdout):
        raise RuntimeError(f"module preparation failed: {result.stderr[-1500:]}")
    print(f"{result.stdout.strip()} {time.monotonic() - started:.3f}s")


def measure(args):
    args.output.parent.mkdir(parents=True, exist_ok=True)
    binary = Path(args.binary).resolve()
    temporary = ROOT / "target/tmp"
    temporary.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, TMPDIR=str(temporary), REGEX_SPEED_COUNT=str(args.count))
    command = [str(binary)]
    identities = {"binary": [str(binary), digest(binary)],
                  "source": [str(args.source.resolve()), digest(args.source)],
                  "probe": [str(PROBE), digest(PROBE)]}
    if args.gnu:
        version = subprocess.check_output([str(binary), "-Q", "--batch", "--eval",
                                           "(princ emacs-version)"], text=True).strip()
        if version != "31.1":
            raise RuntimeError(f"GNU 31.1 required, got {version!r}")
        command += ["-Q", "--batch", "-l", str(PROBE)]
    else:
        if args.image:
            command += ["--cold-load-from", str(args.image.resolve())]
            identities["image"] = [str(args.image.resolve()), digest(args.image)]
        else:
            image = Path(str(binary) + ".cold")
            if image.exists():
                identities["image"] = [str(image), digest(image)]
        startup = Path(str(binary) + ".native-startup.el")
        if startup.exists():
            identities["startup"] = [str(startup), digest(startup)]
        if args.module:
            env["REGEX_SPEED_MODULE"] = str(args.module.resolve())
            identities["module"] = [str(args.module.resolve()), digest(args.module)]
        command += ["--load", str(PROBE)]
    started = time.monotonic()
    samples = []
    with args.output.with_suffix(".stdout").open("w") as out, args.output.with_suffix(".stderr").open("w") as err:
        for sample in range(args.samples):
            rows = []
            for index in range(4):
                row = {}
                for mode in (["full"] if args.gnu else ["full", "components"]):
                    process_env = dict(env, REGEX_SPEED_PATTERN=str(index), REGEX_SPEED_MODE=mode)
                    result = subprocess.run(command, cwd=ROOT, env=process_env,
                                            capture_output=True, text=True, timeout=180)
                    out.write(f"sample={sample} pattern={index} mode={mode}\n" + result.stdout)
                    err.write(result.stderr)
                    out.flush()
                    err.flush()
                    if (result.returncode or "REGEX-PROFILE-DONE" not in result.stdout
                            or "uncaught error" in result.stderr):
                        raise RuntimeError(f"incomplete probe: {args.output.with_suffix('.stderr')}")
                    for line in result.stdout.splitlines():
                        full = re.fullmatch(r"full (.*) ([0-9.]+) us", line)
                        component = re.fullmatch(r"([a-z-]+) ([0-9.]+) us", line)
                        if full:
                            if "pattern" in row:
                                raise RuntimeError("duplicate full result")
                            row.update(pattern=full[1], full=float(full[2]))
                        elif component:
                            row[component[1]] = float(component[2])
                if not row.get("pattern") or "full" not in row:
                    raise RuntimeError("missing selected pattern")
                if "cache" in row and "empty-loop" in row:
                    row["cache-raw"] = row["cache"]
                    row["cache"] = round(row["cache"] - row["empty-loop"], 3)
                rows.append(row)
            samples.append(rows)
    patterns = []
    for index in range(4):
        rows = [sample[index] for sample in samples]
        if len({row["pattern"] for row in rows}) != 1:
            raise RuntimeError("inconsistent pattern samples")
        patterns.append({key: (rows[0][key] if key == "pattern" else
                               round(statistics.median(row[key] for row in rows), 3))
                         for key in rows[0]})
    report = {"identities": identities, "patterns": patterns,
              "samples": samples, "method": "fresh-process-warm-gc-median",
              "elapsed": time.monotonic() - started, "gnu": args.gnu, "count": args.count}
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    for row in patterns:
        print(f"{row['pattern']}: {row['full']:.3f} us; cache {row.get('cache', 'n/a')}")


def check(args):
    report = json.loads(args.report.read_text())
    if report.get("count", 0) < 2000:
        raise RuntimeError("acceptance requires at least 2000 matches per pattern")
    if len(report.get("patterns", [])) != 4:
        raise RuntimeError("acceptance requires all four probe patterns")
    if [row.get("pattern") for row in report["patterns"]] != [json.dumps(p) for p in PATTERNS]:
        raise RuntimeError("acceptance requires the exact four probe patterns")
    samples = report.get("samples", [])
    if report.get("method") != "fresh-process-warm-gc-median" or len(samples) < 3:
        raise RuntimeError("acceptance requires at least three fresh-process samples")
    for index, row in enumerate(report["patterns"]):
        for sample in samples:
            if len(sample) != 4 or set(sample[index]) != set(row):
                raise RuntimeError("incomplete samples")
            if sample[index]["pattern"] != row["pattern"]:
                raise RuntimeError("mismatched sample pattern")
            if not report.get("gnu"):
                point = sample[index]
                if not {"cache", "cache-raw", "empty-loop"}.issubset(point):
                    raise RuntimeError("missing raw cache/control timing")
                if point["cache"] != round(point["cache-raw"] - point["empty-loop"], 3):
                    raise RuntimeError("incorrect cache/control difference")
        for key in row:
            if key != "pattern" and row[key] != round(statistics.median(s[index][key] for s in samples), 3):
                raise RuntimeError(f"incorrect median: {key}")
    identities = report.get("identities", {})
    required = {"binary", "source", "probe"}
    if report.get("gnu") is False:
        required.update(("image", "startup"))
    elif report.get("gnu") is not True:
        raise RuntimeError("missing explicit runtime kind")
    if not required.issubset(identities):
        raise RuntimeError("missing required identities")
    for name, record in identities.items():
        if (not isinstance(record, list) or len(record) != 2
                or not all(isinstance(value, str) for value in record)
                or not re.fullmatch(r"[0-9a-f]{64}", record[1])):
            raise RuntimeError(f"malformed identity: {name}")
        path, expected = record
        if digest(path) != expected:
            raise RuntimeError(f"stale {name}: {path}")
    field = "cache" if args.criterion == "cache" else "full"
    limit = 2 if field == "cache" else 20
    if args.limit is not None:
        limit = args.limit
    if field == "cache" and limit > 2:
        raise RuntimeError("the cache acceptance limit is fixed at 2 us")
    passed = all(field in row and 0 < row[field] <= limit for row in report["patterns"])
    print(f"REGEX-SPEED {field} <= {limit} us: {'PASS' if passed else 'FAIL'}")
    return 0 if passed else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="action", required=True)
    prep = sub.add_parser("prepare")
    prep.add_argument("output", type=Path)
    prep.add_argument("--source", type=Path, default=SOURCE)
    prep.add_argument("--emacs", default=os.environ.get("EMACS", "emacs"))
    prep.add_argument("--public-wrappers", action="store_true")
    measure_parser = sub.add_parser("measure")
    measure_parser.add_argument("binary")
    measure_parser.add_argument("output", type=Path)
    measure_parser.add_argument("--image", type=Path)
    measure_parser.add_argument("--module", type=Path)
    measure_parser.add_argument("--source", type=Path, default=SOURCE)
    measure_parser.add_argument("--gnu", action="store_true")
    measure_parser.add_argument("--count", type=int, default=2000)
    measure_parser.add_argument("--samples", type=int, choices=range(3, 10), default=3)
    checker = sub.add_parser("check")
    checker.add_argument("report", type=Path)
    checker.add_argument("--criterion", choices=["cache", "match"], required=True)
    checker.add_argument("--limit", type=float)
    args = parser.parse_args()
    return {"prepare": prepare, "measure": measure, "check": check}[args.action](args) or 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"REGEX-SPEED ERROR: {error}")
        raise SystemExit(2)
