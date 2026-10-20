#!/usr/bin/env python3
"""Pin GNU's real-world corpus and profile paired complete N/0 runs.

Each binary/module/layout/count gets a fresh process. Startup, warmup and
post-run GC are included in total Ir. Profiles and identities are retained.
This single harness replaces hand-edited one-pattern measurement scripts.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
PROBE = ROOT / "tools/ai/regex-tramp-probe.el"
LAYOUTS = (0, 700, 1400, 2100)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def corpus(args):
    expression = r'''(progn
      (require 'tramp)
      (load (expand-file-name "test/regex-tramp-handler-repro.el") nil t)
      (let ((rows nil) (index 0)
            (paths '("/home/user/.emacs.d/external-packages/all-the-icons.el/data/data-faicons.el"
                     "/tmp/project/src/main.c" "/ssh:user@example.org:/tmp/file.el"
                     "(defun some-file (x) (message \\\"file.el\\\"))")))
        (dolist (pattern trs-regexps)
          (push (list (format "tramp-repro-%d" index) pattern (list trs-file)) rows)
          (setq index (1+ index)))
        (setq index 0)
        (dolist (entry file-name-handler-alist)
          (push (list (format "gnu-handler-%d" index) (car entry) paths) rows)
          (setq index (1+ index)))
        (setq index 0)
        (dolist (entry (seq-take auto-mode-alist 30))
          (push (list (format "auto-mode-%d" index) (car entry) paths) rows)
          (setq index (1+ index)))
        (setq index 0)
        (dolist (pattern '("\\`\\(.*\\)\\(\\)\\'"
                            "\\`\\(.*\\)\\(\\.el\\)\\'"
                            "\\`\\(.*\\)\\(\\.so\\|\\.elc\\|\\.elc\\.gz\\|\\.el\\|\\.el\\.gz\\)\\'" "file"))
          (push (list (format "regex-speed-%d" index) pattern paths) rows)
          (setq index (1+ index)))
        (setq rows (nreverse rows))
        (with-temp-file (getenv "RT_OUTPUT")
          (insert ";;; Pinned GNU 31.1 corpus; regenerate with regex-tramp.py corpus.\n")
          (insert "(setq rt-corpus '") (prin1 rows (current-buffer))
          (insert ")\n"))
        (princ (format "RT-CORPUS %d\n" (length rows))))))'''
    output = args.output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    result = subprocess.run([args.emacs, "-Q", "--batch", "--eval", expression],
                            cwd=ROOT, env=dict(os.environ, RT_OUTPUT=str(output)),
                            capture_output=True, text=True, timeout=60)
    if result.returncode or not re.search(r"RT-CORPUS [1-9][0-9]*", result.stdout):
        raise RuntimeError(result.stderr[-1000:])
    print(result.stdout.strip())


def profile(args):
    if args.segments:
        return segments(args)
    binary = args.binary.resolve()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise RuntimeError(f"output must be empty: {output}")
    identities = {"binary": [str(binary), digest(binary)],
                  "probe": [str(PROBE), digest(PROBE)],
                  "corpus": [str(args.corpus.resolve()), digest(args.corpus)]}
    for suffix in (".cold", ".native-startup.el"):
        path = Path(str(binary) + suffix)
        if not args.gnu and path.exists():
            identities[suffix] = [str(path), digest(path)]
    if args.module:
        identities["module"] = [str(args.module.resolve()), digest(args.module)]
    if args.gnu:
        version = subprocess.check_output([str(binary), "-Q", "--batch", "--eval",
                                           "(princ emacs-version)"], text=True).strip()
        if version != "31.1":
            raise RuntimeError(f"GNU 31.1 required: {version}")
    indices = [int(index) for index in args.indices.split(",")]

    def run(task):
        index, layout, count = task
        stem = output / f"case-{index}-layout-{layout}-n-{count}"
        env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
                   ABI_LAYOUT_PAD="x" * layout, RT_INDEX=str(index),
                   RT_COUNT=str(count), RT_CORPUS=str(args.corpus.resolve()))
        env.pop("RT_MODULE", None)
        if args.module:
            env["RT_MODULE"] = str(args.module.resolve())
        command = [str(binary), *( ["-Q", "--batch", "-l"] if args.gnu else ["--load"] ), str(PROBE)]
        if not args.wall:
            command = ["valgrind", "--tool=callgrind", "--error-exitcode=97",
                       "--dump-instr=no", "--collect-jumps=no", "--collect-atstart=yes",
                       f"--callgrind-out-file={stem}.cg", *command]
        started = time.monotonic()
        with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
            process = subprocess.run(command, cwd=ROOT, env=env, stdout=out, stderr=err, timeout=580)
        stdout = stem.with_suffix(".out").read_text()
        stderr = stem.with_suffix(".err").read_text()
        answers = re.findall(r"^RT-RESULT (.+)$", stdout, re.M)
        metrics = re.findall(r"^RT-METRIC ([0-9]+) ([0-9.]+)$", stdout, re.M)
        if (process.returncode or "RT-DONE" not in stdout or len(answers) != 1
                or len(metrics) != 1 or "uncaught error" in stderr):
            raise RuntimeError(f"incomplete profile: {stem}")
        total = None
        if not args.wall:
            summary = re.findall(r"^summary: ([0-9]+)$", stem.with_suffix(".cg").read_text(), re.M)
            if len(summary) != 1 or int(summary[0]) <= 0:
                raise RuntimeError(f"empty instruction profile: {stem}")
            total = int(summary[0])
        result = {"index": index, "layout": layout, "count": count,
                  "calls": int(metrics[0][0]), "ms": float(metrics[0][1]),
                  "Ir": total, "answer": answers[0], "elapsed": time.monotonic() - started}
        print(f"case={index} layout={layout} n={count} Ir={total}", flush=True)
        return result

    tasks = [(i, layout, n) for i in indices
             for layout in ((0,) if args.wall else LAYOUTS)
             for n in ((args.count,) if args.wall else (0, args.count))]
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        samples = list(pool.map(run, tasks))
    rows = []
    for index in indices:
        values = []
        answers = {s["answer"] for s in samples if s["index"] == index}
        if len(answers) != 1:
            raise RuntimeError(f"layout/control answer mismatch: {index}")
        for layout in ((0,) if args.wall else LAYOUTS):
            sample = next(s for s in samples if (s["index"], s["layout"], s["count"]) == (index, layout, args.count))
            if sample["calls"] <= 0:
                raise RuntimeError("no measured calls")
            if args.wall:
                values.append(sample["ms"] / sample["calls"])
            else:
                zero = next(s for s in samples if (s["index"], s["layout"], s["count"]) == (index, layout, 0))
                delta = sample["Ir"] - zero["Ir"]
                if delta <= 0 or zero["calls"] != 0:
                    raise RuntimeError(f"nonpositive N/0 difference: {index}")
                values.append(delta / sample["calls"])
        rows.append({"index": index, "answer": next(iter(answers)),
                     "per_match": sum(values) / len(values), "layouts": values})
    (output / "result.json").write_text(json.dumps(
        {"identities": identities, "samples": samples, "rows": rows,
         "metric": "ms" if args.wall else "N-minus-0-Ir-including-GC",
         "layouts": [0] if args.wall else list(LAYOUTS)}, indent=2) + "\n")


def segments(args):
    """Amortize startup with documented client-request dump/zero intervals.

    Complete N and 0 processes each run the entire ordered pinned corpus.
    Every interval includes all allocations and its explicit post-run GC.
    """
    if args.gnu or args.wall or args.indices != "all":
        raise RuntimeError("segments requires standalone Ir and --indices all")
    binary = args.binary.resolve()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        raise RuntimeError(f"output must be empty: {output}")
    script = ROOT / "tools/ai/regex-tramp-segments.el"
    identities = {key: [str(path), digest(path)] for key, path in
                  (("binary", binary), ("probe", script), ("corpus", args.corpus.resolve()))}
    for suffix in (".cold", ".native-startup.el"):
        path = Path(str(binary) + suffix)
        if path.exists():
            identities[suffix] = [str(path), digest(path)]
    if args.module:
        identities["module"] = [str(args.module.resolve()), digest(args.module)]

    def run(task):
        layout, count = task
        stem = output / f"layout-{layout}-n-{count}"
        env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
                   ABI_LAYOUT_PAD="x" * layout, RT_COUNT=str(count),
                   RT_CORPUS=str(args.corpus.resolve()))
        env.pop("RT_MODULE", None)
        if args.module:
            env["RT_MODULE"] = str(args.module.resolve())
        answers, calls, results = {}, {}, []
        started = time.monotonic()
        with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
            process = subprocess.run(
                ["valgrind", "--tool=callgrind", "--error-exitcode=97",
                 f"--callgrind-out-file={stem}.cg", str(binary), "--load", str(script)],
                cwd=ROOT, env=env, stdout=out, stderr=err, timeout=1800)
        stdout = stem.with_suffix(".out").read_text()
        for line in stdout.splitlines():
            answer = re.fullmatch(r"RT-RESULT ([0-9]+) (.+)", line)
            call = re.fullmatch(r"RT-CALLS ([0-9]+) ([0-9]+)", line)
            if answer:
                answers[int(answer[1])] = answer[2]
            if call:
                calls[int(call[1])] = int(call[2])
        dumps = list(output.glob(stem.name + ".cg.*"))
        if (process.returncode or "RT-SEGMENTS-DONE" not in stdout
                or not answers or set(answers) != set(calls) or len(dumps) != len(answers)):
            raise RuntimeError(f"incomplete segmented profile: {stem}")
        for index in sorted(answers):
            dump = Path(str(stem) + f".cg.{index+1}")
            text = dump.read_text()
            summary = re.findall(r"^summary: ([0-9]+)$", text, re.M)
            if len(summary) != 1 or "Client Request" not in text:
                raise RuntimeError(f"missing client-request summary: {dump}")
            results.append({"index": index, "layout": layout, "count": count,
                            "calls": calls[index], "Ir": int(summary[0]),
                            "answer": answers[index]})
        print(f"layout={layout} n={count} segments={len(results)} seconds={time.monotonic()-started:.3f}", flush=True)
        return results

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        samples = [sample for group in pool.map(run, [(l, n) for l in LAYOUTS for n in (0, args.count)])
                   for sample in group]
    indices = sorted({s["index"] for s in samples})
    rows = []
    for index in indices:
        points = [s for s in samples if s["index"] == index]
        if len(points) != 8 or len({p["answer"] for p in points}) != 1:
            raise RuntimeError(f"incomplete/inconsistent case: {index}")
        values = []
        for layout in LAYOUTS:
            zero = next(p for p in points if (p["layout"], p["count"]) == (layout, 0))
            full = next(p for p in points if (p["layout"], p["count"]) == (layout, args.count))
            delta = full["Ir"] - zero["Ir"]
            if delta <= 0 or full["calls"] <= 0 or zero["calls"] != 0:
                raise RuntimeError(f"invalid paired segment: {index}")
            values.append(delta / full["calls"])
        rows.append({"index": index, "answer": points[0]["answer"],
                     "per_match": sum(values)/4, "layouts": values})
    (output / "result.json").write_text(json.dumps(
        {"identities": identities, "samples": samples, "rows": rows,
         "metric": "N-minus-0-Ir-including-GC", "layouts": list(LAYOUTS),
         "method": "client-request-dump-zero"}, indent=2) + "\n")


def checked_report(path):
    report = json.loads(path.read_text())
    for name, (filename, expected) in report["identities"].items():
        if digest(filename) != expected:
            raise RuntimeError(f"stale {name}: {filename}")
    if report["layouts"] != list(LAYOUTS) or report["metric"] != "N-minus-0-Ir-including-GC":
        raise RuntimeError("complete four-layout Ir required")
    indices = [row["index"] for row in report["rows"]]
    if not indices or len(set(indices)) != len(indices):
        raise RuntimeError("empty or duplicate rows")
    for row in report["rows"]:
        points = [p for p in report["samples"] if p["index"] == row["index"]]
        if len(points) != 8 or len({p["answer"] for p in points}) != 1:
            raise RuntimeError("missing sample or inconsistent outputs")
        values = []
        for layout in LAYOUTS:
            zero = [p for p in points if p["layout"] == layout and p["count"] == 0]
            full = [p for p in points if p["layout"] == layout and p["count"] > 0]
            if len(zero) != 1 or len(full) != 1:
                raise RuntimeError("missing/duplicate paired run")
            zero, full = zero[0], full[0]
            if zero["calls"] != 0 or full["calls"] <= 0 or full["Ir"] <= zero["Ir"]:
                raise RuntimeError("empty/nonpositive measurement")
            values.append((full["Ir"] - zero["Ir"]) / full["calls"])
        if row["layouts"] != values or row["per_match"] != sum(values)/4:
            raise RuntimeError("incorrect instruction arithmetic")
        if row["answer"] != points[0]["answer"]:
            raise RuntimeError("incorrect row answer")
    return report


def compare(args):
    baseline = checked_report(args.baseline)
    result = checked_report(args.report)
    if (result["identities"]["corpus"][1] != baseline["identities"]["corpus"][1]
            or [r["index"] for r in result["rows"]] != [r["index"] for r in baseline["rows"]]
            or len(result["rows"]) != args.expected_count):
        raise RuntimeError("different or incomplete pinned corpus")
    failed = []
    for row, old in zip(result["rows"], baseline["rows"]):
        if row["answer"] != old["answer"] or row["per_match"] > old["per_match"] * args.limit:
            failed.append({"index": row["index"], "ratio": row["per_match"]/old["per_match"],
                           "same_answer": row["answer"] == old["answer"]})
    print(json.dumps({"cases": len(result["rows"]), "limit": args.limit, "failures": failed}))
    if failed:
        raise SystemExit(1)


def corpus_body(path):
    raw = path.read_bytes()
    markers = list(re.finditer(rb"^RT-CORPUS-CHECK ([0-9]+)\n", raw, re.M))
    if len(markers) != 1 or int(markers[0][1]) != 162:
        raise RuntimeError("missing/incorrect pinned corpus completion count")
    marker = markers[0]
    if raw[marker.end():] not in (b"", b"nil\n"):
        raise RuntimeError("unexpected output after corpus completion")
    return raw[:marker.end()]


def compare_corpus(args):
    if corpus_body(args.report) != corpus_body(args.baseline):
        raise RuntimeError("pinned corpus differs from GNU")
    print("RT-CORPUS 162 public results/captures PASS")


def check_abi(args):
    text = args.report.read_text()
    rows = re.findall(
        r"^Ir regex layout (\d+) base (\d+) cand (\d+); GC base (\d+) cand (\d+); output identical; delta [^\n]+$",
        text, re.M)
    if ([int(r[0]) for r in rows] != list(LAYOUTS)
            or len(re.findall(r"^Ir regex layout ", text, re.M)) != 4
            or any(min(int(r[1]), int(r[2])) <= 0 or min(int(r[3]), int(r[4])) < 10 for r in rows)):
        raise RuntimeError("incomplete four-layout/10-GC ABI evidence")
    old = sum(int(r[1]) for r in rows)/4
    new = sum(int(r[2]) for r in rows)/4
    print(json.dumps({"base": old, "head": new, "ratio": new/old,
                      "limit": 1.02, "passed": new <= old*1.02}))
    if new > old*1.02:
        raise SystemExit(1)


def check_receipt(args):
    receipt = json.loads(args.report.read_text())
    if receipt.get("passed") is not True or not receipt.get("files"):
        raise RuntimeError("missing successful receipt/files")
    for filename, expected in receipt["files"].items():
        if digest(ROOT/filename) != expected:
            raise RuntimeError(f"stale receipt file: {filename}")
    print(f"RT-RECEIPT {receipt['criterion']} PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    pin = sub.add_parser("corpus")
    pin.add_argument("output", type=Path)
    pin.add_argument("--emacs", default="emacs")
    pin.set_defaults(run=corpus)
    meter = sub.add_parser("profile")
    meter.add_argument("binary", type=Path)
    meter.add_argument("output", type=Path)
    meter.add_argument("--corpus", type=Path, default=ROOT / "test/regex-tramp-corpus.el")
    meter.add_argument("--indices", default="0,1")
    meter.add_argument("--count", type=int, default=5)
    meter.add_argument("--jobs", type=int, default=2)
    meter.add_argument("--module", type=Path)
    meter.add_argument("--gnu", action="store_true")
    meter.add_argument("--wall", action="store_true")
    meter.add_argument("--segments", action="store_true")
    meter.set_defaults(run=profile)
    check = sub.add_parser("compare")
    check.add_argument("report", type=Path)
    check.add_argument("baseline", type=Path)
    check.add_argument("--expected-count", type=int, default=42)
    check.add_argument("--limit", type=float, default=1.0)
    check.set_defaults(run=compare)
    parity = sub.add_parser("compare-corpus")
    parity.add_argument("report", type=Path)
    parity.add_argument("baseline", type=Path)
    parity.set_defaults(run=compare_corpus)
    abi = sub.add_parser("check-abi")
    abi.add_argument("report", type=Path)
    abi.set_defaults(run=check_abi)
    receipt = sub.add_parser("check-receipt")
    receipt.add_argument("report", type=Path)
    receipt.set_defaults(run=check_receipt)
    args = parser.parse_args()
    args.run(args)


if __name__ == "__main__":
    main()
