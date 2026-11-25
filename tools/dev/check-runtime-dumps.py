#!/usr/bin/env python3
"""Compare two flat runtime dumps under the same NeLisp binary and fixtures."""

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import json
import os
import re
import shutil
import subprocess
import sys


TIMING_ENTRY = {
    "par-forward-word-perbuf-time": "fw-time-row",
    "par-functionp-time": "fp-time-row",
    "par-save-excursion-time": "sx-time-row",
}
TIMING_LABEL = {
    "par-defvar-rle-cost": b"C1 let2000 n=2e5/n=0 < 3",
    "par-forward-word-perbuf-time": b"T1 forward-word in save-excursion",
    "par-functionp-time": b"T1 functionp < 2 us per call",
    "par-fxenv": b"P2 equal env ratio",
    "par-fxshape": b"LP bounded time",
    "par-fxshape-cov": b"P3 two shapes ratio",
    "par-save-excursion-time": b"T1 save-excursion",
}
DONE = "NEMACS-DUMP-CHECK-END"
STATE = r"""
(progn
  (prin1 (cons 'features features)) (terpri)
  (dolist (entry load-history)
    (prin1 (cons 'load-history entry)) (terpri))
  (dolist (name '(load require provide featurep after-load-alist
                  nemacs-batch-main nemacs-runtime-dump-reinit
                  load-path load-suffixes nemacs-runtime-dump-case-marker
                  buffer-file-name default-major-mode default-directory
                  cl-pushnew cl-generic-define-method custom-reevaluate-setting
                  custom-delayed-init-variables regexp-opt string-match
                  forward-word functionp save-excursion comment-column
                  tab-bar-mode tab-bar-current-tab tab-line-mode
                  tab-line-new-tab-choice dired-mode org-mode procedurep
                  process-file call-process process-connection-type
                  make-process process-live-p system-type emacs-version
                  coding-system-for-read default-buffer-file-coding-system))
    (let* ((fn (and (fboundp name) (symbol-function name)))
           (closure (and (nelisp--closure-p fn) (not (subrp fn))
                         (nelisp--closure-cons fn))))
      (prin1 (list name (fboundp name) (or closure fn)
                   (boundp name) (and (boundp name) (symbol-value name))))
      (terpri)))
  (princ "NEMACS-DUMP-CHECK-END\n")
  (kill-emacs 0))
"""


def execute(args, command, cwd, env, stdout, stderr, deadline):
    with stdout.open("wb") as out, stderr.open("wb") as err:
        return subprocess.run(
            ["timeout", "--kill-after=5", str(deadline), "taskset", "-c", args.cpus, *command],
            cwd=cwd, env=env, stdin=subprocess.DEVNULL, stdout=out, stderr=err,
        ).returncode


def observe(args, image, out, fixture, only=None):
    out.mkdir()
    home, temp = fixture / "home", fixture / "tmp"
    env = dict(os.environ, LC_ALL="C.UTF-8", TZ="UTC", NELISP_HOME=str(args.root),
               HOME=str(home), TMPDIR=str(temp),
               XDG_CACHE_HOME=str(home / ".cache"),
               XDG_CONFIG_HOME=str(home / ".config"), XDG_DATA_HOME=str(home / ".local/share"))
    env.setdefault("NELISP_GNU_LISP", str(args.root / "vendor/emacs-lisp"))
    command = [str(args.binary), "--cold-load-from", str(image), "--eval"]
    results = {}

    def run(name, form, ending=DONE):
        if fixture.exists():
            shutil.rmtree(fixture)
        (home / ".emacs.d").mkdir(parents=True)
        temp.mkdir()
        out_file, err_file = out / (name + ".out"), out / (name + ".err")
        status = execute(args, [*command, form], args.root, env,
                         out_file, err_file, args.parity_timeout)
        completed = ending.encode() in out_file.read_bytes().splitlines()
        results[name] = [status, completed]

    if only is None:
        run("state", STATE)
        if args.state_only:
            return results
    tests = args.root / "test"
    files = [tests / (only + ".el")] if only else sorted(tests.glob("par-*.el"))
    for file in files:
        action = TIMING_ENTRY.get(file.stem)
        tail = f" ({action})" if action else ""
        if file.stem == "par-startup-init":
            env["PAR_STARTUP_INIT_CASE"] = "direct"
        run(file.stem, f"(progn (nemacs-runtime-dump-reinit) "
            f"(load {json.dumps(str(file))} nil t t t){tail} "
            f'(terpri) (princ "{DONE}\\n") (kill-emacs 0))')
        env.pop("PAR_STARTUP_INIT_CASE", None)
    if only is not None:
        return results

    results["smoke"] = observe_smoke(args, image, out)
    (out / "status.json").write_text(json.dumps(results, indent=2) + "\n")
    return results


def observe_smoke(args, image, out, scenario=None):
    smoke_env = dict(os.environ, SMOKE_DUMP=str(image), SMOKE_NELISP=str(args.binary),
                     SMOKE_CPUS=args.cpus, SMOKE_EMACS=str(args.emacs))
    command = [str(args.root / "tools/smoke-run.sh")]
    if scenario is not None:
        command.append(scenario)
        smoke_env["SMOKE_TIMEOUT"] = "420"
    smoke_stdout, smoke_stderr = out / "smoke.out", out / "smoke.err"
    rc = execute(args, command, args.root, smoke_env,
                 smoke_stdout, smoke_stderr, args.smoke_timeout)
    match = re.search(rb"(?m)^Evidence: (.+)$", smoke_stdout.read_bytes())
    if not match:
        raise ValueError(f"smoke suite did not report an evidence directory (exit {rc})")
    smoke_run = Path(os.fsdecode(match[1]))
    for transcript in smoke_run.glob("*.transcript"):
        (out / transcript.name).write_bytes(
            transcript.read_bytes().replace(os.fsencode(smoke_run), b"<smoke-run>"))
    return [rc, len(list(out.glob("*.transcript")))]


def comparable(path, omit_timing_result=False):
    data = path.read_bytes()
    if path.name == "smoke.out":
        data = re.sub(rb"(?m)^(Evidence: |Elapsed: ).*\n", b"", data)
    elif path.suffix == ".err" and path.stem in TIMING_ENTRY:
        data = re.sub(rb"(?m)^ +\(mean [0-9.]+ us\)\n", b"", data)
    elif path.name == "par-elc-load.err":
        # The probe deliberately compresses a newly named temporary file.
        data = re.sub(rb"par-elc-byte8-[0-9]+-[0-9]+\.elc\.gz",
                      b"par-elc-byte8-<temporary>.elc.gz", data)
    elif omit_timing_result:
        label = re.escape(TIMING_LABEL[path.stem])
        data = re.sub(rb"(?m)^(" + label + rb"[^\n]*? +)(t|nil)$",
                      rb"\1<clock>", data)
    return data


def retry_timing(args, full, incremental):
    """Compare clock thresholds across five fresh pairs in alternating order."""
    name = full.stem
    evidence = args.evidence / "timing" / name
    evidence.mkdir(parents=True)
    records = {args.full: [], args.incremental: []}
    for trial in range(5):
        pair = [(args.full, "full"), (args.incremental, "incremental")]
        for image, label in (reversed(pair) if trial % 2 else pair):
            out = evidence / f"{trial}.{label}"
            result = observe(args, image, out, args.evidence / "fixture", only=name)
            probe = out / full.name
            if (result[name] != [0, True]
                    or comparable(probe, True) != comparable(full, True)
                    or comparable(out / (name + ".err")) != comparable(full.with_suffix(".err"))):
                return False
            records[image].append(comparable(probe))
    modes = [Counter(values).most_common(1)[0][0] for values in records.values()]
    match = modes[0] == modes[1]
    print(f"Timing {name}: {'matching' if match else 'differing'} majority of five pairs",
          flush=True)
    return match


def smoke_sections(path):
    data = comparable(path)
    headings = list(re.finditer(rb"(?m)^([a-z-]+) +(?:PASS|DIFF|ERROR) +GNU=", data))
    if not headings or headings[0].start() != 0:
        return {}
    ends = [heading.start() for heading in headings[1:]] + [len(data)]
    return {heading[1].decode(): data[heading.start():end]
            for heading, end in zip(headings, ends, strict=True)}


def retry_smoke(args, scenario):
    evidence = args.evidence / "smoke-retry" / scenario
    sides = evidence / "full", evidence / "incremental"
    for out in sides:
        out.mkdir(parents=True)
    with ThreadPoolExecutor(2) as pool:
        a = pool.submit(observe_smoke, args, args.full, sides[0], scenario)
        b = pool.submit(observe_smoke, args, args.incremental, sides[1], scenario)
    statuses = a.result(), b.result()
    if statuses[0] != statuses[1] or statuses[0][0] not in (0, 1) or statuses[0][1] != 2:
        return False
    names = {p.name for out in sides for p in out.iterdir()}
    equal = all(all((out / name).is_file() for out in sides) and
                comparable(sides[0] / name) == comparable(sides[1] / name)
                for name in names)
    print(f"Smoke {scenario}: {'matched' if equal else 'differed'} at 420s",
          flush=True)
    return equal


def resolve_smoke_timeouts(args, full, incremental, differences):
    if not any(name == "smoke.out" or name.endswith(".transcript") for name in differences):
        return differences
    left, right = [smoke_sections(path / "smoke.out") for path in (full, incremental)]
    resolved = set()
    for scenario in sorted(left.keys() & right.keys()):
        transcripts = {f"{scenario}.{engine}.transcript" for engine in ("gnu", "nelisp")}
        changed = left[scenario] != right[scenario] or transcripts.intersection(differences)
        timed_out = any(b"=124/incomplete-or-errors" in result.splitlines()[0]
                        for result in (left[scenario], right[scenario]))
        if changed and timed_out and retry_smoke(args, scenario):
            resolved.update(transcripts)
            right[scenario] = left[scenario]
    if resolved and list(left) == list(right) and left == right:
        resolved.add("smoke.out")
    return [name for name in differences if name not in resolved]


def compare(args, full, incremental, left, right):
    differences = []
    for name in sorted({p.name for directory in (full, incremental) for p in directory.iterdir()}):
        a, b = full / name, incremental / name
        if not a.exists() or not b.exists():
            differences.append(name)
        elif comparable(a) != comparable(b):
            if (a.suffix == ".out" and a.stem in TIMING_LABEL
                    and left[a.stem] == right[a.stem] == [0, True]
                    and comparable(a, True) == comparable(b, True)
                    and retry_timing(args, a, b)):
                continue
            differences.append(name)
    return resolve_smoke_timeouts(args, full, incremental, differences)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("full", type=Path)
    parser.add_argument("incremental", type=Path)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--binary", type=Path, default=Path("target/nelisp"))
    parser.add_argument("--emacs", type=Path, default=Path("/usr/local/bin/emacs-31.1"))
    parser.add_argument("--evidence", type=Path, required=True)
    parser.add_argument("--cpus", required=True)
    parser.add_argument("--parity-timeout", type=int, default=600,
                        help="deadline per parity fixture in seconds")
    parser.add_argument("--smoke-timeout", type=int, default=900)
    parser.add_argument("--state-only", action="store_true",
                        help="compare the heap state alone for diagnosis")
    args = parser.parse_args()
    args.root = args.root.resolve()
    args.binary = (args.root / args.binary).absolute()
    for name in ("full", "incremental", "evidence"):
        setattr(args, name, getattr(args, name).resolve())
    if args.full.samefile(args.incremental):
        parser.error("supply two independent output files")
    args.evidence.mkdir(parents=True)
    full, incremental = (args.evidence / label for label in ("full", "incremental"))
    try:
        left = observe(args, args.full, full, args.evidence / "fixture")
        right = observe(args, args.incremental, incremental, args.evidence / "fixture")
        differences = compare(args, full, incremental, left, right)
    except (OSError, ValueError) as error:
        parser.exit(1, f"dump equivalence: {error}\n")
    if not all(row["state"] == [0, True] for row in (left, right)):
        differences.append("incomplete state")
    if not args.state_only and not all(row["smoke"][1] == 18 and row["smoke"][0] in (0, 1)
                                       for row in (left, right)):
        differences.append("incomplete smoke suite")
    failed_probes = 0
    for name, (status, complete) in left.items():
        if name.startswith("par-") and (status or not complete):
            print(f"baseline probe {name}: exit={status}, completed={complete}",
                  file=sys.stderr)
            failed_probes += 1
            if status in (0, 124, 137):
                differences.append(f"incomplete {name}")
    if differences:
        print("DIFF " + ", ".join(differences))
        return 1
    if args.state_only:
        print("MATCH state")
    else:
        print(f"MATCH state, {len(left) - 2} parity probes "
              f"({failed_probes} matching baseline errors), 9 smoke scenarios")
    return 0


if __name__ == "__main__":
    sys.exit(main())
