#!/usr/bin/env python3
"""Twelve bounded real-runtime children for the early native identity slice."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
SEED_SHA = "6a82754090baadad3f9cf82398140934d77c573190ecbbb2f5fbb137047c694b"
D5_SHA = "f381509f099768e225ad1d9c7f9889b24431c25d3823ac2eaf454f0d4fc3dba9"
COLD_SHA = "b51e34e012051df45c3b2043aead6b562d337f5279eb4190726bcc78b75a9056"

COMMON = r''';;; -*- lexical-binding: t; -*-
(defvar ei-native (fboundp 'nelisp--debug-switch))
(defvar ei-left '(1 (2 3) "four" [5 6]))
(defvar ei-right '(1 (2 3) "four" [5 6]))
(defvar ei-before (and ei-native (symbol-function 'nelisp--equal-reference)))
(defvar ei-before-body (and ei-native (nthcdr 3 ei-before)))
(defvar ei-alias-before (and ei-native (fboundp 'nelisp--native-eq)))
(defun ei-loop (n) (let ((i 0) (answer nil))
  (while (< i n) (setq answer (equal ei-left ei-right) i (1+ i))) answer))
(defun ei-out (label value) (prin1 (list label value)) (terpri))
(defun ei-catch (fn) (condition-case e (funcall fn) (error (list 'error (car e)))))
(ei-loop 3)
'''
ENGINE = r'''
(ei-loop 3)
(ei-out 'EI-ENGINE
 (if ei-native
     (list (not (eq ei-before (symbol-function 'nelisp--equal-reference)))
           (not (eq ei-before-body (nthcdr 3 (symbol-function 'nelisp--equal-reference))))
           (vectorp (gethash (nthcdr 3 (symbol-function 'nelisp--equal-reference)) nelisp--fx-table))
           (vectorp (gethash (nthcdr 3 (symbol-function 'ei-loop)) nelisp--fx-table))
           ei-alias-before
           (and (fboundp 'nelisp--native-eq) (symbol-function 'nelisp--native-eq)))
   '(gnu native)))
'''
CONTRACT = r'''
(defvar ei-saved (symbol-function 'equal))
(defvar ei-count 0)
(defun ei-count-around (original &rest args)
 (setq ei-count (1+ ei-count)) (apply original args))
(defun ei-raise (&rest ignored) (signal 'error '("Public predicate reached")))
(defun ei-nil (&rest ignored) nil)
(defun ei-true (&rest ignored) t)
(defvar ei-identities
 (list (copy-sequence "identity") 'ei-symbol 17 (cons 1 2) (vector 3)
       (make-marker) (record 'ei-record 1) (make-hash-table) 1.25))
(defun ei-identical-results ()
 (let ((objects ei-identities) (values nil))
  (while objects
   (setq values (cons (condition-case e (funcall ei-saved (car objects) (car objects))
                       (error (list 'error (car e)))) values)
         objects (cdr objects)))
  (nreverse values)))
(ei-out 'EI-IDENTITY (ei-identical-results))
(ei-out 'EI-DISTINCT
 (list (equal (copy-sequence "same") (copy-sequence "same"))
       (equal "left" "right") (equal '(1 (2 3)) '(1 (2 3)))
       (equal '(1 2) '(1 3)) (equal [1 [2]] [1 [2]]) (equal [1 2] [1 3])
       (equal (make-hash-table) (make-hash-table))))
(ei-out 'EI-NUMERIC
 (list (equal 0 0.0) (equal 0.0 -0.0) (equal 1.25 1.25)
       (ei-catch (lambda () (let ((n (read "0.0e+NaN"))) (equal n n))))))
(ei-out 'EI-SYMBOL-POS
 (if (fboundp 'make-symbol-with-pos)
     (ei-catch (lambda () (let ((s (make-symbol-with-pos 'ei-symbol 7))) (equal s s))))
   'unavailable))
(advice-add 'equal :around #'ei-count-around)
(setq ei-count 0)
(setq ei-answer (equal ei-left ei-right))
(setq ei-observed ei-count)
(advice-remove 'equal #'ei-count-around)
(ei-out 'EI-OUTER-ADVICE (list ei-answer ei-observed))
(defun ei-poison-around (original a b)
 (let ((saved (symbol-function 'equal)))
  (unwind-protect (progn (fset 'equal #'ei-raise) (funcall original a b))
   (fset 'equal saved))))
(advice-add 'equal :around #'ei-poison-around)
(setq ei-answer (ei-catch (lambda () (equal ei-left ei-right))))
(advice-remove 'equal #'ei-poison-around)
(ei-out 'EI-OUTER-REDEFINE ei-answer)
(advice-add 'stringp :around #'ei-count-around)
(setq ei-count 0)
(setq ei-answer (ei-identical-results))
(setq ei-observed ei-count)
(advice-remove 'stringp #'ei-count-around)
(ei-out 'EI-PRED-COUNTS (list ei-answer ei-observed))
(advice-add 'eq :around #'ei-count-around)
(setq ei-count 0)
(setq ei-answer (ei-identical-results))
(setq ei-observed ei-count)
(advice-remove 'eq #'ei-count-around)
(ei-out 'EI-EQ-COUNTS (list ei-answer ei-observed))
(let ((saved-eq (symbol-function 'eq)))
 (unwind-protect
  (dolist (replacement '(ei-nil ei-true ei-raise))
   (fset 'eq replacement)
   (setq ei-answer (ei-identical-results))
   (setq ei-distinct (list (condition-case e (funcall ei-saved '(1) '(2)) (error (list 'error (car e))))
                           (condition-case e (funcall ei-saved [1] [2]) (error (list 'error (car e))))
                           (condition-case e (funcall ei-saved "a" "b") (error (list 'error (car e))))))
   (fset 'eq saved-eq)
   (ei-out replacement (list ei-answer ei-distinct)))
  (fset 'eq saved-eq)))
(let ((saved (symbol-function 'stringp)))
 (unwind-protect
  (progn
   (fset 'stringp #'ei-raise)
   (setq ei-answer (ei-identical-results))
   (setq ei-distinct (condition-case e (funcall ei-saved '(1) '(2))
                       (error (list 'error (car e)))))
   (fset 'stringp saved)
   (ei-out 'EI-PRED-RAISE-IDENTITY ei-answer)
   (ei-out 'EI-PREEXISTING-NONIDENTICAL-PREDICATE ei-distinct))
  (fset 'stringp saved)))
;; Diagnostics are outside the timed phase and use the actual allocation counter.
(ei-loop 10) (garbage-collect)
(if ei-native
 (let* ((before (nth 10 (nelisp--debug-switch 0))) (answer (ei-loop 5000))
        (after (nth 10 (nelisp--debug-switch 0))))
  (ei-out 'EI-ALLOC (list 5000 answer (- after before))))
 (ei-out 'EI-ALLOC '(gnu not-measured)))
'''
WALL = r'''
(ei-loop 10) (garbage-collect)
(setq ei-start (float-time))
(setq ei-answer (ei-loop 5000))
(ei-out 'EI-WALL (list 5000 ei-answer (- (float-time) ei-start)))
'''


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def capped():
    resource.setrlimit(resource.RLIMIT_AS, (8 * 1024**3, 8 * 1024**3))


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--native", type=Path, required=True)
    ap.add_argument("--gnu", type=Path, required=True)
    ap.add_argument("--output", type=Path, required=True)
    ap.add_argument("--continue-contracts", type=Path)
    args = ap.parse_args()
    assert sha(args.native) == D5_SHA
    cold = Path(str(args.native) + ".cold")
    assert sha(cold) == COLD_SHA
    assert not any(k.startswith("NELISP_") for k in os.environ)
    seed = ROOT / "target/argframe/equal-identity-seed.el"
    source = ROOT / "scripts/nelisp-stdlib-prelude.el"
    assert sha(seed) == SEED_SHA
    text = source.read_text()
    overlay = text[text.index("(unless (fboundp 'nelisp--native-equal)"):
                   text.index("\n\n;; The public builtin dispatches to this Lisp reference")]
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    report = {"source_sha256": sha(source), "seed_sha256": sha(seed),
              "runner_sha256": sha(__file__), "native_sha256": sha(args.native),
              "cold_sha256": sha(cold), "gnu_sha256": sha(args.gnu.resolve()),
              "contracts": {}, "walls": [], "prior_harness_rerun": False}

    def run(role, name, payload):
        fixture = out / f"{name}.el"
        fixture.write_text(COMMON + (overlay if role == "new" else "") + ENGINE
                           + payload + '\n(princ "EI-DONE\\n")\n(kill-emacs 0)\n')
        argv = ([str(args.gnu.resolve()), "-Q", "--batch"] if role == "gnu"
                else [str(args.native.resolve())]) + ["--load", str(fixture)]
        started = time.monotonic()
        try:
            child = subprocess.run(argv, cwd=ROOT, capture_output=True, timeout=30,
                                   preexec_fn=capped())
            stdout, stderr, rc = child.stdout, child.stderr, child.returncode
        except subprocess.TimeoutExpired as e:
            stdout, stderr, rc = e.stdout or b"", e.stderr or b"", -9
        (out / f"{name}.stdout").write_bytes(stdout)
        (out / f"{name}.stderr").write_bytes(stderr)
        lines = stdout.decode().splitlines()
        assert rc == 0 and not stderr and lines[-1:] == ["EI-DONE"], (name, rc, lines[-3:])
        expected_engine = {"gnu": "(EI-ENGINE (gnu native))",
                           "old": "(EI-ENGINE (nil nil t t nil nil))",
                           "new": "(EI-ENGINE (t t t t nil (builtin eq)))"}[role]
        assert lines[0] == expected_engine, (name, lines[0])
        return {"role": role, "rc": rc, "seconds": time.monotonic()-started,
                "fixture_sha256": sha(fixture), "stdout_sha256": sha(out/f"{name}.stdout"),
                "stderr_sha256": sha(out/f"{name}.stderr"), "lines": lines, "argv": argv}

    for role in ("gnu", "old", "new"):
        if args.continue_contracts and role in ("gnu", "old"):
            stem = args.continue_contracts / f"contract-{role}"
            lines = stem.with_suffix(".stdout").read_text().splitlines()
            report["contracts"][role] = {
                "role":role, "reused_prior_contract":True,
                "complete":lines[-1:] == ["EI-DONE"],
                "fixture_sha256":sha(stem.with_suffix(".el")),
                "stdout_sha256":sha(stem.with_suffix(".stdout")),
                "stderr_sha256":sha(stem.with_suffix(".stderr")),
                "lines":lines, "path":str(stem.resolve())}
        else:
            report["contracts"][role] = run(role, f"contract-{role}", CONTRACT)
        print(f"contract {role}: complete", flush=True)
    import re
    def rows(role):
        return {line.split(" ", 1)[0][1:]: line
                for line in report["contracts"][role]["lines"][1:] if line.startswith("(EI-") or line.startswith("(ei-")}
    gnu, old, new = (rows(role) for role in ("gnu", "old", "new"))
    diagnostic = {"EI-ALLOC", "EI-PRED-COUNTS", "EI-EQ-COUNTS"}
    report["existing_differences"] = [k for k in gnu if k not in diagnostic and k in old and old[k] != gnu[k]]
    report["unqualified_old_cases"] = [k for k in gnu if k not in old]
    report["new_differences"] = [k for k in gnu if k not in diagnostic and new[k] != gnu[k] and new[k] != old.get(k)]
    truth = "(t t t t t t t t t)"
    report["negative_control"] = {
        "old_predicate_path_observed": old["EI-PRED-COUNTS"] != f"(EI-PRED-COUNTS ({truth} 0))",
        "candidate_predicates_skipped": new["EI-PRED-COUNTS"] == gnu["EI-PRED-COUNTS"] == f"(EI-PRED-COUNTS ({truth} 0))",
        "old_eq_path_observed": old["EI-EQ-COUNTS"] != gnu["EI-EQ-COUNTS"],
        "candidate_eq_skipped": new["EI-EQ-COUNTS"] == gnu["EI-EQ-COUNTS"],
        "old_raising_eq_fails": "ei-raise" not in old,
        "candidate_raising_predicate_safe": new["EI-PRED-RAISE-IDENTITY"] == gnu["EI-PRED-RAISE-IDENTITY"],
    }
    # Three fresh-process samples per provider; reverse old/new order each round.
    for round_no, order in enumerate((("old", "new", "gnu"), ("new", "old"), ("old", "new"))):
        for role in order:
            result = run(role, f"wall-{round_no}-{role}", WALL)
            match = re.fullmatch(r"\(EI-WALL \(5000 t ([0-9.eE+-]+)\)\)", result["lines"][1])
            assert match, result["lines"]
            result.update({"round": round_no, "wall_seconds": float(match[1]),
                           "short_interval": float(match[1]) < .1})
            report["walls"].append(result)
            print(f"wall {round_no}/{role}: {float(match[1]):.6f}s", flush=True)
    report["paired_new_old"] = [
        next(x["wall_seconds"] for x in report["walls"] if x["round"]==n and x["role"]=="new") /
        next(x["wall_seconds"] for x in report["walls"] if x["round"]==n and x["role"]=="old") for n in range(3)]
    report["median_new_old"] = statistics.median(report["paired_new_old"])
    report["pass"] = not report["new_differences"] and all(report["negative_control"].values())
    (out/"report.json").write_text(json.dumps(report,indent=2)+"\n")
    print(json.dumps({k:report[k] for k in ("existing_differences","new_differences","negative_control","paired_new_old","pass")}),flush=True)
    return 0 if report["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
