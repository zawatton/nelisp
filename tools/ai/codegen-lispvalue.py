#!/usr/bin/env python3
"""Doc 229 Lisp-value corpus: GNU Lisp_Object semantics in native artifacts.

Usage:
  codegen-lispvalue.py run TAG ROOT BINARY
      Compile every case of `nelisp-aot-differential-lisp-value-cases'
      (always taken from this worktree) with ROOT's compiler, load and run
      each one on BINARY with ROOT's loader, compare it with the
      interpreter, and write target/codegen/lispvalue/TAG.json.

      TAG base: ROOT target/codegen/base-src (9d5f4a515), the controls.
      TAG cand: ROOT this worktree, the candidate.

  codegen-lispvalue.py e2e VALIDATE_LOG BINARY
      Record the end-to-end criterion in target/codegen/lispvalue-e2e.json:
      VALIDATE_LOG (target/codegen/validate.sh on BINARY, a build of this
      tree) and a byte-identity control for the dialect -- the five Doc 224
      kernels compiled by this tree and by 1042eedbc's compiler.

Cases run in chunks of reader processes, as the AOT differential driver
does; a chunk that dies is rerun one case per process.  The runs are
correctness runs, not timed, so they stay on CPUs 0-7.
"""
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys

REPO = os.path.realpath(os.path.join(os.path.dirname(__file__), "..", ".."))
OUT = os.path.join(REPO, "target", "codegen", "lispvalue")
CASES_FILE = os.path.join(REPO, "scripts", "nelisp-aot-differential-cases.el")
COMPILER_FILES = ["lisp/nelisp-aot-compiler.el", "lisp/nelisp-artifact.el",
                  "lisp/nelisp-native-load.el",
                  "test/nelisp-aot-differential-driver.el"]
LIMIT = "ulimit -v 8388608; "


CHUNK = 8


def native_flags(diff, n):
    """Return each case's native flag from the generated manifest."""
    r = sh("emacs --batch -Q --eval %s" % shell_quote(
        '(progn (load "%s/cases.el" nil t) (princ (mapconcat (lambda (e) '
        '(if (nth 3 e) "1" "0")) nelisp-aot-differential-manifest "")))' % diff),
        diff, 120)
    flags = r.stdout.strip()
    if len(flags) != n:
        sys.exit("manifest has %d native flags for %d cases" % (len(flags), n))
    return [f == "1" for f in flags]


def run_chunk(root, binary, diff, chunk, size):
    prelude = os.path.join(diff, "prelude-%d-%d.el" % (size, chunk))
    with open(prelude, "w") as f:
        f.write('(load "%s/lisp/nelisp-native-load.el")\n' % root)
        f.write('(load "%s/cases.el")\n' % diff)
        f.write('(defvar nelisp-aot-differential-dir "%s")\n' % diff)
        f.write("(defvar nelisp-aot-differential-chunk %d)\n" % chunk)
        f.write("(defvar nelisp-aot-differential-chunk-size %d)\n" % size)
        # The harness is this tree's; the compiler and loader are ROOT's.
        f.write('(load "%s/test/nelisp-aot-differential-driver.el")\n' % REPO)
    try:
        p = sh("taskset -c 0-7 %s --load %s" % (binary, prelude), root, 120 + 60 * size)
        return p.stdout + p.stderr
    except subprocess.TimeoutExpired:
        return "TIMEOUT"


def parse_chunk(out, names):
    """Map each case that finished to (STATUS, DETAIL)."""
    result = {}
    lines = out.splitlines()
    finished = any(l.startswith("differential chunk") for l in lines)
    current = None
    wrong = {}
    order = []
    for l in lines:
        if l.startswith("RUN "):
            current = l[4:].strip()
            order.append(current)
            wrong[current] = []
        elif l.startswith("WRONG") and current:
            wrong[current].append(l)
    for k, name in enumerate(order):
        last = k == len(order) - 1
        if last and not finished:
            break
        ws = wrong[name]
        if not ws:
            result[name] = ("pass", "")
        else:
            refusal = re.search(r"cannot load[^\n]*?(\(\(:[a-z-]+[^)]*\)\))", ws[0])
            if refusal:
                result[name] = ("refused", refusal.group(1)[:160])
            else:
                result[name] = ("wrong", ws[0][:160])
    if order and not finished:
        result[order[-1]] = ("crash", "")
    return result


def sha_file(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


def compiler_digest(root):
    h = hashlib.sha256()
    for rel in COMPILER_FILES:
        h.update(rel.encode())
        h.update(sha_file(os.path.join(root, rel)).encode())
    return h.hexdigest()


def sh(cmd, cwd, timeout, env=None):
    return subprocess.run(["bash", "-c", LIMIT + cmd], cwd=cwd, env=env,
                          capture_output=True, text=True, timeout=timeout)


def write_generator(work):
    """Write gen.el (the case list as data) and return the case metadata."""
    gen = os.path.join(work, "gen.el")
    meta = os.path.join(work, "meta.json")
    form = (
        "(let* ((cases nelisp-aot-differential-lisp-value-cases)"
        "       (entries (nelisp-aot-differential-lisp-value-entries t)))"
        "  (with-temp-file \"%s\""
        "    (insert \";; -*- lexical-binding: t -*-\\n\""
        "            \"(require 'nelisp-aot-differential-cases)\\n\""
        "            \"(defun codegen-lv-cases () '\" (prin1-to-string entries) \")\\n\""
        "            \"(advice-add 'nelisp-aot-differential-cases :override\""
        "            \" #'codegen-lv-cases)\\n\"))"
        "  (with-temp-file \"%s\""
        "    (insert \"[\" (mapconcat (lambda (c)"
        "      (format \"[%%S, %%S, %%d]\" (format \"%%s\" (nth 0 c)) (symbol-name (nth 1 c))"
        "              (length (nth 3 c)))) cases \", \") \"]\")))"
    ) % (gen, meta)
    r = sh("taskset -c 0-7 emacs --batch -Q -L lisp -L src -L scripts "
           "--eval '(setq load-prefer-newer t)' -l nelisp-aot-differential-cases "
           "--eval %s" % shell_quote(form), REPO, 300)
    if r.returncode != 0 or not os.path.exists(meta):
        sys.exit("generator dump failed: " + r.stderr[-400:])
    stages = json.load(open(meta))
    names = re.findall(r'\("([a-z0-9-]+)" "\(defun ', open(gen).read())
    if len(names) != len(stages):
        sys.exit("case list mismatch: %d names, %d entries" % (len(names), len(stages)))
    return [{"name": n, "stage": s, "kind": k, "args": a}
            for n, (s, k, a) in zip(names, stages)]


def shell_quote(s):
    return "'" + s.replace("'", "'\"'\"'") + "'"


def run(tag, root, binary):
    root = os.path.realpath(root)
    binary = os.path.realpath(binary)
    work = os.path.join(OUT, tag)
    shutil.rmtree(work, ignore_errors=True)
    os.makedirs(work)
    # Fingerprint the inputs before compiling: an edit during the run must
    # not be recorded as what was tested.
    digest_before = compiler_digest(root)
    cases_before = sha_file(CASES_FILE)
    cases = write_generator(work)
    diff = os.path.join(work, "diff")
    env = dict(os.environ, NELISP_DIFF_DIR=diff)
    r = sh("taskset -c 0-7 emacs --batch -Q -L lisp -L src -L scripts "
           "--eval '(setq load-prefer-newer t)' -l %s -f nelisp-aot-differential-main"
           % os.path.join(work, "gen.el"), root, 900, env)
    if not os.path.exists(os.path.join(diff, "count.txt")):
        sys.exit("compile step failed: " + (r.stderr or r.stdout)[-400:])
    natives = native_flags(diff, len(cases))
    pending = list(range(len(cases)))
    while pending:
        # Chunks of CHUNK cases amortize the reader's startup; a chunk that
        # dies is rerun one case per process so the crash is pinned down.
        size = CHUNK if pending[0] % CHUNK == 0 else 1
        first = pending[0]
        idxs = pending[:size]
        out = run_chunk(root, binary, diff, first // size, size)
        parsed = parse_chunk(out, [cases[i]["name"] for i in idxs])
        for i in idxs:
            got = parsed.get(cases[i]["name"])
            if got is None and size > 1:
                continue
            status, detail = got or (("timeout" if out == "TIMEOUT" else "crash"), "")
            if status == "pass" and not natives[i]:
                status = "not-native"
            cases[i].update(status=status, detail=detail)
            print("%-24s %-8s %s %s" % (cases[i]["name"], cases[i]["kind"], status, detail[:60]),
                  flush=True)
        done = [i for i in idxs if "status" in cases[i]]
        if size > 1 and len(done) < size:
            # Rerun the unfinished cases singly.
            rest = [i for i in idxs if i not in done]
            pending = rest + pending[size:]
            for i in rest:
                out = run_chunk(root, binary, diff, i, 1)
                got = parse_chunk(out, [cases[i]["name"]]).get(cases[i]["name"])
                status, detail = got or (("timeout" if out == "TIMEOUT" else "crash"), "")
                if status == "pass" and not natives[i]:
                    status = "not-native"
                cases[i].update(status=status, detail=detail)
                print("%-24s %-8s %s %s" % (cases[i]["name"], cases[i]["kind"], status,
                                            detail[:60]), flush=True)
            pending = pending[len(rest):]
        else:
            pending = pending[size:]
    if compiler_digest(root) != digest_before or sha_file(CASES_FILE) != cases_before:
        digest_before = "changed-during-run"
    rec = {"tag": tag, "root": root, "binary": binary, "binary_sha256": sha_file(binary),
           "compiler_digest": digest_before, "cases_sha256": cases_before,
           "cases": cases}
    with open(os.path.join(OUT, tag + ".json"), "w") as f:
        json.dump(rec, f, indent=1)
    bad = sum(1 for c in cases if c["status"] != "pass")
    print("%s: %d cases, %d not passing" % (tag, len(cases), bad))


PRE229 = "1042eedbc"
KERNELS = ("byte-scan", "list-walk", "fixnum-loop", "class-table", "vector-sum")


def build_kernel(kernel, exe, lisp_root):
    if os.path.exists(exe):
        os.unlink(exe)
    src = os.path.join(REPO, "test", "perf", "codegen-kernels")
    sh("taskset -c 0-7 emacs --batch -Q -L %s/lisp -L %s/src -l %s/compile-kernel.el "
       "--eval '(codegen-kernel-compile \"%s/%s.nl.el\" \"%s\")'"
       % (lisp_root, lisp_root, src, src, kernel, exe), REPO, 600)
    return sha_file(exe) if os.path.exists(exe) else None


def e2e(log, binary):
    binary = os.path.realpath(binary)
    pre = os.path.join(REPO, "target", "codegen", "pre229-src")
    if not os.path.isdir(os.path.join(pre, "lisp")):
        os.makedirs(pre, exist_ok=True)
        sh("git archive %s lisp src | tar -x -C %s" % (PRE229, pre), REPO, 300)
    work = os.path.join(OUT, "kernels")
    os.makedirs(work, exist_ok=True)
    differ = []
    for k in KERNELS:
        a = build_kernel(k, os.path.join(work, k + ".pre229"), pre)
        b = build_kernel(k, os.path.join(work, k + ".tip"), REPO)
        if not a or a != b:
            differ.append(k)
    text = open(log).read()
    m = re.search(r"VALIDATE \S+: (\d+) failing groups", text)
    g = re.search(r"GATE-COUNT: (\d+) cases, (\d+) failures", text)
    r = sh("emacs --batch -Q -L lisp -L src -L scripts -l nelisp-aot-differential-cases "
           "--eval '(princ (mapconcat (lambda (x) (format \"%s\" x)) "
           "nelisp-aot-differential-lisp-value-stages \" \"))'", REPO, 300)
    rec = {"binary": binary, "binary_sha256": sha_file(binary),
           "tree_digest": compiler_digest(REPO), "validate_log": os.path.realpath(log),
           "validate_failing_groups": int(m.group(1)) if m else None,
           "standalone_checks_failures": int(g.group(2)) if g else None,
           "lisp_value_stages": r.stdout.split(),
           "kernels_identical": True if not differ else differ}
    with open(os.path.join(REPO, "target", "codegen", "lispvalue-e2e.json"), "w") as f:
        json.dump(rec, f, indent=1)
    print(json.dumps(rec))


def main():
    if len(sys.argv) == 5 and sys.argv[1] == "run":
        run(sys.argv[2], sys.argv[3], sys.argv[4])
        return 0
    if len(sys.argv) == 4 and sys.argv[1] == "e2e":
        e2e(sys.argv[2], sys.argv[3])
        return 0
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main())
