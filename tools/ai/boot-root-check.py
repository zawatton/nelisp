#!/usr/bin/env python3
"""Compare precise root storage in source and companion-image boots.

No diagnostic builtin or compiler bootstrap is needed: use the executable's
own ELF symbols with the existing pointer API.  This keeps the probe on the
ordinary CLI path and avoids loading the native compiler merely to inspect GC.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/boot-image"
TIMINGS = {}


def fixture(symbols):
    def address(name):
        return str(symbols[name])

    # These are root families, not all pointer-shaped runtime control words.
    # The mark entry points are nl_gc_mark_roots and the recorded collector;
    # both call the same rootstack/worker/cache/symbol-sidecar walkers.
    source = r'''
(defun boot-root-row (name slot permanent)
  (let ((boot (ptr-call IS_BOOT slot 0 0 0 0 0)))
    (princ (format "ROOT %s %d %d\n" name slot boot))
    (when (and permanent (/= boot 1))
      (error "init root %s occupies a collectable image hole" name))))
(let* ((base (car (nelisp--arena-stats))) (context GC_CONTEXT)
       (env (ptr-read-u64 context 64)) (index 0))
  ;; Frame zero is the driver's seven-argument root record. Further records
  ;; are per-call evaluator roots, and deliberately vary with probe evaluation.
  (dolist (name '(env result out pool src cursor builtin-symbol))
    (boot-root-row (format "driver/%s" name)
                   (ptr-read-u64 context (+ 64 (* index 8))) t)
    (setq index (+ index 1)))
  (boot-root-row "env/globals" env t)
  (boot-root-row "env/frames" (+ env 32) t)
  (boot-root-row "env/unbound" (+ env 64) t)
  ;; TAG and VALUE are inline slots in the arena's reserved metadata prefix.
  ;; They are unconditionally walked, even when the exit flag is clear.
  (boot-root-row "exit/tag" (+ base 24) t)
  (boot-root-row "exit/value" (+ base 56) t)
  (let ((entry (ptr-read-u64 base 872)))
    (when (/= entry 0) (boot-root-row "shared/symentry" entry nil)))
  (princ (format "RANGE evaluator %d %d\n" ROOT_REGION
                 (ptr-read-u64 ROOT_TOP 0)))
  (princ (format "RANGE pinned %d %d\n" PIN_REGION
                 (ptr-read-u64 PIN_CONTROL 24)))
  (princ (format "FRAMES %d WORKERS %d\n" (ptr-read-u64 context 0)
                 (ptr-read-u64 THREADS 0)))
  (princ (format "CACHE macro %d function %d symbols %d\n"
                 (ptr-read-u64 MX_CACHE 0) (ptr-read-u64 FV_CACHE 0)
                 (ptr-read-u64 SYMBOL_NAMES 0)))
  (princ (format "BACKTRACE %d CATCH %d\n" (ptr-read-u64 env 176)
                 (ptr-read-u64 CATCH_HEAD 0)))
  ;; Init-populated BSS symbol slots own interned names. The BSS slots
  ;; themselves are never arena allocations and are not image relocations.
  (boot-root-row "frame/lexframe-symbol" FRAME_SYMBOL nil)
  (boot-root-row "frame/hash-symbol" HASH_SYMBOL nil))
(princ (format "HEAP %S\n" (nelisp--arena-walk-verify)))
(princ "BOOT-ROOT-CENSUS PASS\n")
'''
    for token, name in {
        "IS_BOOT": "nl_gc_is_boot", "GC_CONTEXT": "nl_gc_loop_ctx",
        "ROOT_REGION": "nl_rootstack_region", "ROOT_TOP": "nl_rootstack_top",
        "PIN_REGION": "nl_root_pin_region", "PIN_CONTROL": "nl_root_pin_control",
        "THREADS": "nl_thread_registry", "MX_CACHE": "nl_mxcache_table_base",
        "FV_CACHE": "nl_fvcache_table_base", "SYMBOL_NAMES": "nl_symbol_name_state",
        "CATCH_HEAD": "nl_catch_head",
        "FRAME_SYMBOL": "nl_frame_push_sym0", "HASH_SYMBOL": "nl_frame_push_sym1",
    }.items():
        source = source.replace(token, address(name))
    return source


def run(binary, argv, env, name):
    started = time.monotonic()
    completed = subprocess.run([str(binary), *map(str, argv)], cwd=ROOT, env=env,
                               capture_output=True, text=True, timeout=90)
    TIMINGS[name] = time.monotonic() - started
    (OUT / (name + ".out")).write_text(completed.stdout)
    (OUT / (name + ".err")).write_text(completed.stderr)
    assert completed.returncode == 0 and not completed.stderr, (
        name, completed.returncode, completed.stdout, completed.stderr)
    return completed.stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", default=os.environ.get("BOOT_IMAGE_BIN"), required=False)
    parser.add_argument("--census-only", action="store_true",
                        help="Compare startup root families without the separate retention corpus")
    args = parser.parse_args()
    binary = Path(args.binary).resolve(strict=True)
    assert Path(str(binary) + ".cold").is_file(), "An image companion is required"
    OUT.mkdir(parents=True, exist_ok=True)
    nm = subprocess.check_output(["nm", str(binary)], text=True)
    symbols = {parts[2]: int(parts[0], 16) for line in nm.splitlines()
               if len(parts := line.split()) == 3}
    env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
               XDG_CACHE_HOME=str(ROOT / "target/cache"))
    Path(env["TMPDIR"]).mkdir(parents=True, exist_ok=True)
    rows = {}
    with tempfile.TemporaryDirectory(prefix="boot-roots-", dir=env["TMPDIR"]) as temp:
        script = Path(temp) / "roots.el"
        script.write_text(fixture(symbols))
        # Same executable and digest, with no companion: the real source-boot
        # fallback, rather than a second binary with a different native ABI.
        source_binary = Path(temp) / "source-boot"
        shutil.copy2(binary, source_binary)
        for mode, executable in (("source", source_binary), ("image", binary)):
            output = run(executable, ["--load", script], env, "roots-" + mode)
            assert "BOOT-ROOT-CENSUS PASS\n" in output, output
            assert output.split("HEAP ")[1].splitlines()[0].endswith(" 1)"), output
            roots = [line.split()[1] for line in output.splitlines() if line.startswith("ROOT ")]
            assert len(roots) >= 12, "Empty or incomplete root enumeration"
            rows[mode] = roots
            # Keep the original retention test untouched. Extra diagnostics
            # can change allocation layout and mask the pre-fix corruption.
            for padding in (() if args.census_only else (None, "", "x" * 127)):
                case_env = dict(env, EVAL_BIN=str(executable))
                if padding is not None:
                    case_env["BOOT_ROOT_PADDING"] = padding
                output = run(executable, ["--load", "test/standalone-exit-stash-root-check.el"],
                             case_env, "roots-" + mode + "-stash-" + str(padding is None) + "-" + str(len(padding or "")))
                assert "-- PASS\n" in output and "live bytes held" in output, output
    assert rows["source"] == rows["image"], rows
    receipt = dict(sha256=hashlib.sha256(binary.read_bytes()).hexdigest(), roots=rows,
                   cases=2 if args.census_only else 8, seconds=TIMINGS, result="PASS",
                   retention="not run" if args.census_only else "PASS")
    (OUT / "roots.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(json.dumps(receipt))


if __name__ == "__main__":
    main()
