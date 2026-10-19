#!/usr/bin/env python3
"""Probe ordinary and N3 shared-cell evaluation in a built word-ABI binary."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def main():
    binary = Path(sys.argv[1]).resolve()
    symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
        ["nm", "-n", str(binary)], text=True).splitlines()
        if len(p := line.split()) == 3 and p[1] in ("t", "T")}
    root = ROOT / "target/abi-cell-word"
    root.mkdir(parents=True, exist_ok=True)
    directory = Path(tempfile.mkdtemp(dir=root))
    cache = directory / "cache"
    cache.mkdir()
    source = "(setq " + " ".join(f"{name} {symbols[symbol]}" for name, symbol in (
        ("abi-cell-eval", "nl_eval_inner_w"),
        ("abi-cell-cons", "nelisp_cons_construct_w"),
        ("abi-cell-get", "nl_cell_get_value_w"))) + ")\n" + r'''
(let ((checked 0))
  (dolist (marker '(0 13107))
    (dolist (expected '(3 7 69 -31))
      (let* ((cell (alloc-bytes 32 8)) (payload (alloc-bytes 16 8))
             (value (if (= marker 0) expected
                      (ptr-call abi-cell-cons 69 expected 0 0 0 0))))
        (ptr-write-u64 cell 0 11) (ptr-write-u64 cell 8 payload)
        (ptr-write-u64 cell 16 marker) (ptr-write-u64 cell 24 0)
        (ptr-write-u64 payload 0 value) (ptr-write-u64 payload 8 1)
        (dolist (producer (list abi-cell-eval abi-cell-get))
          (let ((actual (ptr-call producer cell 0 0 0 0 0)))
            (unless (= actual expected)
              (error "cell marker %d: expected WORD %d, got %d" marker expected actual)))
          (setq checked (1+ checked))))))
  (princ (format "ABI-CELL-WORD checked=%d PASS\n" checked)))
'''
    script = directory / "probe.el"
    script.write_text(source)
    with (directory / "stdout").open("w") as out, (directory / "stderr").open("w") as err:
        result = subprocess.run([str(binary), "--load", str(script)], cwd=ROOT,
                                env=dict(os.environ, TMPDIR=str(directory),
                                         XDG_CACHE_HOME=str(cache)),
                                stdout=out, stderr=err, timeout=120)
    passed = (result.returncode == 0 and not (directory / "stderr").stat().st_size
              and "ABI-CELL-WORD checked=16 PASS\n" in (directory / "stdout").read_text())
    print(f"cell WORD probe {'PASS' if passed else 'FAIL'}: {directory}")
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
