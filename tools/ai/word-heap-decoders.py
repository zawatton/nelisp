#!/usr/bin/env python3
"""Exercise existing WORD encodings through the linked decoder entry points.

Usage: word-heap-decoders.py OUTPUT BINARY [BINARY ...]
Addresses come from each binary's symbol table. No new-tag objects are fed
to old decoders: S1 dispatch is a prerequisite for introducing those objects.
"""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys


def main():
    output = Path(sys.argv[1]).resolve()
    output.mkdir(parents=True, exist_ok=True)
    reports = []
    for index, name in enumerate(sys.argv[2:]):
        binary = Path(name).resolve()
        symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
            ["nm", "-n", str(binary)], text=True).splitlines()
                   if len(p := line.split()) == 3}

        def call(fn, arg):
            return f"(ptr-call {symbols[fn]} {arg} 0 0 0 0 0)"

        forms = ["(nelisp--debug-switch 7)"]
        expected = {}
        for word, tag in ((3, 0), (7, 1), (-67, 2), (1, 2), (69, 2)):
            key = f"word-{word}"
            values = [call(fn, str(word)) for fn in ("nl_val_is_imm", "nl_val_tag", "nl_word_tag64", "wf_bytecode_w_tag", "nl_word_payload")]
            expected[key] = [1, tag, tag, tag, word >> 2 if tag == 2 else 0]
            forms.append(f'(princ (format "WH-DECODE {key} %S\\n" (list {" ".join(values)})))')
        # An explicitly allocated legacy Sexp integer box and cons box.
        alloc = symbols["nl_alloc_bytes"]
        forms.extend((f"(defvar wh-box (ptr-call {alloc} 32 8 0 0 0 0))",
                      "(ptr-write-u64 wh-box 0 2)", "(ptr-write-u64 wh-box 8 -17)"))
        funcs = ("nl_val_is_imm", "nl_val_tag", "nl_word_tag64", "wf_bytecode_w_tag", "nl_word_payload", "wf_bytecode_w_int")
        values = " ".join(call(fn, "wh-box") for fn in funcs)
        forms.append(f'(princ (format "WH-DECODE legacy-int %S\\n" (list {values})))')
        expected["legacy-int"] = [0, 2, 2, 2, -17, -17]
        forms.extend((f"(defvar wh-cons (ptr-call {alloc} 24 8 0 0 0 0))",
                      "(ptr-write-u64 wh-cons 0 7)", "(ptr-write-u64 wh-cons 8 3)",
                      "(ptr-write-u64 wh-cons 16 0)", "(ptr-write-u64 wh-box 0 7)",
                      "(ptr-write-u64 wh-box 8 wh-cons)"))
        values = " ".join(call(fn, "wh-box") for fn in ("nl_val_tag", "nl_word_tag64", "wf_bytecode_w_tag", "nl_cons_car_word", "nl_cons_cdr_word"))
        forms.append(f'(princ (format "WH-DECODE legacy-cons %S\\n" (list {values})))')
        expected["legacy-cons"] = [7, 7, 7, 7, 3]
        forms.append('(princ "WH-DECODE-DONE\\n")')
        probe = output / f"binary-{index}.el"
        probe.write_text("\n".join(forms) + "\n")
        run = subprocess.run([str(binary), "--load", str(probe), "--eval", "nil"], capture_output=True, timeout=90)
        probe.with_suffix(".out").write_bytes(run.stdout)
        probe.with_suffix(".err").write_bytes(run.stderr)
        probe.with_suffix(".rc").write_text(str(run.returncode) + "\n")
        actual = {k: list(map(int, v.split())) for k, v in re.findall(r"WH-DECODE (\S+) \(([^)]+)\)", run.stdout.decode())}
        if run.returncode or run.stderr or b"WH-DECODE-DONE\n" not in run.stdout or actual != expected:
            raise ValueError(f"decoder probe failed: {binary}: {actual}")
        linkage = "dynamic" if "INTERP" in subprocess.check_output(["readelf", "-l", str(binary)], text=True) else "static"
        reports.append(dict(binary=str(binary), sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
                            cold_sha256=hashlib.sha256(Path(str(binary) + ".cold").read_bytes()).hexdigest(), linkage=linkage,
                            cases=actual, expected=expected, probe=str(probe), returncode=run.returncode))
    (output / "results.json").write_text(json.dumps(reports, indent=2) + "\n")
    print(f"PASS existing encodings: binaries={len(reports)} cases={sum(len(r['cases']) for r in reports)}")


if __name__ == "__main__":
    main()
