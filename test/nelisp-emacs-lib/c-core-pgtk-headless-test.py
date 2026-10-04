#!/usr/bin/env python3
"""Verify the PGTK C-core probe is complete and safe in headless batch mode."""
import json
import os
from collections import Counter
from pathlib import Path
import re
import subprocess


ROOT = Path(__file__).resolve().parents[2]
PROBE = ROOT / "test/nelisp-emacs-lib/c-core-probes/pgtkfns-1.el"
DRIVER = ROOT / "test/nelisp-emacs-lib/c-core-parity-driver.el"
NAMES = {"pgtk-backend-display-class", "pgtk-display-monitor-attributes-list", "pgtk-font-name",
         "pgtk-frame-edges", "pgtk-frame-geometry", "pgtk-frame-restack", "pgtk-get-page-setup",
         "pgtk-mouse-absolute-pixel-position", "pgtk-page-setup-dialog", "pgtk-print-frames-dialog",
         "pgtk-set-monitor-scale-factor", "pgtk-set-mouse-absolute-pixel-position"}
EXPECTED_FORMS = 32
EXPECTED_COUNTS = {"pgtk-backend-display-class": 2, "pgtk-display-monitor-attributes-list": 2,
                   "pgtk-font-name": 3, "pgtk-frame-edges": 3, "pgtk-frame-geometry": 3,
                   "pgtk-frame-restack": 3, "pgtk-get-page-setup": 3,
                   "pgtk-mouse-absolute-pixel-position": 2, "pgtk-page-setup-dialog": 2,
                   "pgtk-print-frames-dialog": 3, "pgtk-set-monitor-scale-factor": 3,
                   "pgtk-set-mouse-absolute-pixel-position": 3}


def run(command, env, cwd):
    return subprocess.run(command, env=env, cwd=cwd, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, timeout=20)


def main():
    host = os.environ.get("EMACS", "emacs")
    version = run([host, "--version"], os.environ.copy(), ROOT)
    if version.returncode or version.stderr or not version.stdout.startswith(b"GNU Emacs 31.1"):
        raise SystemExit("requires clean GNU Emacs 31.1")
    env = os.environ.copy()
    env["C_CORE_UNIT"] = "pgtkfns-1"
    result = run([host, "-Q", "--batch", "-l", str(DRIVER)], env, ROOT)
    text = result.stdout.decode("utf-8", "strict")
    lines = text.splitlines()
    rows = [line for line in lines if line.startswith("P| ")]
    names = {m.group(1) for line in rows if (m := re.match(r"P\| ([^ ]+) \|", line))}
    counts = Counter(m.group(1) for line in rows if (m := re.match(r"P\| ([^ ]+) \|", line)))
    cases = {
        "host_exits_zero": result.returncode == 0,
        "host_stderr_empty": result.stderr == b"",
        "one_done_marker": lines.count("P-DONE") == 1 and lines[-1:] == ["P-DONE"],
        "all_original_names_retained": names == NAMES,
        "form_count_preserved": len(rows) == EXPECTED_FORMS,
        "per_primitive_form_counts_preserved": counts == EXPECTED_COUNTS,
        "dialogs_are_safe_validation_only": (
            all("wrong-number-of-arguments" in line for line in rows if line.startswith("P| pgtk-page-setup-dialog "))
            and sum("wrong-number-of-arguments" in line for line in rows if line.startswith("P| pgtk-print-frames-dialog ")) == 2
            and sum("wrong-type-argument" in line for line in rows if line.startswith("P| pgtk-print-frames-dialog ")) == 1),
        "unsafe_mouse_set_is_arity_only": all("wrong-number-of-arguments" in line for line in rows
                                               if line.startswith("P| pgtk-set-mouse-absolute-pixel-position ")),
    }
    print(json.dumps({"cases": cases, "passed": sum(cases.values()), "total": len(cases),
                      "forms": len(rows), "primitive_names": len(names), "host_rc": result.returncode}, sort_keys=True))
    return 0 if all(cases.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
