"""Record the brief's existing host suites per file, including their counts."""
import glob
import json
from pathlib import Path
import re
import subprocess
import sys

lisp, evidence = sys.argv[1:]
directory = Path(evidence)
directory.mkdir(parents=True, exist_ok=True)
files = sorted(set(sum((glob.glob(pattern) for pattern in (
    "test/nelisp-native-cache-test.el", "test/nelisp-native-gccjit-test.el",
    "test/v1-single-validation-test.el", "test/nelisp-native-funcall-v2-test.el",
    "test/nelisp-native-load*-test.el", "test/nelisp-bytecode-native-rooted-cfg-*-test.el",
    "test/p1a-test.el", "test/nelisp-bytecode-coverage-audit*test.el")), [])))
rows = {}
for file in files:
    out = directory / (Path(file).stem + ".out")
    with out.open("w") as stream:
        result = subprocess.run(["timeout", "120", "emacs", "-Q", "--batch",
            "-L", lisp, "-L", "src", "-L", "scripts", "-L", "test", "-L",
            "packages/nl-ffi/src", "-l", file, "-f", "ert-run-tests-batch-and-exit"],
            stdout=stream, stderr=subprocess.STDOUT)
    summary = re.search(r"Ran (\d+) tests, (\d+) results as expected, (\d+) unexpected", out.read_text())
    rows[file] = dict(rc=result.returncode,
                     total=int(summary[1]) if summary else None,
                     expected=int(summary[2]) if summary else None,
                     unexpected=int(summary[3]) if summary else None)
directory.joinpath("counts.json").write_text(json.dumps(rows, indent=2))
print(json.dumps(dict(files=len(rows), tests=sum(row["total"] or 0 for row in rows.values()),
                     failures=[file for file, row in rows.items() if row["rc"]])))
sys.exit(bool(any(row["rc"] for row in rows.values())))
