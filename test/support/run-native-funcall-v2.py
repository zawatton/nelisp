"""Bound one reader process and retain its executable identity and timing."""
import hashlib
import json
import os
from pathlib import Path
import resource
import subprocess
import sys
import time

binary, work, phase, driver = sys.argv[1:]
directory = Path(work)
command = ["timeout", "-k", "5", "290", binary]
if os.environ.get("F1B_COLD") == "1":
    cold = Path(binary + ".cold").resolve(strict=True)
    command += ["--cold-load-from", str(cold)]
command += ["-L", "lisp", "-L", "src", "-L", "scripts", "-L",
            "packages/nl-ffi/src", "-L", "packages/nl-prelude/src", "-l", driver]
start = time.monotonic()
with directory.joinpath(phase + ".out").open("w") as stdout, directory.joinpath(phase + ".err").open("w") as stderr:
    result = subprocess.run(command, stdout=stdout, stderr=stderr)
directory.joinpath(phase + ".json").write_text(json.dumps(dict(
    seconds=time.monotonic() - start, rc=result.returncode,
    binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
    cold_sha256=hashlib.sha256(cold.read_bytes()).hexdigest() if os.environ.get("F1B_COLD") == "1" else None,
    peak_rss_kib=resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss), indent=2))
if result.returncode:
    print(directory.joinpath(phase + ".err").read_text()[-6000:])
    print("F1B failed evidence=" + work)
    sys.exit(result.returncode if result.returncode > 0 else 1)
