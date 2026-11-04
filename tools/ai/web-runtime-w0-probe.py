#!/usr/bin/env python3
"""Run bounded W0 diagnostics, preserving the failed general-runtime exit.

One command replaces manual compiler invocation, Node reservation diagnosis,
fixture encoding, provider source lookup, and artifact collection. Set
NOTES_ROOT when the common Git directory is outside the Notes workspace.
This tool never generates application JavaScript or supplies Lisp imports.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import shlex
import shutil
import signal
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "target" / "web-runtime-w0"


def bounded():
    """Retain the workspace's 8 GiB address-space cap in every child."""
    cap = 8388608 * 1024
    resource.setrlimit(resource.RLIMIT_AS, (cap, cap))


def run(name, command, env=None, timeout=120):
    """Capture one command with a wall deadline and bounded console output."""
    start = time.monotonic()
    with (OUTPUT / f"{name}.log").open("w") as log:
        process = subprocess.Popen(command, cwd=ROOT, env=env, stdout=log,
                                   stderr=subprocess.STDOUT, preexec_fn=bounded,
                                   start_new_session=True)
        try:
            rc = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            # The group includes make/guard descendants, even if its leader exits.
            for sig in (signal.SIGTERM, signal.SIGKILL):
                try:
                    os.killpg(process.pid, sig)
                except ProcessLookupError:
                    pass
                if sig == signal.SIGTERM:
                    try:
                        process.wait(timeout=0.5)
                    except subprocess.TimeoutExpired:
                        pass
            process.wait()
            rc = 124
    text = (OUTPUT / f"{name}.log").read_text(errors="replace")
    row = dict(name=name, command=command, rc=rc,
               elapsed_seconds=round(time.monotonic() - start, 6),
               log=f"target/web-runtime-w0/{name}.log")
    if name.startswith("smoke"):
        row.update(validated=text.count("validate=true"),
                   returned=sum(line.startswith("result=") for line in text.splitlines()))
    print(f"{name}: rc={rc} elapsed={row['elapsed_seconds']}s", flush=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-first-only", action="store_true",
                        help="diagnose shared runtime source without repeating smoke fixtures")
    args = parser.parse_args()
    OUTPUT.mkdir(parents=True, exist_ok=True)
    common = Path(subprocess.check_output(
        ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
        cwd=ROOT, text=True).strip())
    notes = Path(os.environ.get("NOTES_ROOT", common.parent.parent.parent))
    guard = notes / "bin" / "heavy-run.sh"
    if not guard.is_file():
        raise SystemExit("Cannot locate Notes heavy-run.sh; set NOTES_ROOT")
    commands = []
    identities = {}
    emacs = shutil.which("emacs")
    if not emacs:
        raise SystemExit("Emacs is required for source-first compilation")
    executables = {"emacs": emacs}
    if not args.source_first_only:
        node = shutil.which("node")
        if not node:
            raise SystemExit("Node is required; no alternate engine is selected automatically")
        executables["node"] = node
        # Disable Node's oversized trap reservation; keep explicit bounds checks.
        wrapper = OUTPUT / "bounded-node"
        wrapper.mkdir(exist_ok=True)
        launcher = wrapper / "node"
        launcher.write_text("#!/bin/sh\nexec " + shlex.quote(node) +
                            ' --disable-wasm-trap-handler "$@"\n')
        launcher.chmod(0o755)
        commands.append(run("smoke-default", [str(guard), "make", "wasm-smoke",
                                              "EMACS=" + emacs]))
        env = os.environ.copy()
        env["PATH"] = str(wrapper) + os.pathsep + env["PATH"]
        commands.append(run("smoke-bounded", [str(guard), "make", "wasm-smoke",
                                              "EMACS=" + emacs], env))
    for name, executable in executables.items():
        identity = run(name + "-identity", [executable, "--version"], timeout=10)
        identities[name] = dict(path=str(Path(executable).resolve()),
                                sha256=hashlib.sha256(Path(executable).read_bytes()).hexdigest(),
                                version=(OUTPUT / (name + "-identity.log")).read_text().splitlines()[0],
                                rc=identity["rc"])
    # Never let an interrupted compiler's old report describe a new attempt.
    (OUTPUT / "source-first.json").unlink(missing_ok=True)
    commands.append(run("source-first", [str(guard), emacs, "-Q", "--batch",
                        "-L", "lisp", "-L", "src", "-L", "tools/ai",
                        "--eval", "(setq load-prefer-newer t)",
                        "-l", "web-runtime-w0-probe", "-f", "web-runtime-w0-probe-run"]))
    source = json.loads((OUTPUT / "source-first.json").read_text())
    result = dict(schema_version=1, w0_exit="blocked", commands=commands,
                  source_first=source, memory_cap_kib=8388608,
                  source_first_only=args.source_first_only,
                  executable_identities=identities,
                  node_flags=[] if args.source_first_only else ["--disable-wasm-trap-handler"],
                  limitations=["W0 application execution and its builtin-removal negative control remain red",
                               "Node diagnostics do not establish real-browser execution"])
    (OUTPUT / "lane-result.json").write_text(json.dumps(result, indent=2) + "\n")
    print("W0 remains red: see target/web-runtime-w0/lane-result.json")
    return 1


if __name__ == "__main__":
    sys.exit(main())
