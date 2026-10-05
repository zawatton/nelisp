#!/usr/bin/env bash
# Run against a dynamic reader; no build, cold image, or new native is needed.
# Usage: tools/exit-group-smoke.sh [BINARY] [RESULT-DIRECTORY]
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec python3 - "$root" "${1:-${NELISP_BIN:-$root/target/nelisp}}" "${2:-$root/target/exit-group-smoke}" <<'PY'
import hashlib
import json
import os
from pathlib import Path
import platform
import signal
import subprocess
import sys
import time

root, binary, output = map(lambda p: Path(p).resolve(), sys.argv[1:])
if platform.system() != "Linux" or platform.machine() != "x86_64":
    sys.exit("exit-group-smoke requires Linux x86_64 and /proc task visibility")
output.mkdir(parents=True, exist_ok=True)
if not binary.is_file() or not os.access(binary, os.X_OK):
    sys.exit(f"missing executable: {binary}")

# The runtime's clone workers use CLONE_VM|CLONE_FS|CLONE_FILES, without
# CLONE_THREAD: they have separate thread groups and cannot expose this bug.
# Existing dlopen/dlsym/ptr-call primitives start a real libc pthread instead.
# pause ignores its extra ABI argument and never returns without a signal;
# no worker executes Lisp, touches its heap, or calls back into the evaluator.
setup = '''
(defun exit-smoke-cstring (s)
  (let ((p (alloc-bytes (+ (length s) 1) 1)) (i 0))
    (while (< i (length s))
      (ptr-write-u8 p i (aref s i)) (setq i (+ i 1)))
    (ptr-write-u8 p i 0) p))
(setq exit-smoke-lib (nl-ffi-call "dlopen" 0 2))
(setq exit-smoke-create
      (nl-ffi-call "dlsym" exit-smoke-lib (exit-smoke-cstring "pthread_create")))
(setq exit-smoke-pause
      (nl-ffi-call "dlsym" exit-smoke-lib (exit-smoke-cstring "pause")))
(unless (and (> exit-smoke-create 0) (> exit-smoke-pause 0))
  (error "pthread symbols missing"))
(setq exit-smoke-slot (alloc-bytes 8 8))
(unless (= (ptr-call exit-smoke-create exit-smoke-slot 0 exit-smoke-pause 0 0 0) 0)
  (error "pthread_create failed"))
(princ "EXIT-GROUP-WORKER-READY\\n")
'''
barrier = '(read-string "")\n'
immediate = '(princ "EXIT-GROUP-UNREACHABLE\\n")\n'
cases = [
    ("script-eof", "script", "nil\n", 0, None),
    ("script-return", "script", "23\n", 23, None),
    ("stdin-eof", "repl", "", 0, None),
    ("exit", "script", "(exit 37)\n", 37, None),
    ("kill-emacs", "script", "(kill-emacs 41)\n" + immediate, 41, None),
    ("exit-process", "script", "(nelisp--exit-process 43)\n" + immediate, 43, None),
    ("portable-exit", "script", "(nelisp-portable-syscall 'exit 47)\n" + immediate,
     47, None),
    ("fatal-eval", "script", '(error "exit-group-fatal")\n' + immediate,
     1, "exit-group-fatal"),
    ("fatal-read", "script", "(\n", 1, "nelisp:"),
    # RLIMIT_AS=0 prevents any NEW mapping, leaving existing mappings intact.
    # A 2 GiB request forces arena growth to fail without mapping/touching it.
    ("fatal-allocation", "script", '''
(setq exit-smoke-limit (alloc-bytes 16 8))
(ptr-write-u64 exit-smoke-limit 0 0)
(ptr-write-u64 exit-smoke-limit 8 0)
(unless (= (syscall-direct 160 9 exit-smoke-limit 0 0 0 0) 0)
  (error "setrlimit failed"))
(alloc-bytes 2147483648 8)
''' + immediate, 88, None),
]

def tasks(pid):
    taskdir = Path(f"/proc/{pid}/task")
    result = {}
    try:
        for task in taskdir.iterdir():
            try:
                status = (task / "status").read_text()
                result[task.name] = next(line for line in status.splitlines()
                                         if line.startswith("State:"))
            except FileNotFoundError:
                pass
    except FileNotFoundError:
        pass
    return result

results = []
for name, mode, ending, expected, diagnostic in cases:
    source = output / f"{name}.el"
    # REPL reads one line at a time. It needs no read-string barrier: leave
    # stdin open after sending SETUP, then close it only after task inspection.
    source.write_text(setup + (barrier + ending if mode == "script" else ""))
    command = [str(binary), str(source)] if mode == "script" else [str(binary), "--repl", "--no-prompt", "--no-print"]
    stdout, stderr = output / f"{name}.out", output / f"{name}.err"
    row = dict(path=name, command=command, expected=expected, passed=False)
    with stdout.open("wb") as out, stderr.open("wb") as err:
        process = subprocess.Popen(command, cwd=root, stdin=subprocess.PIPE,
                                   stdout=out, stderr=err, start_new_session=True)
        try:
            if mode == "repl":
                # A single progn is one REPL line, including the function body.
                process.stdin.write(("(progn " + setup.replace("\n", " ") + ")\n").encode())
                process.stdin.flush()
            deadline = time.monotonic() + 30  # startup is not the quit deadline
            while time.monotonic() < deadline and process.poll() is None:
                if "EXIT-GROUP-WORKER-READY" in stdout.read_text():
                    break
                time.sleep(0.02)
            ready_tasks = tasks(process.pid)
            row["tasks_before_exit"] = ready_tasks
            if ("EXIT-GROUP-WORKER-READY" not in stdout.read_text()
                    or len(ready_tasks) < 2 or process.poll() is not None):
                raise RuntimeError("worker setup failed: ready marker and two live tasks required")
            start = time.monotonic()
            if mode == "script":
                process.stdin.write(b"go\n")
                process.stdin.flush()
            process.stdin.close()  # also triggers the stdin EOF case
            try:
                row["returncode"] = process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                row["timeout"] = True
                row["tasks_after_timeout"] = tasks(process.pid)
                raise RuntimeError("process did not terminate within 5 seconds")
            row["exit_seconds"] = round(time.monotonic() - start, 3)
            row["tasks_after_exit"] = tasks(process.pid)
            text, errors = stdout.read_text(), stderr.read_text()
            if row["returncode"] != expected or row["tasks_after_exit"]:
                raise RuntimeError("wrong exit status or surviving tasks")
            if "EXIT-GROUP-UNREACHABLE" in text:
                raise RuntimeError("immediate/fatal exit continued evaluating")
            if diagnostic is not None:
                if diagnostic not in errors:
                    raise RuntimeError("expected fatal diagnostic missing")
            elif errors:
                raise RuntimeError("unexpected stderr")
            row["passed"] = True
        except (RuntimeError, BrokenPipeError) as error:
            row["error"] = str(error)
        finally:
            # Only clean up a failed probe; forced kill never counts as PASS.
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
            process.wait(timeout=5)
            if not process.stdin.closed:
                process.stdin.close()
    results.append(row)
    print(f"{'PASS' if row['passed'] else 'FAIL'} {name}: "
          f"{row.get('returncode', row.get('error'))} (expected {expected})", flush=True)

report = dict(binary=str(binary), sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
              results=results)
(output / "results.json").write_text(json.dumps(report, indent=2) + "\n")
failed = sum(not row["passed"] for row in results)
print(f"GATE-COUNT checked={len(results)} findings={failed}")
sys.exit(bool(failed))
PY
