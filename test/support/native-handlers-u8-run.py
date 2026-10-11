"""Bound actual reader invocations; keep GNU oracle and native receipts separate."""
import hashlib
import concurrent.futures
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
CASES = "catch condition nested stackset cleanup loop nil missing outer object".split()


def verdict(phase, rc, elapsed, output, errors, expected=None):
    """Never count a pending admission or a zero-entry run as native success."""
    if rc or elapsed >= 300 or errors:
        return False
    if phase == "compile":
        markers = [line for line in output.splitlines() if line.startswith("U8B-COMPILED fixture=")]
        return (bool(markers) and len(set(markers)) == len(markers)
                and output.count("U8B-COMPILE-COMPLETE fixtures=" + str(len(markers))) == 1)
    if phase == "vm":
        # The standalone source loader may print the final form's string
        # value. Compare the explicit observations, not that REPL echo.
        records = "".join(line + "\n" for line in output.splitlines()
                          if line.startswith(("U8B-OBSERVE ", "U8B-VM-COMPLETE ")))
        return records == expected and "U8B-VM-COMPLETE" in records
    records = "".join(line + "\n" for line in output.splitlines()
                      if line.startswith(("U8B-OBSERVE ", "U8B-VM-COMPLETE ")))
    if records != expected:
        return False
    lines = output.splitlines()
    markers = [line for line in lines if line.startswith("U8B-NATIVE-PASS ")]
    if len(markers) != 1:
        return False
    fields = dict(word.split("=", 1) for word in markers[0].split()[1:])
    return (fields.get("refused") == "0" and int(fields.get("cases", "0")) > 0
            and int(fields.get("native-entries", "0")) >= int(fields["cases"]))


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    phase, *binaries = sys.argv[1:]
    selected = None
    if binaries and binaries[0] == '--backend':
        _, selected, *binaries = binaries
        if selected not in ('in-house', 'gccjit', 'template') or len(binaries) != 1:
            raise SystemExit('Invalid backend/binary selection')
        binaries *= 2
    if phase not in ("native", "vm") or len(binaries) != 2:
        raise SystemExit("Expected native|vm STATIC DYNAMIC")
    directory = Path(tempfile.mkdtemp(prefix="handlers-u8-", dir=ROOT / "target"))
    os.chmod(directory, 0o700)
    source = ROOT / "test/support/native-handlers-u8-fixtures.el"
    fixture = directory / "fixture.el"
    shutil.copyfile(source, fixture)
    env = os.environ.copy()
    env["U8B_FIXTURE_SOURCE"] = str(fixture)
    # Environment arguments, not interpolated Lisp/shell quoting.
    command = [env.get("EMACS", "emacs"), "-Q", "--batch", "--eval",
               '(progn (require (quote bytecomp)) '
               '(setq byte-compile-error-on-warn t) '
               '(unless (byte-compile-file (getenv "U8B_FIXTURE_SOURCE")) '
               '(error "U8 fixture compilation failed")))']
    with (directory / "compile.out").open("w") as out, (directory / "compile.err").open("w") as err:
        subprocess.run(command, env=env, stdout=out, stderr=err, check=True, timeout=30)
    fixture = fixture.with_suffix(".elc")
    selection = env.get("U8B_CASE_FILTER", " ".join(CASES)).split()
    if not selection or len(set(selection)) != len(selection) or any(case not in CASES for case in selection):
        raise SystemExit("U8B_CASE_FILTER must contain distinct known fixture names")
    if "outer" in selection:
        selection = ["catch", "condition"] + [case for case in selection if case not in ("catch", "condition")]
    batch = int(env.get("U8B_VM_BATCH_SIZE", "6"))
    if batch not in (1, 6, 8):
        raise SystemExit("U8B_VM_BATCH_SIZE must be 1, 6 or 8")
    # U8r makes nil/missing locally catchable. Opt-in batch 8 verifies every
    # explicit oracle record in one process; batch 1/6 retain legacy controls.
    native_batch = int(env.get("U8B_NATIVE_BATCH_SIZE", "1"))
    if native_batch not in (1, 2, 8):
        raise SystemExit("U8B_NATIVE_BATCH_SIZE must be 1, 2 or 8")
    groups = ([selection[i:i + native_batch] for i in range(0, len(selection), native_batch)] if phase == "native" else
              [selection] if batch == 8 or selection != CASES else
              [CASES[i:i + batch] for i in range(0, 6, batch)] + [[case] for case in CASES[6:]])
    receipts = []
    source_hashes = {str(p.relative_to(ROOT)): sha(p) for p in [source,
                    ROOT / "test/support/native-entry-observer.el",
                    ROOT / "test/standalone-native-handlers-u8-driver.el",
                    Path(__file__).resolve(),
                    ROOT / "test/standalone-native-handlers-u8-smoke.sh",
                    ROOT / "scripts/nelisp-standalone-build.el",
                    ROOT / "lisp/nelisp-native-frame-v2.el",
                    ROOT / "lisp/nelisp-bytecode-handlers-u8.el",
                    ROOT / "lisp/nelisp-bytecode-compiler-input.el",
                    ROOT / "lisp/nelisp-bytecode-native-rooted-cfg.el",
                    ROOT / "lisp/nelisp-bytecode-native-rooted-cfg-plan.el",
                    ROOT / "lisp/nelisp-bytecode-native-rooted-cfg-shared-emit.el",
                    ROOT / "lisp/nelisp-bytecode-native-rooted-cfg-contract.el",
                    ROOT / "lisp/nelisp-native-funcall-v2.el",
                    ROOT / "lisp/nelisp-native-cache.el"]}
    backends = [selected] if selected else env.get("U8B_BACKEND_FILTER", "in-house gccjit").split()
    if not backends or len(set(backends)) != len(backends) or any(backend not in ("in-house", "gccjit", "template") for backend in backends):
        raise SystemExit("U8B_BACKEND_FILTER must contain distinct known backends")
    base_env = env.copy()

    def run_backend(backend, path):
        env = base_env.copy()
        receipts = []
        if backend not in backends:
            return receipts
        path = Path(path).resolve(strict=True)
        reader = directory / ("reader-" + backend)
        shutil.copyfile(path, reader)
        startup_source = Path(str(path) + '.native-startup.el')
        if startup_source.is_file():
            shutil.copyfile(startup_source, Path(str(reader) + '.native-startup.el'))
        os.chmod(reader, 0o500)
        cold_source = Path(str(path) + ".cold")
        cold = Path(str(reader) + ".cold")
        if cold_source.is_file():
            shutil.copyfile(cold_source, cold)
        cache_base = Path(env.get("U8B_NATIVE_CACHE_BASE", str(directory))).resolve()
        cache = cache_base / ("cache-" + backend)
        cache.mkdir(mode=0o700, exist_ok=True)
        os.chmod(cache, 0o700)
        for index, group in enumerate(groups):
            env.update(U8B_FIXTURE=str(fixture), U8B_PHASE=phase, U8B_BACKEND=backend,
                       U8B_CASES=" ".join(group), NELISP_NATIVE_CACHE=str(cache),
                       NELISP_ROOTED_CFG_STAGE_LOG=str(directory / (backend + "-" + str(index) + ".stages")))
            for operation in (("compile", "run") if phase == "native" else ("run",)):
                prefix = backend + "-" + str(index) + "-" + operation
                env["U8B_OPERATION"] = operation
                expected = None
                if phase in ("native", "vm"):
                    oracle = subprocess.run([env.get("EMACS", "emacs"), "-Q", "--batch",
                                             "-l", str(fixture), "--eval",
                                             '(u8b-oracle (mapcar (quote intern) '
                                             '(split-string (getenv "U8B_CASES"))))'],
                                            env=env, capture_output=True, text=True, timeout=30, check=True)
                    if oracle.stderr:
                        raise RuntimeError("GNU oracle stderr: " + oracle.stderr)
                    expected = oracle.stdout
                    (directory / (prefix + ".expected")).write_text(expected)
                command = ["timeout", "-k", "5", "290", str(reader)]
                if cold.is_file():
                    command += ["--cold-load-from", str(cold)]
                for folder in ("lisp", "src", "scripts", "packages/nl-ffi/src", "packages/nl-prelude/src"):
                    command += ["-L", folder]
                command += ["--load", "test/standalone-native-handlers-u8-driver.el"]
                start = time.monotonic()
                with (directory / (prefix + ".out")).open("w") as out, (directory / (prefix + ".err")).open("w") as err:
                    result = subprocess.run(command, env=env, stdout=out, stderr=err, cwd=ROOT)
                elapsed = time.monotonic() - start
                output = (directory / (prefix + ".out")).read_text()
                errors = (directory / (prefix + ".err")).read_text()
                receipt_phase = "compile" if operation == "compile" else phase
                receipt = dict(backend=backend, phase=receipt_phase, fixtures=group, rc=result.returncode,
                               seconds=elapsed, passed=verdict(receipt_phase, result.returncode, elapsed,
                                                              output, errors, expected),
                               binary_sha256=sha(reader), cold_sha256=sha(cold) if cold.is_file() else None,
                               startup_sha256=sha(startup_source) if startup_source.is_file() else None,
                               fixture_sha256=sha(fixture), source_sha256=source_hashes)
                receipts.append(receipt)
                (directory / (prefix + ".json")).write_text(json.dumps(receipt, indent=2))
                print(output + errors[-3000:], end="", flush=True)
                print(f"U8B-RECEIPT backend={backend} phase={receipt_phase} group={index} "
                      f"rc={result.returncode} passed={receipt['passed']} seconds={elapsed:.3f}", flush=True)
        return receipts

    jobs = int(base_env.get("U8B_BACKEND_JOBS", "1"))
    if jobs not in (1, 2):
        raise SystemExit("U8B_BACKEND_JOBS must be 1 or 2")
    pairs = [(selected, binaries[0])] if selected else list(zip(("in-house", "gccjit"), binaries))
    # Each backend owns its environment, process, reader and private cache.
    # Parallelism changes elapsed wall time, never a reader's deadline/verdict.
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        for rows in pool.map(lambda pair: run_backend(*pair), pairs):
            receipts.extend(rows)
    (directory / "receipts.json").write_text(json.dumps(receipts, indent=2))
    print("U8B-EVIDENCE=" + str(directory))
    if all(row["passed"] for row in receipts):
        return 0
    # A timeout, crash or unexpected error is a failure, not pending work.
    return 2 if phase == "native" and all(row["rc"] == 2 for row in receipts) else 1


if __name__ == "__main__":
    raise SystemExit(main())
