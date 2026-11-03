#!/usr/bin/env python3
"""Hardware-witness the nested primary entry and loop BODY on a chosen ON image.

Adapted from experimental-speedup-20261031/verify-primary-route.py: the image,
its SHA-256, and the entry address come from the command line, because the
recorded script hard-codes the r3 image.  BODY is entry+0x4a0, valid only when
the function's normalized code matches the recorded image (checked separately).
"""
import hashlib, json, os, shutil, signal, subprocess, sys
from pathlib import Path
import lane_runner_phased as lr

BASE = Path(__file__).resolve().parent
WITNESS = lr.root_dir() / "out/qualification/fixtures/route-witness.py"
image, image_sha, entry = Path(sys.argv[1]), sys.argv[2], int(sys.argv[3], 16)
out_json = Path(sys.argv[4])

def sha(p): return hashlib.sha256(Path(p).read_bytes()).hexdigest()

env = {k: os.environ[k] for k in ("HOME", "USER", "LOGNAME") if k in os.environ}
env.update(PATH="/usr/bin:/bin", LANG="C.UTF-8", LC_ALL="C.UTF-8",
           BENCH_ROW="nested-function-call2", BENCH_N="3", BENCH_HIGH="3", BENCH_WARMUPS="2",
           NELISP_SCRATCH_ROOT=str(BASE / "scratch"), NELISP_LANE_IMAGE=str(image),
           NELISP_LANE_IMAGE_SHA256=image_sha)
argv = lr.command_for("on", "unused", env)
scratch = Path(env["NELISP_OWNED_SCRATCH"])
driver = lr.driver_path("on", "nested-function-call2", env)
paths = [image, Path(str(image) + ".cold"), driver, BASE / "runtime-bootstrap-on.el", WITNESS]
pins = {str(p): sha(p) for p in paths}
assert pins[str(image)] == image_sha, "image hash mismatch"
config = dict(binary=str(image), output=str(scratch / "witness.json"), pins=pins,
              points=[dict(label="primary_entry", address=hex(entry), min=3, max=3),
                      dict(label="primary_loop_BODY", address=hex(entry + 0x4a0), min=9, max=9)])
(scratch / "config.json").write_text(json.dumps(config, indent=2) + "\n")
env["V8_WITNESS_CONFIG"] = str(scratch / "config.json")
idx = argv.index(str(image))
argv[idx:idx] = ["gdb", "--nx", "--batch", "-ex", "source " + str(WITNESS), "--args"]
proc = subprocess.Popen(["taskset", "-c", "1", *argv], env=env, stdin=subprocess.DEVNULL,
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
timed_out = False
try:
    out, err = proc.communicate(timeout=60)
except subprocess.TimeoutExpired:
    timed_out = True
finally:
    if proc.poll() is None:
        os.killpg(proc.pid, signal.SIGTERM)
        try: proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, signal.SIGKILL); proc.wait()
if timed_out:
    out, err = proc.communicate()
report = json.loads((scratch / "witness.json").read_text()) if (scratch / "witness.json").exists() else {}
same = all(sha(p) == d for p, d in pins.items())
good = (not timed_out and proc.returncode == 0 and report.get("status") == "PASS" and not err and same
        and out.count(b"BENCH-CHECK n=3 result=3:120 elapsed=") == 1)
rec = dict(status="NATIVE_BODY_OBSERVED" if good else "HOLD", rc=proc.returncode, timed_out=timed_out,
           witness=report, pins=pins, pins_unchanged=same, argv=argv,
           stdout=out.decode(errors="replace"), stderr_bytes=len(err))
shutil.rmtree(scratch)
out_json.write_text(json.dumps(rec, indent=2) + "\n")
print(rec["status"], report.get("counts"))
sys.exit(0 if good else 1)
