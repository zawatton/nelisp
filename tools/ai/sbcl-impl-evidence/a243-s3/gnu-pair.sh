#!/bin/bash
# Paired wall-time comparison: GNU Emacs 31.1 vs NeLisp build33 and build42.
# Rows: bare start (load a file that does nothing) and the Doc 240 app
# launcher row hash-get1024 (N=16384); GNU runs the cross-language driver.el.
set -u
W=$HOME/.cache/tmp/nelisp-sbcl-impl-f12-20261031
B=$HOME/.cache/tmp/nelisp-sbcl-impl-builds
GNU=${GNU_EMACS:-/usr/local/bin/emacs-31.1}
GDRV=$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el
PRODUCE=$W/tools/nelisp-application-image-produce.sh
LAUNCH=$W/tools/nelisp-application-image-launch.sh
R=${ROUNDS:-15}
OUT=${OUT:-$HOME/.cache/tmp/a243-gnu/gnu-pair.txt}
mkdir -p "$(dirname "$OUT")"
T=$(mktemp -d "$HOME/.cache/tmp/a243-gnu/run.XXXXXX") || exit 1
trap 'rm -rf "$T"' EXIT
echo '(kill-emacs 0)' >"$T/nop.el"
for k in 33 42; do
  mkdir -p "$T/b$k"
  DRV=$B/run/b$k; [ -r "$DRV/benchmark-driver-native-on.el" ] || DRV=$B/run/b33
  "$PRODUCE" "$B/build-on$k/nelisp-dynamic" "$DRV/benchmark-driver-native-on.el" hash-get1024 "$T/b$k/hash-get1024.img" >"$T/produce$k.log" 2>&1 \
    || { echo "FAIL: produce b$k"; tail -3 "$T/produce$k.log"; exit 1; }
done
python3 -I - "$B" "$GNU" "$GDRV" "$LAUNCH" "$T" "$R" "$OUT" <<'EOF'
import sys, os, subprocess, time, statistics, re, resource, hashlib
B, gnu, gdrv, launch, T, R, out = sys.argv[1:]; R = int(R)
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def sha(p): return hashlib.sha256(open(p, 'rb').read()).hexdigest()
def quiet():
    l = float(open('/proc/loadavg').read().split()[0])
    def st():
        f = open('/proc/stat').readline().split()[1:]; v = list(map(int, f)); return sum(v), v[3] + v[4]
    a = st(); time.sleep(2); b = st()
    busy = (b[0] - a[0] - (b[1] - a[1])) / ((b[0] - a[0]) / os.cpu_count())
    return l, busy
nb = {k: f"{B}/build-on{k}/nelisp-dynamic" for k in (33, 42)}
lenv = lambda k: {"NELISP_APP_IMAGE_DIR": f"{T}/b{k}", "NELISP_LANE_IMAGE": nb[k], "NELISP_LANE_IMAGE_SHA256": sha(nb[k]),
                  "NELISP_SCRATCH_ROOT": f"{T}/scratch", "BENCH_ROW": "hash-get1024", "BENCH_N": "16384",
                  "BENCH_HIGH": "16384", "BENCH_WARMUPS": "0"}
genv = {"BENCH_ENGINE": "gnu", "BENCH_ROW": "hash-get1024", "BENCH_N": "16384"}
CMD = {"gnu-bare": ([gnu, "-Q", "--batch", "-l", f"{T}/nop.el"], {}),
       "b33-bare": ([nb[33], "--load", f"{T}/nop.el"], {}),
       "b42-bare": ([nb[42], "--load", f"{T}/nop.el"], {}),
       "gnu-app": ([gnu, "-Q", "--batch", "-l", gdrv], genv),
       "b33-app": ([launch, "--load", "PLACEHOLDER"], lenv(33)),
       "b42-app": ([launch, "--load", "PLACEHOLDER"], lenv(42))}
os.makedirs(f"{T}/scratch", exist_ok=True)
names = list(CMD); rows = {k: [] for k in names}; res = {k: set() for k in names}
q0 = quiet()
for i in range(R):
    for k in (names if i % 2 == 0 else names[::-1]):
        c, e = CMD[k]
        t = time.perf_counter()
        p = subprocess.run(c, env=dict(os.environ, **e), stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=600, preexec_fn=cap)
        w = (time.perf_counter() - t) * 1000
        if p.returncode != 0:
            print(f"FAIL {k} rc={p.returncode}: {(p.stderr or p.stdout)[-300:]}"); sys.exit(1)
        if k.endswith("app"):
            m = re.findall(r"BENCH-CHECK n=(\d+) result=(\S+) elapsed=", p.stdout)
            if len(m) != 1: print(f"FAIL {k}: BENCH-CHECK {m}"); sys.exit(1)
            res[k].add(m[0][1])
        rows[k].append(w)
q1 = quiet()
med = {k: statistics.median(v) for k, v in rows.items()}
L = [f"paired GNU vs NeLisp, {R} alternating rounds, wall ms (parent perf_counter)",
     f"quiet before load1 {q0[0]:.2f} busy {q0[1]:.2f}; after load1 {q1[0]:.2f} busy {q1[1]:.2f}"]
for k in names: L.append(f"{k:>9}: median {med[k]:7.1f} min {min(rows[k]):7.1f} max {max(rows[k]):7.1f}")
for k in (33, 42):
    L.append(f"b{k}/gnu: bare {med[f'b{k}-bare']/med['gnu-bare']:.3f}  app {med[f'b{k}-app']/med['gnu-app']:.3f}")
L.append(f"b42/b33: bare {med['b42-bare']/med['b33-bare']:.3f}  app {med['b42-app']/med['b33-app']:.3f}")
L.append("results " + str({k: sorted(v) for k, v in res.items() if v}))
open(out, 'w').write("\n".join(L) + "\n"); print("\n".join(L))
EOF
