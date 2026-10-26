import resource, subprocess, sys, time
# usage: rss.py BIN EXPR [timeout]  -- run EXPR under the 8 GiB address-space cap; print output tail, rc, peak RSS (RUSAGE_CHILDREN), seconds
bin_, expr = sys.argv[1], sys.argv[2]
to = int(sys.argv[3]) if len(sys.argv) > 3 else 300
def lim(): resource.setrlimit(resource.RLIMIT_AS, (8388608*1024, 8388608*1024))
t = time.time()
p = subprocess.run([bin_, "-Q", "--batch", "--eval", expr, "--eval", "(ignore)"], capture_output=True, preexec_fn=lim, timeout=to)
dt = time.time() - t
ru = resource.getrusage(resource.RUSAGE_CHILDREN)
out = (p.stdout + p.stderr).decode("utf-8", "replace").strip()
print(f"out={out[-200:]!r} rc={p.returncode} rss={ru.ru_maxrss//1024}MB t={dt:.2f}s")
