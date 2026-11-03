#!/bin/bash
# Doc 240 S0 probe: run the generated driver's preparation once, dump the heap,
# then time fresh processes that cold-load the dump and run only the benchmark main.
# Usage: probe-app-image.sh IMAGE GENERATED_DRIVER OUT_DIR
set -u
I=$1; DRV=$2; OUT=$3; mkdir -p "$OUT" || exit 1
SHA=$(sha256sum "$I" | cut -d' ' -f1)
python3 -I - "$DRV" "$OUT" <<'P' || exit 1
import sys
s=open(sys.argv[1]).read().split("\n"); out=sys.argv[2]
top=[i for i,l in enumerate(s) if l.startswith("(")]
assert len(top)==20 and s[top[19]].startswith("(kill-emacs 0)"), "unexpected driver shape"
cut=top[18]
open(out+"/prep.el","w").write("\n".join(s[:cut])+'\n(princ (format "DUMPED %d\\n" (nelisp--arena-dump-image-stream (getenv "APP_IMG"))))\n(kill-emacs 0)\n')
open(out+"/main.el","w").write("\n".join(s[cut:]))
P
ulimit -v 8388608
env -i HOME="$HOME" PATH="$PATH" BENCH_ROW=hash-get1024 BENCH_N=16 BENCH_HIGH=16 BENCH_WARMUPS=0 BENCH_ENGINE=nelisp \
  NELISP_PRODUCER_IMAGE_SHA256="$SHA" TMPDIR="$OUT" XDG_CACHE_HOME="$OUT/cache" APP_IMG="$OUT/app.img" \
  "$I" --load "$OUT/prep.el" | grep -q '^DUMPED [0-9]' || { echo "dump failed"; exit 1; }
python3 -I - "$I" "$DRV" "$OUT" "$SHA" <<'P'
import subprocess,time,statistics,os,re,sys
I,DRV,OUT,SHA=sys.argv[1:]
base=dict(HOME=os.environ["HOME"],PATH=os.environ["PATH"],BENCH_ROW="hash-get1024",BENCH_ENGINE="nelisp",NELISP_PRODUCER_IMAGE_SHA256=SHA,TMPDIR=OUT,XDG_CACHE_HOME=OUT+"/cache")
cfg={"full-driver":[I,"--load",DRV],"cold-app":[I,"--cold-load-from",OUT+"/app.img","--load",OUT+"/main.el"]}
res={}
for n in (16,65536):
  env=dict(base,BENCH_N=str(n),BENCH_HIGH=str(n),BENCH_WARMUPS="0")
  for k,c in cfg.items():
    w=[];e=[]
    for i in range(7):
      t=time.perf_counter();r=subprocess.run(c,env=env,capture_output=True,text=True);w.append(time.perf_counter()-t)
      m=re.search(r"result=(\S+) elapsed=(\S+)",r.stdout)
      if r.returncode or not m: sys.exit(f"{k} N={n} failed rc={r.returncode}")
      e.append(float(m.group(2)))
    res[(n,k)]=m.group(1)
    print(f"N={n:6} {k:12} wall {statistics.median(w)*1000:7.1f} ms  kernel {statistics.median(e)*1e9/n:7.1f} ns/step  result={m.group(1)}")
  if res[(n,"full-driver")]!=res[(n,"cold-app")]: sys.exit(f"result mismatch at N={n}")
P
