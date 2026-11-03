#!/usr/bin/env python3
"""run1.py LABEL BINARY ARGS... ; env from X_ENV (json). No bwrap: plain subprocess,
cleared env, bash ulimit -v 8388608 -c 0, timeout 300s, whole-process wall via perf_counter."""
import sys,os,subprocess,time,json,re
label,binary,*args=sys.argv[1:]
env={"HOME":os.environ["HOME"],"PATH":"/usr/bin:/bin","LANG":"C.UTF-8","TMPDIR":os.environ.get("X_TMP","/tmp")}
env.update(json.loads(os.environ.get("X_ENV","{}")))
cmd=["/bin/bash","-c",'ulimit -v 8388608; ulimit -c 0; exec "$@"',"x",binary]+args
t=time.perf_counter()
try:
    p=subprocess.run(cmd,env=env,stdin=subprocess.DEVNULL,capture_output=True,timeout=300)
    rc=p.returncode;out=p.stdout.decode(errors="replace");err=p.stderr.decode(errors="replace")
except subprocess.TimeoutExpired as e:
    rc=124;out=(e.stdout or b"").decode(errors="replace");err="TIMEOUT"
w=time.perf_counter()-t
rec={"label":label,"rc":rc,"wall_s":round(w,3),"route":None,"result":None,"kernel_s":None,"stdout_tail":out[-400:],"stderr_tail":err[-400:]}
m=re.search(r"route=(\S+)",out); rec["route"]=m and m.group(1)
m=re.search(r"result=(\d+:\d+) elapsed=([0-9.]+)",out)
if m: rec["result"]=m.group(1);rec["kernel_s"]=float(m.group(2))
os.makedirs("out",exist_ok=True)
open(f"out/{label}.json","w").write(json.dumps(rec,indent=1))
print(json.dumps({k:rec[k] for k in("label","rc","wall_s","route","result","kernel_s")}))
