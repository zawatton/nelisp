#!/usr/bin/env python3
import os,sys,subprocess,time,json,re,csv,shutil
H=os.path.expanduser("~")
RUN=H+"/.cache/tmp/nelisp-sbcl-impl-builds/run"
D=RUN+"/appready20"; X=RUN+"/expB20"
B=H+"/.cache/tmp/nelisp-sbcl-impl-builds/build-on20/nelisp-dynamic"
SHA="1333e71cc60f016428b1e00f2852956211763726a91a43cdd0a2ec9dc3f99ae4"
OUT=D+"/raw"; os.makedirs(OUT,exist_ok=True); os.makedirs(D+"/scratch",exist_ok=True)
ROWS=[("integer-sum",16000000),("function-call2",4194304),("vector-write",4194304),("vector-read",4194304),
("vector-allocate64",1048576),("string-slice16",1048576),("string-concat16",1048576),("hash-get1024",65536)]
PAIRS=7
def others():
    p=subprocess.run(["ps","-eo","pid,psr,pcpu,args"],capture_output=True,text=True).stdout.splitlines()
    n=[];
    for l in p[1:]:
        if ("nelisp-dynamic" in l or "emacs-31.1" in l) and "cohort.py" not in l and "ps -eo" not in l: n.append(l.split()[1])
    return n
def benv(row,n):
    return {"BENCH_ROW":row,"BENCH_N":str(n),"BENCH_HIGH":str(n),"BENCH_WARMUPS":"2"}
def run(arm,row,n):
    base={"HOME":H,"PATH":"/usr/bin:/bin","LANG":"C.UTF-8","TMPDIR":D+"/scratch"}
    e=benv(row,n)
    if arm=="normal":
        env={**base,**e,"NELISP_LANE":"on","NELISP_LANE_IMAGE":B,"NELISP_LANE_IMAGE_SHA256":SHA,"NELISP_SCRATCH_ROOT":D+"/scratch"}
        cmd=["taskset","-c","1","python3","-I",X+"/lane_runner_phased.py","--load","placeholder"]
    elif arm=="direct":
        env={**base,**e,"BENCH_ENGINE":"nelisp","NELISP_PRODUCER_IMAGE_SHA256":SHA}
        cmd=["taskset","-c","1","/bin/bash","-c",'ulimit -v 8388608; ulimit -c 0; exec "$@"',"x",B,"--load",X+"/benchmark-driver-native-on.el"]
    else:
        env={**base,**e,"BENCH_ENGINE":"nelisp","NELISP_PRODUCER_IMAGE_SHA256":SHA}
        cmd=["taskset","-c","1","/bin/bash","-c",'ulimit -v 8388608; ulimit -c 0; exec "$@"',"x",B,"--cold-load-from",D+"/app.cold","--load",D+"/appready-kernel.el"]
    oth=others(); la=open("/proc/loadavg").read().split()[0]
    t=time.perf_counter()
    p=subprocess.run(cmd,env=env,stdin=subprocess.DEVNULL,capture_output=True,timeout=300)
    w=time.perf_counter()-t
    out=p.stdout.decode(errors="replace")
    m=re.search(r"^BENCH-CHECK .*$",out,re.M)
    return {"row":row,"arm":arm,"rc":p.returncode,"wall_s":round(w,4),"check":m.group(0) if m else "",
            "reused":int("APPREADY-REUSE" in (out+p.stderr.decode(errors="replace"))),"other_procs":len(oth),"other_cpus":" ".join(oth),"loadavg1":la}
if __name__=="__main__":
    w=csv.DictWriter(open(OUT+"/runs.csv","w"),fieldnames=["row","pair","arm","order","rc","wall_s","check","reused","other_procs","other_cpus","loadavg1"]); w.writeheader()
    for row,n in ROWS:
        for k in range(1,PAIRS+1):
            order=["normal","restored"] if k%2 else ["restored","normal"]
            order.append("direct")
            for i,arm in enumerate(order):
                r=run(arm,row,n); r["pair"]=k; r["order"]=i
                w.writerow(r)
        sys.stdout.write(row+" done\n"); sys.stdout.flush()
