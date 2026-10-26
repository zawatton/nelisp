#!/usr/bin/env python3
"""Run every generated fixture case on a built binary and compare with the row.
usage: verify-fixtures.py BIN OUTJSON   (cwd = repo root; run under ulimit/heavy-run)"""
import subprocess,sys,os,json,tempfile
bin_,out=sys.argv[1],sys.argv[2]
rows=[l.rstrip('\n').split('\t') for l in open('test/nelisp-prelude-bytecode-parity-fixtures.tsv') if not l.startswith('#')][1:]
rows=[r for r in rows if r[4].startswith(('gnu-31.1-reference','runtime-source-compiled'))]
cwd=tempfile.mkdtemp(dir=os.path.abspath('target/bcadopt/cwd'))
res={}
for r in rows:
    f=os.path.join(cwd,'c.el')
    open(f,'w').write('(setq print-escape-newlines t print-length nil print-level nil)\n(princ (format "%%S" (condition-case e (eval (quote %s)) (error (list :driver-err (car e))))))\n(terpri)\n'%r[1])
    try:
        p=subprocess.run([bin_,'--load',f,'--'],capture_output=True,timeout=15,cwd=cwd,stdin=subprocess.DEVNULL)
        o=p.stdout.decode(errors='replace')
        got=o.split('\n"')[0] if False else o
        # --load echoes the value after the princ output on a new line
        got=o.split('\n')[0]
        res[r[0]]='ok' if got==r[2] else 'mismatch:%s'%got[:80]
    except subprocess.TimeoutExpired: res[r[0]]='timeout'
json.dump(res,open(out,'w'))
import collections
print(collections.Counter(v.split(':')[0] for v in res.values()))
