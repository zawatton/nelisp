import json,glob,os,sys
bad=[]
for d in glob.glob('target/reader1/timing/rp-read-*/report.json')+glob.glob('target/reader1/timing/bigload/report.json'):
    e=json.load(open(d))['engines']; r=e['cand']['inner_ns_per_step']['median']/e['base']['inner_ns_per_step']['median']
    if r>1.05: bad.append((os.path.basename(os.path.dirname(d)),round(r,3)))
print('rows slower than 1.05x:',bad); sys.exit(1 if bad else 0)
