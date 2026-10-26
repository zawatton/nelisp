import os,collections,sys
root=os.getcwd()
tup=collections.OrderedDict()
for l in open('target/bcadopt/work/tuples.tsv'):
    n,k,form,res=l.rstrip('\n').split('\t')
    tup.setdefault(n,{'kind':k,'rows':[]})['rows'].append((form,res))
os.makedirs('target/bcadopt/probe/run',exist_ok=True)
hdr0='(setq print-escape-newlines t print-length nil print-level nil)\n(defun nl-bc--p (i f) (princ (format "%d\\t%S\\n" i (condition-case e (eval f) (error (list :driver-err (car e)))))))\n'
bodies={}
for l in open('target/bcadopt/work/args.tsv'):
    n,a,k,b=l.rstrip('\n').split('\t'); bodies.setdefault(n,(a,b))
names=[]
defchk='(defun nl-bc--same (name args body) (let ((f (symbol-function name))) (and (eq (type-of f) (quote interpreted-function)) (equal (aref f 0) args) (let ((fb (aref f 1))) (or (equal fb body) (and (stringp (car body)) (cdr body) (equal fb (cdr body))))))))\n'
for n,d in tup.items():
    cell=open('target/bcadopt/probe/cells/%s.cell.el'%n).read() if os.path.exists('target/bcadopt/probe/cells/%s.cell.el'%n) else None
    if cell is None: continue
    a,b=bodies[n]
    hdr=hdr0+defchk+'(princ (format "DEF\\t%S\\n" (nl-bc--same (quote '+n+') (quote '+a+') (quote '+b+'))))\n'
    body=''.join('(nl-bc--p %d (quote %s))\n'%(i,f) for i,(f,r) in enumerate(d['rows']))
    open('target/bcadopt/probe/run/%s.B.el'%n,'w').write(hdr+body+'(princ "DONE\\n")\n')
    open('target/bcadopt/probe/run/%s.C.el'%n,'w').write(hdr+cell+body+'(princ "DONE\\n")\n')
    names.append(n)
open('target/bcadopt/probe/run/names.txt','w').write('\n'.join(names)+'\n')
print(len(names))
