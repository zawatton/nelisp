import re,collections
rep=[l.rstrip('\n').split('\t') for l in open('target/bcadopt/work/report-host1.tsv') if not l.startswith('#')][1:]
args=collections.defaultdict(list)
for l in open('target/bcadopt/work/args.tsv'):
    n,a,k,_b=l.rstrip("\n").split("\t"); args[n].append(a)
cons={}
for l in open('target/bcadopt/work/consts.tsv'):
    n,c=l.rstrip('\n').split('\t',1); cons.setdefault(n,c)
cnt=collections.Counter(r[2] for r in rep)
deny_name=re.compile(r'^(eval|load|require|provide|write|delete|kill|rename|copy-file|make-directory|call-process|start-process|make-process|process|shell|set|fset|defalias|put|unintern|makunbound|fmakunbound|add-|remove-|run-|exit|message|print|princ|prin1|terpri|sleep|sit-for|accept-|garbage|nelisp-buffer|nelisp-marker|nelisp-overlay|nelisp--buffer|erase|insert|narrow|widen|goto|read|with-|save-|cl-defstruct|defvar|defconst|define|backtrace|debug|signal|error|user-|y-or|yes-or|completing|read-|kbd|key|keymap|define-key|use-|local-set|global-|setq|make-local|make-variable|kill-local|buffer-local|default-|bury|switch|set-|select|pop-to|display|redisplay|recursive|abort|top-level|exit-|suspend|raise|lower|x-|frame|window|face|font|mouse|track|timer|run-with|cancel|advice|nadvice|advice-|trace|profiler|eieio|cl-generic|nelisp-cl-generic|autoload|symbol-file|locate|file|directory|expand-file|abbreviate|substitute-in|find-|insert-file|temp|make-temp|create|open|close|seq-|json|url|network|open-network|dbus|gnutls|sqlite|mapatoms|obarray|intern-soft|nelisp--env|nelisp--native|nelisp--fx|nelisp-eln|nelisp--eln|nelisp--bytecode|nelisp-bytecode|nelisp--unicode|nelisp-unicode)', re.I)
deny_const=set('write-region delete-file rename-file make-directory delete-directory copy-file call-process make-process start-process kill-emacs set-file-modes set fset defalias eval load kill-buffer erase-buffer delete-region insert-file-contents set-buffer-multibyte puthash clrhash remhash set-default setplist put makunbound fmakunbound unintern intern nelisp--env-capture-roots'.split())
out=[]
skipped=collections.Counter()
for r in rep:
    if r[4]!='no-parity-fixture': continue
    n=r[2]
    if cnt[n]>1: skipped['dup']+=1; continue
    if len(args[n])!=1: skipped['argdup']+=1; continue
    if deny_name.match(n) and not re.match(r'^(seq-|file-name|expand-file|abbreviate-file|substitute-in|file-name-)',n): skipped['name']+=1; continue
    c=cons.get(n,'')
    syms=set(re.findall(r'[^\s()]+',c))
    if syms & deny_const: skipped['const']+=1; continue
    out.append((n,args[n][0],r[5]))
open('target/bcadopt/work/cands.tsv','w').write(''.join('%s\t%s\t%s\n'%x for x in out))
print(len(out),dict(skipped))
