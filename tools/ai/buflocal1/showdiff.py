#!/usr/bin/env python3
"""showdiff.py CORPUS GNU.txt NL.txt : print forms whose result differs."""
import sys, re
corpus, g, n = sys.argv[1:4]
forms = [l.rstrip("\n") for l in open(corpus) if l.strip() and not l.startswith(";")]
def parse(p):
    d = {}; cur = None
    for l in open(p):
        m = re.match(r"^(\d+) \((ok|err) ", l)
        if m: cur = int(m.group(1)); d[cur] = l.rstrip("\n")
        elif cur is not None: d[cur] += "\\n" + l.rstrip("\n")
    return d
G, N = parse(g), parse(n)
bad = 0
for i in sorted(G):
    if G[i] != N.get(i):
        bad += 1
        print("#%d %s" % (i, forms[i-1][:200] if i-1 < len(forms) else "?"))
        print("   gnu: " + G[i].split(" ",1)[1][:160]); print("   nl : " + N.get(i, "MISSING").split(" ",1)[-1][:160])
print("diverging: %d of %d" % (bad, len(G)))
