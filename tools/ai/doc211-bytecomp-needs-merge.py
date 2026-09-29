#!/usr/bin/env python3
"""Combine standalone/host call-cell traces into the committed S3.1 TSV."""
import csv, pathlib, re, sys
root = pathlib.Path(__file__).resolve().parents[2]
def read(path):
    with open(path, newline="") as f: return {r["name"]: int(r["count"]) for r in csv.DictReader(f, delimiter="\t")}
standalone, host = map(read, sys.argv[1:3])
rows=[]
for name in sorted(set(standalone)|set(host)):
    a,b=standalone.get(name,0),host.get(name,0)
    rows.append((name,"function","runtime",f"standalone={a};host={b}","S6.10 byte-compile-form wrapper; cell calls"))
sources=[root/"vendor/emacs-lisp/emacs-lisp"/(n+".el") for n in ("bytecomp","byte-opt","cconv","macroexp","byte-run")]
sources += [root/n for n in ("test/nelisp-eln-s6-measure.sh","test/nelisp-eln-s6-measure-driver.el","tools/nelisp-eln-s610-evidence.el")]
text="\n".join(p.read_text(errors="replace") for p in sources)
# API state variables are declarations/references using the variable-bearing
# buffer/file/marker/syntax namespace. Function rows remain runtime-only.
variables=set(re.findall(r"\(defvar\s+((?:buffer|mark|syntax|default-directory|case-fold|inhibit-read-only)[A-Za-z0-9-]*)",text))
variables |= set(re.findall(r"\b(?:setq|setq-default)\s+((?:buffer|mark|syntax|default-directory|case-fold|inhibit-read-only)[A-Za-z0-9-]*)",text))
variables |= set(re.findall(r"\((?:let|let\*)\s+\(\(\s*((?:buffer|mark|syntax|default-directory|case-fold|inhibit-read-only)[A-Za-z0-9-]*)",text))
for name in sorted(variables):
    rows.append((name,"variable","static","static","source variable reference"))
out=root/"tools/ai/doc211-bytecomp-need.tsv"
with out.open("w") as f:
    f.write("# Function counts are function-cell wrapper counts on standalone and GNU 31.1; callers are not attributed. Native/internal calls bypassing Lisp function cells are not observed. Variable rows are static source references, not runtime reads; static scan recognizes buffer/mark/syntax/default-directory/case-fold/inhibit-read-only names in defvar, setq, and let bindings and may miss indirect or differently named variable access.\n")
    f.write("# Runtime path: bytecomp.el load + S6.10 wrapper invoked on both byte-compile-form corpus forms.\n")
    f.write("name\tkind\tevidence\tcount\tnotes\n")
    w=csv.writer(f,delimiter="\t",lineterminator="\n"); w.writerows(rows)
print(f"wrote {out}: {len(rows)} rows")
