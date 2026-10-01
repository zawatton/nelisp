#!/usr/bin/env bash
set -euo pipefail
mode=${1:?usage: doc211-cutover.sh freeze|guards}
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/target/progress"
mkdir -p "$out"
case "$mode" in
  freeze)
    python3 - "$root" "$out/doc211-cutover.json" <<'PY'
import json,subprocess,sys,datetime
root,out=sys.argv[1:]
def git(*args): return subprocess.check_output(['git','-C',root,*args],text=True).strip()
data={'frozen_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),
      'branch':git('branch','--show-current'),'nelisp_head':git('rev-parse','HEAD'),
      'import_commit':'8cd5899aea24701eef5ddcddf1edea0603704f66',
      'nel_lib_imported_head':'d5bfa934','ccore_snapshot':'target/progress/doc211-ccore-freeze.json'}
with open(out,'w') as f: json.dump(data,f,indent=2); f.write('\n')
print(f"PASS freeze nelisp={data['nelisp_head']} nel-lib={data['nel_lib_imported_head']}")
PY
    ;;
  guards)
    python3 - "$root" <<'PY'
import re,sys,csv,subprocess,os
from pathlib import Path
root=Path(sys.argv[1])
makefile=Path(os.environ.get('DOC211_GUARDS_MAKEFILE',root/'nelisp-emacs-lib/Makefile'))
decisions=Path(os.environ.get('DOC211_GUARDS_TSV',root/'tools/ai/doc211-cutover-guards.tsv'))
text=makefile.read_text()

def target_rules(source):
 rules={}; current=None; recipe=[]
 def save():
  if current is not None: rules[current]=(deps,recipe[:])
 for physical in source.splitlines():
  if physical.startswith(('\t',' ')):
   if current is not None and physical.startswith('\t'): recipe.append(physical.strip())
   continue
  line=physical
  # Parse logical declaration lines, keeping recipes associated below.
  if ':' in line and not line.lstrip().startswith('#'):
   # A target-specific assignment (target: VAR = value) is not a rule.
   head=line.split(':',1)[0].strip()
   tail=line.split(':',1)[1].strip()
   if re.match(r'^[A-Za-z_][A-Za-z0-9_]*\s*(?:[:+?!]?=)',tail):
    continue
   save()
   current=head.split()[0] if head else None
   recipe=[]
   deps=tail
   continue
  if line and not line.lstrip().startswith('#'):
   save(); current=None; recipe=[]
 save()
 return rules

# Join Make continuations before recognizing declarations; recipes remain intact.
logical=[]; pending=''
for physical in text.splitlines():
 if pending:
  pending += ' ' + physical.strip()
 else: pending=physical
 if pending.endswith('\\'):
  pending=pending[:-1].rstrip(); continue
 logical.append(pending); pending=''
if pending: logical.append(pending)
rules=target_rules('\n'.join(logical))
if 'nemacs-library-gate' not in rules: raise SystemExit('FAIL nemacs-library-gate target missing')

# The gate's recursive make recipe is part of its structural dependency closure.
gate_deps,gate_recipe=rules['nemacs-library-gate']
recursive=[]
for command in gate_recipe:
 if '$(MAKE)' not in command: continue
 match=re.search(r'\$\(MAKE\).*?(?:^|\s)([A-Za-z0-9_.-]+)\s*$',command)
 if match: recursive.append(match.group(1))
if recursive:
 expected={'compile','nemacs-library-gate-checks'}
 if set(recursive)!=expected or len(recursive)!=len(expected):
  raise SystemExit(f'FAIL unknown gate recursive structure: {recursive}')
 if 'compile' not in rules or 'nemacs-library-gate-checks' not in rules:
  raise SystemExit('FAIL recursive gate target missing: compile/nemacs-library-gate-checks')
 checks_deps,checks_recipe=rules['nemacs-library-gate-checks']
 if any('$(MAKE)' in command for command in checks_recipe):
  raise SystemExit('FAIL unknown nested aggregate under nemacs-library-gate-checks')
 # compile remains one legacy decision; only the aggregate runner's immediate
 # prerequisites are expanded into legacy decisions.
 targets={'compile',*checks_deps.split()}
else:
 targets=set(gate_deps.split())
with decisions.open(newline='') as stream:
 rows=list(csv.DictReader(stream,delimiter='\t'))
by={}
duplicates=[]
for row in rows:
 target=row['old_target']
 if target in by: duplicates.append(target)
 by[target]=row
if duplicates: raise SystemExit(f'FAIL duplicate decisions: {sorted(set(duplicates))}')
missing=sorted(targets-set(by)); extra=sorted(set(by)-targets)
if missing or extra: raise SystemExit(f'FAIL decision coverage missing={missing} extra={extra}')
for r in rows:
 if r['decision'] not in ('drop','rehomed'): raise SystemExit(f"FAIL unknown decision {r['decision']!r}: {r['old_target']}")
 if r['decision']=='drop' and not r['reason'].strip(): raise SystemExit(f"FAIL missing drop reason: {r['old_target']}")
 if r['decision']=='rehomed':
  target=r['nelisp_target']
  if not target or not re.match(r'^[A-Za-z0-9_./-]+$',target): raise SystemExit(f"FAIL invalid rehome target: {r['old_target']}")
  if subprocess.run(['make','-s',target],cwd=root).returncode: raise SystemExit(f"FAIL rehomed {r['old_target']} -> make {target}")
print(f'PASS guards targets={len(rows)} rehomed={sum(r["decision"]=="rehomed" for r in rows)} dropped={sum(r["decision"]=="drop" for r in rows)}')
PY
    ;;
  *) echo "usage: $0 freeze|guards" >&2; exit 2 ;;
esac
