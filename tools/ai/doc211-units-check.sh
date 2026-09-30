#!/usr/bin/env bash
set -euo pipefail
stage=${1:?usage: doc211-units-check.sh STAGE [MANIFEST]}
manifest=${2:-$(dirname "$0")/doc211-units.tsv}
python3 - "$stage" "$manifest" <<'PY'
import csv,sys,shlex,os
stage,path=sys.argv[1:]
rows=list(csv.DictReader(open(path),delimiter='\t'))
required=['unit_id','stage','files','function_count','owner_check_command','status']
if not rows or list(rows[0])!=required: raise SystemExit('FAIL manifest columns/empty')
rows=[r for r in rows if r['stage']==stage]
if not rows: raise SystemExit(f'FAIL no units for {stage}')
seen={}
for r in rows:
 files=[x for x in r['files'].split(',') if x]
 if not files or int(r['function_count'])<0 or int(r['function_count'])>12: raise SystemExit(f"FAIL size/empty files: {r['unit_id']}")
 if len(files)>1: raise SystemExit(f"FAIL unit spans files: {r['unit_id']}")
 if not r['owner_check_command'].strip(): raise SystemExit(f"FAIL missing command: {r['unit_id']}")
 argv=shlex.split(r['owner_check_command'])
 command=argv[0]
 if command in ('bash','sh','python3'):
  targets=[x for x in argv[1:] if not x.startswith('-')]
  if not targets or not os.path.exists(targets[0]): raise SystemExit(f"FAIL command target missing: {r['unit_id']}")
 elif '/' in command and not os.path.exists(command): raise SystemExit(f"FAIL command path missing: {r['unit_id']}")
 if r['status'] not in ('done','open','blocked'): raise SystemExit(f"FAIL status: {r['unit_id']}")
 for f in files:
  if f in seen and r['status']!='done' and seen[f]!='done': raise SystemExit(f'FAIL overlapping open units: {f}')
  seen[f]=r['status']
print(f'PASS {stage} units={len(rows)}')
PY
