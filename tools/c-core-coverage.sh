#!/usr/bin/env bash
# Measure GNU Emacs C-primitive binding coverage from the generated census.
if [ -z "${BASH_VERSION:-}" ]; then exec bash "$0" "$@"; fi
set -u
here=$(cd "$(dirname "$0")/.." && pwd) || exit 1
cd "$here" || exit 1
BIN=${NELISP_BIN:-$here/target/nelisp}
HOST=${EMACS:-emacs}
CENSUS=build/c-core-census.tsv
MISSING=build/c-core-missing.tsv
IDENTITY=build/c-core-coverage.identity
AREAS="x-gui display process buffer chars files other"
EXPECTED=1460

classify() {
  awk -F'\t' '{n=$1; c="other";
    if (n ~ /^(x-|x_)/) c="x-gui";
    else if (n ~ /(window|frame|display|terminal|tty|redisplay|face|font|image|fringe|scroll|mouse|tab-bar|tool-bar|menu|pixel|posn|cursor|overlay|margin)/) c="display";
    else if (n ~ /(process|network|serial|dbus|notify|gnutls|sqlite|treesit|json|libxml|module|thread|mutex|condition-var)/) c="process";
    else if (n ~ /(buffer|marker|text-property|char-table|syntax|category|case|keymap|key|input|command|minibuffer|completion|abbrev|undo|region|narrow|point|insert|delete|search|match|regexp)/) c="buffer";
    else if (n ~ /(coding|charset|char|string|decode|encode|compose|unibyte|multibyte|ccl)/) c="chars";
    else if (n ~ /(file|directory|dir|path|expand|locate|load|dump|pdumper|native-comp|comp-)/) c="files";
    print n "\t" c}'
}

validate_data() {
  python3 - "$CENSUS" "$MISSING" "$EXPECTED" <<'PY'
import re, sys
census, missing, expected = sys.argv[1], sys.argv[2], int(sys.argv[3])
rows=[]
with open(census, encoding='utf-8') as f:
    for line in f:
        if line.startswith('#') or not line.strip(): continue
        p=line.rstrip('\n').split('\t')
        if len(p) != 5 or not re.fullmatch(r'[^\s\x00-\x1f]+', p[0]) or p[1] not in {'native','interpreted','absent'}:
            raise SystemExit('invalid census row')
        rows.append(p)
names=[r[0] for r in rows]
if len(rows) != expected: raise SystemExit(f'census count {len(rows)} != {expected}')
if len(set(names)) != expected: raise SystemExit('census names are duplicate')
with open(missing, encoding='utf-8') as f: miss=[x.rstrip('\n').split('\t') for x in f if x.strip()]
if any(len(x)!=2 or x[1] not in {'x-gui','display','process','buffer','chars','files','other'} for x in miss):
    raise SystemExit('invalid missing row')
if len({x[0] for x in miss}) != len(miss): raise SystemExit('duplicate missing name')
by={r[0] for r in rows}
if any(x[0] not in by for x in miss): raise SystemExit('missing name absent from census')
rules=[('x-gui',r'^(x-|x_)'),('display',r'(window|frame|display|terminal|tty|redisplay|face|font|image|fringe|scroll|mouse|tab-bar|tool-bar|menu|pixel|posn|cursor|overlay|margin)'),('process',r'(process|network|serial|dbus|notify|gnutls|sqlite|treesit|json|libxml|module|thread|mutex|condition-var)'),('buffer',r'(buffer|marker|text-property|char-table|syntax|category|case|keymap|key|input|command|minibuffer|completion|abbrev|undo|region|narrow|point|insert|delete|search|match|regexp)'),('chars',r'(coding|charset|char|string|decode|encode|compose|unibyte|multibyte|ccl)'),('files',r'(file|directory|dir|path|expand|locate|load|dump|pdumper|native-comp|comp-)')]
def area(name): return next((a for a,p in rules if re.search(p,name)), 'other')
if any(area(n)!=a for n,a in miss): raise SystemExit('missing area does not match census name')
PY
}

fingerprint() {
  python3 - "$here" "$BIN" "$CENSUS" "$MISSING" <<'PY'
import glob, hashlib, json, os, sys
root, binary, census, missing=sys.argv[1:]
paths=[binary, binary+'.cold', os.path.join(root,'build/nemacs-bootstrap.el'),
       os.path.join(root,census), os.path.join(root,missing),
       os.path.join(root,'tools/c-core-coverage.sh'),
       os.path.join(root,'tools/nelisp-c-primitive-census.sh')]
paths += sorted(glob.glob(os.path.join(root,'packages/*/src/*.el')))
if not glob.glob(os.path.join(root,'packages/*/src/*.el')):
    raise SystemExit('no package API source files found')
h=hashlib.sha256()
for p in paths:
    if not os.path.isfile(p): raise SystemExit('identity input missing: '+p)
    rel=os.path.relpath(p,root) if os.path.commonpath([root,os.path.abspath(p)])==root else os.path.abspath(p)
    h.update(rel.encode()+b'\0'+hashlib.sha256(open(p,'rb').read()).digest()+b'\n')
print(json.dumps({'sha256':h.hexdigest(),'inputs':len(paths)},sort_keys=True))
PY
}

fresh() {
  [ -f "$IDENTITY" ] || { echo "c-core-coverage: no identity metadata; run regen" >&2; return 1; }
  validate_data || { echo "c-core-coverage: census/missing data invalid; run regen" >&2; return 1; }
  local current stored
  current=$(fingerprint) || return 1
  stored=$(cat "$IDENTITY") || return 1
  [ "$current" = "$stored" ] || { echo "c-core-coverage: inputs changed; run regen" >&2; return 1; }
}

summary() {
  local a
  for a in $AREAS; do printf '%-8s missing %d\n' "$a" "$(awk -F'\t' -v a="$a" '$2==a' "$MISSING" | wc -l)"; done
}

regen() {
  mkdir -p build || return 1
  rm -f "$IDENTITY" "$IDENTITY.tmp"
  local version rc probe names out line done_count
  version=$("$HOST" --batch --eval '(princ emacs-version)' 2>build/c-core-host.err) || { echo "c-core-coverage: host version failed" >&2; return 1; }
  [[ "$version" =~ ^31\.1([.-]|$) && ! -s build/c-core-host.err ]] || { echo "c-core-coverage: host $HOST is not clean GNU Emacs 31.1" >&2; return 1; }
  [ -f build/nemacs-bootstrap.el ] || { echo "c-core-coverage: bootstrap bundle missing" >&2; return 1; }
  (cd "$here" && EMACS="$HOST" timeout 600 bash tools/nelisp-c-primitive-census.sh --bin "$BIN" --out "$here/$CENSUS") >build/c-core-census.log 2>&1 || { echo "c-core-coverage: census failed" >&2; return 1; }
  [ -f "$CENSUS" ] || { echo "c-core-coverage: census output missing" >&2; return 1; }
  awk -F'\t' '!/^#/ && NF==5 {print $1}' "$CENSUS" > build/c-core-probe-names.txt
  probe=build/c-core-probe.el
  { echo '(load (expand-file-name "build/nemacs-bootstrap.el") nil t)'; echo '(dolist (s (quote ('; cat build/c-core-probe-names.txt; echo '))) (unless (fboundp s) (princ (format "C-CORE-MISSING %s\n" s))))'; echo '(progn (princ "C-CORE-PROBE-DONE\n") t)'; } >"$probe"
  timeout 300 "$BIN" --load "$probe" >build/c-core-probe.out 2>build/c-core-probe.err; rc=$?
  [ "$rc" -eq 0 ] && [ ! -s build/c-core-probe.err ] || { echo "c-core-coverage: bundle probe failed (rc=$rc)" >&2; return 1; }
  [ -s build/c-core-probe.out ] && [ "$(tail -c 1 build/c-core-probe.out | wc -l)" -eq 1 ] || { echo "c-core-coverage: incomplete probe output" >&2; return 1; }
  : > "$MISSING"; done_count=0; trailing_result=0
  while IFS= read -r line; do
    if [ "$done_count" -eq 1 ]; then
      if [ "$line" = t ] && [ "$trailing_result" -eq 0 ]; then trailing_result=1; continue; fi
      echo "c-core-coverage: unexpected output after probe marker" >&2; return 1
    fi
    case "$line" in
      'C-CORE-MISSING '*) [ -n "${line#C-CORE-MISSING }" ] || { echo "c-core-coverage: empty missing name" >&2; return 1; }; printf '%s\n' "${line#C-CORE-MISSING }" | classify >>"$MISSING" ;;
      C-CORE-PROBE-DONE) done_count=$((done_count+1));;
      *) echo "c-core-coverage: unexpected probe output" >&2; return 1;;
    esac
  done < build/c-core-probe.out
  [ "$done_count" -eq 1 ] || { echo "c-core-coverage: probe marker count $done_count" >&2; return 1; }
  sort -o "$MISSING" "$MISSING"
  validate_data || { echo "c-core-coverage: generated data invalid" >&2; return 1; }
  local identity_tmp
  identity_tmp=$(fingerprint) || return 1
  printf '%s\n' "$identity_tmp" > "$IDENTITY.tmp" && mv "$IDENTITY.tmp" "$IDENTITY" || return 1
  echo "c-core-coverage: $((EXPECTED-$(wc -l <"$MISSING")))/$EXPECTED C primitives bound after bootstrap"
  summary
}

case "${1:-}" in
  regen) regen ;;
  summary) fresh && summary ;;
  check)
    area=${2:-}
    case " $AREAS " in *" $area "*) ;; *) echo "usage: $0 check {$AREAS}" >&2; exit 2 ;; esac
    fresh || exit 1
    n=$(awk -F'\t' -v a="$area" '$2==a' "$MISSING" | wc -l)
    echo "c-core-coverage: $area missing $n"
    [ "$n" -eq 0 ] ;;
  *) echo "usage: $0 {regen|summary|check AREA}" >&2; exit 2 ;;
esac
