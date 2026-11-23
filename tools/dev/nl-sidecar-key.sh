#!/bin/sh
# nl-sidecar-key.sh KEYFILE BIN [-e ENVNAME]... [-x TEXT]... -- PATH...
# Early-cutoff skip key of a bake output (design2 C).  KEYFILE holds
#   sha256(BIN bytes, every PATH, values of the ENVNAMEs, every literal TEXT).
# TEXT carries inputs that are not files (the checkout path); list BIN.cold and the
# makefile holding the bake recipe as PATHs so they are keyed too.
# PATH is a file (its bytes) or a directory (every .el below it except .git and
# target, by relative name and content).  Missing paths count as absent.  KEYFILE
# is rewritten ONLY when its content changes, so a make rule that depends on it
# re-bakes exactly when a bake input changed (a touch or an identical rebuilt
# binary does not).  Prints the key.  Never skips on doubt: any error exits 1.
set -u
keyfile=$1; bin=$2; shift 2
envs=""; xs=""
while [ $# -gt 0 ]; do
  case $1 in
    -e) envs="$envs $2"; shift 2 ;;
    -x) xs="$xs
$2"; shift 2 ;;
    --) shift; break ;;
    *) echo "nl-sidecar-key: bad arg $1" >&2; exit 2 ;;
  esac
done
[ -r "$bin" ] || { echo "nl-sidecar-key: binary $bin unreadable" >&2; exit 1; }
key=$(
  {
    printf 'bin %s\n' "$(sha256sum < "$bin" | cut -d' ' -f1)"
    [ -z "$xs" ] || printf 'x %s\n' "$xs"
    for n in $envs; do eval "v=\${$n-}"; printf 'env %s=%s\n' "$n" "$v"; done
    for p in "$@"; do
      if [ -d "$p" ]; then
        (cd "$p" && find . -name .git -prune -o -name target -prune -o -type f -name '*.el' -print \
           | LC_ALL=C sort | while IFS= read -r f; do printf 'f %s %s\n' "$f" "$(sha1sum < "$f" | cut -d' ' -f1)"; done)
      elif [ -f "$p" ]; then
        printf 'file %s %s\n' "$(basename "$p")" "$(sha1sum < "$p" | cut -d' ' -f1)"
      else
        printf 'absent %s\n' "$(basename "$p")"
      fi
    done
  } | sha256sum | cut -d' ' -f1
) || exit 1
[ -n "$key" ] || exit 1
if [ ! -f "$keyfile" ] || [ "$(cat "$keyfile")" != "$key" ]; then
  mkdir -p "$(dirname "$keyfile")" && printf '%s\n' "$key" > "$keyfile.$$" && mv "$keyfile.$$" "$keyfile" || exit 1
fi
printf '%s\n' "$key"
