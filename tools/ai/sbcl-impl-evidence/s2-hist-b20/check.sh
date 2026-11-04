#!/bin/sh
# S2.8: on build20, with application preparation (frozen build-on20/src inputs) and a READY txn,
# the heap dump is accepted with no trial dump, a trial dump at start, and a trial dump after
# preparation; each returns the bytes written, equal to the file size.  HIST_DIR overrides the
# dump directory (an unwritable one is the negative control).
cd "$(dirname "$0")" || exit 1
I=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on20/nelisp-dynamic
SHA=$(cat image.sha256)
test "$(sha256sum $I | cut -d' ' -f1)" = "$SHA" || exit 1
W=${HIST_DIR:-$(mktemp -d)}
for h in h-none h-start h-install; do
  out=$( (ulimit -v 8388608; ulimit -c 0; env BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256=$SHA HIST_DIR=$W timeout 300 $I --load $h.el < /dev/null 2>&1) )
  echo "$out" | python3 -I -c "import re,sys; m=re.search(r'^PROBE final RET (\d+) SIZE (\d+)$', sys.stdin.read(), re.M); sys.exit(0 if m and m.group(1)==m.group(2) and int(m.group(1))>1000000 else 1)" || { echo "refused or failed: $h"; [ -z "$HIST_DIR" ] && rm -rf "$W"; exit 1; }
done
[ -z "$HIST_DIR" ] && rm -rf "$W"
echo PASS
