#!/bin/sh
# S2.9 (mode alloc): allocation after restoring the app-ready image is within 2x of a normal process.
# S2.8 (mode hist): the heap dump is accepted with no trial dump, a trial dump at start, and one after install.
cd "$(dirname "$0")" || exit 1
B=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on14/nelisp-dynamic
A=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/run/appready14
test "$(sha256sum $B | cut -d' ' -f1)" = e0a95142bfcde56fff63b47b4da5471a820b91fcdf0a6928e79d8221e4ce3ea9 || exit 1
run() { (ulimit -v 8388608; ulimit -c 0; env BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256=e0a95142bfcde56fff63b47b4da5471a820b91fcdf0a6928e79d8221e4ce3ea9 timeout 300 "$B" "$@" < /dev/null 2>&1); }
case "${1:-alloc}" in
alloc)
  test "$(sha256sum $A/app.cold | cut -d' ' -f1)" = "$(cat app.cold.sha256)" || exit 1
  n=$(run --load loop.el | sed -n 's/^LOOP300k //p'); r=$(run --cold-load-from $A/app.cold --load loop.el | sed -n 's/^LOOP300k //p')
  echo "normal=$n restored=$r"
  python3 -I -c "import sys; n,r=float('$n'),float('$r'); sys.exit(0 if 0<n and r<=2*n else 1)" || exit 1 ;;
hist)
  for h in h-none h-start h-install; do
    # The dump returns the bytes written (-1 on a write failure, without signaling);
    # require a real image whose size equals the returned count.
    run --load $h.el | python3 -I -c "import re,sys; m=re.search(r'^PROBE final RET (\\d+) SIZE (\\d+)$', sys.stdin.read(), re.M); sys.exit(0 if m and m.group(1)==m.group(2) and int(m.group(1))>1000000 else 1)" || { echo "refused or failed: $h"; exit 1; }
  done ;;
esac
echo PASS
