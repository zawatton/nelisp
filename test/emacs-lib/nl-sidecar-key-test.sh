#!/bin/sh
# Early cutoff of the baked images (mk/emacs-lib.mk + tools/dev/nl-sidecar-key.sh).
# A fake NELISP_BIN counts bakes; no NeLisp build, no real bake.  Each step has
# an expected bake count; a final control with a key script that always
# rewrites its key file must FAIL the touch step (proves the test can see it).
# usage: test/emacs-lib/nl-sidecar-key-test.sh   (run from the repo root)
ulimit -v 8388608
root=$(cd "$(dirname "$0")/../.." && pwd -P)
run() { # $1 = key script to install
  t=$(mktemp -d "${TMPDIR:-$HOME/.cache/tmp}/nlsk.XXXXXX") || exit 2
  mkdir -p "$t/tools/dev" "$t/scripts" "$t/build" "$t/target" "$t/src" "$t/lisp" "$t/mk"
  cp "$1" "$t/tools/dev/nl-sidecar-key.sh"; chmod +x "$t/tools/dev/nl-sidecar-key.sh"
  cp "$root/mk/emacs-lib.mk" "$t/mk/"
  for f in nemacs-runtime-image-preload nemacs-runtime-process-preload nemacs-runtime-frame-tab-preload nemacs-runtime-dump-reinit; do echo "($f)" > "$t/scripts/$f.el"; done
  : > "$t/scripts/build-nelisp-bootstrap.el"; : > "$t/scripts/standalone-source-normalize.el"; sleep 1; echo b > "$t/build/fixed-bundle.el"; echo '(a)' > "$t/src/x.el"
  printf '#!/bin/sh\necho x >> "%s/bakes"\necho img > "$2"\n' "$t" > "$t/target/nelisp"; chmod +x "$t/target/nelisp"
  : > "$t/bakes"; echo "$t"
}
mk() { (cd "$1" && env "${2:-X=1}" timeout 120 make -s -f mk/emacs-lib.mk NELISP_ROOT=. NELISP_BIN=target/nelisp NEMACS_BOOTSTRAP_BUNDLE=build/fixed-bundle.el NELISP_EMACS_API_SRC_FILES= NELISP_BOOTSTRAP_VENDOR_INPUTS= build/nemacs-runtime.nlri >/dev/null 2>"$1/err.log"); }
count() { wc -l < "$1/bakes" | tr -d ' '; }
fail=0
check() { # dir expected label
  got=$(count "$1"); if [ "$got" = "$2" ]; then echo "ok   $3 ($got)"; else echo "FAIL $3: bakes=$got want $2"; fail=1; sed -n 1,5p "$1/err.log"; fi; }
steps() {
  d=$1; mk "$d"; check "$d" 1 "first bake"
  mk "$d"; check "$d" 1 "no change: skipped"
  sleep 1; touch "$d/src/x.el" "$d/scripts/nemacs-runtime-image-preload.el"; mk "$d"; check "$d" 1 "touch only: skipped"
  echo '(b)' > "$d/src/x.el"; mk "$d"; check "$d" 2 "input content edit: baked"
  cp "$d/target/nelisp" "$d/target/n2"; sleep 1; mv "$d/target/n2" "$d/target/nelisp"; mk "$d"; check "$d" 2 "identical re-linked binary: skipped"
  echo '# changed' >> "$d/target/nelisp"; mk "$d"; check "$d" 3 "binary changed: baked"
  mk "$d" NELISP_STANDALONE_NATIVE_COMPILER_COLD=1; check "$d" 4 "flag flipped: baked"
  rm -f "$d/build/nemacs-runtime.nlri.key"; sleep 1; mk "$d" NELISP_STANDALONE_NATIVE_COMPILER_COLD=1; check "$d" 5 "missing key: baked"
  rm -f "$d/build/nemacs-runtime.nlri"; mk "$d" NELISP_STANDALONE_NATIVE_COMPILER_COLD=1; check "$d" 6 "missing image: baked"
}
d=$(run "$root/tools/dev/nl-sidecar-key.sh"); steps "$d"; rm -rf "$d"
# Control: a key script that always rewrites the key file must break the touch step.
bad=$(mktemp "${TMPDIR:-$HOME/.cache/tmp}/nlsk-bad.XXXXXX")
sed 's/^if \[ ! -f "\$keyfile" \] || \[ "\$(cat "\$keyfile")" != "\$key" \]; then/if true; then/' "$root/tools/dev/nl-sidecar-key.sh" > "$bad"
cmp -s "$bad" "$root/tools/dev/nl-sidecar-key.sh" && { echo "FAIL control mutation did not apply"; exit 1; }
d=$(run "$bad"); mk "$d"; sleep 1; mk "$d"; got=$(count "$d")
if [ "$got" -gt 1 ]; then echo "ok   control (always-rewrite key script re-bakes on a no-change run: $got)"; else echo "FAIL control not detected"; fail=1; fi
rm -rf "$d" "$bad"
exit $fail
