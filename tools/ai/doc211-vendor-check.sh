#!/usr/bin/env bash
set -euo pipefail
mode=${1:?usage: doc211-vendor-check.sh exist|versions|differing|api-boundary}
root=$(cd "$(dirname "$0")/../.." && pwd)
case "$mode" in
  exist|versions)
    total=0
    while IFS=$'\t' read -r path release source digest; do
      [[ $path == emacs-lisp/* || $path == emacs-lisp-api/* ]] || continue
      ((total+=1))
      test "$release" = 'GNU Emacs 31.1'
      test -s "$root/vendor/$path"
      actual=$(sha256sum "$root/vendor/$path" | cut -d' ' -f1)
      test "$actual" = "${digest#sha256=}"
    done < "$root/vendor/ORIGIN"
    test "$total" -ge 29
    (cd "$root" && sha256sum -c vendor/SHA256SUMS >/dev/null)
    echo "PASS vendor $mode files=$total"
    ;;
  differing)
    for f in emacs-lisp/ring.el custom.el comint.el progmodes/compile.el isearch.el emacs-lisp/cl-macs.el; do
      if test -s "$root/vendor/emacs-lisp/$f"; then base=emacs-lisp; else base=emacs-lisp-api; fi
      test -s "$root/vendor/$base/$f"
      grep -q "vendor/$base/$f" "$root/vendor/SHA256SUMS"
      grep -q "^$base/$f[[:space:]]" "$root/vendor/ORIGIN"
    done
    echo 'PASS differing files=6 policy=GNU-31.1; API additions isolated from core search path'
    ;;
  api-boundary)
    # API vendor tree must not enter the core load-path or source/build dependencies.
    if rg -n 'vendor/emacs-lisp-api' "$root/scripts/nelisp-standalone-build.el" "$root/scripts/nelisp-stdlib-prelude.el" "$root/src" "$root/lisp"; then
      echo 'FAIL core build/prelude references API vendor tree' >&2; exit 1
    fi
    if rg -n 'emacs-lisp-api' "$root/scripts/nelisp-standalone-build.el"; then
      echo 'FAIL API vendor tree appears in core standalone build' >&2; exit 1
    fi
    test -d "$root/vendor/emacs-lisp-api"
    echo 'PASS API vendor tree excluded from core load-path and source dependencies'
    ;;
  *) echo "unknown mode: $mode" >&2; exit 2;;
esac
