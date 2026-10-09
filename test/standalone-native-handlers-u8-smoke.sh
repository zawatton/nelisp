#!/usr/bin/env bash
# Native admission and VM prerequisite evidence have independent verdicts.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
case "${1:-}" in
  --backend)
    case "${2:-}" in in-house|gccjit|template) ;; *) echo 'Unknown backend' >&2; exit 2;; esac
    exec python3 test/support/native-handlers-u8-run.py native --backend "$2" "${3:-target/nelisp-static}"
    ;;
  --host)
    export U8_ERT_SELECTOR="${2:-}"
    exec timeout -k 5 290 "${EMACS:-emacs}" -Q --batch -L lisp -L src -L test \
      -l nelisp-native-handlers-u8-test \
      --eval '(ert-run-tests-batch-and-exit (let ((selector (getenv "U8_ERT_SELECTOR"))) (if (equal selector "") t selector)))'
    ;;
  --both)
    exec python3 test/support/native-handlers-u8-run.py native \
      "${2:-target/nelisp-static}" "${3:-target/nelisp-dyn}"
    ;;
  --vm)
    exec python3 test/support/native-handlers-u8-run.py vm \
      "${2:-target/nelisp-static}" "${3:-target/nelisp-dyn}"
    ;;
  --bench)
    exec timeout -k 5 290 "${EMACS:-emacs}" -Q --batch -L lisp -L src \
      -l test/support/native-handlers-u8-benchmark.el
    ;;
  *) printf '%s\n' 'Usage: standalone-native-handlers-u8-smoke.sh --host [ERT regexp] | --both [static dynamic] | --vm [static dynamic] | --bench' >&2; exit 2 ;;
esac
