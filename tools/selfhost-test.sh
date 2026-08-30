#!/usr/bin/env bash
# Stage 3 self-host test: the standalone NeLisp interpreter (target/nelisp,
# a pure-elisp AOT-AOT binary, ZERO Rust)
# loads its OWN compiler toolchain as source and compiles a program
# (incl. a recursive `fact') to a native x86_64 ELF — with NO emacs in the
# loop — then we exec that ELF and assert its exit code.
#
# Pipeline proven here:
#   source sexp --(standalone interpreter running nelisp-aot-compiler)-->
#   native ELF --(kernel exec)--> exit code
#
# Depends on:
#   * scripts/nelisp-stdlib-prelude.el   (cl-some etc.)
#   * lisp/nelisp-aot-compiler.el    (--native-emit-enabled gate = nil)
#   * lisp/nelisp-asm-x86_64.el          (variadic logior/logand/logxor in the
#                                         standalone interpreter — see
#                                         scripts/nelisp-standalone-build.el)
#   * lisp/nelisp-elf-write.el, lisp/nelisp-static-linker.el
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"
EMACS="${EMACS:-emacs}"

# A gate whose only output is its own PASS lines is indistinguishable from one
# that ran nothing.  CASES counts the checks that finished; the trap reports it
# however the script exits, so a failure says how far it got.  Findings comes
# from the exit status: this script stops at the first failure.
CASES=0
REPORT_COUNT=1

selfhost_report_count() {
  gate_rc=$?
  if [ "$REPORT_COUNT" -eq 1 ]; then
    if [ "$gate_rc" -eq 0 ]; then
      printf 'GATE-COUNT checked=%s findings=0\n' "$CASES"
    else
      printf 'GATE-COUNT checked=%s findings=1\n' "$CASES"
    fi
  fi
}
trap selfhost_report_count EXIT
OUT_DIR="$here/target/linux-smoke"
EMIT_ONLY=0
SELECTED_SMOKES=()
SMOKE_NAMES=(fact)

usage() {
  echo "usage: $0 [--emacs EMACS] [--smoke all|fact] [--emit-only] [--list]" >&2
}

smoke_exists() {
  local want="$1" item
  for item in "${SMOKE_NAMES[@]}"; do
    if [ "$item" = "$want" ]; then
      return 0
    fi
  done
  return 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs)
      if [ "$#" -lt 2 ]; then usage; exit 2; fi
      EMACS="$2"
      shift 2
      ;;
    --smoke)
      if [ "$#" -lt 2 ]; then usage; exit 2; fi
      if [ "$2" = "all" ]; then
        SELECTED_SMOKES=()
        shift 2
        continue
      fi
      if ! smoke_exists "$2"; then
        echo "[selfhost] FAIL: unknown smoke '$2'" >&2
        echo "available smokes: all ${SMOKE_NAMES[*]}" >&2
        exit 2
      fi
      SELECTED_SMOKES+=("$2")
      shift 2
      ;;
    --emit-only)
      EMIT_ONLY=1
      shift
      ;;
    --list)
      echo "available smokes:"
      echo "  all"
      for name in "${SMOKE_NAMES[@]}"; do
        echo "  $name"
      done
      exit 0
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage
      exit 2
      ;;
  esac
done

mkdir -p "$OUT_DIR"

echo "--- Linux x86_64 ELF self-host smoke ---"
uname -a
"$EMACS" --version | head -1
echo "output: $OUT_DIR"

selected_smoke_p() {
  local name="$1" item
  if [ "${#SELECTED_SMOKES[@]}" -eq 0 ]; then
    return 0
  fi
  for item in "${SELECTED_SMOKES[@]}"; do
    if [ "$item" = "$name" ]; then
      return 0
    fi
  done
  return 1
}

RB="target/nelisp"
reader_needs_build() {
  if [ ! -x "$RB" ]; then
    return 0
  fi
  if [ scripts/nelisp-standalone-build.el -nt "$RB" ]; then
    return 0
  fi
  case "$(uname -s)" in
    Linux)
      file "$RB" 2>/dev/null | grep -q 'ELF 64-bit LSB executable, x86-64' || return 0
      ;;
  esac
  return 1
}

# Needs a host that can run the configured target; the build script's own
# predicate decides, not a uname table here.  Three outcomes, so that "could not
# ask" cannot be mistaken for "reasoned skip".
set +e
"$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
set -e
case "$host_rc" in
  0) ;;
  3)
    REPORT_COUNT=0
    printf 'GATE-SKIP target %s cannot run on host %s/%s\n' \
      "${NELISP_STANDALONE_TARGET:-linux-x86_64}" "$(uname -s)" "$(uname -m)"
    echo "[selfhost] SKIP: target cannot run on this host"
    exit 0
    ;;
  *)
    echo "[selfhost] FAIL: cannot ask host runnability (rc=$host_rc)" >&2
    exit 1
    ;;
esac

if reader_needs_build; then
  echo "[selfhost] building reader binary..."
  "$EMACS" --batch -Q -L lisp -L src -L scripts \
        --eval '(setq load-prefer-newer t)' \
        -l nelisp-standalone-build -f nelisp-standalone-build-reader >/dev/null 2>&1
fi

write_driver_prelude() {
  local driver="$1"
  # Dependency order matters now that `require' signals on a missing file
  # instead of silently succeeding.  `nelisp-aot-compiler.el' requires the two
  # assemblers, the layout and the ELF writer, so their `provide' has to have
  # run before its requires do -- which, in one concatenated file, means they
  # come first.
  cat scripts/nelisp-stdlib-prelude.el \
      lisp/nelisp-asm-arm64.el \
      lisp/nelisp-asm-x86_64.el \
      lisp/nelisp-sexp-layout.el \
      lisp/nelisp-elf-write.el \
      lisp/nelisp-aot-compiler.el \
      lisp/nelisp-static-linker.el > "$driver"

  # Positional wrapper: standalone NeLisp does not yet bind cl-defun &key
  # defaults on positional calls, so the self-host entry mirrors the body.
  cat >> "$driver" <<'WRAP'
(defun nelisp-selfhost-compile (sexp file-path)
  "Positional x86_64 / _start self-host entry (mirrors compile-sexp body)."
  (let* ((nelisp-aot-compiler--label-counter 0)
         (nelisp-aot-compiler--arch 'x86_64)
         (ir (nelisp-aot-compiler--parse sexp nil))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (str-offsets (car collected))
         (str-rodata-bytes (cdr collected))
         (table-collected (nelisp-aot-compiler--collect-tables ir))
         (table-offsets (car table-collected))
         (table-bytes (cdr table-collected))
         (str-rodata-len (length str-rodata-bytes))
         (rodata-bytes (concat str-rodata-bytes table-bytes))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (pass1-table-vaddrs (mapcar (lambda (e) (cons (car e) 0)) table-offsets))
         (pass1 (nelisp-aot-compiler--pass ir defuns str-offsets 0 pass1-table-vaddrs))
         (text-size (nelisp-asm-x86_64-buffer-pos pass1))
         (rodata-vaddr (+ nelisp-aot-compiler--text-vaddr text-size))
         (table-vaddrs (mapcar (lambda (e)
                          (let* ((nm (car e)) (info (cdr e)) (off (plist-get info :offset)))
                            (cons nm (+ rodata-vaddr str-rodata-len off)))) table-offsets))
         (pass2 (nelisp-aot-compiler--pass ir defuns str-offsets rodata-vaddr table-vaddrs))
         (text-bytes (nelisp-asm-x86_64-resolve-fixups pass2))
         (have-rodata (> (length rodata-bytes) 0))
         (symbols (cons (list :name "_start" :value 0 :size (length text-bytes)
                              :section 'text :bind 'global :type 'func)
                        (if have-rodata
                            (list (list :name "rodata_blob" :value 0 :size (length rodata-bytes)
                                        :section 'rodata :bind 'local :type 'object))
                          nil)))
         (sections (list :text text-bytes
                         :rodata (if have-rodata rodata-bytes (unibyte-string))
                         :symbols symbols :entry-sym "_start" :machine 'x86_64)))
    (nelisp-elf-write-binary file-path sections)
    file-path))
WRAP
}

run_fact_smoke() {
  local name="fact"
  local driver="$OUT_DIR/nelisp-linux-selfhost-$name.driver.el"
  local outbin="$OUT_DIR/nelisp-linux-selfhost-$name"
  local log="$OUT_DIR/nelisp-linux-selfhost-$name.compile.log"
  if ! selected_smoke_p "$name"; then
    return
  fi

  rm -f "$driver" "$outbin" "$log"
  write_driver_prelude "$driver"
  printf "(progn (nelisp-selfhost-compile '(seq (defun fact (n) (if (< n 1) 1 (* n (fact (- n 1))))) (exit (fact 5))) \"%s\") 88)\n" "$outbin" >> "$driver"

  echo "[selfhost] compiling (fact 5) via standalone interpreter (zero emacs)..."
  set +e
  "$RB" "$driver" >"$log" 2>&1
  compile_rc=$?
  set -e
  # The driver's last form is 88 and a file run by the binary exits with the
  # value of its last form, so 88 is what a completed load looks like.  The
  # check read 0, which no successful run can produce; selfhost-mt-test.sh has
  # always checked for 88 against a driver written the same way.
  if [ "$compile_rc" != "88" ]; then
    echo "[selfhost] FAIL: $name driver returned $compile_rc (expected 88 = file load completed)"
    tail -8 "$log" | sed 's/^/    /'
    exit 1
  fi
  CASES=$((CASES + 1))
  if [ ! -s "$outbin" ]; then
    echo "[selfhost] FAIL: $name produced no ELF"; exit 1
  fi
  CASES=$((CASES + 1))
  if [ "$EMIT_ONLY" = 1 ]; then
    echo "[selfhost] PASS: $name -> built"
    return
  fi
  chmod +x "$outbin"
  set +e
  "$outbin"
  run_rc=$?
  set -e
  if [ "$run_rc" = "120" ]; then
    CASES=$((CASES + 1))
    echo "[selfhost] PASS: $name -> exit 120 (5! = 120)"
  else
    echo "[selfhost] FAIL: $name produced binary exit $run_rc (expected 120)"; exit 1
  fi
}

run_fact_smoke
echo "[selfhost] all PASS - Linux x86_64 self-host smoke OK"
