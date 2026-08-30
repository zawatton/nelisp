#!/usr/bin/env bash
# Stage 4 PRODUCTION PARALLEL BUILD test: the standalone NeLisp interpreter
# (pure-elisp AOT-AOT binary, ZERO Rust, ZERO emacs) compiles MULTIPLE
# units CONCURRENTLY — N worker tasks each running the FULL AOT compiler
# at the same time.
#
# Each worker is spawned with `fork-spawn' = fork(2) (syscall 57), giving it a
# COW-isolated copy of the whole address space (its own arena / mirror / frames
# / env), so N concurrent compiles share NO mutable eval state and cannot race.
# Workers join through a MAP_SHARED counter (atomic-fetch-add visible across the
# COW boundary) that the parent `thread-join's on.  Each worker compiles a
# distinct program -> its own native ELF; we then exec all of them and assert
# every exit code, so a lost/torn/raced compile would fail the assertion.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"

# Needs a host that can run the configured target; the build script's own
# predicate decides, not a uname table here.  Same convention as
# tools/selfhost-test.sh.  2026-08-23 Windows inventory: this Linux-only
# fork(2)/mmap/ELF path exited 127 while building the reader instead of
# reporting a reasoned skip.
set +e
emacs --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
set -e
case "$host_rc" in
  0) ;;
  3)
    printf 'GATE-SKIP target %s cannot run on host %s/%s\n' \
      "${NELISP_STANDALONE_TARGET:-linux-x86_64}" "$(uname -s)" "$(uname -m)"
    echo "[parallel] SKIP: target cannot run on this host"
    exit 0
    ;;
  *)
    echo "[parallel] FAIL: cannot ask host runnability (rc=$host_rc)" >&2
    exit 1
    ;;
esac

RB="target/nelisp"
if [ ! -x "$RB" ]; then
  echo "[parallel] building reader binary..."
  emacs --batch -Q -L lisp -L src -L scripts \
        --eval '(setq load-prefer-newer t)' \
        -l nelisp-standalone-build -f nelisp-standalone-build-reader >/dev/null 2>&1
fi

driver="$(mktemp /tmp/nelisp-par-XXXXXX.el)"
o1="$(mktemp /tmp/nelisp-par-o1-XXXXXX)"; o2="$(mktemp /tmp/nelisp-par-o2-XXXXXX)"
o3="$(mktemp /tmp/nelisp-par-o3-XXXXXX)"; o4="$(mktemp /tmp/nelisp-par-o4-XXXXXX)"
trap 'rm -f "$driver" "$o1" "$o2" "$o3" "$o4"' EXIT

# Dependency order matters now that `require' signals on a missing file instead
# of silently succeeding: `nelisp-aot-compiler.el' requires the two assemblers,
# the layout and the ELF writer, so in one concatenated file their `provide' has
# to run first.  Same list and same reason as selfhost-test.sh.
cat scripts/nelisp-stdlib-prelude.el \
    lisp/nelisp-asm-arm64.el \
    lisp/nelisp-asm-x86_64.el \
    lisp/nelisp-sexp-layout.el \
    lisp/nelisp-elf-write.el \
    lisp/nelisp-aot-compiler.el \
    lisp/nelisp-static-linker.el > "$driver"

# positional self-host compile entry (compile-sexp is a cl-defun with &key,
# not callable positionally on standalone NeLisp).
cat >> "$driver" <<'WRAP'
(defun nelisp-selfhost-compile (sexp file-path)
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

# MAP_SHARED join counter at 0x08000000 (flags 49 = MAP_SHARED|ANON|FIXED) so the
# forked workers' atomic-fetch-add is visible to the parent.  4 workers, each a
# fork(2) running the full compiler on a distinct program, in parallel.
cat >> "$driver" <<EOF
(syscall-direct 9 134217728 65536 3 49 -1 0)
(ptr-write-u64 134217728 0 0)
(fork-spawn (quote (progn (nelisp-selfhost-compile (quote (exit 11)) "$o1") (atomic-fetch-add 134217728 1))))
(fork-spawn (quote (progn (nelisp-selfhost-compile (quote (exit 22)) "$o2") (atomic-fetch-add 134217728 1))))
(fork-spawn (quote (progn (nelisp-selfhost-compile (quote (exit 33)) "$o3") (atomic-fetch-add 134217728 1))))
(fork-spawn (quote (progn (nelisp-selfhost-compile (quote (exit 44)) "$o4") (atomic-fetch-add 134217728 1))))
(thread-join 134217728 4)
0
EOF

echo "[parallel] 4 workers compiling concurrently (fork(2)+full compiler, zero emacs)..."
rm -f "$o1" "$o2" "$o3" "$o4"
timeout 120 "$RB" "$driver" >/dev/null 2>&1 || true

fail=0
for pair in "$o1:11" "$o2:22" "$o3:33" "$o4:44"; do
  f="${pair%%:*}"; want="${pair##*:}"
  if [ ! -s "$f" ]; then echo "[parallel] FAIL: worker output $f missing"; fail=1; continue; fi
  chmod +x "$f"
  set +e; "$f"; got=$?; set -e
  if [ "$got" != "$want" ]; then echo "[parallel] FAIL: $f -> $got (expected $want)"; fail=1; fi
done

# Machine-readable tail: four workers, four checks.  Without it a clean run and
# a run that started no workers print the same thing to `nelisp-ai.sh gate'.
if [ "$fail" = 0 ]; then
  echo "GATE-COUNT checked=4 findings=0"
  echo "[parallel] PASS: 4 units compiled CONCURRENTLY by standalone NeLisp -> 11,22,33,44"
  exit 0
else
  echo "GATE-COUNT checked=4 findings=1"
  exit 1
fi
