#!/usr/bin/env bash
set -uo pipefail

gate_root=$(cd "$(dirname "$0")/.." && pwd)
runtime_root=${NELISP_RUNTIME_ROOT:-$gate_root}
binary=${1:-"$runtime_root/target/nelisp"}
host=${EMACS:-emacs}
if [[ "$binary" != /* ]]; then
  binary="$runtime_root/$binary"
fi
artifact_dir=${NELISP_ABI_ARTIFACT_DIR:-$(mktemp -d "${TMPDIR:-/tmp}/nelisp-vmjit-abi.XXXXXX")}
mkdir -p "$artifact_dir"
artifact="$artifact_dir/gc-eq-two.neln"
driver="$gate_root/test/standalone-vm-jit-boxed-abi-driver.el"
printf 'artifacts=%s\n' "$artifact_dir"

fail() {
  printf 'standalone-vm-jit-boxed-abi-smoke: FAIL: %s\n' "$*" >&2
  exit 1
}

same() {
  [[ "$1" == "$2" ]]
}

expected_jit='((t nil) 4 1 t (wrong-type-argument listp 1))'
accept_jit_result() {
  same "$1" "$expected_jit"
}

if [[ ! -x "$binary" ]]; then
  fail "standalone binary is not executable: $binary"
fi
binary_sha=$(sha256sum "$binary" | awk '{print $1}')
printf 'binary=%s sha256=%s\n' "$binary" "$binary_sha"

expected_eq_fingerprint=791da0d6580bf2a511b7af86ce18c15ac0ee4bfa92c0dd74008ade9859b4bfdd
expected_cadr_fingerprint=c6c15a50ceb4415464aa59e1039a615620c69efc13ecc001f8485d6fd89ba13f

export NELISP_ABI_ARTIFACT_DIR="$artifact_dir"
if ! "$host" -Q --batch -L "$runtime_root/src" -L "$runtime_root/lisp" \
    -L "$runtime_root/scripts" \
    -l "$driver" --eval '(nelisp-test-build-vm-jit-boxed-abi-fixture)' \
    >"$artifact_dir/aot-build.log" 2>&1; then
  tail -n 8 "$artifact_dir/aot-build.log" >&2
  fail 'Host could not compile the AOT fixture'
fi
if [[ ! -f "$artifact" ]]; then
  fail "AOT artifact missing: $artifact"
fi

host_output=$("$host" -Q --batch -L "$runtime_root/src" -L "$runtime_root/lisp" \
  -L "$runtime_root/scripts" -l "$driver" \
  --eval '(nelisp-test-vm-jit-boxed-abi-host-probe)') || fail 'Host bytecode probe failed'
host_result=${host_output##*$'\n'}
expected_host="(t \"$expected_eq_fingerprint\" \"$expected_cadr_fingerprint\" (t nil (wrong-type-argument listp 1)))"
if ! same "$host_result" "$expected_host"; then
  printf 'host-result=%s\nexpected=%s\n' "$host_result" "$expected_host" >&2
  fail 'Host version, fingerprint, or reference values differ'
fi

artifact_lisp_path=$(NELISP_ABI_ARTIFACT="$artifact" "$host" -Q --batch \
  --eval '(prin1 (getenv "NELISP_ABI_ARTIFACT"))')
cd "$runtime_root"
vm_output=$("$binary" --eval '
(progn
  (require (quote nelisp-bytecode-jit))
  (let* ((eq-fn (make-byte-code 514 (unibyte-string 1 1 61 135) [] 4))
         (cadr-fn (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
         (same-cons (cons 9 nil))
         (distinct-cons (cons 9 nil)))
    (let ((nelisp-bytecode-jit--dispatch-active t))
      (list (funcall eq-fn same-cons same-cons)
            (funcall eq-fn same-cons distinct-cons)
            (condition-case data
                (progn (funcall cadr-fn 1) (quote missed))
              (wrong-type-argument data))))))') || fail 'VM probe failed'
vm_result=${vm_output##*$'\n'}
if ! same "$vm_result" '(t nil (wrong-type-argument listp 1))'; then
  printf 'vm-result=%s\n' "$vm_result" >&2
  fail 'VM boxed identity or invalid-input result differs from Host'
fi

jit_output=$("$binary" --eval '
(progn
  (require (quote nelisp-bytecode-jit))
  (setq nelisp-bytecode-jit-threshold 2)
  (let* ((eq-fn (make-byte-code 514 (unibyte-string 1 1 61 135) [] 4))
         (cadr-fn (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
         (same-cons (cons 9 nil))
         (distinct-cons (cons 9 nil))
         (before nelisp-bytecode-jit--native-call-count)
         (fallback-before (plist-get (nelisp-bytecode-jit-status)
                                     :interpreter-fallbacks))
         (warm-cold (funcall eq-fn (quote foo) (quote foo)))
         (warm-hot (funcall eq-fn (quote foo) (quote foo)))
         (same (funcall eq-fn same-cons same-cons))
         (distinct (funcall eq-fn same-cons distinct-cons))
         (retained (progn (garbage-collect)
                          (funcall eq-fn same-cons same-cons)))
         (after-valid nelisp-bytecode-jit--native-call-count)
         (invalid (condition-case data
                      (progn (funcall cadr-fn 1) (quote missed))
                    (wrong-type-argument data)))
         (fallback-after (plist-get (nelisp-bytecode-jit-status)
                                    :interpreter-fallbacks)))
    (list (list same distinct)
          (- after-valid before)
          (- fallback-after fallback-before)
          retained invalid)))') || fail 'JIT probe failed'
jit_result=${jit_output##*$'\n'}
if ! accept_jit_result "$jit_result"; then
  printf 'jit-result=%s\n' "$jit_output" >&2
  fail 'JIT boxed identity, native delta, GC retention, or fallback differs from Host'
fi

aot_output=$("$binary" --eval "
(progn
  (require (quote nelisp-bytecode-jit))
  (require (quote nelisp-native-load))
  (let* ((left (cons 9 nil))
         (right (cons 9 nil))
         (before nelisp-bytecode-jit--native-call-count)
         (same (nelisp-native-load-exec $artifact_lisp_path
                                        \"gc-eq-two\" (list left left)))
         (distinct (nelisp-native-load-exec $artifact_lisp_path
                                            \"gc-eq-two\" (list left right))))
    (list same distinct
          (- nelisp-bytecode-jit--native-call-count before))))") || fail 'direct AOT loader probe failed'
aot_result=${aot_output##*$'\n'}
if ! same "$aot_result" '(t nil 0)'; then
  printf 'aot-result=%s\n' "$aot_output" >&2
  fail 'Direct AOT GC/eq result or JIT-counter separation failed'
fi

# Mutate the captured native-call delta and feed it through the real JIT predicate.
negative_jit_result=${jit_result/\ 4\ /\ 0\ }
if same "$negative_jit_result" "$jit_result"; then
  fail 'negative control did not mutate the captured JIT result'
fi
if accept_jit_result "$negative_jit_result"; then
  fail 'acceptance predicate accepted the mutated JIT result'
fi
printf 'negative-control=detected (mutated captured JIT result)\n'

binary_sha_after=$(sha256sum "$binary" | awk '{print $1}')
printf 'binary-sha-after=%s\n' "$binary_sha_after"
if ! same "$binary_sha_after" "$binary_sha"; then
  fail "standalone binary changed during probes: before=$binary_sha after=$binary_sha_after"
fi

printf 'standalone-vm-jit-boxed-abi-smoke: PASS (fingerprints %s/%s; VM=JIT=Host; native calls=4; fallback=1; direct AOT calls excluded from JIT count; artifacts=%s)\n' \
  "$expected_eq_fingerprint" "$expected_cadr_fingerprint" "$artifact_dir"
