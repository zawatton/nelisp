#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
binary=${1:-target/nelisp}
host=${EMACS:-emacs}
cd "$root"
if [[ "$binary" != /* ]]; then
  binary="$root/$binary"
fi

expected_hash=c6c15a50ceb4415464aa59e1039a615620c69efc13ecc001f8485d6fd89ba13f

host_output=$("$host" -Q --batch --eval '
(let* ((fn (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
       (leaf (list 22 33))
       (nested (list 11 leaf))
       (fingerprint
        (secure-hash
         (quote sha256)
         (prin1-to-string
          (list (aref fn 0)
                (mapconcat (lambda (b) (number-to-string b))
                           (append (aref fn 1) nil) ",")
                (aref fn 2) (aref fn 3)))))
       (values (list (funcall fn (list 11 22 33))
                     (funcall fn nil)
                     (funcall fn nested)))
       (invalid (condition-case data
                    (progn (funcall fn 1) (quote missed))
                  (wrong-type-argument data))))
  (garbage-collect)
  (prin1 (list (if (string-match-p "\\`31\\.1" emacs-version) t nil)
               fingerprint values (eq (nth 2 values) leaf) invalid)))')
host_result=${host_output##*$'\n'}
if [[ "$host_result" != "(t \"$expected_hash\" (22 nil (22 33)) t (wrong-type-argument listp 1))" ]]; then
  echo "standalone-bytecode-jit-cadr-return-smoke: Host mismatch: $host_result" >&2
  exit 1
fi

standalone_output=$("$binary" --eval '
(progn
  (require (quote nelisp-bytecode-jit))
  (setq nelisp-bytecode-jit-threshold 1)
  (let* ((fn (make-byte-code 257 (unibyte-string 137 65 64 135) [] 2))
         (leaf (list 22 33))
         (nested (list 11 leaf))
         (fingerprint
          (secure-hash
           (quote sha256)
           (prin1-to-string
            (list (aref fn 0)
                  (mapconcat (lambda (b) (number-to-string b))
                             (append (aref fn 1) nil) ",")
                  (aref fn 2) (aref fn 3)))))
         (starting-count nelisp-bytecode-jit--native-call-count)
         (vm-values
          (let ((nelisp-bytecode-jit--dispatch-active t))
            (list (funcall fn (list 11 22 33))
                  (funcall fn nil)
                  (funcall fn nested))))
         (vm-count nelisp-bytecode-jit--native-call-count)
         (flat-result (funcall fn (list 11 22 33)))
         (flat-count nelisp-bytecode-jit--native-call-count)
         (nil-result (funcall fn nil))
         (nil-count nelisp-bytecode-jit--native-call-count)
         (nested-result (funcall fn nested))
         (nested-count nelisp-bytecode-jit--native-call-count)
         (nested-eq-before-release (eq nested-result leaf))
         (_ (setcar (nthcdr 2 vm-values)
                    (copy-tree (nth 2 vm-values))))
         (vm-copy-distinct (not (eq nested-result (nth 2 vm-values))))
         (invalid (condition-case data
                      (progn (funcall fn 1) (quote missed))
                    (wrong-type-argument data)))
         (final-count nelisp-bytecode-jit--native-call-count))
    (garbage-collect)
    (when (= final-count starting-count)
      (princ (format "jit-status=%S\n" (nelisp-bytecode-jit-status))))
    (list :fingerprint fingerprint
          :vm-values vm-values
          :vm-native-delta (- vm-count starting-count)
          :jit-values (list flat-result nil-result nested-result)
          :native-deltas (list (- flat-count vm-count)
                               (- nil-count flat-count)
                               (- nested-count nil-count)
                               (- final-count nested-count))
          :nested-eq-before-release nested-eq-before-release
          :vm-copy-distinct vm-copy-distinct
          :nested-value-after-input-release
          (progn (setcar nested nil)
                 (setq nested nil leaf nil)
                 (garbage-collect)
                 nested-result)
          :invalid invalid)))')
standalone_result=${standalone_output##*$'\n'}
expected_standalone="(:fingerprint \"$expected_hash\" :vm-values (22 nil (22 33)) :vm-native-delta 0 :jit-values (22 nil (22 33)) :native-deltas (1 1 1 0) :nested-eq-before-release t :vm-copy-distinct t :nested-value-after-input-release (22 33) :invalid (wrong-type-argument listp 1))"
if [[ "$standalone_result" != "$expected_standalone" ]]; then
  echo "standalone-bytecode-jit-cadr-return-smoke: standalone mismatch: $standalone_output" >&2
  exit 1
fi

echo "standalone-bytecode-jit-cadr-return-smoke: PASS (Host/VM/JIT parity, three native calls, returned cons survives input release and GC, VM signal fallback)"
