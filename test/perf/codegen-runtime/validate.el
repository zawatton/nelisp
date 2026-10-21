;;; validate.el --- independent GNU fixture goldens -*- lexical-binding: t; -*-
(require 'cl-lib)
(defun codegen-runtime--golden (name n r)
  "Compute the fixture checksum with ordinary GNU Lisp, without C lowering."
  (let ((sum 0))
    (dotimes (i n)
      (setq sum
            (+ sum
               (cond
                ((equal name "nelisp_mirror_lookup_entry") (+ 1792 (* 32 (logand i 1))))
                ((equal name "nelisp_eval_call_w") (+ (* i 4) 2))
                ((equal name "wf_bytecode_execute_frame") 169)
                ((equal name "nl_alloc_bytes") (if (= (logand i 1) 0) 602 88))
                ((equal name "nl_gc_sweep_merge_range") 568)
                ((equal name "nl_bytes_byte_at") (+ (* (logand i 31) 7) 3))
                ((equal name "nelisp_symbol_key_equal") (if (= (logand i 3) 2) 0 1))
                ((equal name "wf_bytecode_w_fast_run")
                 ;; PC returns to zero, SP stays one, and 256 add1
                 ;; instructions update the fixnum word from zero to 256.
                 (+ 1 (logior (ash 256 2) 1)))
                ((equal name "nl_root_frame") (+ 32 (* 3 (logand i 7))))
                ((equal name "nelisp_fnv1a_step4")
                 (let ((h 2166136261))
                   (dotimes (k (+ 16 (logand i 3)))
                     (setq h (logand (* (logxor h (+ (* k 7) 3)) 16777619) 4294967295)))
                   h))
                (t (error "No independent golden for %s" name))))))
    (format "%016x" (logand (* sum r) 18446744073709551615))))

(defun codegen-runtime-validate (name nl &optional gcc)
  "Check full 64-bit outputs of both executables against GNU Lisp goldens."
  (let ((count 0))
    (dolist (pair '((0 1) (1 1) (3 1) (4 1) (7 3) (33 1) (127 3)))
      (let* ((n (car pair)) (r (cadr pair))
             (expected (codegen-runtime--golden name n r)))
        (dolist (binary (delq nil (list nl gcc)))
          (with-temp-buffer
            (insert (format "%08d %d\n" n r))
            (let ((rc (call-process-region (point-min) (point-max) binary t t nil)))
              (unless (and (equal rc 0) (equal (string-trim (buffer-string)) expected))
                (error "%s n=%d r=%d: expected %s, got %S rc=%S"
                       name n r expected (string-trim (buffer-string)) rc))))))
      (setq count (+ count 1)))
    (princ (format "%s: %d GNU goldens, %d executable(s) agree at full width\n" name count (if gcc 2 1)))))
