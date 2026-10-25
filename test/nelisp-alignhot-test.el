;;; nelisp-alignhot-test.el --- Layout-stable hot code (alignhot) -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defconst nelisp-alignhot-test--source
  '(seq
    (defun ah_small (a) (+ a 1))
    (defun ah_switch (k)
      (if (= k 0) 100 (if (= k 1) 101 (if (= k 2) 102 (if (= k 3) 103
        (if (= k 4) 104 (if (= k 5) 105 (if (= k 6) 106 (if (= k 7) 107
          (if (= k 8) 108 (if (= k 9) 109 7)))))))))))
    (defun ah_loop (n)
      (let ((i 0) (s 0)) (while (< i n) (setq s (+ s i)) (setq i (+ i 1))) s))
    (defun ah_last (a b) (+ (ah_small a) b)))
  "A multi-defun unit with a dense jump table and a loop.")

(defun nelisp-alignhot-test--unit (&optional policy perturb)
  (let ((nelisp-aot-compiler-hot-alignment policy)
        (nelisp-aot-compiler-alignhot-perturb perturb))
    (nelisp-aot-compile-to-link-unit nelisp-alignhot-test--source)))

(defun nelisp-alignhot-test--u32 (text pos)
  (let ((v (logior (aref text pos) (ash (aref text (+ pos 1)) 8)
                   (ash (aref text (+ pos 2)) 16) (ash (aref text (+ pos 3)) 24))))
    (if (>= v #x80000000) (- v #x100000000) v)))

(defun nelisp-alignhot-test--table-targets (unit)
  "Jump-table targets (text offsets) of the first table in UNIT's text."
  (let* ((text (plist-get unit :text))
         (pos (cl-loop for i from 0 below (- (length text) 7)
                       when (and (= (aref text i) #x4c) (= (aref text (+ i 1)) #x8d)
                                 (= (aref text (+ i 2)) #x15))
                       return i))
         (table (and pos (+ pos 7 (nelisp-alignhot-test--u32 text (+ pos 3))))))
    (should table)
    (delete-dups
     (cl-loop for e from table by 4 repeat 10
              collect (+ e 4 (nelisp-alignhot-test--u32 text e))))))

(defun nelisp-alignhot-test--defun-offsets (unit)
  (mapcar (lambda (d) (plist-get d :offset)) (plist-get unit :defuns)))

(ert-deftest nelisp-alignhot/entries-and-unit-size-aligned ()
  (let ((unit (nelisp-alignhot-test--unit
               '(:entry 64 :arm 32 :loop 32 :unit-tail 64))))
    (should (= 4 (length (plist-get unit :defuns))))
    (dolist (off (nelisp-alignhot-test--defun-offsets unit))
      (should (= 0 (% off 64))))
    (should (= 0 (% (length (plist-get unit :text)) 64)))))

(ert-deftest nelisp-alignhot/jump-table-arms-aligned ()
  (let* ((unit (nelisp-alignhot-test--unit
                '(:entry 64 :arm 32 :loop 32 :unit-tail 64)))
         (targets (nelisp-alignhot-test--table-targets unit)))
    (should (>= (length targets) 10))
    (dolist (target targets)
      (should (= 0 (% target 32))))))

(ert-deftest nelisp-alignhot/off-switch-keeps-seed-layout ()
  (let ((unit (nelisp-alignhot-test--unit nil)))
    ;; Seed layout: 16-byte entries only, no tail pad, unaligned arms.
    (dolist (off (nelisp-alignhot-test--defun-offsets unit))
      (should (= 0 (% off 16))))
    (should (cl-some (lambda (off) (/= 0 (% off 64)))
                     (nelisp-alignhot-test--defun-offsets unit)))
    (should (cl-some (lambda (target) (/= 0 (% target 32)))
                     (nelisp-alignhot-test--table-targets unit)))))

(ert-deftest nelisp-alignhot/off-switch-bytes-equal-seed-compiler ()
  "With the policy nil the unit is byte-identical to the seed compiler's."
  (let* ((seed (expand-file-name "target/alignhot/seed/lisp"
                                 (file-name-directory
                                  (directory-file-name
                                   (file-name-directory (locate-library "nelisp-aot-compiler"))))))
         (out (make-temp-file "alignhot-seed-" nil ".el")))
    (skip-unless (file-exists-p (expand-file-name "nelisp-aot-compiler.el" seed)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (should (= 0 (call-process
                          (expand-file-name invocation-name invocation-directory) nil t nil
                          "--batch" "-Q" "-L" seed
                          "-L" (file-name-directory (locate-library "nelisp-aot-compiler"))
                          "-L" (expand-file-name "../src" (file-name-directory (locate-library "nelisp-aot-compiler")))
                          "--eval" "(require 'nelisp-aot-compiler)"
                          "--eval" (format "(with-temp-file %S (set-buffer-multibyte nil) (let ((u (nelisp-aot-compile-to-link-unit '%S))) (insert (plist-get u :text))))"
                                           out nelisp-alignhot-test--source)))))
          (let ((expected (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally out)
                            (buffer-string)))
                (actual (nelisp-alignhot-test--unit nil)))
            (should (> (length expected) 0))
            (should (equal (append expected nil) (append (plist-get actual :text) nil)))))
      (delete-file out))))

(ert-deftest nelisp-alignhot/perturb-shifts-entry-by-int3-block ()
  (let* ((base (nelisp-alignhot-test--unit '(:entry 64 :unit-tail 64)))
         (pert (nelisp-alignhot-test--unit '(:entry 64 :unit-tail 64) '("ah_loop" . 4096)))
         (off-base (plist-get (cl-find "ah_loop" (plist-get base :defuns)
                                       :key (lambda (d) (plist-get d :name)) :test #'equal) :offset))
         (off-pert (plist-get (cl-find "ah_loop" (plist-get pert :defuns)
                                       :key (lambda (d) (plist-get d :name)) :test #'equal) :offset)))
    (should (>= (- off-pert off-base) 4096))
    (should (= 0 (% off-pert 64)))
    (should (= 0 (% (- off-pert off-base) 64)))
    (should (>= (cl-count #xCC (plist-get pert :text)) 4096))
    (should (= 0 (cl-count #xCC (plist-get base :text))))))

(provide 'nelisp-alignhot-test)
