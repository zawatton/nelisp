;;; -*- lexical-binding: t; -*-
;; tools/ai/gc-int-pin-progress.org: run from the repo root after `make ffi-loader'
;; has built the fixtures.  Stress probe: keep mapping loader fixtures in one process and, after
;; each open, probe openat on a file that certainly exists.
(load "packages/nl-ffi/src/nl-ffi.el")
(let* ((abs (expand-file-name "target/nl-ffi-loader-fixture-ctor-dep.so"))
       (fixtures '("target/nl-ffi-loader-fixture.so" "target/nl-ffi-loader-fixture-dep-root.so"
                   "target/nl-ffi-loader-fixture-init.so" "target/nl-ffi-loader-fixture-ifunc.so"
                   "target/nl-ffi-loader-fixture-tls-ie.so" "target/nl-ffi-loader-fixture-ctor-root.so"))
       (n (string-to-number (or (getenv "STRESS_N") "60")))
       (opens 0) (open-errs nil) (probe-fails nil) (i 0))
  (while (< i n)
    (dolist (f fixtures)
      (condition-case e (progn (nl-ffi-loader-open f) (setq opens (1+ opens)))
        (error (push (list i f e) open-errs)))
      (let* ((pc (nl-ffi-loader--cstring abs))
             (fd (syscall-direct nl-ffi-loader--sys-openat nl-ffi-loader--at-fdcwd pc
                                 nl-ffi-loader--o-rdonly 0 0 0)))
        (if (nl-ffi-loader--syscall-error-p fd)
            (let ((k 0) (bs nil) (b 1))
              (while (and (< k 300) (/= b 0)) (setq b (ptr-read-u8 pc k)) (push b bs) (setq k (1+ k)))
              (push (list i f fd pc (equal (concat (nreverse (cdr bs))) abs) (length abs) (1- k)) probe-fails)
              (when (= (length probe-fails) 1)
                (princ (format "BYTES %S\n" (let ((k 0) (l nil)) (while (< k 80) (push (ptr-read-u8 pc k) l) (setq k (1+ k))) (nreverse l))))
                (princ (format "PC hex=%x\n" pc))
                (let ((maps (with-temp-buffer (insert-file-contents "/proc/self/maps") (buffer-string))))
                  (dolist (line (split-string maps "\n" t))
                    (let* ((r (car (split-string line " ")))
                           (ab (split-string r "-"))
                           (lo (string-to-number (car ab) 16)) (hi (string-to-number (cadr ab) 16)))
                      (when (and (<= lo pc) (< pc hi)) (princ (format "MAP %s\n" line))))))))
          (syscall-direct nl-ffi-loader--sys-close fd 0 0 0 0 0))))
    (setq i (1+ i)))
  (princ (format "STRESS n=%d opens=%d open-errs=%d probe-fails=%d first-open-err=%S first-probe-fail=%S\n"
                 n opens (length open-errs) (length probe-fails)
                 (car (last open-errs)) (car (last probe-fails)))))
