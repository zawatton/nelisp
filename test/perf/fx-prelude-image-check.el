;;; fx-prelude-image-check.el --- the prelude is on the VM in a dumped image -*- lexical-binding: t -*-
;; Usage: nelisp --cold-load-from NELISP.cold --load test/perf/fx-prelude-image-check.el
;; tools/ai/eval-core-progress.org S3.1: the cold-image dump compiles every
;; prelude function the VM covers (`nelisp--fx-register-prelude'), so a
;; `--cold-load-from' boot starts with them compiled -- including those the
;; prelude defines before the VM's registration hooks, which a normal boot
;; leaves to the interpreter -- and calling them compiles nothing more.
;; Prints one FX-PRELUDE-IMAGE line; non-zero exit on failure.
(let* ((fails 0)
       (check (lambda (what got want)
                (unless (equal got want)
                  (setq fails (1+ fails))
                  (princ (format "FX-PRELUDE-IMAGE FAIL %s got %S want %S\n" what got want)))))
       (vm-p (lambda (f)
               (let ((fn (symbol-function f)))
                 (and (eq (car-safe fn) 'closure)
                      (vectorp (gethash (nthcdr 3 fn) nelisp--fx-table))))))
       (count0 (nelisp--fx-count))
       (min-count (if (getenv "FX_PRELUDE_MIN") (string-to-number (getenv "FX_PRELUDE_MIN")) 1000)))
  (unless (>= count0 min-count)
    (setq fails (1+ fails))
    (princ (format "FX-PRELUDE-IMAGE FAIL %d compiled records at startup, want >= %d\n" count0 min-count)))
  ;; Defined before the hooks: never compiled by a normal boot.
  (dolist (f '(compare-strings last butlast))
    (funcall check (list 'vm f) (funcall vm-p f) t))
  (funcall check 'compare-strings (compare-strings "abc" nil nil "abd" nil nil) -3)
  (funcall check 'last (last '(1 2 3)) '(3))
  (funcall check 'butlast (butlast '(1 2 3)) '(1 2))
  (funcall check 'string-prefix-p (list (string-prefix-p "ab" "abc") (string-prefix-p "x" "abc")) '(t nil))
  (funcall check 'no-compile-on-call (nelisp--fx-count) count0)
  (princ (format "FX-PRELUDE-IMAGE records=%d failures=%d %s\n" count0 fails (if (= fails 0) "PASS" "FAIL")))
  (kill-emacs (if (= fails 0) 0 1)))
