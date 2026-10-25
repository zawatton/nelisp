;;; nelisp-aot-stackmap-test.el --- Liveness stack map emission (smap1) -*- lexical-binding: t; -*-
;; Stage 1: the compiler records, per call site, which fixed-frame words may be read after the call.
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(defun smap-test--run (source)
  "Compile SOURCE (defuns) with stack maps on; return (TEXT . RECORDS)."
  (let* ((nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--label-counter 0)
         (ir (nelisp-aot-compiler--parse source nil))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer 'sysv))
         (cell (list nil))
         (nelisp-aot-compiler--stackmap-records cell))
    (dolist (d defuns) (nelisp-aot-compiler--emit-defun d buf))
    (let* ((fixups (copy-sequence (aref buf 3)))
           (relocs (nelisp-asm-x86_64-extract-relocs buf))
           (text (nelisp-asm-x86_64-resolve-fixups buf)))
      (cons text (nelisp-aot-compiler--stackmap-finish (car cell) text relocs fixups)))))

(defun smap-test--masks (source)
  "Return the live masks of the first function's sites."
  (let ((rec (cadr (smap-test--run source))))
    (mapcar #'cdr (plist-get rec :sites))))

;; Every unit-compiled defun reserves the hidden boundary slots after its parameters
;; (`--object-hidden-boundary-fenv'); a first `let' slot follows them.
(defun smap-test--hidden ()
  (- (plist-get (cadr (smap-test--run '(defun f (a) (+ a 1)))) :slots) 1))

(ert-deftest smap-dead-param-is-not-live ()
  ;; a is never read: only b stays live across the call.
  (should (equal (smap-test--masks '(defun f (a b) (seq (g 1) b))) '(2))))

(ert-deftest smap-both-params-live ()
  (should (equal (smap-test--masks '(defun f (a b) (seq (g 1) (+ a b)))) '(3))))

(ert-deftest smap-param-dies-after-last-call ()
  ;; a is an argument of the first call (kept live for that call only), b is read after both.
  (should (equal (smap-test--masks '(defun f (a b) (seq (g a) (h 1) b))) '(3 2))))

(ert-deftest smap-fresh-let-slot-not-live-before-it-is-written ()
  ;; x (slot 1) is written by the call's own result: dead during the call.
  (let ((rec (cadr (smap-test--run '(defun f (a) (let ((x (g a))) (+ x 1)))))))
    (should (equal (mapcar #'cdr (plist-get rec :sites)) '(1)))
    (should (>= (plist-get rec :words) (+ 2 (smap-test--hidden))))))

(ert-deftest smap-loop-keeps-induction-variables-live ()
  (should (equal (smap-test--masks
                  '(defun f (n) (let ((i 0)) (seq (while (< i n) (seq (g i) (setq i (+ i 1)))) i))))
                 (list (+ 1 (ash 1 (+ 1 (smap-test--hidden))))))))

(ert-deftest smap-no-calls-no-sites ()
  (let ((rec (cadr (smap-test--run '(defun f (a b) (+ a b))))))
    (should (null (plist-get rec :sites)))))

(ert-deftest smap-sites-are-return-addresses-after-call-opcodes ()
  (let* ((run (smap-test--run '(defun f (a b) (seq (g a) (h b) (g b)))))
         (text (car run)) (rec (cadr run)) (start (plist-get rec :start)))
    (should (= (length (plist-get rec :sites)) 3))
    (dolist (s (plist-get rec :sites))
      (should (= (aref text (+ start (car s) -5)) #xE8)))))

(ert-deftest smap-variadic-and-observer-stay-full ()
  (let ((rec (cadr (smap-test--run '(defun f (a) (seq (g a) (aot-current-sp)))))))
    (should (plist-get rec :reason))
    (should (null (plist-get rec :sites)))))

(ert-deftest smap-disabled-emits-identical-bytes ()
  (let* ((src '(defun f (a b) (seq (g a) (if (< a b) (h b) (g 2)) (+ a b))))
         (on (car (smap-test--run src)))
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--label-counter 0)
         (ir (nelisp-aot-compiler--parse src nil))
         (buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (dolist (d (nelisp-aot-compiler--collect-defuns ir)) (nelisp-aot-compiler--emit-defun d buf))
    (should (equal on (nelisp-asm-x86_64-resolve-fixups buf)))))

;; ---- the table unit and the cache sidecar (generator side) ----

(defun smap-test--unit ()
  (nelisp-standalone--compile-to-unit
   "smap-test.o" '(seq (defun smap_a (a b) (seq (smap_ext 1) b))
                       (defun smap_b (n) (let ((i 0)) (seq (while (< i n) (seq (smap_ext i) (setq i (+ i 1)))) i))))))

(defun smap-test--u (bytes off n)
  (let ((v 0)) (dotimes (i n) (setq v (logior v (ash (aref bytes (+ off i)) (* 8 i))))) v))

(ert-deftest smap-unit-carries-sidecar-through-the-cache-encoding ()
  (let* ((u (smap-test--unit))
         (back (with-temp-buffer
                 (let ((print-escape-nonascii t) (print-length nil) (print-level nil))
                   (prin1 (nelisp-standalone--unit-cache-encode u) (current-buffer)))
                 (goto-char (point-min))
                 (nelisp-standalone--unit-cache-decode (read (current-buffer))))))
    (should (plist-get u :stackmap))
    (should (equal (plist-get u :stackmap) (plist-get back :stackmap)))))

(ert-deftest smap-table-unit-layout-matches-the-recorded-sites ()
  (let* ((u (smap-test--unit))
         (recs (plist-get u :stackmap))
         (sm (nelisp-standalone--stackmap-unit (list u)))
         (text (cdr (assq 'text (plist-get sm :sections))))
         (tab 16))
    ;; thunk: lea rax,[rip+9]; ret -- 9 bytes past the end of the 7-byte lea is the table at +16.
    (should (equal (substring text 0 8) (unibyte-string #x48 #x8d #x05 9 0 0 0 #xc3)))
    (should (= (smap-test--u text tab 4) #x4d534c4e))
    (should (= (smap-test--u text (+ tab 8) 4) (length recs)))
    (should (= (smap-test--u text (+ tab 20) 4) (- (length text) tab)))
    (should (= (length (plist-get sm :relocs)) (length recs)))
    (dolist (r (plist-get sm :relocs))
      (should (eq (plist-get r :type) 'pc32))
      (should (= (plist-get r :addend) 4)))
    (let ((maps-off (smap-test--u text (+ tab 16) 4)) (i 0))
      (dolist (rec recs)
        (let* ((fe (+ tab 24 (* 12 i)))
               (nsites (smap-test--u text (+ fe 8) 2))
               (sites-off (smap-test--u text (+ fe 4) 4)))
          (should (= nsites (length (plist-get rec :sites))))
          (let ((j 0))
            (dolist (s (plist-get rec :sites))
              (let* ((se (+ tab sites-off (* 8 j)))
                     (moff (smap-test--u text (+ se 4) 4))
                     (m (+ tab maps-off moff))
                     (fixed (smap-test--u text m 2)))
                (should (= (smap-test--u text se 4) (car s)))
                (should (= fixed (+ 2 (plist-get rec :words))))
                (should (= (smap-test--u text (+ m 8) (ceiling fixed 8.0)) (cdr s))))
              (setq j (1+ j)))))
        (setq i (1+ i))))))

;; Per-target rule: the table is plain text plus a bss block; no data-blob, no data/rodata section.
(ert-deftest smap-table-unit-has-no-data-blob-on-any-target ()
  (dolist (target '(linux-x86_64 windows-x86_64 macos-aarch64 linux-aarch64))
    (let* ((nelisp-standalone--target target)
           (u (nelisp-standalone--stackmap-unit nil)))
      (should (equal (mapcar #'car (plist-get u :sections)) '(text bss)))
      (should (= (length (cdr (assq 'text (plist-get u :sections)))) 48))
      (should (equal (mapcar (lambda (s) (plist-get s :name)) (plist-get u :symbols))
                     '("nl_stackmap_base" "nl_stackmap_state"))))))

(provide 'nelisp-aot-stackmap-test)
