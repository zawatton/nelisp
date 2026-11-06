;;; nelisp-standalone-arena-rewrite-test.el --- Source move equivalence -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-standalone-arena-rewrite)

;; Frozen original builder definitions provide the independent source oracle.
(defun nelisp-standalone-arena-rewrite-test--base-reference (&optional target)
  "Return standalone arena base for TARGET."
  (pcase (or target nelisp-standalone--target)
    ((or 'windows-x86_64 'windows-aarch64) nelisp-standalone--windows-arena-base)
    ('macos-aarch64 nelisp-standalone--macos-arena-base)
    (_ nelisp-standalone--arena-base)))

(defun nelisp-standalone-arena-rewrite-test--reference (source)
  "Doc 140 Stage 8: rewrite fixed-arena-base metadata immediates in SOURCE to
load the runtime chunk-0 base from the driver-owned `nl_arena_base' bss slot,
so NO normal runtime path embeds a fixed arena base.

For every chunked native target (linux-x86_64, windows-x86_64,
macos-aarch64), every integer atom N in [target-base, target-base+span) — the
compact metadata block plus the chunk-0 descriptor — becomes `(+ (ptr-read-u64
(data-addr nl_arena_base) 0) OFF)' where OFF = N - target-base.  Because in
the chunked model that whole range is always arena-base-relative (the chunk-0
bump cursor at +0, control slots, the chunk-0 descriptor), the rewrite is
uniform and unambiguous.

The `nl_arena_init' defun is left untouched: it is the one site that reserves
chunk 0 via a NULL-based OS allocation, seeds `nl_arena_base' from the runtime
return value, and may carry reservation SIZE literals that must never be
confused with a fixed base immediate."
  (if (not (memq nelisp-standalone--target
                 '(linux-x86_64 windows-x86_64 macos-aarch64 linux-aarch64
                   windows-aarch64)))
      source
    (let ((base (nelisp-standalone-arena-rewrite-test--base-reference))
          (span nelisp-standalone--arena-rebase-span))
      (cl-labels
          ((rewrite-int
            (n)
            (if (and (integerp n) (<= base n) (< n (+ base span)))
                `(+ (heap-read-u64 (data-addr nl_arena_base) 0) ,(- n base))
              n))
           (walk
            (form)
            (cond
             ;; Leave the base-establishing init untouched (mmap(NULL) call +
             ;; SIZE literal live here; the runtime base var does the writes).
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_arena_init))
              form)
             ((integerp form) (rewrite-int form))
             ((consp form) (cons (walk (car form)) (walk (cdr form))))
             (t form))))
        (let ((max-lisp-eval-depth (max max-lisp-eval-depth 10000)))
          (walk source))))))

(ert-deftest nelisp-standalone-arena-rewrite/all-targets-and-boundaries ()
  (dolist (case '((linux-x86_64 . #x10000000) (linux-aarch64 . #x10000000)
                  (windows-x86_64 . #x70000000) (windows-aarch64 . #x70000000)
                  (macos-aarch64 . #x800000000)))
    (let* ((nelisp-standalone--target (car case)) (base (cdr case))
           (init (list 'defun 'nl_arena_init nil base))
           (vector (vector base))
           (source (list 'seq (- base 1) base (+ base #xfff) (+ base #x1000)
                         (cons base (+ base 3)) vector init))
           (expected (let ((nelisp-standalone--arena-rebase-span #x1000))
                       (nelisp-standalone-arena-rewrite-test--reference source)))
           (actual (nelisp-standalone--chunk-arena-rewrite source)))
      (should (equal actual expected))
      (should (eq (nth 6 actual) vector))
      (should (eq (nth 7 actual) init))
      (should (equal (nth 2 actual)
                     '(+ (heap-read-u64 (data-addr nl_arena_base) 0) 0)))
      (should (= (nth 4 actual) (+ base #x1000))))))

(ert-deftest nelisp-standalone-arena-rewrite/unsupported-and-init-identity ()
  (let* ((nelisp-standalone--target 'unsupported-target)
         (source '(seq #x10000000)))
    (should (eq (nelisp-standalone--chunk-arena-rewrite source) source)))
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source '(defun nl_arena_init () (seq #x10000000 #x10000fff))))
    (should (eq (nelisp-standalone--chunk-arena-rewrite source) source))))

(defun nelisp-standalone-arena-rewrite-test--definition (name)
  (with-temp-buffer
    (insert-file-contents (locate-library "nelisp-standalone-arena-rewrite"))
    (goto-char (point-min))
    (re-search-forward (concat "^(defconst " (regexp-quote (symbol-name name)) "[ \n]"))
    (goto-char (match-beginning 0))
    (read (current-buffer))))

(ert-deftest nelisp-standalone-arena-rewrite/environment-and-aliases ()
  (let ((process-environment (copy-sequence process-environment))
        (nelisp-standalone--target nelisp-standalone--target)
        (nelisp-standalone--windows-arena-base nelisp-standalone--windows-arena-base))
    (dolist (case '((nil . linux-x86_64) ("linux-arm64" . linux-aarch64)
                    ("windows-arm64" . windows-aarch64) ("macos-arm64" . macos-aarch64)
                    ("unsupported-target" . unsupported-target)))
      (setenv "NELISP_STANDALONE_TARGET" (car case))
      (should (eq (eval (nth 2 (nelisp-standalone-arena-rewrite-test--definition
                               'nelisp-standalone--target)))
                  (cdr case))))
    (dolist (case '((nil . #x70000000) ("" . #x70000000)
                    ("0x71000000" . #x71000000) ("1895825408" . #x71000000)))
      (setenv "NELISP_STANDALONE_WINDOWS_ARENA_BASE" (car case))
      (let ((nelisp-standalone--windows-arena-base
             (eval (nth 2 (nelisp-standalone-arena-rewrite-test--definition
                           'nelisp-standalone--windows-arena-base)))))
        (should (= nelisp-standalone--windows-arena-base (cdr case)))
        (should (= (nelisp-standalone--target-arena-base 'windows-aarch64) (cdr case)))))))

(ert-deftest nelisp-standalone-arena-rewrite/public-accessors-and-dynamic-target ()
  (should (= (nelisp-standalone-arena-rewrite-base) nelisp-standalone--arena-base))
  (should (= (nelisp-standalone-arena-rewrite-span) nelisp-standalone--arena-rebase-span))
  (should (= (nelisp-standalone-arena-rewrite-windows-base) nelisp-standalone--windows-arena-base))
  (let ((original (nelisp-standalone-arena-rewrite-target)))
    (dolist (target '(linux-x86_64 linux-aarch64 windows-x86_64 windows-aarch64 macos-aarch64))
      (let* ((nelisp-standalone--target target)
             (base (nelisp-standalone-arena-rewrite-target-base))
             (source (list 'defun 'f nil base)))
        (should (eq (nelisp-standalone-arena-rewrite-target) target))
        (should (= base (nelisp-standalone--target-arena-base target)))
        (should (equal (nelisp-standalone-arena-rewrite-source source)
                       (nelisp-standalone-arena-rewrite-test--reference source)))
        (let ((nelisp-standalone--target 'unsupported-target))
          (should (eq (nelisp-standalone-arena-rewrite-target) 'unsupported-target))
          (should (eq (nelisp-standalone-arena-rewrite-source source) source)))
        (should (eq (nelisp-standalone-arena-rewrite-target) target))))
    (should (eq (nelisp-standalone-arena-rewrite-target) original))))

(provide 'nelisp-standalone-arena-rewrite-test)
