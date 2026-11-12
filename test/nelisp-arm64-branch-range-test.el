;;; nelisp-arm64-branch-range-test.el --- arm64 BL range / literal hoist regression -*- lexical-binding: t; -*-

;; Ledger criterion S3.1 (tools/ai/ci-speedup-progress.org): the macOS
;; stage-d leg failed with
;;   nelisp-asm-arm64-error :branch-out-of-range -184546428
;; because on aarch64 targets driver.o embedded ~40 MB of REPL-prelude string
;; literals as ~176 MB of mov/store code, past the +-128 MiB reach of BL.
;; Fix ee11327d8 hoists large literals to rodata on aarch64 as on x86_64.
;;
;; Two properties are pinned here, both without a native build:
;;  1. the assembler still rejects a BL beyond +-128 MiB (failure mode), and
;;  2. on aarch64 targets, the unit's text size is O(1) in the literal length
;;     (red before ee11327d8, where it grew by ~4.4 words per 8 bytes).

(require 'ert)
(require 'cl-lib)

(defconst nelisp-arm64-range-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(dolist (directory '("../lisp" "../src" "../scripts"))
  (add-to-list 'load-path (expand-file-name directory nelisp-arm64-range-test--dir)))
(require 'bytecomp)
(require 'nelisp-asm-arm64)
(require 'nelisp-aot-compiler)
(require 'nelisp-static-linker)
(require 'nelisp-standalone-arena-rewrite)
(require 'nelisp-version)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-cc-fxir-native)

(defvar nelisp-standalone--repo-root
  (expand-file-name ".." nelisp-arm64-range-test--dir))

;; Load only the build-script definitions reachable from the roots below (the
;; same technique as nelisp-standalone-build-trace-test.el); no build runs.
(defun nelisp-arm64-range-test--load-closure (file roots)
  "Evaluate the top-level definitions of FILE reachable from ROOTS."
  (let ((table (make-hash-table :test #'eq)) (order nil)
        (seen (make-hash-table :test #'eq)) (todo (copy-sequence roots))
        (wanted nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form) (memq (car form) '(defun defmacro defvar defconst))
                         (symbolp (cadr form)))
                (unless (gethash (cadr form) table) (push (cadr form) order))
                (puthash (cadr form) (append (gethash (cadr form) table) (list form))
                         table))))
        (end-of-file nil)))
    (setq order (nreverse order))
    (while todo
      (let ((name (pop todo)))
        (unless (gethash name seen)
          (puthash name t seen)
          (when (gethash name table)
            (push name wanted)
            (let ((stack (list (gethash name table))))
              (while stack
                (let ((x (pop stack)))
                  (cond ((and x (symbolp x)) (when (gethash x table) (push x todo)))
                        ((consp x) (push (car x) stack)
                                   (when (cdr x) (push (cdr x) stack)))))))))))
    (dolist (name order)
      (when (memq name wanted)
        (dolist (form (gethash name table))
          (if (or (memq name roots) (memq (car form) '(defun defmacro)))
              (eval form t)
            (condition-case nil (eval form t) (error nil))))))))

(defvar nelisp-standalone--target)
(nelisp-arm64-range-test--load-closure
 (expand-file-name "../scripts/nelisp-standalone-build.el" nelisp-arm64-range-test--dir)
 '(nelisp-standalone--compile-to-unit
   nelisp-standalone--hoist-large-str-lits
   nelisp-standalone--str-lit-blob-min))

;;; Assembler failure mode

(defconst nelisp-arm64-range-test--bl-reach (ash 1 27)
  "BL imm26 reaches [-2^27, 2^27 - 4] bytes (+-128 MiB).")

(defun nelisp-arm64-range-test--resolve-bl (label-pos)
  "Assemble one BL at slot 0 whose label is moved to LABEL-POS, then resolve."
  (let ((buf (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-bl buf 'far)
    (setq buf (nelisp-asm-arm64-define-label buf 'far))
    (setcdr (assq 'far (nelisp-asm-arm64-buffer-labels buf)) label-pos)
    (nelisp-asm-arm64-resolve-fixups buf)
    (nelisp-asm-arm64-buffer-bytes buf)))

(ert-deftest nelisp-arm64-range-bl-accepts-limit-rejects-beyond ()
  (let ((reach nelisp-arm64-range-test--bl-reach))
    ;; Just inside, forward and backward.
    (should (nelisp-arm64-range-test--resolve-bl (- reach 4)))
    (should (nelisp-arm64-range-test--resolve-bl (- reach)))
    ;; One instruction word beyond, forward and backward.
    (should (equal (should-error (nelisp-arm64-range-test--resolve-bl reach)
                                 :type 'nelisp-asm-arm64-error)
                   `(nelisp-asm-arm64-error :branch-out-of-range ,reach :at-slot 0)))
    (should (equal (cadr (should-error (nelisp-arm64-range-test--resolve-bl (- (- reach) 4))
                                       :type 'nelisp-asm-arm64-error))
                   :branch-out-of-range))
    ;; The displacement reported by the macOS stage-d CI failure.
    (should (equal (should-error (nelisp-arm64-range-test--resolve-bl -184546428)
                                 :type 'nelisp-asm-arm64-error)
                   '(nelisp-asm-arm64-error :branch-out-of-range -184546428 :at-slot 0)))))

;;; Literal text size on aarch64 targets

(defun nelisp-arm64-range-test--unit-text (target literal-length &optional unhoisted)
  "Return (TEXT-BYTES . HOISTED-BLOBS) for a unit holding one big literal.
The literal is LITERAL-LENGTH bytes long.  With UNHOISTED the hoisting pass is
skipped, which is what aarch64 targets effectively did before ee11327d8."
  (let* ((nelisp-standalone--target target)
         (lit (make-string literal-length ?a))
         (source `(seq (defun f () (sexp-write-str-lit 'slot ,lit))
                       (defun g () 1)))
         (hoisted (if unhoisted (list source)
                    (nelisp-standalone--hoist-large-str-lits source "lit")))
         (unit (let ((inhibit-message t) (message-log-max nil))
                 (nelisp-standalone--compile-to-unit "f.o" (car hoisted)))))
    (cons (length (cdr (assq 'text (plist-get unit :sections)))) (cdr hoisted))))

(ert-deftest nelisp-arm64-range-text-is-constant-in-literal-length ()
  (dolist (target '(macos-aarch64 linux-aarch64))
    (let* ((small (nelisp-arm64-range-test--unit-text target (* 1024 1024)))
           (large (nelisp-arm64-range-test--unit-text target (* 8 1024 1024))))
      ;; The literal leaves the text as one rodata blob ...
      (should (= (length (cdr small)) 1))
      (should (= (length (cdr large)) 1))
      (should (>= (length (cdr (car (cdr large)))) (* 8 1024 1024)))
      ;; ... so growing it by 7 MiB adds (almost) nothing to the text.
      (should (< (abs (- (car large) (car small))) 1024))
      (should (< (car large) 4096)))))

(ert-deftest nelisp-arm64-range-unhoisted-text-is-linear-in-literal ()
  "Document why the hoist is required: unhoisted code is linear in the literal.
At this rate (a few text bytes per literal byte) the tens of MB of REPL
prelude literals in driver.o grow the text to the order of the 128 MiB BL
reach, which is how the macOS stage-d leg overflowed."
  (dolist (target '(macos-aarch64 linux-aarch64))
    (let* ((n (* 64 1024))
           (unhoisted (car (nelisp-arm64-range-test--unit-text target n t)))
           (hoisted (car (nelisp-arm64-range-test--unit-text target n))))
      (should (> unhoisted (* 2 n)))
      (should (> unhoisted (* 100 hoisted))))))

(provide 'nelisp-arm64-branch-range-test)
;;; nelisp-arm64-branch-range-test.el ends here
