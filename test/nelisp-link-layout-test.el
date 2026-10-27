;;; nelisp-link-layout-test.el --- hot-function link layout (layout lane)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `nelisp-link-x86-unit-pieces', `nelisp-link-split-unit' and
;; `nelisp-link-layout-units' (lisp/nelisp-static-linker.el): a unit split into
;; per-defun pieces links to the same bytes; listed defuns move to the front at
;; their residues, independent of the size of the code before them; cross-defun
;; calls stay correct; internal piece symbols are never exported.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-asm-x86_64)
(require 'nelisp-static-linker)

(defun nelisp-link-layout-test--unit (name &optional pre)
  "A three-defun unit NAME: f calls g, g jumps into h's body, h returns.
PRE bytes of INT3 precede f (a function that grew)."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (when pre (nelisp-asm-x86_64-emit-bytes buf (fillarray (make-string pre 0) #xcc)))
    (nelisp-asm-x86_64-align buf 64)
    (nelisp-asm-x86_64-define-label buf 'f)
    (nelisp-asm-x86_64-call-rel32 buf 'g)
    (nelisp-asm-x86_64-ret buf)
    (nelisp-asm-x86_64-align buf 64)
    (nelisp-asm-x86_64-define-label buf 'g)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x90 #x90))
    (nelisp-asm-x86_64-jmp-rel32 buf 'h-body)
    (nelisp-asm-x86_64-align buf 64)
    (nelisp-asm-x86_64-define-label buf 'h)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x90))
    (nelisp-asm-x86_64-define-label buf 'h-body)
    (nelisp-asm-x86_64-call-rel32 buf 'f)
    (nelisp-asm-x86_64-ret buf)
    (let* ((text (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (names '("f" "g" "h"))
           (syms (mapcar (lambda (n) (nelisp-link-symbol n (cdr (assq (intern n) labels))
                                                         :section 'text :bind 'global :type 'func))
                         names)))
      (append (nelisp-link-unit-make name (list (cons 'text text)) syms nil)
              (list :stackmap (mapcar (lambda (n) (list :name n :sites nil)) names))
              (nelisp-link-x86-unit-pieces labels (nelisp-asm-x86_64-buffer-fixups buf) names)))))

(defun nelisp-link-layout-test--entry ()
  (nelisp-link-unit-make "start.o" (list (cons 'text (unibyte-string #xc3)))
                         (list (nelisp-link-symbol "_start" 0 :section 'text :bind 'global)) nil))

(defun nelisp-link-layout-test--link (units base)
  "Link UNITS at text BASE; return (TEXT . SYMTAB)."
  (let* ((layout (list (cons 'text base) (cons 'rodata #x900000)
                       (cons 'data #xa00000) (cons 'bss #xb00000)))
         (r (nelisp-link-units-2pass units layout 'x86_64)))
    (nelisp-link--check-hot-alignment units (plist-get r :section-offsets) layout
                                      (plist-get r :symtab) 'x86_64)
    (cons (cdr (assq 'text (plist-get r :bytes))) (plist-get r :symtab))))

(defun nelisp-link-layout-test--va (symtab name)
  (plist-get (nelisp-link-symtab-lookup symtab name) :value))

(defun nelisp-link-layout-test--rel32-target (text base at)
  "Target address of the rel32 field at text offset AT."
  (let ((d (logior (aref text at) (ash (aref text (+ at 1)) 8)
                   (ash (aref text (+ at 2)) 16) (ash (aref text (+ at 3)) 24))))
    (when (>= d #x80000000) (setq d (- d #x100000000)))
    (+ base at 4 d)))

(ert-deftest nelisp-link-layout-pieces-record-cross-defun-fixups ()
  (let* ((u (nelisp-link-layout-test--unit "u.o"))
         (pieces (plist-get u :pieces)))
    (should (equal (mapcar #'car pieces) '("f" "g" "h")))
    (should (equal (mapcar #'cdr pieces) '(0 64 128)))
    ;; f->g, g->h-body and h->f cross; nothing else exists.
    (should (= (length (plist-get u :xfixups)) 3))))

(ert-deftest nelisp-link-layout-split-links-to-identical-bytes ()
  (let* ((base #x401000)
         (whole (nelisp-link-layout-test--link
                 (list (nelisp-link-layout-test--entry) (nelisp-link-layout-test--unit "u.o")) base))
         (split (nelisp-link-layout-test--link
                 (cons (nelisp-link-layout-test--entry)
                       (nelisp-link-split-unit (nelisp-link-layout-test--unit "u.o")))
                 base)))
    (should (equal (car whole) (car split)))
    (dolist (n '("f" "g" "h"))
      (should (= (nelisp-link-layout-test--va (cdr whole) n)
                 (nelisp-link-layout-test--va (cdr split) n))))
    (let ((pieces (nelisp-link-split-unit (nelisp-link-layout-test--unit "u.o"))))
      (should (equal (mapcar (lambda (p) (mapcar (lambda (f) (plist-get f :name))
                                                 (plist-get p :stackmap)))
                             pieces)
                     '(("f") ("g") ("h")))))))

(ert-deftest nelisp-link-layout-hot-defuns-first-at-residue ()
  (let* ((base #x401c00)
         (order (list :modulus 4096 :entries '(("h" . 192) ("f" . 1024))))
         (units (nelisp-link-layout-units
                 (list (nelisp-link-layout-test--entry) (nelisp-link-layout-test--unit "u.o")
                       (nelisp-link-unit-make "cold.o" (list (cons 'text (make-string 100 0))) nil nil))
                 order base))
         (r (nelisp-link-layout-test--link units base))
         (text (car r)) (st (cdr r))
         (h (nelisp-link-layout-test--va st "h")) (f (nelisp-link-layout-test--va st "f"))
         (g (nelisp-link-layout-test--va st "g")))
    (should (= (mod h 4096) 192))
    (should (= (mod f 4096) 1024))
    (should (< h f))
    (should (< f g))                    ; g is not listed: it stays in the cold part
    ;; f's call reaches g; g's jump reaches h-body = h+1; h's call reaches f.
    (should (= (nelisp-link-layout-test--rel32-target text base (+ (- f base) 1)) g))
    (should (= (nelisp-link-layout-test--rel32-target text base (+ (- g base) 3)) (1+ h)))
    (should (= (nelisp-link-layout-test--rel32-target text base (+ (- h base) 2)) f))
    ;; Pads are INT3 and never reached: the byte before h is 0xCC.
    (should (= (aref text (1- (- h base))) #xcc))))

(ert-deftest nelisp-link-layout-residue-independent-of-preceding-size ()
  (let ((vas nil))
    (dolist (pre '(0 64 1000 4096))
      (let* ((base #x401c00)
             (units (nelisp-link-layout-units
                     (list (nelisp-link-layout-test--entry)
                           (nelisp-link-unit-make "grown.o" (list (cons 'text (make-string (+ 64 pre) 0))) nil nil)
                           (nelisp-link-layout-test--unit "u.o"))
                     (list :modulus 4096 :entries '(("g" . 448)))
                     base))
             (st (cdr (nelisp-link-layout-test--link units base))))
        (push (mod (nelisp-link-layout-test--va st "g") 4096) vas)))
    (should (equal vas '(448 448 448 448)))))

(ert-deftest nelisp-link-layout-off-keeps-units ()
  (let ((units (list (nelisp-link-layout-test--entry) (nelisp-link-layout-test--unit "u.o"))))
    (should (eq (nelisp-link-layout-units units (list :modulus 4096 :entries nil) #x401000) units))
    (let ((nelisp-link-hot-layout nil))
      (should (eq (nelisp-link-layout-units units (list :modulus 4096 :entries '(("f" . 0))) #x401000)
                  units)))))

(ert-deftest nelisp-link-layout-residue-check-and-export ()
  (let* ((base #x401c00)
         (units (nelisp-link-layout-units
                 (list (nelisp-link-layout-test--entry) (nelisp-link-layout-test--unit "u.o"))
                 (list :modulus 4096 :entries '(("g" . 256))) base))
         (r (nelisp-link-layout-test--link units base)))
    ;; Internal piece symbols resolve relocations but are never exported.
    (should (cl-some (lambda (k) (nelisp-link-piece-symbol-p k)) (hash-table-keys (cdr r))))
    (should-not (cl-some (lambda (s) (nelisp-link-piece-symbol-p (plist-get s :name)))
                         (nelisp-link--export-symtab (cdr r) (list (cons 'text base)))))
    ;; Linking the same units at another base breaks the residue: refused.
    (should-error (nelisp-link-layout-test--link units (+ base 64)) :type 'nelisp-link-error)))

(ert-deftest nelisp-link-layout-read-hot-order ()
  (let ((f (make-temp-file "hot-order")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "# c\nmodulus 8192\nfoo 128\nbar 4096\n"))
          (should (equal (nelisp-link-read-hot-order f)
                         '(:modulus 8192 :entries (("foo" . 128) ("bar" . 4096)))))
          (with-temp-file f (insert "foo 100\n"))
          (should-error (nelisp-link-read-hot-order f) :type 'nelisp-link-error))
      (delete-file f))))

(ert-deftest nelisp-link-layout-single-function-unit-moves-whole ()
  (let* ((base #x401c00)
         (leaf (nelisp-link-unit-make "leaf.o" (list (cons 'text (unibyte-string #x90 #xc3)))
                                      (list (nelisp-link-symbol "leaf" 0 :section 'text :bind 'global))
                                      nil))
         (units (nelisp-link-layout-units
                 (list (nelisp-link-layout-test--entry)
                       (nelisp-link-unit-make "cold.o" (list (cons 'text (make-string 100 0))) nil nil)
                       leaf)
                 (list :modulus 4096 :entries '(("leaf" . 2048))) base))
         (st (cdr (nelisp-link-layout-test--link units base))))
    (should (equal (mapcar (lambda (u) (plist-get u :name)) units)
                   '("start.o" "hot-pad-1.o" "leaf.o" "cold.o")))
    (should (= (mod (nelisp-link-layout-test--va st "leaf") 4096) 2048))))

(provide 'nelisp-link-layout-test)
;;; nelisp-link-layout-test.el ends here
