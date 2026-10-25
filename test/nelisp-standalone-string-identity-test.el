;;; nelisp-standalone-string-identity-test.el --- Stable String hash checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defvar nelisp-string-identity-test--memory nil)
(defvar nelisp-string-identity-test--headers nil)

(defun nelisp-string-identity-test--true (value)
  "Use native zero-is-false semantics for integer and host boolean tests."
  (and value (not (eql value 0))))

(defun nelisp-string-identity-test--definition (name)
  "Return the actual production definition of NAME without a generator scan."
  (or (cl-find name nelisp-standalone--applyfn-bf-helpers :key #'cadr)
      (cl-find name nelisp-standalone--applyfn-ht-helpers :key #'cadr)
      (cl-find name (cdr nelisp-standalone--arena-source) :key #'cadr)
      (cl-find name (cdr nelisp-cc-string-blocks--source) :key #'cadr)
      (error "Missing production definition: %S" name)))

(defun nelisp-string-identity-test--host (form)
  "Translate only native sequencing and shifts; retain production control flow."
  (if (atom form) form
    (pcase (car form)
      ('if `(if (nelisp-string-identity-test--true
                 ,(nelisp-string-identity-test--host (cadr form)))
                ,(nelisp-string-identity-test--host (caddr form))
              ,(nelisp-string-identity-test--host (cadddr form))))
      ('seq (cons 'progn (mapcar #'nelisp-string-identity-test--host (cdr form))))
      ('data-addr #x1000)
      ('sar `(ash ,(nelisp-string-identity-test--host (cadr form))
                  (- ,(nelisp-string-identity-test--host (caddr form)))))
      (_ (mapcar #'nelisp-string-identity-test--host form)))))

(defun nelisp-string-identity-test--read (address offset)
  (let ((value (gethash (+ address offset) nelisp-string-identity-test--memory 'absent)))
    (when (eq value 'absent) (error "Unmapped identity fixture read: %x" (+ address offset)))
    value))

(defun nelisp-string-identity-test--run (body &optional hash-mutation)
  "Execute BODY using production DSL functions and bounded checked memory."
  (let* ((names '(bf_identity_hash bf_eq2 nl_word_tag64 nl_val_tag nl_word_payload
                 nl_word_data nl_word_length nl_string_nbytes nl_string_view_header
                 nl_ht_identity_key_hash heap-read-u64 heap-read-u8 nl_string_header_p
                 heap-sexp-tag-word nl_positioned_flag nl_ht_key_hash nl_string_mapping_holding ptr-read-u64))
         (saved (mapcar (lambda (name) (cons name (and (fboundp name) (symbol-function name)))) names))
         (nelisp-string-identity-test--memory (make-hash-table :test 'eql))
         (nelisp-string-identity-test--headers '(#x500000 #x500020 #x500040 #x500060 #x500080 #x5000a0 #x5000c0 #x600000)))
    (unwind-protect
        (progn
          (dolist (name (cl-subseq names 0 10))
            (eval (nelisp-string-identity-test--host
                   (if (and hash-mutation (eq name 'bf_identity_hash))
                       hash-mutation (nelisp-string-identity-test--definition name))) t))
          (fset 'heap-read-u64 #'nelisp-string-identity-test--read)
          (fset 'heap-read-u8 #'nelisp-string-identity-test--read)
          (fset 'ptr-read-u64 #'nelisp-string-identity-test--read)
          (fset 'nl_string_header_p (lambda (s) (if (memq s nelisp-string-identity-test--headers) 1 0)))
          (fset 'heap-sexp-tag-word (lambda (w) (nl_val_tag w)))
          (fset 'nl_positioned_flag (lambda (_) 0))
          (fset 'nl_ht_key_hash (lambda (_key _depth) 997))
          (fset 'nl_string_mapping_holding
                (lambda (data)
                  (cond ((and (>= data #x6ff000) (< data #x701000)) #x6ff004)
                        ((and (>= data #x7ff000) (< data #x801000)) #x7ff004)
                        ((and (>= data #x710000) (< data #x710100)) #x710005)
                        (t 0))))
          (dolist (header nelisp-string-identity-test--headers)
            (puthash header 5 nelisp-string-identity-test--memory)
            (puthash (+ header 8) 5 nelisp-string-identity-test--memory)
            (puthash (+ header 24) (+ #x700000 (- header #x500000)) nelisp-string-identity-test--memory))
          (puthash #x500028 -1 nelisp-string-identity-test--memory)
          (puthash #x500040 0 nelisp-string-identity-test--memory)
          (puthash #x500048 0 nelisp-string-identity-test--memory)
          (puthash #x500060 0 nelisp-string-identity-test--memory)
          (puthash #x500068 -1 nelisp-string-identity-test--memory)
          (dolist (entry '((#x6ff008 . #x700100) (#x6ffff8 . #x500000)
                           (#x700018 . #x500020) (#x710008 . #x710100)
                           (#x710010 . #x5000a0) (#x5000b8 . #x710018)
                           (#x10a0 . #x600000) (#x10a8 . #x600020)
                           (#x10e8 . #x600100) (#x600018 . #x600030)
                           (#x600028 . #x600000) (#x700038 . #xdead)
                           (#x700058 . #x500000) (#x700000 . #xdead)
                           (#x6ff010 . #x5000c0) (#x5000d8 . #x6ff018)
                           (#x7ff008 . #x800100) (#x7ffff8 . #x500000)
                           (#x530000 . 781)))
            (puthash (car entry) (cdr entry) nelisp-string-identity-test--memory))
          (dolist (view `((#x400100 5 ,(logior #x500000 (ash 1 61) (ash 1 62)) #x700000 5)
                          (#x400120 5 #x500000 #x700000 5)
                          (#x400140 14 ,(logior #x500020 (ash 1 61)) #x700020 5)
                          (#x400160 14 #x500020 #x700020 5)
                          (#x400180 5 5 #x900000 5) (#x4001a0 5 5 #x900000 5)
                          (#x4001c0 5 0 0 0) (#x4001e0 14 0 0 0)
                          (#x400200 13 1 #x910008 3) (#x400220 13 0 0 0)
                          (#x400240 5 5 #x700000 5) (#x400260 14 5 #x700020 5)
                          (#x400280 5 5 #x600030 5) (#x4002a0 5 5 #x710018 5)
                          (#x4002c0 5 5 #x700001 5) (#x4002e0 5 5 #x6ff010 5)
                          (#x400300 5 5 #x700100 5) (#x400320 5 5 #x700040 5)
                          (#x400340 5 5 #x700060 5) (#x400360 5 5 #x700000 4)
                          (#x400380 5 5 #x600108 5) (#x4003a0 5 5 #x700008 5)
                          (#x4003c0 5 5 #x6ff018 5) (#x4003e0 5 5 #x600028 5)
                          (#x400400 5 5 #x600100 5)))
            (dotimes (i 4)
              (puthash (+ (car view) (* i 8)) (nth (1+ i) view) nelisp-string-identity-test--memory)))
          (funcall body))
      (dolist (entry saved)
        (if (cdr entry) (fset (car entry) (cdr entry)) (fmakunbound (car entry)))))))

(defun nelisp-string-identity-test--stable-data-and-length ()
  (let ((before (bf_identity_hash #x500004)))
    (puthash #x500018 #x800000 nelisp-string-identity-test--memory)
    (puthash #x400250 #x800000 nelisp-string-identity-test--memory)
    ;; GC repairs all retained canonical compatibility views as it moves data.
    (puthash #x400110 #x800000 nelisp-string-identity-test--memory)
    (puthash #x400130 #x800000 nelisp-string-identity-test--memory)
    (should (= before (bf_identity_hash #x500004)))
    (should (= before (bf_identity_hash #x400100)))
    (should (= before (bf_identity_hash #x400120)))
    (should (= before (bf_identity_hash #x400240)))
    (puthash #x500008 7 nelisp-string-identity-test--memory)
    (should (= before (bf_identity_hash #x500004)))))

(ert-deftest nelisp-string-identity-data-compaction-and-length ()
  (nelisp-string-identity-test--run #'nelisp-string-identity-test--stable-data-and-length))

(ert-deftest nelisp-string-identity-canonical-and-legacy-eq-law ()
  (nelisp-string-identity-test--run
   (lambda ()
     (dolist (pair '((#x500004 #x400100) (#x500004 #x400120)
                     (#x500024 #x400140) (#x500024 #x400160)
                     (#x400180 #x4001a0) (#x500004 #x400240)
                     (#x500024 #x400260) (#x600004 #x400280) (#x5000a4 #x4002a0)
                     (#x5000c4 #x4003c0)))
       (should (= 1 (bf_eq2 (car pair) (cadr pair))))
       (should (= (bf_identity_hash (car pair)) (bf_identity_hash (cadr pair)))))
     (should-not (= (bf_identity_hash #x500004) (bf_identity_hash #x500084)))
     (should (= (+ (ash #x900000 -3) (* 31 5)) (bf_identity_hash #x400180))))))

(ert-deftest nelisp-string-identity-reverse-owner-guards ()
  (nelisp-string-identity-test--run
   (lambda ()
     ;; Unaligned, block prefix, used-end, invalid owner, wrong data,
     ;; length mismatch, and outside image bounds retain legacy hashing.
     ;; Missing fixture words make any speculative pointer read fail.
     (dolist (word '(#x4002c0 #x4002e0 #x400300 #x400320 #x400340 #x400360 #x400380
                     #x4003a0 #x4003e0 #x400400))
       (should (= (+ (ash (nl_word_data word) -3) (* 31 (nl_word_length word)))
                  (bf_identity_hash word))))
     (puthash #x10a8 0 nelisp-string-identity-test--memory)
     (puthash #x10e8 0 nelisp-string-identity-test--memory)
     (should (= (+ (ash #x600030 -3) (* 31 5)) (bf_identity_hash #x400280))))))

(ert-deftest nelisp-string-identity-empty-and-bignum-paths ()
  (nelisp-string-identity-test--run
   (lambda ()
     (dolist (word '(#x500044 #x500064 #x4001c0 #x4001e0))
       (should (= 5 (bf_identity_hash word))))
     (should (= 13 (bf_identity_hash #x400220)))
     (should (= 13 (bf_identity_hash #x530006)))
     (should (= (+ (ash #x910008 -3) (* 31 3)) (bf_identity_hash #x400200)))
     (should (= (bf_identity_hash #x400200) (nl_ht_identity_key_hash #x400200 1)))
     (should (= 997 (nl_ht_identity_key_hash #x400200 2))))))

(ert-deftest nelisp-string-identity-movable-data-mutation-is-red ()
  (let* ((production (copy-tree (nelisp-string-identity-test--definition 'bf_identity_hash)))
         ;; Replace only the canonical-header return expression. This is
         ;; the old movable-data identity, not a mock implementation oracle.
         (mutation (cl-subst '(+ (sar (nl_word_data p) 3) (* 31 (nl_word_length p)))
                             '(sar header 3) production :test #'equal)))
    (should-not (equal production mutation))
    (should-error
     (nelisp-string-identity-test--run #'nelisp-string-identity-test--stable-data-and-length mutation)
     :type 'ert-test-failed)))

(provide 'nelisp-standalone-string-identity-test)
;;; nelisp-standalone-string-identity-test.el ends here
