;;; nelisp-eln-s6-pair-admission-test.el --- S6.16 pair second-body admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting a `gnu-eval-subr-pair' profile's
;; second (compiler-macro) body, the genuine vendor `zerop--anon-cmacro':
;;
;;   - the genuine 60-byte body matches exactly the `cons-form-constant'
;;     template (one Fcons import, slot 1119, and d_reloc[1] `='), and
;;     changing any single fixed byte of it matches no shape at all;
;;   - its spec is binary and slot 1119 is an authenticated fixed-arity-2
;;     runtime service;
;;   - only an exact multi-import proof that reads no
;;     f_symbols_with_pos_enabled_reloc cell is admitted as a second-body
;;     import proof; any other import shape fails closed;
;;   - the shared import table is sized for, and filled from, both proofs,
;;     and a second-body slot colliding with the first body's slot, or
;;     with the register/Feval slots, is rejected;
;;   - the second body's own lease (role plist :lease2) is valid only for
;;     the second body's capability, never for the first body's.
;;
;; Native memory is unavailable on host Emacs; `ptr-read-u64' /
;; `ptr-write-u64' / `nl-ffi-memory-address' are served from a hash table.
;; The end-to-end native path, including a tampered second body being
;; rejected by a real registration, is covered by the last test when
;; NELISP_S6_PAIR_BIN names a standalone binary and the genuine artifact
;; exists (see tools/ai/eln-progress.org S6.16).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-s6-pair-test--cmacro-hex
  (concat "488b05a12e0000534889f731f6488b18ff93f8220000bf02000000"
          "4889c6ff93f82200004889c6488b056a2e0000488b7808488b83f8"
          "2200005bffe0")
  "Genuine GNU 31.1 (ba35c031) bytes of vendor `zerop--anon-cmacro', as
emitted at vaddr #x1130 of ~/.cache/tmp/s6-survey-lex/zerop gnu-zerop.eln.")

(defun nelisp-eln-s6-pair-test--bytes ()
  "Return the genuine compiler-macro body as a unibyte string."
  (let* ((hex nelisp-eln-s6-pair-test--cmacro-hex)
         (bytes (make-string (/ (length hex) 2) 0)))
    (dotimes (i (length bytes))
      (aset bytes i (string-to-number (substring hex (* 2 i) (+ 2 (* 2 i)))
                                      16)))
    (string-to-unibyte bytes)))

;;; Exact template

(ert-deftest nelisp-eln-s6-pair-genuine-cmacro-body-matches ()
  (let ((analysis (nelisp-eln-tail-code-analyze-multi-import-call
                   (nelisp-eln-s6-pair-test--bytes) #x1130)))
    (should (eq (plist-get analysis :shape) 'cons-form-constant))
    (should (eq (plist-get analysis :proof) :multi-import-call))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1119)))
    ;; Both GOT loads resolve to the artifact's own GOT entries.
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (= (plist-get (car (plist-get analysis :data-relocations))
                          :got-vaddr)
               #x3fc8))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(1)))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s6-pair-tampered-cmacro-body-rejected ()
  "Every single changed fixed byte of the second body matches no shape."
  (let* ((bytes (nelisp-eln-s6-pair-test--bytes))
         (template (plist-get (cdr (assq 'cons-form-constant
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-tail-code-analyze-multi-import-call
                       tampered #x1130))
          (setq checked (1+ checked)))))
    (should (= checked 52))
    ;; A truncated or extended body is no shape either.
    (should-not (nelisp-eln-tail-code-analyze-multi-import-call
                 (substring bytes 0 59) #x1130))
    (should-not (nelisp-eln-tail-code-analyze-multi-import-call
                 (concat bytes (string-to-unibyte "\220")) #x1130))))

(ert-deftest nelisp-eln-s6-pair-spec-is-binary-and-authenticated ()
  (let ((spec (cdr (assq 'cons-form-constant
                         nelisp-eln-native-subr--multi-import-specs)))
        (d (nelisp-eln-native-subr--runtime-services-descriptor 1119)))
    (should (equal (plist-get spec :ports)
                   '((1119 fixed 2 (lisp lisp) lisp))))
    (should (equal (plist-get spec :constants) '((1 . =))))
    (should (= (nelisp-eln-native-subr-multi-arity
                (list :shape 'cons-form-constant))
               2))
    ;; Unary shapes keep arity 1.
    (should (= (nelisp-eln-native-subr-multi-arity
                (list :shape 'car-eq-constant))
               1))
    (should (eq (plist-get d :status) 'supported))
    (should (eq (plist-get d :convention) 'fixed))
    (should (equal (plist-get d :arity) 2))))

;;; Second-body import proof

(ert-deftest nelisp-eln-s6-pair-second-import-proof-admission ()
  (let ((multi (list :proof :multi-import-call :shape 'cons-form-constant
                     :imports (list (list :slot 1119)))))
    ;; No imports at all: nothing to authenticate.
    (should-not (nelisp-eln-registration--pair-second-imports
                 (list :kind 'simple)))
    (should (eq (nelisp-eln-registration--pair-second-imports
                 (list :kind 'multi :analysis multi))
                multi))
    ;; A multi proof reading the symbols-with-pos cell fails closed.
    (should-error (nelisp-eln-registration--pair-second-imports
                   (list :kind 'multi
                         :analysis (append multi
                                           '(:symbols-with-pos-address 4096))))
                  :type 'nelisp-eln-registration-error)
    ;; Any other import shape (its validator only knows the owner's
    ;; first lease) fails closed.
    (dolist (kind '(tail many cxr))
      (should-error (nelisp-eln-registration--pair-second-imports
                     (list :kind kind
                           :analysis (list :imports (list (list :slot 5)))))
                    :type 'nelisp-eln-registration-error))))

(defmacro nelisp-eln-s6-pair-test--with-memory (&rest body)
  "Run BODY with `ptr-read-u64'/`ptr-write-u64' over a hash table
`memory' and deterministic callback port entry addresses."
  `(let ((memory (make-hash-table :test 'eql)))
     (cl-letf (((symbol-function 'ptr-read-u64)
                (lambda (address offset)
                  (gethash (+ address offset) memory 0)))
               ((symbol-function 'ptr-write-u64)
                (lambda (address offset value)
                  (puthash (+ address offset) value memory)))
               ((symbol-function 'nelisp-eln-callable-import-port-entry-address)
                (lambda (port) (+ #x700000 (* 16 port))))
               ((symbol-function 'nelisp-eln-callable-import-entry-address)
                (lambda () #x600000)))
       ,@body)))

(ert-deftest nelisp-eln-s6-pair-import-table-holds-both-proofs ()
  (nelisp-eln-s6-pair-test--with-memory
   (let* ((first (list :proof :stack-call :imports (list (list :slot 1320))))
          (second (list :proof :multi-import-call
                        :imports (list (list :slot 1119))))
          (preflight (list :tail-imports first :tail-imports2 second))
          (slots (nelisp-eln-registration--expected-table-slots preflight)))
     (should (= slots 1321))
     (nelisp-eln-registration--install-second-imports preflight #x10000 slots)
     (should (= (gethash (+ #x10000 (* 8 1119)) memory) #x700000))
     ;; A wrongly sized table is refused.
     (should-error (nelisp-eln-registration--install-second-imports
                    preflight #x10000 (1+ slots))
                   :type 'nelisp-eln-registration-error)
     ;; The second body's slot may not reuse the first body's slot ...
     (should-error (nelisp-eln-registration--install-second-imports
                    (list :tail-imports first
                          :tail-imports2
                          (list :proof :multi-import-call
                                :imports (list (list :slot 1320))))
                    #x10000 slots)
                   :type 'nelisp-eln-registration-error)
     ;; ... nor the register/Feval callback slots.
     (let ((p (list :tail-imports2
                    (list :proof :multi-import-call
                          :imports (list (list :slot
                                               nelisp-eln-registration--eval-slot))))))
       (should-error (nelisp-eln-registration--install-second-imports
                      p #x10000
                      (nelisp-eln-registration--expected-table-slots p))
                     :type 'nelisp-eln-registration-error)))))

;;; Second-body lease

(ert-deftest nelisp-eln-s6-pair-second-lease-bound-to-second-body ()
  (nelisp-eln-s6-pair-test--with-memory
   (let* ((handle (list 'handle))
          (unit (vector 'unit handle))
          (table-owner (list 'table))
          (cap1 (list 'cap 'module #x1100 #x501100 nil nil 46 'root))
          (cap2 (list 'cap 'module #x1130 #x501130 nil nil 60 'root))
          (proof (list :safe t :proof :multi-import-call
                       :imports (list (list :slot 1119))))
          (entries (list (cons 1119 #x700000)))
          (owner (make-vector 20 nil))
          (lease (vector nelisp-eln-native-subr--tail-lease-marker
                         handle owner table-owner #x10000 #x20000
                         entries proof))
          (nelisp-eln-registration--owners (list owner))
          (nelisp-eln-registration--active-owner owner))
     (aset owner 1 unit)
     (aset owner 8 cap1)
     (aset owner 12 table-owner)
     (aset owner 18 (list :leaf-cap2 cap2 :lease2 lease))
     (puthash #x20000 #x10000 memory)
     (puthash (+ #x10000 (* 8 1119)) #x700000 memory)
     (cl-letf (((symbol-function 'nl-ffi-memory-address)
                (lambda (m) (and (eq m table-owner) #x10000))))
       (should (nelisp-eln-native-subr--multi-lease-valid-p
                lease handle cap2 t))
       ;; Never for the first body's capability.
       (should-not (nelisp-eln-native-subr--multi-lease-valid-p
                    lease handle cap1 t))
       ;; A lease the owner retains in neither place is refused.
       (aset owner 18 (list :leaf-cap2 cap2))
       (should-not (nelisp-eln-native-subr--multi-lease-valid-p
                    lease handle cap2 t))
       ;; A retained lease whose port was overwritten is refused.
       (aset owner 18 (list :leaf-cap2 cap2 :lease2 lease))
       (puthash (+ #x10000 (* 8 1119)) #x700010 memory)
       (should-not (nelisp-eln-native-subr--multi-lease-valid-p
                    lease handle cap2 t))))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s6-pair-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(ert-deftest nelisp-eln-s6-pair-e2e-tampered-second-body-rejected ()
  "A real registration refuses the genuine artifact with one changed
byte in the compiler-macro body, naming that body."
  (skip-unless (and (getenv "NELISP_S6_PAIR_BIN")
                    (file-executable-p (getenv "NELISP_S6_PAIR_BIN"))
                    (file-readable-p
                     (expand-file-name
                      "~/.cache/tmp/s6-survey-lex/zerop/overlay/eln/31.1-ba35c031/gnu-zerop.eln"))))
  (let* ((root nelisp-eln-s6-pair-test--root)
         (dir (make-temp-file "s6-pair-tamper" t))
         (eln (expand-file-name "gnu-zerop.eln" dir))
         (source (expand-file-name "~/.cache/tmp/s6-survey-lex/zerop/zerop.el")))
    (unwind-protect
        (progn
          (copy-file (expand-file-name
                      "~/.cache/tmp/s6-survey-lex/zerop/overlay/eln/31.1-ba35c031/gnu-zerop.eln")
                     eln)
          ;; File offset #x1146 is the compiler macro's `mov $0x2,%edi'
          ;; opcode (text is mapped at file offset = vaddr).
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally eln)
            (should (= (char-after (1+ #x1146)) #xbf))
            (goto-char (1+ #x1147))
            (delete-char 1)
            (insert 6)
            (let ((coding-system-for-write 'binary))
              (write-region nil nil eln)))
          (with-temp-buffer
            (let* ((process-environment
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S6_PAIR_BIN"))
                          process-environment))
                   (default-directory root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "zerop" "--source" source
                        "--corpus" "test/fixtures/s6-corpus/zerop.el")))
              (should-not (eql rc 0))
              (should (string-match-p
                       "leaf-instructions-not-admitted"
                       (buffer-string))))))
      (delete-directory dir t))))

(provide 'nelisp-eln-s6-pair-admission-test)

;;; nelisp-eln-s6-pair-admission-test.el ends here
