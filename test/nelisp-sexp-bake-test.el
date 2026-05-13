;;; nelisp-sexp-bake-test.el --- ERT tests for Doc 95 §95.e bake integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 95 §95.e acceptance: read NELIMG v3 images produced by
;; `build-tool/src/bin/nelisp-baker.rs', re-emit via elisp, assert byte-
;; identity.  Also covers the dump-fixture cross-impl path and §95.c /
;; §95.d non-interference (= §95.e helpers consume a third wire format
;; orthogonal to the variable-length + 32-byte fixed serializers).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-sexp-bake-test--this-file
  (or load-file-name buffer-file-name
      (locate-library "nelisp-sexp-bake-test")
      ;; Final fallback for `emacs --batch -l test/...' where neither
      ;; of the above is set: assume CWD is the project root.
      (expand-file-name "test/nelisp-sexp-bake-test.el"
                        default-directory)))

(add-to-list 'load-path
             (expand-file-name
              "../lisp"
              (file-name-directory nelisp-sexp-bake-test--this-file)))
(require 'nelisp-sexp-dsl)

;;; --- helpers ------------------------------------------------------

(defun nelisp-sexp-bake-test--lisp-dir ()
  "Return the absolute path to lisp/ containing baked .image files."
  (expand-file-name
   "../lisp"
   (file-name-directory nelisp-sexp-bake-test--this-file)))

(defun nelisp-sexp-bake-test--tmp ()
  "Return a fresh temp file path under `temporary-file-directory'."
  (make-temp-file "nelisp-sexp-bake-fixture-" nil ".image"))

;;; --- envelope header parsing --------------------------------------

(ert-deftest nelisp-sexp-bake-magic-version-kind ()
  "Reader pulls magic / version / kind from a real baked image."
  (let* ((path (expand-file-name
                "nelisp-stdlib-hof.el.image"
                (nelisp-sexp-bake-test--lisp-dir)))
         (parsed (nelisp-sexp-bake-read-image path)))
    (should (plist-get parsed :magic-ok))
    (should (= (plist-get parsed :version) 3))
    (should (= (plist-get parsed :kind) 1))
    (should (= (plist-get parsed :n-nodes) 0))
    (should (= (plist-get parsed :n-globals) 0))
    (should (= (plist-get parsed :n-fallback-forms) 1))))

(ert-deftest nelisp-sexp-bake-fallback-form-extracted ()
  "Reader returns the embedded elisp source from fallback forms."
  (let* ((path (expand-file-name
                "nelisp-stdlib-hof.el.image"
                (nelisp-sexp-bake-test--lisp-dir)))
         (parsed (nelisp-sexp-bake-read-image path))
         (forms (plist-get parsed :fallback-forms)))
    (should (= (length forms) 1))
    (should (stringp (car forms)))
    ;; The hof source starts with its header comment.
    (should (string-prefix-p ";;; nelisp-stdlib-hof.el" (car forms)))))

;;; --- byte-identity on real baked output ---------------------------

(ert-deftest nelisp-sexp-bake-verify-single-real-image ()
  "One concrete real baked image round-trips byte-identical."
  (let ((path (expand-file-name
               "nelisp-stdlib-hof.el.image"
               (nelisp-sexp-bake-test--lisp-dir))))
    (should (eq t (nelisp-sexp-bake-verify-image path)))))

(ert-deftest nelisp-sexp-bake-verify-all-real-images ()
  "Every baked .image under lisp/ round-trips byte-identical.
This is the central acceptance test for Doc 95 §95.e (= the elisp
encoder mirrors the Rust `encode_v3_with_fallback' envelope path
exactly for the empty-env / single-fallback subset the baker emits
today)."
  (let* ((dir (nelisp-sexp-bake-test--lisp-dir))
         (images (directory-files dir t "\\.image\\'"))
         (failures nil))
    (should (> (length images) 0))
    (dolist (img images)
      (condition-case e
          (progn
            (nelisp-sexp-bake-verify-image img)
            t)
        (error (push (cons img e) failures))))
    (should (null failures))))

;;; --- dump-fixture round-trip --------------------------------------

(ert-deftest nelisp-sexp-bake-dump-fixture-round-trips ()
  "Encoded fixture survives read + re-encode."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (forms '("(defun foo () 1)" "(defun bar () 2)")))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture forms tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (equal forms (plist-get parsed :fallback-forms)))
            (should (eq t (nelisp-sexp-bake-verify-image tmp)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-dump-fixture-multibyte ()
  "UTF-8 multibyte fallback forms round-trip losslessly."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (forms '("(message \"日本語テスト\")"
                 ";; 漢字 αβγ 🚀")))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture forms tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (equal forms (plist-get parsed :fallback-forms)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-dump-fixture-empty ()
  "Empty fallback-forms list still produces a valid v3 envelope."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (nelisp-sexp-bake-dump-fixture nil tmp)
          (let ((parsed (nelisp-sexp-bake-read-image tmp)))
            (should (= (plist-get parsed :n-fallback-forms) 0))
            (should (null (plist-get parsed :fallback-forms)))
            (should (eq t (nelisp-sexp-bake-verify-image tmp)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; --- error path coverage ------------------------------------------

(ert-deftest nelisp-sexp-bake-bad-magic-rejected ()
  "Garbage magic header signals `nelisp-sexp-bake-bad-magic'."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert (make-string 32 ?X))))
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-bad-magic))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest nelisp-sexp-bake-truncated-rejected ()
  "Image shorter than the 21-byte header signals truncation."
  (let ((tmp (nelisp-sexp-bake-test--tmp)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file tmp
              (set-buffer-multibyte nil)
              (insert (unibyte-string ?N ?E ?L))))
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-truncated))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; --- cross-format non-interference (§95.c / §95.d / §95.e) --------

(ert-deftest nelisp-sexp-bake-format-distinct-from-95c ()
  "§95.c per-Sexp wire bytes do NOT carry the NELIMG v3 header."
  ;; Take an §95.c-encoded Int and confirm it has no NELIMG magic.
  (let ((bytes (nelisp-sexp-write-bytes (nelisp-sexp-make-int 42))))
    (should (stringp bytes))
    (should-not (string-prefix-p nelisp-sexp-bake-v3-magic bytes))))

(ert-deftest nelisp-sexp-bake-format-distinct-from-95d ()
  "§95.d 32-byte JIT record does NOT carry the NELIMG v3 header."
  (let* ((store (nelisp-sexp-jit-make-heap-store))
         (rec (nelisp-sexp-jit-write-bytes
               (nelisp-sexp-make-int 7) store)))
    (should (= (length rec) 32))
    (should-not (string-prefix-p nelisp-sexp-bake-v3-magic rec))))

(ert-deftest nelisp-sexp-bake-95c-95d-still-work-after-95e ()
  "Loading §95.e helpers does not regress §95.c / §95.d round-trip."
  ;; §95.c
  (let ((s (nelisp-sexp-make-cons (nelisp-sexp-make-int 1)
                                  (nelisp-sexp-make-int 2))))
    (should (nelisp-sexp-bytes-round-trip-p s)))
  ;; §95.d
  (should (nelisp-sexp-jit-round-trip-p (nelisp-sexp-make-int 99)))
  (should (nelisp-sexp-jit-round-trip-p (nelisp-sexp-make-float 1.5))))

;;; --- envelope subset boundary -------------------------------------

(ert-deftest nelisp-sexp-bake-unsupported-on-nonzero-nodes ()
  "Reader bails with `nelisp-sexp-bake-unsupported' on N_NODES > 0.
This guards the envelope-only assumption explicitly so future
multi-node baker output is caught at read time instead of silently
producing nonsense fallback strings."
  (let ((tmp (nelisp-sexp-bake-test--tmp))
        (coding-system-for-write 'no-conversion))
    (unwind-protect
        (progn
          ;; Hand-craft a header claiming N_NODES = 1 but with a single
          ;; truncated node tag = TAG_V3_NIL (0x00) so Pass 2 starts
          ;; reading nodes (= a real baker would never emit this, but
          ;; the gate fires before we even reach the malformed body).
          (with-temp-file tmp
            (set-buffer-multibyte nil)
            (insert nelisp-sexp-bake-v3-magic)         ; 8 bytes
            (insert (unibyte-string 3 0 0 0))          ; version = 3
            (insert (unibyte-string 1))                ; kind = 1
            (insert (unibyte-string 1 0 0 0))          ; N_NODES = 1
            (insert (unibyte-string 0))                ; tag = TAG_V3_NIL
            (insert (unibyte-string 0 0 0 0))          ; N_GLOBALS = 0
            (insert (unibyte-string 0 0 0 0)))         ; N_FALLBACK = 0
          (should-error (nelisp-sexp-bake-read-image tmp)
                        :type 'nelisp-sexp-bake-unsupported))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; --- §95.f full encoder / decoder round-trip ----------------------

(defconst nelisp-sexp-bake-test--minimal-empty
  (nelisp-sexp-bake-encode-full nil nil nil)
  "A minimal NELIMG v3 image with 0 nodes / 0 globals / 0 fallback.
25 bytes total = 8 magic + 4 version + 1 kind + 4 N_NODES
+ 4 N_GLOBALS + 4 N_FALLBACK.")

(ert-deftest nelisp-sexp-bake-full-encode-empty-image ()
  "Empty `encode-full' produces a 25-byte well-formed image."
  (let ((bytes nelisp-sexp-bake-test--minimal-empty))
    (should (= (length bytes) 25))
    (should (equal (substring bytes 0 8) nelisp-sexp-bake-v3-magic))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-decode-empty-image ()
  "Decoder returns empty :nodes / :globals / :fallback-forms."
  (let* ((bytes nelisp-sexp-bake-test--minimal-empty)
         (parsed (nelisp-sexp-bake-decode-full bytes)))
    (should (= (plist-get parsed :version) 3))
    (should (= (plist-get parsed :kind) 1))
    (should (null (plist-get parsed :nodes)))
    (should (null (plist-get parsed :globals)))
    (should (null (plist-get parsed :fallback-forms)))))

(ert-deftest nelisp-sexp-bake-full-two-atomic-nodes-round-trip ()
  "Encode + decode an image with two atomic nodes (= Nil, Int)."
  (let* ((nodes (list (list :tag 'nil)
                      (list :tag 'int :value 42)))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
      (should (equal (plist-get parsed :nodes) nodes)))))

(ert-deftest nelisp-sexp-bake-full-all-atomic-tags ()
  "Every atomic node tag round-trips byte-identical."
  (let* ((nodes
          (list (list :tag 'nil)
                (list :tag 't)
                (list :tag 'int :value -123456789012345)
                (list :tag 'float :bits #x4008000000000000) ; = 3.0
                (list :tag 'symbol :name "foo-bar")
                (list :tag 'string :value "hello")
                (list :tag 'mut-str :value "mut")))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
      (should (equal (plist-get parsed :nodes) nodes)))))

(ert-deftest nelisp-sexp-bake-full-cons-references-children ()
  "CONS node with car/cdr indices round-trips with linkage intact."
  (let* ((nodes (list (list :tag 'int :value 1)         ; idx 0
                      (list :tag 'int :value 2)         ; idx 1
                      (list :tag 'cons :car 0 :cdr 1))) ; idx 2
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
      (should (equal (plist-get parsed :nodes) nodes)))))

(ert-deftest nelisp-sexp-bake-full-deep-cons-tree-10-levels ()
  "10-level-deep right-nested cons spine round-trips identically.
Builds (1 . (2 . (3 . ... (10 . nil)))) via direct node records."
  (let* ((nil-idx 0)
         (nodes (list (list :tag 'nil)))
         (next-idx 1)
         (last-cdr nil-idx))
    ;; Walk from 10 down to 1, inserting int + cons pairs.
    (cl-loop for n from 10 downto 1 do
             (let ((int-idx next-idx)
                   (cons-idx (1+ next-idx)))
               (setq nodes
                     (append nodes
                             (list (list :tag 'int :value n)
                                   (list :tag 'cons
                                         :car int-idx
                                         :cdr last-cdr))))
               (setq last-cdr cons-idx)
               (setq next-idx (+ next-idx 2))))
    (let ((bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
      (should (eq t (nelisp-sexp-bake-verify-full bytes)))
      (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
        (should (= (length (plist-get parsed :nodes))
                   (1+ (* 10 2))))))))

(ert-deftest nelisp-sexp-bake-full-shared-sub-tree ()
  "Two CONS nodes referencing the same child idx round-trip."
  (let* ((nodes (list (list :tag 'int :value 7)             ; 0
                      (list :tag 'cons :car 0 :cdr 0)       ; 1
                      (list :tag 'cons :car 0 :cdr 1)))     ; 2
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-vector-with-items ()
  "VECTOR node referencing index list round-trips."
  (let* ((nodes (list (list :tag 'int :value 1)
                      (list :tag 'int :value 2)
                      (list :tag 'int :value 3)
                      (list :tag 'vector :items '(0 1 2 0 1 2))))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-bool-vector ()
  "BOOL_VECTOR with mixed bits round-trips bit-faithfully."
  (let* ((nodes (list (list :tag 'bool-vector
                            :bits (list t nil t t nil nil t))))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let* ((parsed (nelisp-sexp-bake-decode-full bytes))
           (first (car (plist-get parsed :nodes))))
      (should (equal (plist-get first :bits)
                     (list t nil t t nil nil t))))))

(ert-deftest nelisp-sexp-bake-full-cell-and-record ()
  "CELL + RECORD nodes referencing children round-trip."
  (let* ((nodes (list (list :tag 'symbol :name "my-rec")    ; 0
                      (list :tag 'int :value 11)            ; 1
                      (list :tag 'int :value 22)            ; 2
                      (list :tag 'cell :idx 1)              ; 3
                      (list :tag 'record :type-tag 0
                            :slots '(1 2 3))))              ; 4
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-char-table-minimal ()
  "Minimal CHAR_TABLE node (= no entries, no parent) round-trips."
  (let* ((nodes (list (list :tag 'nil)                        ; 0
                      (list :tag 'char-table
                            :subtype 0
                            :default-val 0
                            :entries nil
                            :parent nil
                            :extra nil)))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-char-table-with-entries ()
  "CHAR_TABLE with non-empty entries map + parent chain round-trips."
  (let* ((nodes (list (list :tag 'nil)                        ; 0
                      (list :tag 'int :value 99)              ; 1
                      (list :tag 'int :value 100)             ; 2
                      (list :tag 'char-table
                            :subtype 0
                            :default-val 1
                            :entries '((65 . 2) (97 . 1))
                            :parent (list :tag 'char-table
                                          :subtype 0
                                          :default-val 2
                                          :entries nil
                                          :parent nil
                                          :extra nil)
                            :extra '(1 2))))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

(ert-deftest nelisp-sexp-bake-full-globals-section ()
  "Globals section with value / function / plist / constant round-trips."
  (let* ((nodes (list (list :tag 'int :value 1)            ; 0
                      (list :tag 'int :value 2)            ; 1
                      (list :tag 'int :value 3)))          ; 2
         (globals (list (list :name "alpha"
                              :value 0 :function nil :plist nil
                              :constant nil)
                        (list :name "beta"
                              :value 0 :function 1 :plist 2
                              :constant t)
                        (list :name "gamma"
                              :value nil :function nil :plist nil
                              :constant t)))
         (bytes (nelisp-sexp-bake-encode-full nodes globals nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
      (should (= (length (plist-get parsed :globals)) 3))
      (should (equal (plist-get (nth 1 (plist-get parsed :globals))
                                :name)
                     "beta")))))

(ert-deftest nelisp-sexp-bake-full-fallback-forms-section ()
  "FALLBACK_FORMS section coexists with node + global sections."
  (let* ((nodes (list (list :tag 't)))
         (globals nil)
         (forms '("(message \"a\")" "(message \"b\")"))
         (bytes (nelisp-sexp-bake-encode-full nodes globals forms)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))
    (let ((parsed (nelisp-sexp-bake-decode-full bytes)))
      (should (equal (plist-get parsed :fallback-forms) forms)))))

;;; --- §95.f Phase A DSL interner -----------------------------------

(ert-deftest nelisp-sexp-bake-intern-atomic-dedupe ()
  "Atomic DSL values dedupe structurally during intern."
  (let ((s1 (nelisp-sexp-make-int 42))
        (s2 (nelisp-sexp-make-int 42)))
    (let ((nodes (nelisp-sexp-bake-intern-dsl (list s1 s2))))
      ;; Both interns hit the same atomic key (= one node total).
      (should (= (length nodes) 1))
      (should (equal (car nodes) (list :tag 'int :value 42))))))

(ert-deftest nelisp-sexp-bake-intern-cons-eq-dedupe ()
  "Same cons plist (by eq) dedupes; structurally-equal copies do not."
  (let* ((c1 (nelisp-sexp-make-cons
              (nelisp-sexp-make-int 1) (nelisp-sexp-make-int 2)))
         (c2 c1)                  ; same eq identity
         (c3 (nelisp-sexp-make-cons
              (nelisp-sexp-make-int 1) (nelisp-sexp-make-int 2))))
    (let ((nodes-shared
           (nelisp-sexp-bake-intern-dsl (list c1 c2))))
      ;; c1 and c2 are eq → 1 cons + 2 atomic int = 3 nodes total.
      (should (= (length nodes-shared) 3)))
    (let ((nodes-distinct
           (nelisp-sexp-bake-intern-dsl (list c1 c3))))
      ;; c1 and c3 are structurally equal but eq-distinct → 2 cons.
      ;; Atomic int 1 + int 2 dedupe, so 2 + 2 = 4 nodes total.
      (should (= (length nodes-distinct) 4)))))

(ert-deftest nelisp-sexp-bake-intern-full-pipeline ()
  "Intern + encode + decode + re-encode = byte identity."
  (let* ((dsl (list (nelisp-sexp-make-cons
                     (nelisp-sexp-make-int 1)
                     (nelisp-sexp-make-int 2))
                    (nelisp-sexp-make-vector
                     (list (nelisp-sexp-make-symbol 'foo)
                           (nelisp-sexp-make-str "bar")))))
         (nodes (nelisp-sexp-bake-intern-dsl dsl))
         (bytes (nelisp-sexp-bake-encode-full nodes nil nil)))
    (should (eq t (nelisp-sexp-bake-verify-full bytes)))))

;;; --- §95.f compat with §95.e envelope-only path -------------------

(ert-deftest nelisp-sexp-bake-full-real-images-still-round-trip ()
  "Every baked .image (envelope-only today) round-trips via full codec."
  (let* ((dir (nelisp-sexp-bake-test--lisp-dir))
         (images (directory-files dir t "\\.image\\'"))
         (failures nil))
    (should (> (length images) 0))
    (dolist (img images)
      (condition-case e
          (progn
            (nelisp-sexp-bake-verify-image-full img)
            t)
        (error (push (cons img e) failures))))
    (should (null failures))))

(ert-deftest nelisp-sexp-bake-full-vs-envelope-byte-equal ()
  "§95.f full encoder produces same bytes as §95.e envelope encoder
when N_NODES = 0 / N_GLOBALS = 0 (= they describe the same image)."
  (let* ((forms '("(+ 1 2)"))
         (envelope (nelisp-sexp-bake--encode-envelope forms))
         (full (nelisp-sexp-bake-encode-full nil nil forms)))
    (should (equal envelope full))))

;;; --- §95.f error paths --------------------------------------------

(ert-deftest nelisp-sexp-bake-full-unknown-tag-rejected ()
  "Decoder rejects an unknown node tag byte."
  (let* ((bytes (concat nelisp-sexp-bake-v3-magic
                        (unibyte-string 3 0 0 0)   ; version = 3
                        (unibyte-string 1)         ; kind = 1
                        (unibyte-string 1 0 0 0)   ; N_NODES = 1
                        (unibyte-string #xFF)      ; bogus tag
                        (unibyte-string 0 0 0 0)   ; N_GLOBALS = 0
                        (unibyte-string 0 0 0 0)))) ; N_FALLBACK = 0
    (should-error (nelisp-sexp-bake-decode-full bytes)
                  :type 'nelisp-sexp-bake-unknown-tag)))

(ert-deftest nelisp-sexp-bake-full-malformed-node-on-encode ()
  "Encoder rejects a node plist missing the :tag keyword."
  (should-error
   (nelisp-sexp-bake-encode-full
    (list (list :value 42)) nil nil)
   :type 'nelisp-sexp-bake-malformed-node))

(provide 'nelisp-sexp-bake-test)

;;; nelisp-sexp-bake-test.el ends here
