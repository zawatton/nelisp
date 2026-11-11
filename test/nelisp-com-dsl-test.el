;;; nelisp-com-dsl-test.el --- ERT tests for the COM DLL DSL  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stage S2 unit tests: GUID byte layout, UTF-16 blobs, vtable layout and
;; relocations, the QueryInterface table and the object layout.  The wine
;; end-to-end proof lives in tools/ai/com-dll/wine-e2e.sh.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (root (and this (expand-file-name ".." (file-name-directory this)))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "tools/ai/com-dll" root))))

(require 'nelisp-com-dsl)
(require 'com-sample)

(defun nelisp-com-test--flatten (form)
  (if (consp form)
      (append (nelisp-com-test--flatten (car form))
              (nelisp-com-test--flatten (cdr form)))
    (and form (list form))))

(defun nelisp-com-test--blob (forms name)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'data-blob) (eq (nth 1 f) name)))
              forms))

(defun nelisp-com-test--defun (forms name)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (nth 1 f) name)))
              forms))

(ert-deftest nelisp-com-guid-bytes-golden ()
  (should (equal (nelisp-com-guid-bytes nelisp-com-iid-iunknown)
                 '(0 0 0 0 0 0 0 0 #xc0 0 0 0 0 0 0 #x46)))
  ;; D1/D2/D3 little-endian, trailing 8 bytes verbatim.
  (should (equal (nelisp-com-guid-bytes "{01234567-89AB-CDEF-0123-456789abcdef}")
                 '(#x67 #x45 #x23 #x01 #xab #x89 #xef #xcd
                   #x01 #x23 #x45 #x67 #x89 #xab #xcd #xef)))
  (should (equal (nelisp-com-guid-bytes nelisp-com-sample-clsid)
                 '(#xe0 #x0d #x1c #x7a #x3a #x5f #x6e #x4a
                   #x9d #x11 #x01 #x23 #x45 #x67 #x89 #xab)))
  (should-error (nelisp-com-guid-bytes "{1234}")))

(ert-deftest nelisp-com-wstr-utf16le ()
  (should (equal (nelisp-com-wstr "") '(0 0)))
  (should (equal (nelisp-com-wstr "Ab") '(65 0 98 0 0 0)))
  ;; BMP CJK: U+3042 -> 42 30.
  (should (equal (nelisp-com-wstr "あ") '(#x42 #x30 0 0)))
  ;; Non-BMP U+1F600 -> surrogate pair D83D DE00.
  (should (equal (nelisp-com-wstr (string #x1f600))
                 '(#x3d #xd8 #x00 #xde 0 0)))
  (should (equal (nelisp-com-wstr "a\\b") '(97 0 92 0 98 0 0 0))))

(ert-deftest nelisp-com-blob-forms ()
  (should (equal (nelisp-com-guid-blob 'g nelisp-com-iid-iunknown)
                 `(data-blob g ,(nelisp-com-guid-bytes nelisp-com-iid-iunknown) rodata)))
  (should (equal (nelisp-com-wstr-blob 'w "x") '(data-blob w (120 0 0 0) rodata))))

(ert-deftest nelisp-com-vtable-layout-and-relocs ()
  (let ((vt (nelisp-com-vtable 'vt '(f0 f1 f2 f3))))
    (should (eq (car vt) 'data-blob))
    (should (equal (nth 2 vt) (make-list 32 0)))
    ;; Vtables must live in `data' (never rodata) and carry one abs64
    ;; reloc per slot at 8-byte strides with zero addend.
    (should (eq (nth 3 vt) 'data))
    (should (equal (nth 4 vt) '((0 f0 0) (8 f1 0) (16 f2 0) (24 f3 0))))))

(ert-deftest nelisp-com-call-and-guid-compare-forms ()
  (should (equal (nelisp-com-call 'obj 3 'a 'b)
                 '(call-ptr (ptr-read-u64 (ptr-read-u64 obj 0) 24) obj a b)))
  (should (equal (nelisp-com-guid= 'riid 'g)
                 '(and (= (ptr-read-u64 riid 0) (ptr-read-u64 (data-addr g) 0))
                       (= (ptr-read-u64 riid 8) (ptr-read-u64 (data-addr g) 8)))))
  (should-error (nelisp-com-call '(foo) 1))
  (should-error (nelisp-com-guid= '(foo) 'g)))

(ert-deftest nelisp-com-object-layout-offsets ()
  (should (equal (mapcar #'nelisp-com-iface-offset '(0 1 2 3)) '(0 16 24 32)))
  (should (= (nelisp-com-state-offset 1) 16))
  (should (= (nelisp-com-state-offset 3) 32))
  (should (equal (nelisp-com-base-form 0 'this) 'this))
  (should (equal (nelisp-com-base-form 2 'this) '(- this 24))))

(defun nelisp-com-test--multi-class ()
  (nelisp-com-class-forms
   'k :state-size 24
   :interfaces '((:iid "{aa80e7f7-2021-11d2-93e0-0060b067b86e}" :methods (k_act k_deact))
                 (:iid "{aa80e7f5-2021-11d2-93e0-0060b067b86e}" :methods (k_a k_b k_c k_d k_e k_f))
                 (:iid "{aa80e7f0-2021-11d2-93e0-0060b067b86e}" :methods ()))))

(ert-deftest nelisp-com-class-vtables-per-interface ()
  (let ((forms (nelisp-com-test--multi-class)))
    ;; Interface 0 uses the shared base functions; i>=1 use thunks.
    (should (equal (nth 4 (nelisp-com-test--blob forms 'k_vt_0))
                   '((0 k_qi 0) (8 k_addref 0) (16 k_release 0)
                     (24 k_act 0) (32 k_deact 0))))
    (let ((v1 (nelisp-com-test--blob forms 'k_vt_1)))
      (should (= (length (nth 2 v1)) (* 8 9)))
      (should (equal (cl-subseq (nth 4 v1) 0 3)
                     '((0 k_i1_qi 0) (8 k_i1_addref 0) (16 k_i1_release 0))))
      (should (eq (nth 3 v1) 'data)))
    (should (= (length (nth 2 (nelisp-com-test--blob forms 'k_vt_2))) 24))
    ;; Thunks rebase `this' by the interface offset (16 and 24).
    (should (equal (nth 3 (nelisp-com-test--defun forms 'k_i1_release)) '(k_release (- this 16))))
    (should (equal (nth 3 (nelisp-com-test--defun forms 'k_i2_addref)) '(k_addref (- this 24))))))

(ert-deftest nelisp-com-qi-table ()
  (let* ((forms (nelisp-com-test--multi-class))
         (qi (nelisp-com-test--defun forms 'k_qi))
         (flat (nelisp-com-test--flatten qi)))
    (should (equal (car (nth 2 qi)) 'this))
    ;; One GUID comparison per interface plus IUnknown, in order.
    (should (equal (cl-remove-if-not (lambda (x) (memq x '(k_iid_unk k_iid_0 k_iid_1 k_iid_2)))
                                     flat)
                   '(k_iid_unk k_iid_unk k_iid_0 k_iid_0 k_iid_1 k_iid_1 k_iid_2 k_iid_2)))
    ;; Matching interface i hands out this + offset(i); IUnknown -> interface 0.
    (should (member '(k_qi_ok this ppv (+ this 16)) (nelisp-com-test--flatten-subforms qi)))
    (should (member '(k_qi_ok this ppv (+ this 24)) (nelisp-com-test--flatten-subforms qi)))
    (should (member '(k_qi_ok this ppv this) (nelisp-com-test--flatten-subforms qi)))
    ;; Fallthrough is E_NOINTERFACE with *ppv cleared; null ppv is E_POINTER.
    (should (memq nelisp-com-e-nointerface flat))
    (should (memq nelisp-com-e-pointer flat))
    ;; Each IID blob exists with the right bytes.
    (should (equal (nth 2 (nelisp-com-test--blob forms 'k_iid_1))
                   (nelisp-com-guid-bytes "{aa80e7f5-2021-11d2-93e0-0060b067b86e}")))))

(defun nelisp-com-test--flatten-subforms (form)
  "Return FORM and all its list sub-forms."
  (when (consp form)
    (cons form (cl-mapcan #'nelisp-com-test--flatten-subforms
                          (copy-sequence (cl-remove-if-not #'consp form))))))

(ert-deftest nelisp-com-class-allocation-size ()
  (let* ((forms (nelisp-com-test--multi-class))
         (new (nelisp-com-test--defun forms 'k_new))
         (subs (nelisp-com-test--flatten-subforms new)))
    ;; 3 interfaces: header 8+8*3 = 32, plus 24 bytes of state = 56.
    (should (member '(extern-call HeapAlloc h 8 56) subs))
    (should (member '(ptr-write-u64 o 16 (data-addr k_vt_1)) subs))
    (should (member '(ptr-write-u64 o 24 (data-addr k_vt_2)) subs))
    (should (member '(ptr-write-u64 o 8 1) subs))))

(ert-deftest nelisp-com-server-forms-shape ()
  (let* ((forms (nelisp-com-sample-dll-forms))
         (defuns (cl-loop for f in forms when (eq (car-safe f) 'defun) collect (nth 1 f))))
    (dolist (e nelisp-com-server-exports) (should (memq (intern e) defuns)))
    (should (equal (nth 2 (nelisp-com-test--blob forms 'o_w_key))
                   (nelisp-com-wstr (concat "CLSID\\" nelisp-com-sample-clsid
                                            "\\InprocServer32"))))
    ;; Static factory object points at its vtable through an abs64 reloc.
    (should (equal (nth 4 (nelisp-com-test--blob forms 'o_fac)) '((0 o_fvt 0))))
    (should (equal (length (nth 2 (nelisp-com-test--blob forms 'o_fvt))) 40))))

;;;; Linked image checks

(defun nelisp-com-test--u (bytes off n)
  (let ((v 0)) (dotimes (i n) (setq v (logior v (ash (aref bytes (+ off i)) (* 8 i))))) v))

(defun nelisp-com-test--build (&rest keys)
  (let* ((dir (make-temp-file "com-ert" t))
         (path (expand-file-name "t.dll" dir)))
    (unwind-protect
        (progn (apply #'nelisp-com-build-dll path (nelisp-com-sample-dll-forms) keys)
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally path)
                 (buffer-string)))
      (delete-directory dir t))))

(defun nelisp-com-test--reloc-dir-size (bytes)
  (let* ((pe (nelisp-com-test--u bytes #x3c 4))
         (opt (+ pe 24)))
    (nelisp-com-test--u bytes (+ opt 112 (* 8 5) 4) 4)))

(ert-deftest nelisp-com-linked-dll-exports-and-relocs ()
  (let ((img (nelisp-com-test--build)))
    (dolist (e nelisp-com-server-exports)
      (should (string-match-p (regexp-quote e) (decode-coding-string img 'latin-1))))
    ;; 4 + 5 vtable slots and the factory vtable pointer are DIR64 relocs
    ;; (10 entries x 2 bytes plus block headers): the directory is non-empty.
    (should (> (nelisp-com-test--reloc-dir-size img) 8))))

(ert-deftest nelisp-com-no-reloc-hook-drops-reloc-directory ()
  (should (= 0 (nelisp-com-test--reloc-dir-size
                (nelisp-com-test--build :no-reloc t)))))

(ert-deftest nelisp-com-drop-export-hook ()
  (let ((img (decode-coding-string
              (nelisp-com-test--build :exports '("DllCanUnloadNow" "DllRegisterServer"
                                                 "DllUnregisterServer"))
              'latin-1)))
    (should-not (string-match-p "DllGetClassObject" img))
    (should (string-match-p "DllCanUnloadNow" img))))

(provide 'nelisp-com-dsl-test)
;;; nelisp-com-dsl-test.el ends here
