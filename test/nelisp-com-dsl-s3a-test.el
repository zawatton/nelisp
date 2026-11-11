;;; nelisp-com-dsl-s3a-test.el --- ERT tests for the S3a-1 COM DSL additions  -*- lexical-binding: t; -*-

;;; Commentary:

;; Golden tests for structs, the HRESULT `try' helper, the msctf GUID table,
;; :fields / :extra-iids classes, registration hives and the wbuf library
;; forms.  Run-time behaviour is proven under wine by
;; tools/ai/com-dll/wbuf-e2e.sh and reg-e2e.sh.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (root (and this (expand-file-name ".." (file-name-directory this)))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "tools/ai/com-dll" root))))

(require 'nelisp-com-dsl)
(require 'nelisp-com-wstr-lib)
(require 'wbuf-sample)

(defun s3a-test--blob (forms name)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'data-blob) (eq (nth 1 f) name)))
              forms))

(defun s3a-test--defun (forms name)
  (cl-find-if (lambda (f) (and (eq (car-safe f) 'defun) (eq (nth 1 f) name)))
              forms))

(defun s3a-test--subforms (form)
  (when (consp form)
    (cons form (cl-mapcan #'s3a-test--subforms
                          (copy-sequence (cl-remove-if-not #'consp form))))))

;;;; Structs (sizes and offsets of the Win64 C layouts)

(ert-deftest s3a-struct-golden-layouts ()
  (cl-loop for (name size . offs) in
           '((TF_SELECTION 16 (range . 0) (style . 8))
             (TF_SELECTIONSTYLE 8 (ase . 0) (fInterimChar . 4))
             (TF_DISPLAYATTRIBUTE 36 (crText . 0) (crBk . 8) (lsStyle . 16)
                                  (fBoldLine . 20) (crLine . 24) (bAttr . 32))
             (TF_LANGUAGEPROFILE 56 (clsid . 0) (langid . 16) (catid . 20)
                                 (fActive . 36) (guidProfile . 40))
             (POINT 8 (x . 0) (y . 4))
             (RECT 16 (left . 0) (top . 4) (right . 8) (bottom . 12))
             (MSG 48 (hwnd . 0) (message . 8) (wParam . 16) (lParam . 24)
                  (time . 32) (pt . 36))
             (WNDCLASSEXW 80 (cbSize . 0) (style . 4) (lpfnWndProc . 8)
                          (cbClsExtra . 16) (cbWndExtra . 20) (hInstance . 24)
                          (hIcon . 32) (hCursor . 40) (hbrBackground . 48)
                          (lpszMenuName . 56) (lpszClassName . 64) (hIconSm . 72)))
           do (should (= (nelisp-com-struct-size name) size))
           (dolist (o offs)
             (should (= (nelisp-com-struct-offset name (car o)) (cdr o))))))

(ert-deftest s3a-struct-padding-and-alignment ()
  (nelisp-com-defstruct s3a_pad (a u8) (b u32) (c u8) (d u64) (e u16))
  (should (equal (mapcar (lambda (f) (nelisp-com-struct-offset 's3a_pad f))
                         '(a b c d e))
                 '(0 4 8 16 24)))
  (should (= (nelisp-com-struct-size 's3a_pad) 32))
  (should (= (nelisp-com-struct-align 's3a_pad) 8))
  (nelisp-com-defstruct s3a_arr (n u8) (v (array u16 3)) (g guid) (t0 u8))
  (should (equal (mapcar (lambda (f) (nelisp-com-struct-offset 's3a_arr f))
                         '(n v g t0))
                 '(0 2 8 24)))
  (should (= (nelisp-com-struct-size 's3a_arr) 28))
  (should-error (nelisp-com-struct-offset 's3a_arr 'nope))
  (should-error (nelisp-com-defstruct s3a_bad (a float)))
  (should-error (nelisp-com-struct-size 's3a_missing)))

(ert-deftest s3a-field-accessor-forms ()
  (should (equal (nelisp-com-field 'p 'TF_SELECTION 'range) '(ptr-read-u64 p 0)))
  (should (equal (nelisp-com-set-field 'p 'RECT 'bottom 9)
                 '(ptr-write-u32 p 12 9)))
  ;; signed fields read sign-extended, write the unsigned width.
  (should (equal (nelisp-com-field 'p 'RECT 'top) '(ptr-read-s32 p 4)))
  (should (equal (nelisp-com-set-field 'p 'WNDCLASSEXW 'hIconSm 'v)
                 '(ptr-write-u64 p 72 v)))
  (should (equal (nelisp-com-field-addr 'p 'TF_SELECTION 'style) '(+ p 8)))
  (should (equal (nelisp-com-field-addr 'p 'TF_SELECTION 'range) 'p))
  (should-error (nelisp-com-field 'p 'TF_SELECTION 'style))
  (should-error (nelisp-com-field 'p 'TF_SELECTION 'nope)))

(ert-deftest s3a-struct-alloc-free-forms ()
  (let ((forms (nelisp-com-struct-forms 'TF_DISPLAYATTRIBUTE)))
    (should (member '(extern-call HeapAlloc h 8 36)
                    (s3a-test--subforms (s3a-test--defun
                                         forms 'TF_DISPLAYATTRIBUTE_alloc))))
    (should (s3a-test--defun forms 'TF_DISPLAYATTRIBUTE_free))))

;;;; HRESULT helpers

(ert-deftest s3a-hresult-constants ()
  (should (= nelisp-com-e-invalidarg 2147942487))
  (should (= nelisp-com-e-unexpected 2147549183))
  (should (= nelisp-com-e-fail 2147500037))
  (should (= nelisp-com-e-nointerface 2147500034))
  (should (= nelisp-com-class-e-noaggregation 2147746064))
  (should (= nelisp-com-e-outofmemory 2147942414))
  (should (= (+ nelisp-com-tf-es-sync nelisp-com-tf-es-readwrite) 7))
  (should (= (nelisp-com-hresult-from-win32 5) nelisp-com-e-accessdenied))
  (should (= (nelisp-com-hresult-from-win32 0) 0))
  (should (< nelisp-com-tf-e-locked 4294967296))
  (should (= nelisp-com-tf-e-locked #x80040600)))

(ert-deftest s3a-try-expansion ()
  (should (equal (nelisp-com-failed 'hr) '(/= (logand hr 2147483648) 0)))
  (should (equal (nelisp-com-succeeded '(f)) '(= (logand (f) 2147483648) 0)))
  ;; No bindings: just the result.
  (should (equal (nelisp-com-try nil 0) 0))
  ;; One binding, no cleanup: the failing HRESULT is returned as is.
  (should (equal (nelisp-com-try '((a (f))) 0)
                 '(let ((a (f))) (if (/= (logand a 2147483648) 0) a 0))))
  ;; Two bindings with cleanup: nested, cleanup in the failure arm of each.
  (should (equal (nelisp-com-try '((a (f)) (b (g a))) 'ok '(cleanup) '(more))
                 '(let ((a (f)))
                    (if (/= (logand a 2147483648) 0) (seq (cleanup) (more) a)
                      (let ((b (g a)))
                        (if (/= (logand b 2147483648) 0) (seq (cleanup) (more) b)
                          ok))))))
  (should-error (nelisp-com-try '(((x) (f))) 0)))

;;;; msctf GUID table

(ert-deftest s3a-iid-table-golden-bytes ()
  (should (equal (nelisp-com-guid-bytes (nelisp-com-msctf-guid 'IID_ITfContext))
                 '(#xFD #xE7 #x80 #xAA #x21 #x20 #xD2 #x11
                   #x93 #xE0 #x00 #x60 #xB0 #x67 #xB8 #x6E)))
  (should (equal (nelisp-com-guid-bytes
                  (nelisp-com-msctf-guid 'IID_ITfKeyEventSink))
                 '(#xF5 #xE7 #x80 #xAA #x21 #x20 #xD2 #x11
                   #x93 #xE0 #x00 #x60 #xB0 #x67 #xB8 #x6E)))
  (should (equal (nelisp-com-guid-bytes
                  (nelisp-com-msctf-guid 'CLSID_TF_ThreadMgr))
                 '(#x6B #x9E #x9A #x52 #x87 #x65 #x23 #x4F
                   #xAB #x9E #x9C #x7D #x68 #x3E #x3C #x50)))
  (should (equal (nelisp-com-msctf-blobs '(IID_ITfContext))
                 `((data-blob IID_ITfContext
                              (#xFD #xE7 #x80 #xAA #x21 #x20 #xD2 #x11
                               #x93 #xE0 #x00 #x60 #xB0 #x67 #xB8 #x6E)
                              rodata))))
  (should-error (nelisp-com-msctf-guid 'IID_NoSuch)))

(ert-deftest s3a-iid-table-consistency ()
  (let ((names (mapcar #'car nelisp-com-msctf-guids)))
    (should (= (length names) (length (delete-dups (copy-sequence names)))))
    (let ((guids (mapcar #'cadr nelisp-com-msctf-guids)))
      (should (= (length guids) (length (delete-dups (copy-sequence guids))))))
    (dolist (e nelisp-com-msctf-guids)
      (should (= (length (nelisp-com-guid-bytes (cadr e))) 16)))
    (dolist (n nelisp-com-msctf-guids-unverified) (should (memq n names)))
    (dolist (n '(IID_ITfTextInputProcessor IID_ITfTextInputProcessorEx
                 IID_ITfKeyEventSink IID_ITfThreadMgr IID_ITfKeystrokeMgr
                 IID_ITfContext IID_ITfEditSession IID_ITfComposition
                 IID_ITfCompositionSink IID_ITfDisplayAttributeProvider
                 IID_ITfFunctionProvider IID_ITfCompartmentEventSink
                 IID_ITfLangBarItem IID_ITfLangBarItemButton
                 IID_ITfLangBarItemSink CLSID_TF_ThreadMgr
                 CLSID_TF_InputProcessorProfiles CLSID_TF_CategoryMgr
                 GUID_TFCAT_TIP_KEYBOARD GUID_TFCAT_DISPLAYATTRIBUTEPROVIDER))
      (should (memq n names)))
    (should (= (length (nelisp-com-msctf-blobs)) (length names)))))

;;;; :fields and :extra-iids classes

(defun s3a-test--class ()
  (nelisp-com-class-forms
   'sk :fields '((cnt u32) (tag u16) (h ptr))
   :interfaces '((:iid "{aa80e7f7-2021-11d2-93e0-0060b067b86e}"
                       :methods (a b c))
                 (:iid "{aa80e7f5-2021-11d2-93e0-0060b067b86e}" :methods (d)))
   :extra-iids '((:iid "{6e4e2102-f9cd-433d-b496-303ce03a6507}" :iface 0)
                 ("{aa80e801-2021-11d2-93e0-0060b067b86e}" . 1))))

(ert-deftest s3a-class-fields-layout ()
  (let* ((forms (s3a-test--class))
         (new (s3a-test--defun forms 'sk_new)))
    ;; 2 interfaces: state offset 24; struct (u32,u16,ptr) = 16 bytes.
    (should (= (nelisp-com-struct-size 'sk_state) 16))
    (should (member '(extern-call HeapAlloc h 8 40) (s3a-test--subforms new)))
    (should (equal (nelisp-com-class-field 'sk 'o 'cnt) '(ptr-read-u32 (+ o 24) 0)))
    (should (equal (nelisp-com-class-field 'sk 'o 'h) '(ptr-read-u64 (+ o 24) 8)))
    (should (equal (nelisp-com-class-set-field 'sk 'o 'tag 5)
                   '(ptr-write-u16 (+ o 24) 4 5)))
    (should-error (nelisp-com-class-field 'sk 'o 'nope))
    (should-error (nelisp-com-class-field 'sk_nofields 'o 'x))
    (should-error (nelisp-com-class-forms
                   'bad :fields '((a u8)) :state-size 8
                   :interfaces '((:iid "{aa80e7f7-2021-11d2-93e0-0060b067b86e}"))))))

(ert-deftest s3a-class-extra-iids-qi ()
  (let* ((forms (s3a-test--class))
         (qi (s3a-test--defun forms 'sk_qi))
         (subs (s3a-test--subforms qi)))
    ;; Alias blobs carry the GUID bytes and have no vtable of their own.
    (should (equal (nth 2 (s3a-test--blob forms 'sk_xiid_0))
                   (nelisp-com-guid-bytes "{6e4e2102-f9cd-433d-b496-303ce03a6507}")))
    (should-not (s3a-test--blob forms 'sk_vt_2))
    ;; The alias on interface 0 hands out the object pointer, the one on
    ;; interface 1 hands out this+16.
    (should (member '(sk_qi_ok this ppv this) subs))
    (should (member '(sk_qi_ok this ppv (+ this 16)) subs))
    (should (member '(and (= (ptr-read-u64 riid 0) (ptr-read-u64 (data-addr sk_xiid_0) 0))
                          (= (ptr-read-u64 riid 8) (ptr-read-u64 (data-addr sk_xiid_0) 8)))
                    subs))
    (should (member '(and (= (ptr-read-u64 riid 0) (ptr-read-u64 (data-addr sk_xiid_1) 0))
                          (= (ptr-read-u64 riid 8) (ptr-read-u64 (data-addr sk_xiid_1) 8)))
                    subs))
    ;; Real interfaces are tested before the aliases; unknown -> E_NOINTERFACE.
    (let ((flat (flatten-tree qi)))
      (should (< (cl-position 'sk_iid_1 flat) (cl-position 'sk_xiid_0 flat))))
    (should (memq nelisp-com-e-nointerface (flatten-tree qi)))
    (should-error (nelisp-com-class-forms
                   'bad :interfaces '((:iid "{aa80e7f7-2021-11d2-93e0-0060b067b86e}"))
                   :extra-iids '((:iid "{6e4e2102-f9cd-433d-b496-303ce03a6507}"
                                       :iface 3))))))

;;;; Registration hives

(ert-deftest s3a-hive-default-and-switch ()
  (let ((process-environment (cons "NELISP_COM_HIVE=" process-environment)))
    (should (eq (nelisp-com-default-hive) 'hkcu)))
  (let ((process-environment (cons "NELISP_COM_HIVE=hkcr" process-environment)))
    (should (eq (nelisp-com-default-hive) 'hkcr)))
  (let ((process-environment (cons "NELISP_COM_HIVE=junk" process-environment)))
    (should (eq (nelisp-com-default-hive) 'hkcu))))

(ert-deftest s3a-server-forms-hives ()
  (let* ((clsid "{7a1c0de0-5f3a-4a6e-9d11-0123456789ab}")
         (cls `((:name o :clsid ,clsid)))
         (hkcu (nelisp-com-server-forms cls :hive 'hkcu))
         (hkcr (nelisp-com-server-forms cls :hive 'hkcr))
         (dflt (let ((process-environment (cons "NELISP_COM_HIVE=" process-environment)))
                 (nelisp-com-server-forms cls))))
    (should (equal (nth 2 (s3a-test--blob hkcu 'o_w_key))
                   (nelisp-com-wstr (concat "Software\\Classes\\CLSID\\" clsid
                                            "\\InprocServer32"))))
    (should (equal (nth 2 (s3a-test--blob hkcr 'o_w_key))
                   (nelisp-com-wstr (concat "CLSID\\" clsid "\\InprocServer32"))))
    (should (equal (nth 2 (s3a-test--blob hkcu 'o_w_root))
                   (nelisp-com-wstr (concat "Software\\Classes\\CLSID\\" clsid))))
    ;; The default is HKCU and equals the explicit :hive 'hkcu forms.
    (should (equal dflt hkcu))
    (should (member `(extern-call RegCreateKeyExW ,nelisp-com-hkey-current-user key
                                  0 0 0 131078 0 (data-addr com_hkey) 0)
                    (s3a-test--subforms (s3a-test--defun hkcu 'com_reg_class))))
    (should (member `(extern-call RegCreateKeyExW ,nelisp-com-hkey-classes-root key
                                  0 0 0 131078 0 (data-addr com_hkey) 0)
                    (s3a-test--subforms (s3a-test--defun hkcr 'com_reg_class))))
    (should (member `(extern-call RegDeleteTreeW ,nelisp-com-hkey-current-user
                                  (data-addr o_w_root))
                    (s3a-test--subforms (s3a-test--defun hkcu 'DllUnregisterServer))))
    (should-error (nelisp-com-server-forms cls :hive 'hklm))))

;;;; wbuf library forms

(ert-deftest s3a-wbuf-library-forms ()
  (let ((forms (nelisp-com-wstr-lib-forms)))
    (dolist (n '(wbuf_new wbuf_free wbuf_len wbuf_ptr wbuf_clear wbuf_valid
                 wbuf_reserve wbuf_append_char wbuf_append_wn wbuf_append_w
                 wbuf_append_ascii wbuf_append_cp wbuf_append_u64 wbuf_from_w
                 wbuf_get wbuf_wcslen wbuf_wcscmp wbuf_eq_w wbuf_eq))
      (should (s3a-test--defun forms n)))
    (should (= (nelisp-com-struct-size 'wbuf) 24))
    ;; The validity guard reads ptr/len/cap through the struct accessors.
    (let ((subs (s3a-test--subforms (s3a-test--defun forms 'wbuf_valid))))
      (should (member '(ptr-read-u64 w 0) subs))
      (should (member '(ptr-read-u64 w 8) subs))
      (should (member '(ptr-read-u64 w 16) subs)))
    ;; Compiler limitation: extern-call arguments must not be extern-calls.
    (dolist (f forms)
      (dolist (sf (s3a-test--subforms f))
        (when (eq (car-safe sf) 'extern-call)
          (dolist (a (cddr sf))
            (should-not (eq (car-safe a) 'extern-call))))))
    ;; Every mutator reserves through the HRESULT-checked path.
    (should (member '(wbuf_reserve w 1)
                    (s3a-test--subforms (s3a-test--defun forms 'wbuf_append_char))))))

(ert-deftest s3a-wbuf-ascii-blob ()
  (should (equal (nelisp-com-ascii-blob 'a "ab") '(data-blob a (97 98 0) rodata)))
  (should-error (nelisp-com-ascii-blob 'a "é")))

(ert-deftest s3a-wbuf-sample-compiles-and-negative-hook ()
  "The sample DLL links, and the BAD hook only adds the corrupting write."
  (let* ((good (wbuf-sample-forms nil))
         (bad (wbuf-sample-forms t))
         (g (s3a-test--defun good 'WbufSelfTest))
         (b (s3a-test--defun bad 'WbufSelfTest)))
    (should-not (equal g b))
    (should (member '(ptr-write-u64 w 8 (+ (ptr-read-u64 w 16) 5))
                    (s3a-test--subforms b)))
    (should-not (member '(ptr-write-u64 w 8 (+ (ptr-read-u64 w 16) 5))
                        (s3a-test--subforms g))))
  (let* ((dir (make-temp-file "s3a-ert" t)) (path (expand-file-name "t.dll" dir)))
    (unwind-protect
        (progn (nelisp-com-build-dll path (wbuf-sample-forms nil)
                                     :exports '("WbufSelfTest")
                                     :imports nelisp-com-wstr-lib-imports)
               (should (> (file-attribute-size (file-attributes path)) 4096)))
      (delete-directory dir t))))

(provide 'nelisp-com-dsl-s3a-test)
;;; nelisp-com-dsl-s3a-test.el ends here
