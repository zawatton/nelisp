;;; wbuf-sample.el --- S3a-1 sample: wbuf library, structs, try, :fields, :extra-iids  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds wbuf.dll (exports WbufSelfTest, which runs the checks and returns
;; 0 or a check number) and wbuf-client.exe (LoadLibrary + GetProcAddress +
;; call; exit 42 when WbufSelfTest returns 0, else 100 + the check number).
;; Used by tools/ai/com-dll/wbuf-e2e.sh.
;;
;; Negative-control hook (environment variable, outside the library):
;;   NELISP_WBUF_BAD=1   the self test corrupts a wbuf's `len' above its
;;                       `cap' through the struct accessor before appending;
;;                       the library must reject the append (E_INVALIDARG),
;;                       so the check that expects success fails.

;;; Code:

(require 'cl-lib)
(require 'nelisp-com-dsl)
(require 'nelisp-com-wstr-lib)

(defconst wbuf-sample-iid-a "{aa80e7f7-2021-11d2-93e0-0060b067b86e}")
(defconst wbuf-sample-iid-alias "{6e4e2102-f9cd-433d-b496-303ce03a6507}")
(defconst wbuf-sample-iid-other "{aa80e7f5-2021-11d2-93e0-0060b067b86e}")

(defun wbuf-sample--chain (steps final)
  "Nest STEPS: (check COND CODE), (do FORM) or (bind VAR FORM), ending in FINAL.
A failing check returns CODE from the enclosing defun."
  (if (null steps) final
    (let ((s (car steps)) (rest (wbuf-sample--chain (cdr steps) final)))
      (pcase (car s)
        ('check `(if ,(nth 1 s) ,rest ,(nth 2 s)))
        ('do `(seq ,(nth 1 s) ,rest))
        ('bind `(let ((,(nth 1 s) ,(nth 2 s))) ,rest))))))

(defun wbuf-sample-forms (&optional bad)
  "Return the DLL forms.  BAD enables the corrupt-length hook."
  (let ((w-expect (nelisp-com-wstr (concat "ab12345" (string #x1f600)))))
    (append
     (nelisp-com-iid-table `((g_iid_a ,wbuf-sample-iid-a)
                             (g_iid_alias ,wbuf-sample-iid-alias)
                             (g_iid_other ,wbuf-sample-iid-other)))
     (list (nelisp-com-ascii-blob 'a_ab "ab")
           '(data-blob a_bad (97 128 0) rodata)
           `(data-blob w_expect ,w-expect rodata)
           (nelisp-com-wstr-blob 'w_wrong "ab12346")
           (nelisp-com-wstr-blob 'w_ab "ab")
           (nelisp-com-wstr-blob 'w_xyz "xyz")
           `(data-blob sink ,(make-list 8 0) data))
     (nelisp-com-struct-forms 'TF_SELECTION)
     (nelisp-com-struct-forms 'TF_DISPLAYATTRIBUTE)
     (nelisp-com-wstr-lib-forms)
     ;; Class with named fields; ITfTextInputProcessorEx is an alias IID
     ;; answered by the vtable of interface 0 (slot 5 = ActivateEx).
     (nelisp-com-class-forms
      'k :fields '((cnt u32) (tag u16) (h ptr))
      :interfaces `((:iid ,wbuf-sample-iid-a :methods (k_act k_deact k_actex)))
      :extra-iids `((:iid ,wbuf-sample-iid-alias :iface 0)))
     '((defun k_act (this) 0)
       (defun k_deact (this) 0)
       (defun k_actex (this a b) 7)
       (defun ok_hr () 0)
       (defun fail_hr () 2147500037)
       (defun mark (x) (seq (ptr-write-u64 (data-addr sink) 0 x) 0)))
     ;; try: the failing step stops the chain, cleanup runs once, the
     ;; failing HRESULT (E_FAIL) is the value, the third call never runs.
     `((defun try_probe ()
         (seq (ptr-write-u64 (data-addr sink) 0 0)
              ,(nelisp-com-try '((a (ok_hr)) (b (fail_hr)) (c (mark 99)))
                               0 '(mark 5))))
       (defun try_ok ()
         ,(nelisp-com-try '((a (ok_hr)) (b (ok_hr))) 77 '(mark 6))))
     `((defun WbufSelfTest ()
         ,(wbuf-sample--chain
           `(;; struct accessors vs raw offsets (TF_SELECTION: 16 bytes)
             (bind sel (TF_SELECTION_alloc))
             (check (/= sel 0) 1)
             (do ,(nelisp-com-set-field 'sel 'TF_SELECTION 'range 4660))
             (do ,(nelisp-com-set-field `(+ sel 8) 'TF_SELECTIONSTYLE 'ase 1))
             (do ,(nelisp-com-set-field `(+ sel 8) 'TF_SELECTIONSTYLE
                                        'fInterimChar 5))
             (check (= (ptr-read-u64 sel 0) 4660) 2)
             (check (= (ptr-read-u32 sel 8) 1) 3)
             (check (= (ptr-read-u32 sel 12) 5) 4)
             (check (= ,(nelisp-com-field 'sel 'TF_SELECTION 'range) 4660) 5)
             (bind da (TF_DISPLAYATTRIBUTE_alloc))
             (do ,(nelisp-com-set-field 'da 'TF_DISPLAYATTRIBUTE 'bAttr 3))
             (do ,(nelisp-com-set-field 'da 'TF_DISPLAYATTRIBUTE 'lsStyle 2))
             (check (= (ptr-read-u32 da 32) 3) 6)
             (check (= (ptr-read-u32 da 16) 2) 7)
             (check (= (TF_SELECTION_free sel) 0) 8)
             (check (= (TF_DISPLAYATTRIBUTE_free da) 0) 9)
             ;; wbuf: capacity 4 is raised to 16, so 25 units force growth.
             (bind w (wbuf_new 4))
             (check (/= w 0) 10)
             (check (= (wbuf_append_ascii w (data-addr a_ab)) 0) 11)
             (check (= (wbuf_len w) 2) 12)
             (check (= (wbuf_eq_w w (data-addr w_ab)) 1) 13)
             (check (= (wbuf_append_u64 w 12345) 0) 14)
             (check (= (wbuf_append_cp w 128512) 0) 15)
             (check (= (wbuf_len w) 9) 16)
             (check (= (wbuf_eq_w w (data-addr w_expect)) 1) 17)
             (check (= (wbuf_eq_w w (data-addr w_wrong)) 0) 18)
             (check (= (wbuf_get w 7) 55357) 19)
             (check (= (wbuf_get w 8) 56832) 20)
             (check (= (wbuf_get w 9) 65536) 21)
             (check (= (wbuf_get w (- 0 1)) 65536) 22)
             (check (= (wbuf_wcscmp (data-addr w_ab) (data-addr w_xyz)) (- 0 1)) 23)
             (check (= (wbuf_wcscmp (data-addr w_xyz) (data-addr w_ab)) 1) 24)
             (check (= (wbuf_wcscmp (data-addr w_ab) (data-addr w_ab)) 0) 25)
             ;; bounds / argument checks
             (check (= (wbuf_append_cp w 55296) ,nelisp-com-e-invalidarg) 26)
             (check (= (wbuf_append_cp w 1114112) ,nelisp-com-e-invalidarg) 27)
             (check (= (wbuf_append_ascii w (data-addr a_bad))
                       ,nelisp-com-e-invalidarg) 28)
             (check (= (wbuf_len w) 9) 29)
             (bind w2 (wbuf_from_w (data-addr w_expect)))
             (check (= (wbuf_eq w w2) 1) 30)
             (do (wbuf_clear w2))
             (check (= (wbuf_eq w w2) 0) 31)
             (check (= (wbuf_free w2) 0) 32)
             ,@(when bad
                 `((do ,(nelisp-com-set-field
                         'w 'wbuf 'len `(+ ,(nelisp-com-field 'w 'wbuf 'cap) 5)))))
             (check (= (wbuf_append_char w 65) 0) 33)
             (check (= (wbuf_len w) 10) 34)
             (check (= (wbuf_free w) 0) 35)
             ;; class: :fields state and :extra-iids alias
             (bind o (k_new))
             (check (/= o 0) 40)
             (do ,(nelisp-com-class-set-field 'k 'o 'cnt 123456))
             (do ,(nelisp-com-class-set-field 'k 'o 'tag 513))
             (do ,(nelisp-com-class-set-field 'k 'o 'h 77))
             (check (= ,(nelisp-com-class-field 'k 'o 'cnt) 123456) 41)
             (check (= ,(nelisp-com-class-field 'k 'o 'tag) 513) 42)
             (check (= ,(nelisp-com-class-field 'k 'o 'h) 77) 43)
             (check (= (k_qi o (data-addr g_iid_alias) (data-addr sink)) 0) 44)
             (check (= (ptr-read-u64 (data-addr sink) 0) o) 45)
             (check (= (k_qi o (data-addr g_iid_a) (data-addr sink)) 0) 46)
             (check (= (k_qi o (data-addr g_iid_other) (data-addr sink))
                       ,nelisp-com-e-nointerface) 47)
             (check (= (ptr-read-u64 (data-addr sink) 0) 0) 48)
             ;; ActivateEx (vtable slot 5) via a non-variable object expression
             (check (= ,(nelisp-com-call '(+ o 0) 5 'o 0 0) 7) 49)
             (check (= (k_release o) 2) 50)
             (check (= (k_release o) 1) 51)
             (check (= (k_release o) 0) 52)
             ;; try macro
             (check (= (try_probe) ,nelisp-com-e-fail) 60)
             (check (= (ptr-read-u64 (data-addr sink) 0) 5) 61)
             (check (= (try_ok) 77) 62))
           0))))))

(defconst wbuf-sample-client-imports
  '(("kernel32.dll" "ExitProcess" "LoadLibraryW" "GetProcAddress")))

(defun wbuf-sample-client-forms ()
  "Return client forms: load wbuf.dll, call WbufSelfTest."
  `(,(nelisp-com-wstr-blob 'w_dll "wbuf.dll")
    ,(nelisp-com-ascii-blob 'a_fn "WbufSelfTest")
    (defun _start ()
      (let ((m (extern-call LoadLibraryW (data-addr w_dll))))
        (if (= m 0) (extern-call ExitProcess 10)
          (let ((f (extern-call GetProcAddress m (data-addr a_fn))))
            (if (= f 0) (extern-call ExitProcess 11)
              (let ((r (call-ptr f)))
                (extern-call ExitProcess (if (= r 0) 42 (+ 100 r)))))))))))

(defun wbuf-sample-build-all (dir)
  "Build wbuf.dll and wbuf-client.exe into DIR."
  (make-directory dir t)
  (nelisp-com-build-dll
   (expand-file-name "wbuf.dll" dir)
   (wbuf-sample-forms (equal (getenv "NELISP_WBUF_BAD") "1"))
   :exports '("WbufSelfTest")
   :imports nelisp-com-wstr-lib-imports)
  (nelisp-com-build-exe (expand-file-name "wbuf-client.exe" dir)
                        (wbuf-sample-client-forms)
                        wbuf-sample-client-imports)
  dir)

(provide 'wbuf-sample)
;;; wbuf-sample.el ends here
