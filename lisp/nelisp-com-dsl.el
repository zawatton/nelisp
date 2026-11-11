;;; nelisp-com-dsl.el --- COM in-proc server DSL for the NeLisp AOT/PE pipeline  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stage S2 of the native TSF/COM DLL plan.  Plain Elisp functions that
;; return forms in the restricted AOT integer subset (see
;; `nelisp-aot-compile-to-link-unit'), plus a thin build driver that links
;; them into a PE32+ DLL or EXE.  Nothing here is specific to one COM class:
;; the same helpers scale to many interfaces per object and many classes per
;; DLL (a TIP needs ITfTextInputProcessor[Ex], ITfKeyEventSink, ...).
;;
;; Object model (per-instance, heap allocated with HeapAlloc):
;;
;;   offset 0        vtable pointer of interface 0 (== the object pointer)
;;   offset 8        reference count
;;   offset 16+8*(i-1)  vtable pointer of interface i (i >= 1)
;;   offset 8+8*N    first byte of class private state (N = interface count)
;;
;; Interface pointer i is the object pointer plus `nelisp-com-iface-offset'.
;; Slots 0..2 of every vtable are generated QueryInterface/AddRef/Release;
;; for i >= 1 they are thunks that rebase `this' and call the shared
;; implementation.  User methods on interface i >= 1 receive the raw
;; interface pointer; use `nelisp-com-base-form' to get the object base.
;;
;; Data layout rules proven under wine: vtables live in the `data' section
;; (abs64 relocs to function symbols), GUIDs and wide strings in `rodata'.
;; All HRESULT literals stay below 2^32.
;;
;; Entry points are plain generated defuns; the DLL exports are chosen by
;; the build driver.  .pdata/.xdata is not emitted (deferred, see the plan).

;;; Code:

(require 'cl-lib)

(declare-function nelisp-aot-compile-to-link-unit "nelisp-aot-compiler")
(declare-function nelisp-link-unit-from-aot "nelisp-static-linker")
(declare-function nelisp-link-units-pe32 "nelisp-static-linker")
(declare-function nelisp-link-units-pe32-dll "nelisp-static-linker")

;;;; Constants

(defconst nelisp-com-s-ok 0)
(defconst nelisp-com-s-false 1)
(defconst nelisp-com-e-nointerface 2147500034)
(defconst nelisp-com-e-notimpl 2147500033)
(defconst nelisp-com-e-pointer 2147500035)
(defconst nelisp-com-e-fail 2147500037)
(defconst nelisp-com-e-outofmemory 2147942414)
(defconst nelisp-com-class-e-classnotavailable 2147746132)
(defconst nelisp-com-class-e-noaggregation 2147746064)

(defconst nelisp-com-iid-iunknown "{00000000-0000-0000-c000-000000000046}")
(defconst nelisp-com-iid-iclassfactory "{00000001-0000-0000-c000-000000000046}")

(defconst nelisp-com-hkey-classes-root '(- 0 2147483648)
  "AOT form for HKEY_CLASSES_ROOT (0x80000000 sign-extended).")
(defconst nelisp-com-hkey-current-user '(- 0 2147483647)
  "AOT form for HKEY_CURRENT_USER (0x80000001 sign-extended).")

(defconst nelisp-com-server-imports
  '(("kernel32.dll" "GetModuleHandleExW" "GetModuleFileNameW"
     "GetProcessHeap" "HeapAlloc" "HeapFree")
    ("advapi32.dll" "RegCreateKeyExW" "RegSetValueExW" "RegCloseKey"
     "RegDeleteTreeW"))
  "Imports needed by the generated server code.")

(defconst nelisp-com-server-exports
  '("DllGetClassObject" "DllCanUnloadNow" "DllRegisterServer"
    "DllUnregisterServer")
  "The four standard in-proc server exports.")

;;;; Data blobs

(defun nelisp-com-guid-bytes (guid)
  "Return the 16 in-memory bytes of GUID, a \"{xxxxxxxx-...}\" string.
D1, D2 and D3 are little-endian, the remaining 8 bytes are in order."
  (let ((h (replace-regexp-in-string "[{}-]" "" guid)))
    (unless (string-match-p "\\`[0-9a-fA-F]\\{32\\}\\'" h)
      (error "Malformed GUID: %S" guid))
    (let ((d1 (string-to-number (substring h 0 8) 16))
          (d2 (string-to-number (substring h 8 12) 16))
          (d3 (string-to-number (substring h 12 16) 16)))
      (append (list (logand d1 255) (logand (ash d1 -8) 255)
                    (logand (ash d1 -16) 255) (logand (ash d1 -24) 255)
                    (logand d2 255) (logand (ash d2 -8) 255)
                    (logand d3 255) (logand (ash d3 -8) 255))
              (cl-loop for i from 8 below 16
                       collect (string-to-number
                                (substring h (* 2 i) (+ 2 (* 2 i))) 16))))))

(defun nelisp-com-wstr (string)
  "Return STRING as a NUL-terminated UTF-16LE byte list (no BOM).
Characters outside the BMP become surrogate pairs."
  (append (append (encode-coding-string string 'utf-16le) nil) '(0 0)))

(defun nelisp-com-guid-blob (name guid)
  "Return a `data-blob' form named NAME holding GUID in rodata."
  `(data-blob ,name ,(nelisp-com-guid-bytes guid) rodata))

(defun nelisp-com-wstr-blob (name string)
  "Return a `data-blob' form named NAME holding STRING as UTF-16LE in rodata."
  `(data-blob ,name ,(nelisp-com-wstr string) rodata))

(defun nelisp-com-zero-blob (name size &optional section)
  "Return a zero-filled `data-blob' of SIZE bytes in SECTION (default data)."
  `(data-blob ,name ,(make-list size 0) ,(or section 'data)))

(defun nelisp-com-vtable (name functions)
  "Return a `data-blob' form NAME whose slots point at FUNCTIONS.
FUNCTIONS is a list of defun symbols in slot order.  Each slot carries an
abs64 relocation so the table survives a rebase.  Always emitted in `data'."
  `(data-blob ,name ,(make-list (* 8 (length functions)) 0) data
              ,(cl-loop for f in functions for i from 0
                        collect (list (* 8 i) f 0))))

;;;; Call-site helpers

(defun nelisp-com-guid= (ptr-var blob)
  "Return a form comparing the GUID at PTR-VAR (a variable symbol) to BLOB."
  (unless (symbolp ptr-var)
    (error "nelisp-com-guid=: PTR-VAR must be a variable, got %S" ptr-var))
  `(and (= (ptr-read-u64 ,ptr-var 0) (ptr-read-u64 (data-addr ,blob) 0))
        (= (ptr-read-u64 ,ptr-var 8) (ptr-read-u64 (data-addr ,blob) 8))))

(defun nelisp-com-call (obj-var slot &rest args)
  "Return a form calling vtable SLOT of the object in OBJ-VAR with ARGS.
OBJ-VAR must be a variable symbol (it is evaluated three times)."
  (unless (symbolp obj-var)
    (error "nelisp-com-call: OBJ-VAR must be a variable, got %S" obj-var))
  `(call-ptr (ptr-read-u64 (ptr-read-u64 ,obj-var 0) ,(* 8 slot))
             ,obj-var ,@args))

(defun nelisp-com-iface-offset (index)
  "Return the byte offset of interface INDEX's vtable pointer in an object."
  (if (= index 0) 0 (+ 8 (* 8 index))))

(defun nelisp-com-state-offset (iface-count)
  "Return the offset of the class-private state for IFACE-COUNT interfaces."
  (+ 8 (* 8 iface-count)))

(defun nelisp-com-base-form (index this-var)
  "Return a form giving the object base from interface INDEX's pointer."
  (let ((off (nelisp-com-iface-offset index)))
    (if (= off 0) this-var `(- ,this-var ,off))))

;;;; Classes

(defun nelisp-com--sym (class &rest parts)
  (intern (mapconcat (lambda (p) (format "%s" p)) (cons class parts) "_")))

(defun nelisp-com--iface-iid-sym (class i)
  (nelisp-com--sym class "iid" i))

(defun nelisp-com--qi-chain (class ifaces)
  "Return the nested-if QueryInterface body for CLASS with IFACES."
  (let ((qi-ok (nelisp-com--sym class "qi_ok")))
    (cl-labels
        ((chain (items)
           (if (null items)
               `(seq (ptr-write-u64 ppv 0 0) ,nelisp-com-e-nointerface)
             (let* ((it (car items)) (i (car it)) (blob (cdr it))
                    (off (nelisp-com-iface-offset i)))
               `(if ,(nelisp-com-guid= 'riid blob)
                    (,qi-ok this ppv ,(if (= off 0) 'this `(+ this ,off)))
                  ,(chain (cdr items)))))))
      (chain
       (append
        ;; IUnknown always resolves to interface 0.
        (list (cons 0 (nelisp-com--sym class "iid_unk")))
        (cl-loop for i from 0 below (length ifaces)
                 collect (cons i (nelisp-com--iface-iid-sym class i))))))))

(defun nelisp-com-class-forms (class &rest spec)
  "Return AOT forms implementing COM class CLASS (a symbol).
SPEC is a plist:
  :interfaces  list of (:iid GUID :methods (FN...)) in object order;
               FN are user defun symbols for slots 3.. of that interface.
  :state-size  bytes of private per-instance state (default 0)
  :init        optional symbol of a defun (base) called after allocation
  :on-free     optional symbol of a defun (base) called before HeapFree
Generated names (prefix CLASS_): iid_unk, iid_N, vt_N, count, new, qi,
addref, release, qi_ok, iN_qi/addref/release thunks (N >= 1).
`CLASS_new' returns a base pointer with reference count 1 (0 on failure)."
  (let* ((ifaces (plist-get spec :interfaces))
         (n (length ifaces))
         (state-size (or (plist-get spec :state-size) 0))
         (size (+ (nelisp-com-state-offset n) state-size))
         (s (lambda (&rest p) (apply #'nelisp-com--sym class p)))
         (count (funcall s "count"))
         (qi (funcall s "qi")) (addref (funcall s "addref"))
         (release (funcall s "release")) (qi-ok (funcall s "qi_ok"))
         (new (funcall s "new"))
         (init (plist-get spec :init)) (on-free (plist-get spec :on-free))
         forms)
    (when (zerop n) (error "Class %s needs at least one interface" class))
    (push (nelisp-com-guid-blob (funcall s "iid_unk") nelisp-com-iid-iunknown)
          forms)
    (push (nelisp-com-zero-blob count 8) forms)
    (cl-loop for ifc in ifaces for i from 0
             do (push (nelisp-com-guid-blob (nelisp-com--iface-iid-sym class i)
                                            (plist-get ifc :iid))
                      forms)
             (let ((slots (if (= i 0)
                              (list qi addref release)
                            (list (funcall s (format "i%d" i) "qi")
                                  (funcall s (format "i%d" i) "addref")
                                  (funcall s (format "i%d" i) "release")))))
               (push (nelisp-com-vtable (funcall s "vt" i)
                                        (append slots (plist-get ifc :methods)))
                     forms))
             (when (> i 0)
               (let ((off (nelisp-com-iface-offset i)))
                 (push `(defun ,(funcall s (format "i%d" i) "qi") (this riid ppv)
                          (,qi (- this ,off) riid ppv))
                       forms)
                 (push `(defun ,(funcall s (format "i%d" i) "addref") (this)
                          (,addref (- this ,off)))
                       forms)
                 (push `(defun ,(funcall s (format "i%d" i) "release") (this)
                          (,release (- this ,off)))
                       forms))))
    (push `(defun ,qi-ok (base ppv ptr)
             (seq (ptr-write-u64 ppv 0 ptr) (,addref base) 0))
          forms)
    (push `(defun ,qi (this riid ppv)
             (if (= ppv 0) ,nelisp-com-e-pointer
               ,(nelisp-com--qi-chain class ifaces)))
          forms)
    (push `(defun ,addref (this) (+ 1 (atomic-fetch-add (+ this 8) 1))) forms)
    (push `(defun ,release (this)
             (let ((n (- (atomic-fetch-add (+ this 8) -1) 1)))
               (if (= n 0)
                   (let ((h (extern-call GetProcessHeap)))
                     (seq ,@(when on-free `((,on-free this)))
                          (atomic-fetch-add (data-addr ,count) -1)
                          (extern-call HeapFree h 0 this)
                          0))
                 n)))
          forms)
    (push `(defun ,new ()
             (let* ((h (extern-call GetProcessHeap))
                    (o (extern-call HeapAlloc h 8 ,size)))
               (if (= o 0) 0
                 (seq ,@(cl-loop for i from 0 below n
                                 collect `(ptr-write-u64
                                           o ,(nelisp-com-iface-offset i)
                                           (data-addr ,(funcall s "vt" i))))
                      (ptr-write-u64 o 8 1)
                      (atomic-fetch-add (data-addr ,count) 1)
                      ,@(when init `((,init o)))
                      o))))
          forms)
    (nreverse forms)))

(defun nelisp-com-factory-forms (class)
  "Return AOT forms for the class factory of CLASS (see `nelisp-com-class-forms').
Generated names: CLASS_fac (object), CLASS_fvt, CLASS_f_qi/addref/release/
create/lock, CLASS_iid_fac, CLASS_locks (server lock count)."
  (let* ((s (lambda (&rest p) (apply #'nelisp-com--sym class p)))
         (fac (funcall s "fac")) (fvt (funcall s "fvt"))
         (iid-fac (funcall s "iid_fac")) (iid-unk (funcall s "iid_unk"))
         (locks (funcall s "locks")) (new (funcall s "new"))
         (cqi (funcall s "qi")) (crel (funcall s "release")))
    (list
     (nelisp-com-guid-blob iid-fac nelisp-com-iid-iclassfactory)
     (nelisp-com-zero-blob locks 8)
     `(data-blob ,fac ,(make-list 8 0) data ((0 ,fvt 0)))
     (nelisp-com-vtable fvt (mapcar s '("f_qi" "f_addref" "f_release"
                                        "f_create" "f_lock")))
     `(defun ,(funcall s "f_qi") (this riid ppv)
        (if (or ,(nelisp-com-guid= 'riid iid-unk)
                ,(nelisp-com-guid= 'riid iid-fac))
            (seq (ptr-write-u64 ppv 0 this) 0)
          (seq (ptr-write-u64 ppv 0 0) ,nelisp-com-e-nointerface)))
     ;; The factory is static: reference counts are irrelevant.
     `(defun ,(funcall s "f_addref") (this) 2)
     `(defun ,(funcall s "f_release") (this) 1)
     `(defun ,(funcall s "f_create") (this outer riid ppv)
        (if (/= outer 0)
            (seq (ptr-write-u64 ppv 0 0) ,nelisp-com-class-e-noaggregation)
          (let ((o (,new)))
            (if (= o 0)
                (seq (ptr-write-u64 ppv 0 0) ,nelisp-com-e-outofmemory)
              (let ((hr (,cqi o riid ppv)))
                (seq (,crel o) hr))))))
     `(defun ,(funcall s "f_lock") (this lock)
        (seq (atomic-fetch-add (data-addr ,locks) (if (= lock 0) -1 1)) 0)))))

;;;; DLL entry points

(defun nelisp-com-server-forms (classes)
  "Return AOT forms for the four DLL exports serving CLASSES.
CLASSES is a list of plists (:name SYM :clsid GUID :threading \"Apartment\")
whose class and factory forms come from `nelisp-com-class-forms' and
`nelisp-com-factory-forms'.  The module path is obtained with
GetModuleHandleExW(FROM_ADDRESS|UNCHANGED_REFCOUNT) on DllRegisterServer.
Registration writes under HKEY_CLASSES_ROOT; DllUnregisterServer deletes
the CLSID subtree."
  (let (forms)
    (push (nelisp-com-zero-blob 'com_hmod 8) forms)
    (push (nelisp-com-zero-blob 'com_hkey 8) forms)
    (push (nelisp-com-zero-blob 'com_path 1040) forms)
    (push (nelisp-com-wstr-blob 'com_w_tm "ThreadingModel") forms)
    (cl-loop
     for c in classes for i from 0
     for name = (plist-get c :name)
     for clsid = (plist-get c :clsid)
     for tm = (or (plist-get c :threading) "Apartment")
     do (push (nelisp-com-guid-blob (nelisp-com--sym name "clsid") clsid) forms)
     (push (nelisp-com-wstr-blob (nelisp-com--sym name "w_key")
                                 (concat "CLSID\\" clsid "\\InprocServer32"))
           forms)
     (push (nelisp-com-wstr-blob (nelisp-com--sym name "w_root")
                                 (concat "CLSID\\" clsid))
           forms)
     (push (nelisp-com-wstr-blob (nelisp-com--sym name "w_tmval") tm) forms))
    ;; DllGetClassObject: one comparison per class, last resort is an error.
    (push `(defun DllGetClassObject (rclsid riid ppv)
             ,(cl-labels
                  ((chain (cs)
                     (if (null cs)
                         `(seq (ptr-write-u64 ppv 0 0)
                               ,nelisp-com-class-e-classnotavailable)
                       (let ((name (plist-get (car cs) :name)))
                         `(if ,(nelisp-com-guid=
                                'rclsid (nelisp-com--sym name "clsid"))
                              (,(nelisp-com--sym name "f_qi")
                               (data-addr ,(nelisp-com--sym name "fac"))
                               riid ppv)
                            ,(chain (cdr cs)))))))
                (chain classes)))
          forms)
    ;; DllCanUnloadNow: S_OK only if no instances and no locks remain.
    (push `(defun DllCanUnloadNow ()
             (if (= ,(cl-reduce
                      (lambda (a b) `(+ ,a ,b))
                      (cl-loop for c in classes
                               for name = (plist-get c :name)
                               collect `(ptr-read-u64
                                         (data-addr ,(nelisp-com--sym name "count")) 0)
                               collect `(ptr-read-u64
                                         (data-addr ,(nelisp-com--sym name "locks")) 0)))
                    0)
                 0 1))
          forms)
    ;; Registers one class: key path blob address, threading value blob,
    ;; value byte count.
    (push `(defun com_reg_class (key tmval tmbytes)
             (seq
              (extern-call RegCreateKeyExW ,nelisp-com-hkey-classes-root key
                           0 0 0 131078 0 (data-addr com_hkey) 0)
              (extern-call RegSetValueExW (ptr-read-u64 (data-addr com_hkey) 0)
                           0 0 1 (data-addr com_path) 520)
              (extern-call RegSetValueExW (ptr-read-u64 (data-addr com_hkey) 0)
                           (data-addr com_w_tm) 0 1 tmval tmbytes)
              (extern-call RegCloseKey (ptr-read-u64 (data-addr com_hkey) 0))
              0))
          forms)
    (push `(defun DllRegisterServer ()
             (seq
              (extern-call GetModuleHandleExW 6 (addr-of DllRegisterServer)
                           (data-addr com_hmod))
              (extern-call GetModuleFileNameW
                           (ptr-read-u64 (data-addr com_hmod) 0)
                           (data-addr com_path) 520)
              ,@(cl-loop for c in classes
                         for name = (plist-get c :name)
                         for tm = (or (plist-get c :threading) "Apartment")
                         collect `(com_reg_class
                                   (data-addr ,(nelisp-com--sym name "w_key"))
                                   (data-addr ,(nelisp-com--sym name "w_tmval"))
                                   ,(* 2 (1+ (length tm)))))
              0))
          forms)
    (push `(defun DllUnregisterServer ()
             (seq
              ,@(cl-loop for c in classes
                         for name = (plist-get c :name)
                         collect `(extern-call
                                   RegDeleteTreeW ,nelisp-com-hkey-classes-root
                                   (data-addr ,(nelisp-com--sym name "w_root"))))
              0))
          forms)
    (nreverse forms)))

;;;; Build driver

(defun nelisp-com--unit (name forms)
  (require 'nelisp-aot-compiler)
  (require 'nelisp-static-linker)
  (nelisp-link-unit-from-aot
   name (nelisp-aot-compile-to-link-unit `(seq ,@forms)
                                         :arch 'x86_64 :format 'coff)))

(cl-defun nelisp-com-build-dll (path forms &key (exports nelisp-com-server-exports)
                                     (imports nelisp-com-server-imports)
                                     dll-name no-reloc image-base)
  "Compile FORMS and link them into the PE32+ DLL PATH.
EXPORTS and IMPORTS default to the standard server sets.  NO-RELOC and
IMAGE-BASE are negative-control hooks passed to the linker."
  (require 'nelisp-static-linker)
  (nelisp-link-units-pe32-dll
   path (list (nelisp-com--unit "com.o" forms)) exports imports
   (list :dll-name (or dll-name (file-name-nondirectory path))
         :no-reloc no-reloc :image-base image-base))
  path)

(defun nelisp-com-build-exe (path forms imports)
  "Compile FORMS (defining `_start') and link them into the EXE PATH."
  (require 'nelisp-static-linker)
  (nelisp-link-units-pe32 path (list (nelisp-com--unit "exe.o" forms))
                          "_start" imports)
  path)

(provide 'nelisp-com-dsl)
;;; nelisp-com-dsl.el ends here
