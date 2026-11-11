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
(defconst nelisp-com-e-invalidarg #x80070057 "E_INVALIDARG.")
(defconst nelisp-com-e-unexpected #x8000FFFF "E_UNEXPECTED.")
(defconst nelisp-com-e-accessdenied #x80070005 "E_ACCESSDENIED.")
(defconst nelisp-com-regdb-e-classnotreg #x80040154 "REGDB_E_CLASSNOTREG.")

;; TSF edit-session flags (msctf.h).  TF_ES_SYNC|TF_ES_READWRITE = 7 is the
;; combination wine accepts; TF_ES_READ|TF_ES_SYNC = 3 is read-only.
(defconst nelisp-com-tf-es-asyncdontcare 0)
(defconst nelisp-com-tf-es-sync 1)
(defconst nelisp-com-tf-es-read 2)
(defconst nelisp-com-tf-es-readwrite 6)
(defconst nelisp-com-tf-es-async 8)
(defconst nelisp-com-tf-default-selection #xFFFFFFFF)
(defconst nelisp-com-tf-anchor-start 0)
(defconst nelisp-com-tf-anchor-end 1)
(defconst nelisp-com-tf-ias-noquery 1)
(defconst nelisp-com-tf-ias-queryonly 2)
(defconst nelisp-com-tf-st-correction 1)
(defconst nelisp-com-tf-popf-all 1)
(defconst nelisp-com-tf-ct-none 0)
(defconst nelisp-com-tf-ct-syscolor 1)
(defconst nelisp-com-tf-ct-colorref 2)
(defconst nelisp-com-tf-e-locked #x80040600)
(defconst nelisp-com-tf-e-stackfull #x80040601)
(defconst nelisp-com-tf-e-notownedrange #x80040602)
(defconst nelisp-com-tf-e-noprovider #x80040603)
(defconst nelisp-com-tf-e-disconnected #x80040604)
(defconst nelisp-com-tf-e-invalidview #x80040605)
(defconst nelisp-com-tf-e-already-exists #x80040606)
(defconst nelisp-com-tf-e-range-not-covered #x80040607)
(defconst nelisp-com-tf-e-composition-rejected #x80040608)
(defconst nelisp-com-tf-e-empty-context #x80040609)
;; textstor.h TS_E_* (from memory of the SDK header; not cross-checked).
(defconst nelisp-com-ts-e-invalidpos #x80040200)
(defconst nelisp-com-ts-e-nolock #x80040201)
(defconst nelisp-com-ts-e-noobject #x80040202)
(defconst nelisp-com-ts-e-noselection #x80040205)
(defconst nelisp-com-ts-e-nolayout #x80040206)
(defconst nelisp-com-ts-e-invalidpoint #x80040207)
(defconst nelisp-com-ts-e-synchronous #x80040208)
(defconst nelisp-com-ts-e-readonly #x80040209)
(defconst nelisp-com-ts-e-format #x8004020A)

(defun nelisp-com-hresult-from-win32 (code)
  "Return HRESULT_FROM_WIN32 of the Win32 error CODE as an integer < 2^32."
  (if (<= code 0) code (logior (logand code #xffff) #x80070000)))

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

;;;; msctf GUID table

(defconst nelisp-com-msctf-guids
  '((IID_ITfTextInputProcessor "{aa80e7f7-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfTextInputProcessorEx "{6e4e2102-f9cd-433d-b496-303ce03a6507}")
    (IID_ITfKeyEventSink "{aa80e7f5-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfKeystrokeMgr "{aa80e7f0-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfThreadMgr "{aa80e801-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfThreadMgrEventSink "{aa80e80e-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfThreadFocusSink "{c0f1db0c-3a20-405c-a303-96b6010a885f}")
    (IID_ITfDocumentMgr "{aa80e7f4-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfContext "{aa80e7fd-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfContextView "{2433bf8e-0f9b-435c-ba2c-180611978c30}")
    (IID_ITfEditSession "{aa80e803-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfRange "{aa80e7ff-2021-11d2-93e0-0060b067b86e}")
    (IID_ITfRangeACP "{057a6296-029b-4154-b79a-0d461d4ea94c}")
    (IID_ITfInsertAtSelection "{55ce16ba-3014-41c1-9ceb-fade1446ac6c}")
    (IID_ITfComposition "{20168d64-5a8f-4a5a-b7bd-cfa29f4d0fd9}")
    (IID_ITfCompositionSink "{a781718c-579a-4b15-a280-32b8577acc5e}")
    (IID_ITfContextComposition "{d40c8aae-ac92-4fc7-9a11-0ee0e23aa39b}")
    (IID_ITfContextOwnerCompositionSink "{5f20aa40-b57a-4f34-96ab-3576f377cc79}")
    (IID_ITfTextEditSink "{8127d409-ccd3-4683-967a-b43d5b482bf7}")
    (IID_ITfProperty "{e2449660-9542-11d2-bf46-00105a2799b5}")
    (IID_ITfDisplayAttributeProvider "{fee47777-163c-4769-996a-6e9c50ad8f54}")
    (IID_ITfDisplayAttributeInfo "{70528852-2f26-4aea-8c96-215b0551baa7}")
    (IID_IEnumTfDisplayAttributeInfo "{7cef04d7-cb75-4e80-a7ab-5f5bc7d332de}")
    (IID_ITfFunctionProvider "{101d6610-0990-11d3-8df0-00105a2799b5}")
    (IID_ITfFunction "{db593490-098f-11d3-8df0-00105a2799b5}")
    (IID_ITfCompartmentMgr "{7dcf57ac-18ad-438b-824d-979bffb74b7c}")
    (IID_ITfCompartment "{bb08f7a9-607a-4384-8623-056892b64371}")
    (IID_ITfCompartmentEventSink "{743abd5f-f26d-48df-8cc5-238492419b64}")
    (IID_ITfSource "{4ea48a35-60ae-446f-8fd6-e6a8d82459f7}")
    (IID_ITfLangBarItem "{73540d69-edeb-4ee9-96c9-23aa30b25916}")
    (IID_ITfLangBarItemButton "{28c7f1d0-de25-11d2-afdd-00105a2799b5}")
    (IID_ITfLangBarItemSink "{57dbe1a0-de25-11d2-afdd-00105a2799b5}")
    (IID_ITfLangBarItemMgr "{ba468c55-9956-4fb1-a59d-52a7dd7cc6aa}")
    (IID_ITfSystemLangBarItem "{1449d9ab-13cf-4687-aa3e-8d8b18574396}")
    (IID_ITfUIElement "{ea1ea137-19df-11d7-a6d2-00065b84435c}")
    (IID_ITfUIElementMgr "{ea1ea135-19df-11d7-a6d2-00065b84435c}")
    (IID_ITfUIElementSink "{ea1ea136-19df-11d7-a6d2-00065b84435c}")
    (IID_ITfCandidateListUIElement "{ea1ea138-19df-11d7-a6d2-00065b84435c}")
    (IID_ITfCategoryMgr "{c3acefb5-f69d-4905-938f-fcadcf4be830}")
    (IID_ITfInputProcessorProfiles "{1f02b6c5-7842-4ee6-8a0b-9a24183a95ca}")
    (IID_ITfInputProcessorProfileMgr "{71c6e74c-0f28-11d8-a82a-00065b84435c}")
    (IID_ITextStoreACP "{28888fe3-c2a0-483a-a3ea-8cb1ce51ff3d}")
    (IID_ITextStoreACPSink "{22d44c94-a419-4542-a272-ae26093ececf}")
    (CLSID_TF_ThreadMgr "{529a9e6b-6587-4f23-ab9e-9c7d683e3c50}")
    (CLSID_TF_InputProcessorProfiles "{33c53a50-f456-4884-b049-85fd643ecfed}")
    (CLSID_TF_CategoryMgr "{a4b544a1-438d-4b41-9325-869523e2d6c7}")
    (CLSID_TF_LangBarMgr "{ebb08c45-6c4a-4fdc-ae53-4eb8c4c7db8e}")
    (GUID_TFCAT_TIP_KEYBOARD "{34745c63-b2f0-4784-8b67-5e12c8701a31}")
    (GUID_TFCAT_DISPLAYATTRIBUTEPROVIDER "{046b8c80-1647-40f7-9b21-b93b81aabc1b}")
    (GUID_TFCAT_TIPCAP_IMMERSIVESUPPORT "{13a016df-560b-46cd-947a-4c3af1e0e35d}")
    (GUID_TFCAT_TIPCAP_SYSTRAYSUPPORT "{25504fb4-7bab-4bc1-9c69-cf81890f0ef5}")
    (GUID_TFCAT_TIPCAP_UIELEMENTENABLED "{49d2f9ce-1f5e-11d7-a6d3-00065b84435c}")
    (GUID_TFCAT_TIPCAP_SECUREMODE "{49d2f9cf-1f5e-11d7-a6d3-00065b84435c}")
    (GUID_TFCAT_TIPCAP_COMLESS "{364215d9-75bc-11d7-a6ef-00065b84435c}")
    (GUID_TFCAT_TIPCAP_INPUTMODECOMPARTMENT "{ccf05dd7-4a87-11d7-a6e2-00065b84435c}")
    (GUID_PROP_ATTRIBUTE "{34b45670-7526-11d2-a147-00105a2799b5}")
    (GUID_COMPARTMENT_KEYBOARD_OPENCLOSE "{58273aad-01bb-4164-95c6-755ba0b5162d}")
    (GUID_COMPARTMENT_KEYBOARD_INPUTMODE_CONVERSION "{ccf05dd8-4a87-11d7-a6e2-00065b84435c}")
    (GUID_COMPARTMENT_KEYBOARD_INPUTMODE_SENTENCE "{ccf05dd9-4a87-11d7-a6e2-00065b84435c}")
    (GUID_LBI_INPUTMODE "{2c77a81e-41cc-4178-a3a7-5f8a987568e6}"))
  "Windows SDK msctf GUIDs used by the TSF text service.
Each entry is (NAME GUID-STRING).  See `nelisp-com-msctf-guids-unverified'.")

;; Cross-check (2026-11-12): every GUID above not listed here has its 16
;; in-memory bytes present in wine's own uuid table (activeds.dll of
;; wine-10.0 and Proton Experimental), so its value agrees with the SDK.  The
;; entries below are absent from wine's headers and are from memory of the
;; Windows SDK only; nelisp-skk-ime/windows/src/guids.h holds project GUIDs
;; only, so it cannot confirm them.  Verify on a Windows build before relying
;; on them for registration.
(defconst nelisp-com-msctf-guids-unverified
  '(IID_ITfTextInputProcessorEx IID_ITfDisplayAttributeProvider
    IID_ITfDisplayAttributeInfo IID_ITfFunction IID_ITfLangBarItemButton
    IID_ITfSystemLangBarItem IID_ITfCandidateListUIElement
    GUID_TFCAT_TIPCAP_IMMERSIVESUPPORT GUID_TFCAT_TIPCAP_SYSTRAYSUPPORT
    GUID_TFCAT_TIPCAP_UIELEMENTENABLED GUID_TFCAT_TIPCAP_SECUREMODE
    GUID_TFCAT_TIPCAP_COMLESS GUID_TFCAT_TIPCAP_INPUTMODECOMPARTMENT
    GUID_COMPARTMENT_KEYBOARD_INPUTMODE_CONVERSION
    GUID_COMPARTMENT_KEYBOARD_INPUTMODE_SENTENCE GUID_LBI_INPUTMODE)
  "Names in `nelisp-com-msctf-guids' that could not be cross-checked.")

(defun nelisp-com-msctf-guid (name)
  "Return the GUID string registered for NAME in `nelisp-com-msctf-guids'."
  (or (cadr (assq name nelisp-com-msctf-guids))
      (error "Unknown msctf GUID name: %S" name)))

(defun nelisp-com-iid-table (entries)
  "Return rodata GUID `data-blob' forms for ENTRIES, a list of (NAME GUID).
Each blob is named NAME, so (data-addr NAME) is the IID pointer."
  (cl-loop for (name guid) in entries collect (nelisp-com-guid-blob name guid)))

(defun nelisp-com-msctf-blobs (&optional names)
  "Return GUID blobs for NAMES (default: every entry of the msctf table)."
  (nelisp-com-iid-table
   (if names
       (mapcar (lambda (n) (list n (nelisp-com-msctf-guid n))) names)
     nelisp-com-msctf-guids)))

;;;; Structs with Win64 C layout

(defconst nelisp-com--scalar-types
  '((u8 1 ptr-read-u8 ptr-write-u8) (i8 1 ptr-read-s8 ptr-write-u8)
    (u16 2 ptr-read-u16 ptr-write-u16) (i16 2 ptr-read-s16 ptr-write-u16)
    (u32 4 ptr-read-u32 ptr-write-u32) (i32 4 ptr-read-s32 ptr-write-u32)
    (u64 8 ptr-read-u64 ptr-write-u64) (i64 8 ptr-read-s64 ptr-write-u64)
    (ptr 8 ptr-read-u64 ptr-write-u64))
  "Scalar field types: (TYPE SIZE READER WRITER); alignment equals size.")

(defvar nelisp-com--structs (make-hash-table :test 'eq)
  "Struct registry: NAME -> (SIZE ALIGN ((FIELD OFFSET TYPE SIZE) ...)).")

(defun nelisp-com--type-size-align (type)
  "Return (SIZE . ALIGN) of field TYPE.
TYPE is a scalar symbol, `guid' (16, align 4), (array T N), (struct NAME) or
(bytes N ALIGN)."
  (cond
   ((assq type nelisp-com--scalar-types)
    (let ((n (nth 1 (assq type nelisp-com--scalar-types)))) (cons n n)))
   ((eq type 'guid) (cons 16 4))
   ((eq (car-safe type) 'array)
    (let ((e (nelisp-com--type-size-align (nth 1 type))))
      (cons (* (car e) (nth 2 type)) (cdr e))))
   ((eq (car-safe type) 'struct)
    (let ((st (gethash (nth 1 type) nelisp-com--structs)))
      (unless st (error "Unknown struct %S" (nth 1 type)))
      (cons (nth 0 st) (nth 1 st))))
   ((eq (car-safe type) 'bytes) (cons (nth 1 type) (nth 2 type)))
   (t (error "Unknown struct field type %S" type))))

(defun nelisp-com--define-struct (name fields)
  "Register struct NAME with FIELDS, a list of (FIELD TYPE); return its size."
  (let ((off 0) (maxal 1) laid)
    (dolist (f fields)
      (let* ((sa (nelisp-com--type-size-align (nth 1 f)))
             (al (cdr sa)))
        (setq off (* al (ceiling off al)))
        (push (list (car f) off (nth 1 f) (car sa)) laid)
        (setq off (+ off (car sa)) maxal (max maxal al))))
    (let ((size (* maxal (ceiling off maxal))))
      (puthash name (list size maxal (nreverse laid)) nelisp-com--structs)
      size)))

(defmacro nelisp-com-defstruct (name &rest fields)
  "Define struct NAME with FIELDS, each (FIELD TYPE), using Win64 C layout.
TYPE is u8 i8 u16 i16 u32 i32 u64 i64 ptr guid, (array TYPE N),
\(struct NAME) or (bytes N ALIGN).  Read and write forms come from
`nelisp-com-field' and `nelisp-com-set-field'."
  (declare (indent 1))
  `(nelisp-com--define-struct ',name ',fields))

(defun nelisp-com--struct (name)
  (or (gethash name nelisp-com--structs) (error "Unknown struct %S" name)))

(defun nelisp-com-struct-size (name) "Return sizeof struct NAME."
       (nth 0 (nelisp-com--struct name)))
(defun nelisp-com-struct-align (name) "Return the alignment of struct NAME."
       (nth 1 (nelisp-com--struct name)))

(defun nelisp-com-struct-offset (name field)
  "Return the byte offset of FIELD in struct NAME."
  (or (nth 1 (assq field (nth 2 (nelisp-com--struct name))))
      (error "Struct %S has no field %S" name field)))

(defun nelisp-com--field (name field)
  (or (assq field (nth 2 (nelisp-com--struct name)))
      (error "Struct %S has no field %S" name field)))

(defun nelisp-com-field-addr (ptr name field)
  "Return a form for the address of FIELD of struct NAME at PTR."
  (let ((off (nelisp-com-struct-offset name field)))
    (if (= off 0) ptr `(+ ,ptr ,off))))

(defun nelisp-com-field (ptr name field)
  "Return a read form for scalar FIELD of struct NAME at PTR (evaluated once)."
  (let* ((f (nelisp-com--field name field))
         (sc (assq (nth 2 f) nelisp-com--scalar-types)))
    (unless sc (error "Field %S.%S is not scalar; use nelisp-com-field-addr"
                      name field))
    `(,(nth 2 sc) ,ptr ,(nth 1 f))))

(defun nelisp-com-set-field (ptr name field value)
  "Return a write form storing VALUE into scalar FIELD of struct NAME at PTR."
  (let* ((f (nelisp-com--field name field))
         (sc (assq (nth 2 f) nelisp-com--scalar-types)))
    (unless sc (error "Field %S.%S is not scalar" name field))
    `(,(nth 3 sc) ,ptr ,(nth 1 f) ,value)))

(defun nelisp-com-struct-forms (name)
  "Return heap helpers for struct NAME: NAME_alloc (zeroed, 0 on failure)
and NAME_free."
  (let ((alloc (nelisp-com--sym name "alloc")) (free (nelisp-com--sym name "free")))
    (list `(defun ,alloc ()
             (let ((h (extern-call GetProcessHeap)))
               (extern-call HeapAlloc h 8 ,(nelisp-com-struct-size name))))
          `(defun ,free (p)
             (if (= p 0) 0
               (let ((h (extern-call GetProcessHeap)))
                 (seq (extern-call HeapFree h 0 p) 0)))))))

;; Structs the TSF text service needs (sizes checked by the ERT golden tests).
(nelisp-com-defstruct TF_SELECTIONSTYLE (ase u32) (fInterimChar i32))
(nelisp-com-defstruct TF_SELECTION (range ptr) (style (struct TF_SELECTIONSTYLE)))
(nelisp-com-defstruct TF_DA_COLOR (type u32) (value u32))
(nelisp-com-defstruct TF_DISPLAYATTRIBUTE
  (crText (struct TF_DA_COLOR)) (crBk (struct TF_DA_COLOR))
  (lsStyle u32) (fBoldLine i32) (crLine (struct TF_DA_COLOR)) (bAttr u32))
(nelisp-com-defstruct TF_LANGUAGEPROFILE
  (clsid guid) (langid u16) (catid guid) (fActive i32) (guidProfile guid))
(nelisp-com-defstruct TF_PRESERVEDKEY (uVKey u32) (uModifiers u32))
(nelisp-com-defstruct POINT (x i32) (y i32))
(nelisp-com-defstruct RECT (left i32) (top i32) (right i32) (bottom i32))
(nelisp-com-defstruct MSG
  (hwnd ptr) (message u32) (wParam u64) (lParam i64) (time u32)
  (pt (struct POINT)))
(nelisp-com-defstruct WNDCLASSEXW
  (cbSize u32) (style u32) (lpfnWndProc ptr) (cbClsExtra i32) (cbWndExtra i32)
  (hInstance ptr) (hIcon ptr) (hCursor ptr) (hbrBackground ptr)
  (lpszMenuName ptr) (lpszClassName ptr) (hIconSm ptr))

;;;; HRESULT discipline

(defun nelisp-com-failed (expr)
  "Return a form that is true when the HRESULT EXPR is a failure code."
  `(/= (logand ,expr 2147483648) 0))

(defun nelisp-com-succeeded (expr)
  "Return a form that is true when the HRESULT EXPR is a success code."
  `(= (logand ,expr 2147483648) 0))

(defun nelisp-com-try (bindings result &rest cleanup)
  "Return a form chaining HRESULT calls with early return on failure.
BINDINGS is a list of (VAR FORM).  Each FORM is evaluated in order and its
HRESULT bound to VAR; on the first FAILED value the CLEANUP forms run (with
all earlier VARs in scope) and the failing HRESULT is the value of the whole
form, skipping everything after it.  If none fail the value is RESULT."
  (cl-labels
      ((chain (bs)
         (if (null bs) result
           (let ((var (car (car bs))) (form (nth 1 (car bs))))
             (unless (symbolp var) (error "nelisp-com-try: bad variable %S" var))
             `(let ((,var ,form))
                (if ,(nelisp-com-failed var)
                    ,(if cleanup `(seq ,@cleanup ,var) var)
                  ,(chain (cdr bs))))))))
    (chain bindings)))

;;;; Call-site helpers

(defun nelisp-com-guid= (ptr-var blob)
  "Return a form comparing the GUID at PTR-VAR (a variable symbol) to BLOB."
  (unless (symbolp ptr-var)
    (error "nelisp-com-guid=: PTR-VAR must be a variable, got %S" ptr-var))
  `(and (= (ptr-read-u64 ,ptr-var 0) (ptr-read-u64 (data-addr ,blob) 0))
        (= (ptr-read-u64 ,ptr-var 8) (ptr-read-u64 (data-addr ,blob) 8))))

(defun nelisp-com-call (obj slot &rest args)
  "Return a form calling vtable SLOT of the object OBJ with ARGS.
OBJ is a variable symbol (used directly) or any other expression, which is
evaluated exactly once through a let binding of `nl_com_obj'."
  (if (symbolp obj)
      `(call-ptr (ptr-read-u64 (ptr-read-u64 ,obj 0) ,(* 8 slot))
                 ,obj ,@args)
    `(let ((nl_com_obj ,obj))
       (call-ptr (ptr-read-u64 (ptr-read-u64 nl_com_obj 0) ,(* 8 slot))
                 nl_com_obj ,@args))))

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

(defun nelisp-com--extra-iids (spec)
  "Normalize :extra-iids SPEC to a list of (GUID . INTERFACE-INDEX)."
  (mapcar (lambda (e)
            (if (consp (cdr e))
                (cons (plist-get e :iid) (or (plist-get e :iface) 0))
              (cons (car e) (cdr e))))
          spec))

(defun nelisp-com--qi-chain (class ifaces &optional extras)
  "Return the nested-if QueryInterface body for CLASS with IFACES.
EXTRAS is a normalized extra-IID list; each answers with its interface's
vtable (alias IIDs such as ITfTextInputProcessorEx on interface 0)."
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
                 collect (cons i (nelisp-com--iface-iid-sym class i)))
        (cl-loop for x in extras for k from 0
                 collect (cons (cdr x) (nelisp-com--sym class "xiid" k))))))))

(defvar nelisp-com--classes (make-hash-table :test 'eq)
  "Class registry filled by `nelisp-com-class-forms': NAME -> (N STRUCT).")

(defun nelisp-com--class-struct (class)
  (let ((e (gethash class nelisp-com--classes)))
    (unless (and e (nth 1 e))
      (error "Class %S has no :fields (call nelisp-com-class-forms first)" class))
    e))

(defun nelisp-com-class-field (class base field)
  "Return a read form for state FIELD of CLASS at object pointer BASE."
  (let* ((e (nelisp-com--class-struct class))
         (st (nth 1 e)) (off (nelisp-com-state-offset (nth 0 e))))
    (nelisp-com-field `(+ ,base ,off) st field)))

(defun nelisp-com-class-set-field (class base field value)
  "Return a write form for state FIELD of CLASS at object pointer BASE."
  (let* ((e (nelisp-com--class-struct class))
         (st (nth 1 e)) (off (nelisp-com-state-offset (nth 0 e))))
    (nelisp-com-set-field `(+ ,base ,off) st field value)))

(defun nelisp-com-class-forms (class &rest spec)
  "Return AOT forms implementing COM class CLASS (a symbol).
SPEC is a plist:
  :interfaces  list of (:iid GUID :methods (FN...)) in object order;
               FN are user defun symbols for slots 3.. of that interface.
  :state-size  bytes of private per-instance state (default 0)
  :fields      named per-instance state, a list of (FIELD TYPE) laid out as
               a struct CLASS_state at the state offset (exclusive with
               :state-size); see `nelisp-com-class-field'
  :extra-iids  IIDs answered with an existing interface's vtable, each
               (:iid GUID :iface INDEX) or (GUID . INDEX); e.g.
               ITfTextInputProcessorEx on the ITfTextInputProcessor vtable
               whose slot 5 (ActivateEx) is listed in :methods
  :init        optional symbol of a defun (base) called after allocation
  :on-free     optional symbol of a defun (base) called before HeapFree
Generated names (prefix CLASS_): iid_unk, iid_N, vt_N, count, new, qi,
addref, release, qi_ok, iN_qi/addref/release thunks (N >= 1).
`CLASS_new' returns a base pointer with reference count 1 (0 on failure)."
  (let* ((ifaces (plist-get spec :interfaces))
         (n (length ifaces))
         (fields (plist-get spec :fields))
         (extras (nelisp-com--extra-iids (plist-get spec :extra-iids)))
         (state-size
          (progn
            (when (and fields (plist-get spec :state-size))
              (error "Class %s: :fields and :state-size are exclusive" class))
            (dolist (x extras)
              (unless (< (cdr x) (length ifaces))
                (error "Class %s: :extra-iids interface %S out of range"
                       class (cdr x))))
            (if fields
                (* 8 (ceiling (nelisp-com--define-struct
                               (nelisp-com--sym class "state") fields)
                              8))
              (or (plist-get spec :state-size) 0))))
         (size (+ (nelisp-com-state-offset n) state-size))
         (s (lambda (&rest p) (apply #'nelisp-com--sym class p)))
         (count (funcall s "count"))
         (qi (funcall s "qi")) (addref (funcall s "addref"))
         (release (funcall s "release")) (qi-ok (funcall s "qi_ok"))
         (new (funcall s "new"))
         (init (plist-get spec :init)) (on-free (plist-get spec :on-free))
         forms)
    (when (zerop n) (error "Class %s needs at least one interface" class))
    (puthash class (list n (and fields (nelisp-com--sym class "state")))
             nelisp-com--classes)
    (cl-loop for x in extras for k from 0
             do (push (nelisp-com-guid-blob (nelisp-com--sym class "xiid" k)
                                            (car x))
                      forms))
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
               ,(nelisp-com--qi-chain class ifaces extras)))
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

(defun nelisp-com-default-hive ()
  "Return the registration hive used when :hive is not given.
`hkcu' (HKEY_CURRENT_USER\\Software\\Classes, parity with the C++ shim)
unless the environment variable NELISP_COM_HIVE is \"hkcr\".  wine ignores
HKCU class registrations for CoCreateInstance, so wine tests build with
NELISP_COM_HIVE=hkcr (HKEY_CLASSES_ROOT, which wine maps to HKLM)."
  (if (equal (getenv "NELISP_COM_HIVE") "hkcr") 'hkcr 'hkcu))

(cl-defun nelisp-com-server-forms (classes &key hive)
  "Return AOT forms for the four DLL exports serving CLASSES.
CLASSES is a list of plists (:name SYM :clsid GUID :threading \"Apartment\")
whose class and factory forms come from `nelisp-com-class-forms' and
`nelisp-com-factory-forms'.  The module path is obtained with
GetModuleHandleExW(FROM_ADDRESS|UNCHANGED_REFCOUNT) on DllRegisterServer.
HIVE is `hkcu' or `hkcr' (default `nelisp-com-default-hive'): `hkcu'
writes HKEY_CURRENT_USER\\Software\\Classes\\CLSID\\..., `hkcr' writes
HKEY_CLASSES_ROOT\\CLSID\\....  DllUnregisterServer deletes the CLSID
subtree in the same hive."
  (let* ((hive (or hive (nelisp-com-default-hive)))
         (root (pcase hive
                 ('hkcu nelisp-com-hkey-current-user)
                 ('hkcr nelisp-com-hkey-classes-root)
                 (_ (error "Unknown registration hive %S" hive))))
         (prefix (if (eq hive 'hkcu) "Software\\Classes\\" ""))
         forms)
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
                                 (concat prefix "CLSID\\" clsid "\\InprocServer32"))
           forms)
     (push (nelisp-com-wstr-blob (nelisp-com--sym name "w_root")
                                 (concat prefix "CLSID\\" clsid))
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
              (extern-call RegCreateKeyExW ,root key
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
                                   RegDeleteTreeW ,root
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
