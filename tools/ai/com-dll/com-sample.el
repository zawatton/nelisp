;;; com-sample.el --- S2 sample COM in-proc server and nelisp-built client  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds com.dll (one class, one custom interface whose GetValue returns
;; 42) and client EXEs with `nelisp-com-dsl', for tools/ai/com-dll/wine-e2e.sh.
;;
;; Client exit codes: 42 success, 10 CoInitializeEx failed, 11 wrong value
;; or reference count, 12 base reservation failed, 100+(HRESULT & 255) when
;; CoCreateInstance fails (184 = REGDB_E_CLASSNOTREG, 102 = E_NOINTERFACE).
;;
;; Negative-control hooks (environment variables, test-only):
;;   NELISP_COM_BAD_IID=1          client.exe requests an unsupported IID
;;   NELISP_COM_DROP_EXPORT=NAME   drop one export from the DLL
;;   NELISP_COM_WRONG_SLOT=1       client.exe calls vtable slot 1 (AddRef)
;;                                 where GetValue lives (slot 3)
;;   NELISP_COM_NO_RELOC=1         DLL without .reloc, and client.exe first
;;                                 reserves the DLL's preferred base so the
;;                                 loader must rebase it
;;   NELISP_COM_HIVE=hkcr          register under HKCR instead of the library
;;                                 default HKCU (wine ignores HKCU class
;;                                 registrations, so wine-e2e.sh sets this)
;;   NELISP_COM_FORCE_REBASE=1     positive control: client.exe reserves the
;;                                 preferred base but the DLL keeps .reloc,
;;                                 so the run must still pass (rebased load)

;;; Code:

(require 'cl-lib)
(require 'nelisp-com-dsl)

(defconst nelisp-com-sample-clsid "{7a1c0de0-5f3a-4a6e-9d11-0123456789ab}")
(defconst nelisp-com-sample-iid "{7a1c0de1-5f3a-4a6e-9d11-0123456789ab}")

(defun nelisp-com-sample-dll-forms (&optional hive)
  "Return the AOT forms of the sample server.
HIVE defaults to `nelisp-com-default-hive' (HKCU unless NELISP_COM_HIVE=hkcr)."
  (append
   (nelisp-com-class-forms
    'o :interfaces `((:iid ,nelisp-com-sample-iid :methods (o_getvalue))))
   (nelisp-com-factory-forms 'o)
   '((defun o_getvalue (this) 42))
   (nelisp-com-server-forms
    `((:name o :clsid ,nelisp-com-sample-clsid :threading "Both"))
    :hive hive)))

(defun nelisp-com-sample-client-forms (&optional bad-iid slot reserve-base)
  "Return client forms.
BAD-IID requests an unsupported IID, SLOT is the GetValue slot (default 3),
RESERVE-BASE is a form evaluating to an address to reserve first."
  (let ((iid (if bad-iid 'g_clsid 'g_iid)))
    `((data-blob g_clsid ,(nelisp-com-guid-bytes nelisp-com-sample-clsid) rodata)
      (data-blob g_iid ,(nelisp-com-guid-bytes nelisp-com-sample-iid) rodata)
      (data-blob pobj ,(make-list 8 0) data)
      ;; VirtualAlloc is on the compiler's dynamic-align list, whose
      ;; extern-call arm spills rsp into this driver-owned slot.
      ,@(when reserve-base '((data-blob nl_dynalign_rsp_save (0 0 0 0 0 0 0 0) data)))
      (defun run ()
        (let ((hr (extern-call CoInitializeEx 0 0)))
          (if (/= (logand hr 2147483648) 0) (extern-call ExitProcess 10)
            (let ((hr2 (extern-call CoCreateInstance (data-addr g_clsid) 0 1
                                    (data-addr ,iid) (data-addr pobj))))
              (if (/= hr2 0) (extern-call ExitProcess (+ 100 (logand hr2 255)))
                (let* ((obj (ptr-read-u64 (data-addr pobj) 0))
                       (v ,(nelisp-com-call 'obj (or slot 3)))
                       (rc ,(nelisp-com-call 'obj 2)))
                  (seq (extern-call CoUninitialize)
                       (extern-call ExitProcess
                                    (if (and (= v 42) (= rc 0)) 42 11)))))))))
      (defun _start ()
        ,(if reserve-base
             `(let ((base ,reserve-base))
                (if (= (extern-call VirtualAlloc base 1048576 12288 4) 0)
                    (extern-call ExitProcess 12)
                  (run)))
           '(run))))))

(defconst nelisp-com-sample-client-imports
  '(("kernel32.dll" "ExitProcess" "VirtualAlloc")
    ("ole32.dll" "CoInitializeEx" "CoCreateInstance" "CoUninitialize")))

(defun nelisp-com-sample-build-all (dir)
  "Build com.dll, client.exe and client-badiid.exe into DIR."
  (make-directory dir t)
  (let* ((drop (getenv "NELISP_COM_DROP_EXPORT"))
         (no-reloc (equal (getenv "NELISP_COM_NO_RELOC") "1"))
         (exports (cl-remove-if (lambda (n) (and drop (string= n drop)))
                                nelisp-com-server-exports)))
    (nelisp-com-build-dll (expand-file-name "com.dll" dir)
                          (nelisp-com-sample-dll-forms)
                          :exports exports :no-reloc no-reloc)
    (nelisp-com-build-exe
     (expand-file-name "client.exe" dir)
     (nelisp-com-sample-client-forms
      (equal (getenv "NELISP_COM_BAD_IID") "1")
      (and (equal (getenv "NELISP_COM_WRONG_SLOT") "1") 1)
      ;; 0x180000000 = default DLL preferred base, written without a
      ;; literal above 2^32.
      (and (or no-reloc (equal (getenv "NELISP_COM_FORCE_REBASE") "1"))
           '(* 6 1073741824)))
     nelisp-com-sample-client-imports)
    (nelisp-com-build-exe (expand-file-name "client-badiid.exe" dir)
                          (nelisp-com-sample-client-forms t)
                          nelisp-com-sample-client-imports))
  dir)

(provide 'com-sample)
;;; com-sample.el ends here
