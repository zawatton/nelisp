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
;;   NELISP_COM_HOOKS=1|fail      server with custom registration hooks: an
;;                                 extra key HookTest (REG_SZ, DWORD and the
;;                                 module path) written by :register-forms and
;;                                 deleted by :unregister-forms; "fail" adds a
;;                                 last hook returning E_FAIL so registration must
;;                                 fail and roll everything back
;;   NELISP_COM_FORCE_REBASE=1     positive control: client.exe reserves the
;;                                 preferred base but the DLL keeps .reloc,
;;                                 so the run must still pass (rebased load)

;;; Code:

(require 'cl-lib)
(require 'nelisp-com-dsl)

(defconst nelisp-com-sample-clsid "{7a1c0de0-5f3a-4a6e-9d11-0123456789ab}")
(defconst nelisp-com-sample-iid "{7a1c0de1-5f3a-4a6e-9d11-0123456789ab}")

(defconst nelisp-com-sample-hook-key "NelispComHookTest")

(defun nelisp-com-sample--hook-forms (hive mode)
  "Return (BLOBS REGISTER-FORMS UNREGISTER-FORMS) for the hook sample.
MODE is the string \"1\" or \"fail\"."
  (let* ((root (nelisp-com-hive-root hive))
         (prefix (if (eq hive 'hkcu) "Software\\Classes\\" "")))
    (list
     (append (nelisp-com-registry-blobs)
             (list (nelisp-com-wstr-blob
                    'hk_key (concat prefix nelisp-com-sample-hook-key))
                   (nelisp-com-wstr-blob 'hk_name "Name")
                   (nelisp-com-wstr-blob 'hk_val "hookval")
                   (nelisp-com-wstr-blob 'hk_cnt "Count")))
     `(,(nelisp-com-with-hkey
         `(k ,root (data-addr hk_key))
         `(let ((a ,(nelisp-com-reg-set-sz-literal 'k 'hk_name 'hk_val "hookval")))
            (if (/= a 0) a
              (let ((b ,(nelisp-com-reg-set-dword 'k 'hk_cnt 7)))
                (if (/= b 0) b
                  ,(nelisp-com-reg-set-sz
                    'k nil '(data-addr com_path)
                    '(ptr-read-u64 (data-addr com_pathbytes) 0)))))))
       ,@(when (equal mode "fail") (list nelisp-com-e-fail)))
     `(,(nelisp-com-reg-delete-tree root '(data-addr hk_key))))))

(defun nelisp-com-sample-dll-forms (&optional hive)
  "Return the AOT forms of the sample server.
HIVE defaults to `nelisp-com-default-hive' (HKCU unless NELISP_COM_HIVE=hkcr)."
  (let* ((hive (or hive (nelisp-com-default-hive)))
         (mode (getenv "NELISP_COM_HOOKS"))
         (hooks (and mode (not (string-empty-p mode))
                     (nelisp-com-sample--hook-forms hive mode))))
    (append
     (nth 0 hooks)
     (nelisp-com-sample--dll-forms hive (nth 1 hooks) (nth 2 hooks)))))

(defun nelisp-com-sample--dll-forms (hive register unregister)
  (append
   (nelisp-com-class-forms
    'o :interfaces `((:iid ,nelisp-com-sample-iid :methods (o_getvalue))))
   (nelisp-com-factory-forms 'o)
   '((defun o_getvalue (this) 42))
   (nelisp-com-server-forms
    `((:name o :clsid ,nelisp-com-sample-clsid :threading "Both"))
    :hive hive :register-forms register :unregister-forms unregister)))

(defconst nelisp-com-sample-regsize-imports
  '(("kernel32.dll" "ExitProcess")
    ("advapi32.dll" "RegOpenKeyExW" "RegQueryValueExW" "RegCloseKey")))

(defun nelisp-com-sample-regsize-forms (&optional hive)
  "Return forms of regsize.exe: check the stored InprocServer32 default value.
Exit 42 when the registry size equals 2 * (wcslen + 1), 50 when it does not,
51 when the key or value cannot be read."
  (let* ((hive (or hive (nelisp-com-default-hive)))
         (prefix (if (eq hive 'hkcu) "Software\\Classes\\" "")))
    `(,(nelisp-com-wstr-blob
        'rs_key (concat prefix "CLSID\\" nelisp-com-sample-clsid "\\InprocServer32"))
      (data-blob rs_hkey ,(make-list 8 0) data)
      (data-blob rs_size ,(make-list 8 0) data)
      (data-blob rs_buf ,(make-list 2080 0) data)
      (defun _start ()
        (if (/= (extern-call RegOpenKeyExW ,(nelisp-com-hive-root hive)
                             (data-addr rs_key) 0 131097 (data-addr rs_hkey))
                0)
            (extern-call ExitProcess 51)
          (seq
           (ptr-write-u64 (data-addr rs_size) 0 2080)
           (let ((rc (extern-call RegQueryValueExW
                                  (ptr-read-u64 (data-addr rs_hkey) 0) 0 0 0
                                  (data-addr rs_buf) (data-addr rs_size))))
             (let ((n 0))
               (seq (extern-call RegCloseKey (ptr-read-u64 (data-addr rs_hkey) 0))
                    (if (/= rc 0) (extern-call ExitProcess 51)
                      (seq (while (/= (ptr-read-u16 (data-addr rs_buf) (* 2 n)) 0)
                             (setq n (+ n 1)))
                           (extern-call ExitProcess
                                        (if (= (ptr-read-u64 (data-addr rs_size) 0)
                                               (* 2 (+ n 1)))
                                            42 50)))))))))))))

(defun nelisp-com-sample-client-forms (&optional bad-iid slot reserve-base)
  "Return client forms.
BAD-IID requests an unsupported IID, SLOT is the GetValue slot (default 3),
RESERVE-BASE is a form evaluating to an address to reserve first."
  (let ((iid (if bad-iid 'g_clsid 'g_iid)))
    `((data-blob g_clsid ,(nelisp-com-guid-bytes nelisp-com-sample-clsid) rodata)
      (data-blob g_iid ,(nelisp-com-guid-bytes nelisp-com-sample-iid) rodata)
      (data-blob pobj ,(make-list 8 0) data)
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
    (nelisp-com-build-exe (expand-file-name "regsize.exe" dir)
                          (nelisp-com-sample-regsize-forms)
                          nelisp-com-sample-regsize-imports)
    (nelisp-com-build-exe (expand-file-name "client-badiid.exe" dir)
                          (nelisp-com-sample-client-forms t)
                          nelisp-com-sample-client-imports))
  dir)

(provide 'com-sample)
;;; com-sample.el ends here
