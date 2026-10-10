;;; nelisp-native-windows.el --- Win64 trust and OS boundaries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)

(defun nelisp-native-windows-call (name &rest args)
  "Call the reader's fixed system-DLL FFI entry NAME. Unknown entries refuse."
  (let ((result (apply #'nl-ffi-call name args)))
    (unless (integerp result) (error "Windows FFI unavailable: %s" name))
    result))

(defun nelisp-native-windows-buffer (size)
  "Reserve a scratch buffer outside the Lisp collector."
  (let ((address (nelisp-native-windows-call "VirtualAlloc" 0 size #x3000 4)))
    (unless (> address 0) (error "Windows scratch allocation failed"))
    address))

(defun nelisp-native-windows-free (address)
  "Release the original reservation ADDRESS."
  (unless (= 1 (nelisp-native-windows-call "VirtualFree" address 0 #x8000))
    (error "Windows reservation release failed")))

(defun nelisp-native-windows-wide (string)
  "Allocate strict UTF-16LE STRING with one terminal NUL. Caller releases it."
  (unless (and (stringp string) (< 0 (length string) 32768)
               (not (string-match-p "\0" string)))
    (error "Windows path bound rejected"))
  (let ((units nil))
    (mapc (lambda (char)
            (cond ((or (< char 0) (> char #x10ffff) (<= #xd800 char #xdfff))
                   (error "Invalid Unicode scalar"))
                  ((< char #x10000) (push char units))
                  (t (let ((word (- char #x10000)))
                       (push (+ #xd800 (ash word -10)) units)
                       (push (+ #xdc00 (logand word #x3ff)) units))))) string)
    (setq units (nreverse units))
    (let ((buffer (nelisp-native-windows-buffer (* 2 (1+ (length units))))) (index 0))
      (dolist (word units)
        (ptr-write-u8 buffer index (logand word 255))
        (ptr-write-u8 buffer (1+ index) (ash word -8))
        (setq index (+ index 2)))
      buffer)))

(defun nelisp-native-windows-decode-wide (buffer count)
  "Decode exactly COUNT UTF-16 units, refusing unpaired surrogates and NUL."
  (let ((index 0) chars)
    (while (< index count)
      (let ((word (+ (ptr-read-u8 buffer (* 2 index))
                     (* 256 (ptr-read-u8 buffer (1+ (* 2 index)))))))
        (setq index (1+ index))
        (cond ((<= #xd800 word #xdbff)
               (unless (< index count) (error "Truncated UTF-16 surrogate"))
               (let ((low (+ (ptr-read-u8 buffer (* 2 index))
                             (* 256 (ptr-read-u8 buffer (1+ (* 2 index)))))))
                 (unless (<= #xdc00 low #xdfff) (error "Invalid UTF-16 surrogate"))
                 (setq index (1+ index))
                 (push (+ #x10000 (* 1024 (- word #xd800)) (- low #xdc00)) chars)))
              ((or (= word 0) (<= #xdc00 word #xdfff)) (error "Invalid UTF-16 path"))
              (t (push word chars)))))
    (apply #'string (nreverse chars))))

(defun nelisp-native-windows-module-path ()
  "Obtain this process's module identity from GetModuleFileNameW(NULL)."
  (let ((buffer (nelisp-native-windows-buffer 65536)))
    (unwind-protect
        (let ((count (nelisp-native-windows-call "GetModuleFileNameW" 0 buffer 32768)))
          (unless (< 0 count 32768) (error "Running PE module path truncated"))
          (nelisp-native-windows-decode-wide buffer count))
      (nelisp-native-windows-free buffer))))

(defun nelisp-native-windows-close (handle)
  (unless (= 1 (nelisp-native-windows-call "CloseHandle" handle))
    (error "Windows handle close failed")))

(defun nelisp-native-windows-open (path access disposition flags &optional security share)
  "Open PATH without following a final reparse point, with no delete sharing.
SHARE defaults to FILE_SHARE_READ (1)."
  (let ((wide (nelisp-native-windows-wide path)))
    (unwind-protect
        (let ((handle (nelisp-native-windows-call "CreateFileW" wide access (or share 1)
                                                (or security 0) disposition
                                                (logior flags #x200000) 0)))
          (unless (and (> handle 0) (/= handle #xffffffffffffffff) (/= handle -1))
            (error "Windows file open refused: %s" path))
          handle)
      (nelisp-native-windows-free wide))))

(defun nelisp-native-windows-attributes (handle)
  "Read actual handle attributes; never infer NTFS trust from POSIX modes."
  (let ((buffer (nelisp-native-windows-buffer 64)))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "GetFileInformationByHandle" handle buffer))
            (error "Windows file identity unavailable"))
          (ptr-read-u32 buffer 0))
      (nelisp-native-windows-free buffer))))

(defun nelisp-native-windows-read-handle (handle offset size)
  "Read exactly SIZE bytes at OFFSET from a pinned HANDLE."
  (unless (and (integerp offset) (<= 0 offset) (integerp size) (<= 0 size 134217728))
    (error "Windows file window bound rejected"))
  (let ((buffer (nelisp-native-windows-buffer (+ 16 (max size 1)))))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "SetFilePointerEx" handle offset 0 0))
            (error "Windows seek failed"))
          (unless (and (= 1 (nelisp-native-windows-call "ReadFile" handle (+ buffer 16) size buffer 0))
                       (= size (ptr-read-u32 buffer 0)))
            (error "Windows file window truncated"))
          (ptr-read-bytes (+ buffer 16) size))
      (nelisp-native-windows-free buffer))))

(defun nelisp-native-windows-file-bytes (path &optional private offset size)
  "Read complete pinned PATH, or a bounded window. PRIVATE requires cache ACL."
  (let ((handle (nelisp-native-windows-open path #x80020000 3 0)))
    (unwind-protect
        (progn
          (when (/= 0 (logand #x400 (nelisp-native-windows-attributes handle)))
            (error "Windows reparse file refused"))
          (when private (nelisp-native-windows-check-security handle))
          (if size (nelisp-native-windows-read-handle handle offset size)
            (let ((buffer (nelisp-native-windows-buffer 8)))
              (unwind-protect
                  (progn
                    (unless (= 1 (nelisp-native-windows-call "GetFileSizeEx" handle buffer))
                      (error "Windows file size unavailable"))
                    (let ((length (ptr-read-u64 buffer 0)))
                      (unless (< 0 length 134217729) (error "Windows file size bound"))
                      (nelisp-native-windows-read-handle handle 0 length)))
                (nelisp-native-windows-free buffer)))))
      (nelisp-native-windows-close handle))))

(defun nelisp-native-windows-sid-string (sid)
  "Convert an OS-owned SID to its canonical string."
  (let ((slot (nelisp-native-windows-buffer 8)))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "ConvertSidToStringSidW" sid slot))
            (error "SID conversion failed"))
          (let ((wide (ptr-read-u64 slot 0)) (count 0))
            (unwind-protect
                (progn
                  (while (and (< count 256) (/= 0 (+ (ptr-read-u8 wide (* 2 count))
                                                    (* 256 (ptr-read-u8 wide (1+ (* 2 count)))))))
                    (setq count (1+ count)))
                  (unless (< count 256) (error "SID string bound"))
                  (nelisp-native-windows-decode-wide wide count))
              (nelisp-native-windows-call "LocalFree" wide))))
      (nelisp-native-windows-free slot))))

(defun nelisp-native-windows-user-sid ()
  "Read TokenUser from the current process token."
  (let ((buffer (nelisp-native-windows-buffer 65552)) (token nil))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "OpenProcessToken"
                           (nelisp-native-windows-call "GetCurrentProcess") 8 buffer))
            (error "Process token unavailable"))
          (setq token (ptr-read-u64 buffer 0))
          (unless (= 1 (nelisp-native-windows-call "GetTokenInformation" token 1
                                                  (+ buffer 16) 65536 (+ buffer 8)))
            (error "TokenUser unavailable"))
          (nelisp-native-windows-sid-string (ptr-read-u64 buffer 16)))
      (when token (nelisp-native-windows-close token))
      (nelisp-native-windows-free buffer))))

(defun nelisp-native-windows-security-trusted-p (owner user control aces attributes)
  "Pure conservative NTFS predicate shared by real handles and host controls.
Only explicit basic allow/deny ACEs are admitted. Every write grant must be
explicit and belong to TokenUser. A protected, non-null DACL is mandatory."
  (and (stringp user) (equal owner user) (= 0 (logand attributes #x400))
       (/= 0 (logand control #x1000)) (/= 0 (logand control 4))
       (consp aces) (<= (length aces) 4096)
       (cl-every (lambda (ace)
                   (and (memq (plist-get ace :type) '(0 1))
                        (integerp (plist-get ace :mask))
                        (stringp (plist-get ace :sid))
                        (or (= (plist-get ace :type) 1)
                            (= 0 (logand (plist-get ace :mask) #x500d0156))
                            (and (= 0 (logand (plist-get ace :flags) #x18))
                                 (equal user (plist-get ace :sid)))))) aces)))

(defun nelisp-native-windows-check-security (handle)
  "Authenticate owner and protected DACL from the opened object HANDLE."
  (let ((buffer (nelisp-native-windows-buffer 65584)))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "GetKernelObjectSecurity" handle 5
                                                  (+ buffer 48) 65536 buffer))
            (error "Cache security descriptor unavailable"))
          (let ((sd (+ buffer 48)) aces)
            (unless (and (= 1 (nelisp-native-windows-call "GetSecurityDescriptorOwner" sd (+ buffer 8) (+ buffer 16)))
                         (= 1 (nelisp-native-windows-call "GetSecurityDescriptorDacl" sd (+ buffer 16) (+ buffer 24) (+ buffer 32)))
                         (= 1 (nelisp-native-windows-call "GetSecurityDescriptorControl" sd (+ buffer 32) (+ buffer 40)))
                         (= 1 (ptr-read-u32 buffer 16)) (> (ptr-read-u64 buffer 24) 0))
              (error "Cache null/invalid DACL refused"))
            (let* ((acl (ptr-read-u64 buffer 24))
                   (count (+ (ptr-read-u8 acl 4) (* 256 (ptr-read-u8 acl 5)))))
              (unless (<= 1 count 4096) (error "Cache ACL count bound"))
              (dotimes (index count)
                (unless (= 1 (nelisp-native-windows-call "GetAce" acl index (+ buffer 40)))
                  (error "Cache ACE unavailable"))
                (let* ((ace (ptr-read-u64 buffer 40)) (type (ptr-read-u8 ace 0)))
                  (unless (memq type '(0 1)) (error "Cache complex ACE refused"))
                  (push (list :type type :flags (ptr-read-u8 ace 1) :mask (ptr-read-u32 ace 4)
                              :sid (nelisp-native-windows-sid-string (+ ace 8))) aces))))
            (unless (nelisp-native-windows-security-trusted-p
                     (nelisp-native-windows-sid-string (ptr-read-u64 buffer 8))
                     (nelisp-native-windows-user-sid)
                     (+ (ptr-read-u8 buffer 32) (* 256 (ptr-read-u8 buffer 33)))
                     aces (nelisp-native-windows-attributes handle))
              (error "Cache owner/DACL/reparse trust refused"))))
      (nelisp-native-windows-free buffer))))

(defun nelisp-native-windows-security (thunk)
  "Call THUNK with a protected current-user SECURITY_ATTRIBUTES pointer."
  (let* ((wide (nelisp-native-windows-wide
                (concat "O:" (nelisp-native-windows-user-sid) "D:P(A;;FA;;;"
                        (nelisp-native-windows-user-sid) ")")))
         (buffer (nelisp-native-windows-buffer 32)) (sd nil))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "ConvertStringSecurityDescriptorToSecurityDescriptorW"
                                                  wide 1 (+ buffer 24) 0))
            (error "Private DACL construction failed"))
          (setq sd (ptr-read-u64 buffer 24))
          (ptr-write-u32 buffer 0 24)
          (ptr-write-u64 buffer 8 sd)
          (funcall thunk buffer sd))
      (when sd (nelisp-native-windows-call "LocalFree" sd))
      (nelisp-native-windows-free buffer)
      (nelisp-native-windows-free wide))))

(defvar nelisp-native-windows-directory-handles nil
  "Pinned ancestor handles (without delete sharing), held until process exit.")

(defun nelisp-native-windows-private-directory (directory)
  "Create a protected directory and pin every ancestor against replacement.
Only local drive paths are eligible. Ancestors may be public but never reparse;
the cache directory itself must have the current owner's protected DACL."
  (let ((path (directory-file-name (expand-file-name directory))))
    (unless (string-match-p "\\`[A-Za-z]:[/\\\\]" path)
      (error "Windows cache requires a local absolute drive path"))
    (let* ((parts (split-string (substring path 3) "[/\\\\]" t))
           (prefix (substring path 0 3)))
      (dolist (part parts)
        (when (member part '("." "..")) (error "Cache path traversal refused"))
        (setq prefix (concat (file-name-as-directory prefix) part))
        (unless (assoc prefix nelisp-native-windows-directory-handles)
          (unless (file-exists-p prefix)
            (nelisp-native-windows-security
             (lambda (sa _sd)
               (let ((wide (nelisp-native-windows-wide prefix)))
                 (unwind-protect
                     (unless (or (= 1 (nelisp-native-windows-call "CreateDirectoryW" wide sa))
                                 (= 183 (nelisp-native-windows-call "GetLastError")))
                       (error "Private directory creation failed"))
                   (nelisp-native-windows-free wide))))))
          ;; Pin the ancestor against rename or deletion (no FILE_SHARE_DELETE),
          ;; but share writes: NTFS opens the parent directory for write when a
          ;; child is renamed into it, and a read-only share mode made every
          ;; MoveFileExW publication fail with ERROR_SHARING_VIOLATION (32).
          (let ((handle (nelisp-native-windows-open prefix #x20001 3 #x2000000 nil 3)))
            (condition-case err
                (progn
                  (unless (= #x10 (logand (nelisp-native-windows-attributes handle) #x410))
                    (error "Cache ancestor is reparse or not a directory"))
                  (push (cons prefix handle) nelisp-native-windows-directory-handles))
              (error (nelisp-native-windows-close handle) (signal (car err) (cdr err)))))))
      (nelisp-native-windows-check-security (cdr (assoc path nelisp-native-windows-directory-handles))))
    directory))

(defun nelisp-native-windows-temporary (prefix)
  "Create a fresh temporary with its protected DACL from the first instant.
CREATE_NEW never adopts an existing file or its outstanding metadata handles."
  (let ((file (make-temp-name prefix)))
    (nelisp-native-windows-security
     (lambda (sa _sd)
       (let ((handle (nelisp-native-windows-open file #xc00e0000 1 0 sa)))
         (unwind-protect (nelisp-native-windows-check-security handle)
           (nelisp-native-windows-close handle)))))
    file))

(defun nelisp-native-windows-protect-file (file)
  "Protect FILE before serialization; exclude concurrent writers while sealing.
The empty temporary may have a token default DACL. Generated bytes are written
only after this check succeeds, and subsequent opens require the private DACL."
  (let ((handle (nelisp-native-windows-open file #xc00e0000 3 0)))
    (unwind-protect
        (progn
          (nelisp-native-windows-security
           (lambda (_sa sd)
             (unless (= 0 (nelisp-native-windows-call "SetSecurityInfo" handle 1 #x80000004 0 0
                          (+ sd (ptr-read-u32 sd 16)) 0))
               (error "Private file DACL installation failed"))))
          (nelisp-native-windows-check-security handle))
      (nelisp-native-windows-close handle))))

(defun nelisp-native-windows-publish (temporary final)
  "Recheck the protected temporary and atomically publish once."
  (nelisp-native-windows-protect-file temporary)
  (let ((from (nelisp-native-windows-wide temporary)) (to (nelisp-native-windows-wide final)))
    (unwind-protect
        (let ((attempt 0) (code nil) (done nil))
          ;; A virus scanner or the indexer may briefly hold the new
          ;; temporary open; retry sharing-violation and access-denied
          ;; failures for up to about 5 s before giving up.
          (while (not done)
            (if (= 1 (nelisp-native-windows-call "MoveFileExW" from to 8))
                (setq done t code 0)
              (setq code (nelisp-native-windows-call "GetLastError")
                    attempt (1+ attempt))
              (if (and (memq code '(5 32 33)) (< attempt 50))
                  ;; The reader has no `sleep-for' and its fixed FFI table no
                  ;; Sleep entry; this rare path waits by polling the clock.
                  (let ((until (+ (float-time) 0.1)))
                    (while (< (float-time) until)))
                (setq done t))))
          (cond
           ((eql code 0) t)
           ((memq code '(80 183))
            ;; Racing winner must satisfy precisely the same trust predicate.
            (nelisp-native-windows-file-bytes final t)
            nil)
           ((and (file-exists-p final) (not (file-exists-p temporary)))
            ;; The move happened although the error read back afterwards was
            ;; not MoveFileExW's own; verify the published file the same way.
            (nelisp-native-windows-file-bytes final t)
            t)
           (t (error "Atomic Windows cache publication failed (error %s after %d attempts)"
                     code attempt))))
      (nelisp-native-windows-free from) (nelisp-native-windows-free to))))

(provide 'nelisp-native-windows)

(defun nelisp-native-windows-map (size)
  "Reserve and commit RW pages, never RWX."
  (let ((address (nelisp-native-windows-call "VirtualAlloc" 0 size #x3000 4)))
    (unless (> address 0) (error "Windows native mapping failed")) address))
(defun nelisp-native-windows-protect (address size protection)
  "Publish R or RX pages, checking protection and instruction-cache publication."
  (unless (memq protection '(1 5)) (error "Windows native protection refused"))
  (let ((buffer (nelisp-native-windows-buffer 64))
        (page (if (= protection 1) 2 #x20)))
    (unwind-protect
        (progn
          (unless (= 1 (nelisp-native-windows-call "VirtualProtect" address size page buffer))
            (error "Windows native protection failed"))
          (unless (and (= 48 (nelisp-native-windows-call "VirtualQuery" address buffer 48))
                       (= page (ptr-read-u32 buffer 36)))
            (error "Windows published page protection differs"))
          (when (= protection 5)
            (unless (= 1 (nelisp-native-windows-call "FlushInstructionCache"
                             (nelisp-native-windows-call "GetCurrentProcess") address size))
              (error "Windows instruction-cache publication failed")))
          0)
      (nelisp-native-windows-free buffer))))
(defun nelisp-native-windows-unmap (address _size)
  "Release the original reservation with MEM_RELEASE."
  (nelisp-native-windows-free address) 0)

(defun nelisp-native-windows-verify-imports (read-window sections symbols names)
  "Prove bounded OS terminals from actual PE thunk, IAT and system DLL export.
The authenticated helper text can terminate only at the fixed kernel32 set."
  (unless (equal names '("ExitProcess" "VirtualAlloc" "VirtualFree"))
    (error "Win64 kernel terminal policy rejected"))
  (let ((imports (nelisp-native-pe-symbols-imports read-window sections))
        (wide (nelisp-native-windows-wide "kernel32.dll"))
        (scratch (nelisp-native-windows-buffer 256)))
    (unwind-protect
        (let ((module (nelisp-native-windows-call "GetModuleHandleW" wide)))
          (unless (> module 0) (error "Kernel module identity unavailable"))
          (dolist (name names)
            (let* ((matches (cl-remove-if-not (lambda (item) (equal name (plist-get item :name))) imports))
                   (import (car matches)) (symbol (gethash name symbols))
                   (section (and symbol (aref sections (plist-get symbol :section))))
                   (address (and symbol (plist-get symbol :address))))
              (unless (and (= (length matches) 1) (equal (plist-get import :dll) "kernel32.dll")
                           symbol (= (plist-get symbol :type) 2) (= 1 (plist-get section :type))
                           (/= 0 (logand (plist-get section :flags) 4)))
                (error "PE kernel terminal ownership rejected"))
              (let* ((file (funcall read-window (+ (plist-get section :offset)
                                                  (- address (plist-get section :address))) 6))
                     (displacement (nelisp-native-pe-symbols-u file 2 4)))
                (when (>= displacement #x80000000) (setq displacement (- displacement #x100000000)))
                (unless (and (equal (substring file 0 2) (unibyte-string #xff #x25))
                             (= (+ address 6 displacement) (plist-get import :address))
                             (equal file (ptr-read-bytes address 6)))
                  (error "PE kernel thunk/IAT mutation refused")))
              (dotimes (index (length name)) (ptr-write-u8 scratch index (aref name index)))
              (ptr-write-u8 scratch (length name) 0)
              (let ((export (nelisp-native-windows-call "GetProcAddress" module scratch)))
                (unless (and (> export 0) (= export (ptr-read-u64 (plist-get import :address) 0)))
                  (error "Live kernel IAT target mutation refused"))))))
      (nelisp-native-windows-free wide) (nelisp-native-windows-free scratch))))
