;;; windows-cold-companion-probe.el --- Fast production companion probe -*- lexical-binding: t; -*-
;; Compile with emacs -Q --batch -L lisp -L src -L scripts -l THIS-FILE.
;; Run under Wine with a 64-byte .cold beside the generated executable.
;; Exit 42 without an acceptable companion; exit 0 verifies repeated pinned
;; reads, writer exclusion, and cleanup. No reader/prelude rebuild is needed.
(require 'nelisp-standalone-build)

(let* ((nelisp-standalone--target 'windows-x86_64)
       (scratch (nelisp-link-unit-make
                 "companion-scratch.obj" (list (cons 'bss 64))
                 (list (nelisp-link-symbol "nl_dynalign_rsp_save" 0
                                          :section 'bss :bind 'global)) nil))
       (main
        (nelisp-standalone--compile-to-unit
         "companion-probe.obj"
         `(seq
           ,@(nelisp-boot-image-native-forms)
           (defun nl_os_alloc_chunk (bytes) (extern-call VirtualAlloc 0 bytes 12288 4))
           (defun nl_os_free_chunk (ptr _bytes) (extern-call VirtualFree ptr 0 32768))
           (defun nl_os_open_read (_path) -1)
           (defun nl_os_close_handle (h) (if (< h 0) 0 (extern-call CloseHandle h)))
           (defun nl_fa_read_all (h buf bytes _offset)
             (let* ((counter (nl_os_alloc_chunk 4096))
                    (ok (extern-call ReadFile h buf bytes counter 0))
                    (n (if (= ok 0) -1 (ptr-read-u32 counter 0))))
               (seq (nl_os_free_chunk counter 4096) n)))
           (defun _start ()
             (let ((token (nl_boot_image_companion)))
               (extern-call ExitProcess
                (if (>= token 0) 42
                  (let* ((buffer (nl_os_alloc_chunk 73728))
                         (path (+ buffer 128)) (i 0)
                         (first (nl_boot_image_open_pinned token))
                         (n1 (nl_fa_read_all first buffer 64 0))
                         (byte (ptr-read-u8 buffer 0))
                         (_close (nl_os_close_handle first))
                         (second (nl_boot_image_open_pinned token))
                         (n2 (nl_fa_read_all second buffer 64 0))
                         (same (= byte (ptr-read-u8 buffer 0)))
                         (writer (extern-call CreateFileW (- 0 token) 1073741824 1 0 3 128 0))
                         (good (and (= n1 64) (= n2 64) same (< writer 0))))
                    (seq
                     (while (/= (ptr-read-u16 (- 0 token) (* i 2)) 0)
                       (ptr-write-u16 path (* i 2) (ptr-read-u16 (- 0 token) (* i 2)))
                       (setq i (+ i 1)))
                     (ptr-write-u16 path (* i 2) 0)
                     (nl_os_close_handle writer)
                     (nl_os_close_handle second)
                     (nl_boot_image_release token)
                     ;; A paired successful writer proves the fixture was
                     ;; writable and every pin/duplicate has been closed.
                     (let ((after (extern-call CreateFileW path 1073741824 1 0 3 128 0)))
                       (seq
                        (nl_os_close_handle after)
                        (nl_os_free_chunk buffer 73728)
                        (if (and good (>= after 0)) 0 42)))))))))))))
  (nelisp-link-units-pe32
   "target/codex/round3/companion-probe.exe" (list main scratch) "_start"
   '(("kernel32.dll" "ExitProcess" "VirtualAlloc" "VirtualFree" "GetModuleFileNameW"
      "CreateFileW" "CloseHandle" "GetFileType" "GetFileInformationByHandle"
      "GetCurrentProcess" "SetFilePointerEx" "DuplicateHandle" "ReadFile")
     ("advapi32.dll" "GetKernelObjectSecurity" "GetSecurityDescriptorOwner" "EqualSid"))))
