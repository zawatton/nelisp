;;; dynalign-threads-sample.el --- S3a-2: dynamic-align extern calls from many threads  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds threads.exe: `_start' creates NTHREADS threads (CreateThread); each
;; loops NITER times calling dynamic-align imports (VirtualAlloc / WriteFile
;; to NUL / VirtualFree, with stack arguments) and verifies the data it reads
;; back plus a closed-form checksum.  `_start' waits for all of them
;; (WaitForMultipleObjects) and exits 42 when every thread reports success,
;; 20+id when thread ID failed, 10 when CreateThread failed.
;;
;; With the legacy single global rsp-save slot the threads clobber each
;; other's saved rsp and the process crashes or hangs; the thread-safe
;; frame-local save passes.  The legacy emission is selected at build time by
;; NELISP_AOT_DYNALIGN_GLOBAL_SLOT=1 (read by the AOT compiler); only then
;; does the unit need to define the `nl_dynalign_rsp_save' blob.
;; Used by tools/ai/com-dll/dynalign-threads-e2e.sh.

;;; Code:

(require 'cl-lib)
(require 'nelisp-com-dsl)

(defconst dynalign-threads-count 6)
(defconst dynalign-threads-iterations 20000)

(defconst dynalign-threads-imports
  '(("kernel32.dll" "ExitProcess" "CreateThread" "WaitForMultipleObjects"
     "VirtualAlloc" "VirtualFree" "CreateFileW" "WriteFile" "CloseHandle")))

(defun dynalign-threads-forms ()
  "Return the EXE forms."
  (let ((n dynalign-threads-count) (iters dynalign-threads-iterations))
    `(,(dynalign-threads--wstr-blob 'w_nul "NUL")
      (data-blob tbuf ,(make-list (* 64 n) 0) data)
      (data-blob hs ,(make-list (* 8 n) 0) data)
      (data-blob res ,(make-list (* 8 n) 0) data)
      ,(nelisp-com-vtable 'tvt '(worker))
      ,@(when (equal (getenv "NELISP_AOT_DYNALIGN_GLOBAL_SLOT") "1")
          '((data-blob nl_dynalign_rsp_save (0 0 0 0 0 0 0 0) data)))
      (defun worker (id)
        (let ((h (extern-call CreateFileW (data-addr w_nul) 1073741824 3 0 3 128 0))
              (i 0) (bad 0) (sum 0)
              (nb (+ (data-addr tbuf) (* id 64))))
          (seq
           (while (< i ,iters)
             (let ((p (extern-call VirtualAlloc 0 4096 12288 4))
                   (v (+ (* id 1000003) i)))
               (seq
                (if (= p 0) (setq bad (+ bad 1))
                  (seq (ptr-write-u64 p 0 v)
                       (ptr-write-u64 nb 0 v)
                       (if (= (extern-call WriteFile h nb 8 (+ nb 8) 0) 0)
                           (setq bad (+ bad 1)) 0)
                       (if (= (ptr-read-u32 nb 8) 8) 0 (setq bad (+ bad 1)))
                       (if (= (ptr-read-u64 p 0) v) 0 (setq bad (+ bad 1)))
                       (setq sum (+ sum (ptr-read-u64 p 0)))
                       (if (= (extern-call VirtualFree p 0 32768) 0)
                           (setq bad (+ bad 1)) 0)))
                (setq i (+ i 1)))))
           (extern-call CloseHandle h)
           (if (= sum (+ (* (* id 1000003) ,iters) ,(/ (* iters (1- iters)) 2)))
               0 (setq bad (+ bad 1)))
           (ptr-write-u64 (+ (data-addr res) (* id 8)) 0
                          (if (= bad 0) 1 0))
           0)))
      (defun _start ()
        (let ((fp (ptr-read-u64 (data-addr tvt) 0)) (k 0) (bad 0))
          (seq
           (while (< k ,n)
             (let ((th (extern-call CreateThread 0 0 fp k 0 0)))
               (seq (if (= th 0) (extern-call ExitProcess 10) 0)
                    (ptr-write-u64 (+ (data-addr hs) (* k 8)) 0 th)
                    (setq k (+ k 1)))))
           (extern-call WaitForMultipleObjects ,n (data-addr hs) 1 180000)
           (setq k 0)
           (while (< k ,n)
             (seq (if (and (= bad 0)
                           (/= (ptr-read-u64 (+ (data-addr res) (* k 8)) 0) 1))
                      (setq bad (+ 20 k)) 0)
                  (setq k (+ k 1))))
           (extern-call ExitProcess (if (= bad 0) 42 bad))))))))

(defun dynalign-threads--wstr-blob (name str)
  "Return a rodata UTF-16LE NUL-terminated blob NAME for ASCII STR."
  `(data-blob ,name
              ,(append (cl-loop for c across str append (list c 0)) '(0 0))
              rodata))

(defun dynalign-threads-build (dir)
  "Build threads.exe into DIR."
  (make-directory dir t)
  (nelisp-com-build-exe (expand-file-name "threads.exe" dir)
                        (dynalign-threads-forms)
                        dynalign-threads-imports)
  dir)

(provide 'dynalign-threads-sample)
;;; dynalign-threads-sample.el ends here
