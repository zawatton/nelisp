;;; regex-tramp-segments.el --- many paired profiles per process -*- lexical-binding: nil; -*-
;; Use the existing raw-memory APIs for Valgrind's documented amd64 client
;; request sequence (valgrind.h). No reader builtin or native entry is added.
;; N and 0 run in separate fresh processes with the same request sequence.
(when (getenv "RT_MODULE")
  (load (getenv "RT_MODULE"))
  (nlre--compiled-cache-clear))
(load (getenv "RT_CORPUS"))
(defvar rt-client-code (syscall-direct 9 0 4096 7 34 -1 0))
(defvar rt-client-args (alloc-bytes 48 8))
(let ((offset 0))
  (dolist (byte '(#x48 #x89 #xf8 #x31 #xd2
                 #x48 #xc1 #xc7 #x03 #x48 #xc1 #xc7 #x0d
                 #x48 #xc1 #xc7 #x3d #x48 #xc1 #xc7 #x33
                 #x48 #x87 #xdb #x48 #x89 #xd0 #xc3))
    (ptr-write-u8 rt-client-code offset byte)
    (setq offset (1+ offset))))
(dotimes (i 6) (ptr-write-u64 rt-client-args (* 8 i) 0))
(defun rt-request (request)
  (ptr-write-u64 rt-client-args 0 request)
  (ptr-call rt-client-code rt-client-args 0 0 0 0 0))
(let ((index 0) (count (string-to-number (getenv "RT_COUNT"))))
  (dolist (row rt-corpus)
    (let ((pattern (nth 1 row)) (subjects (nth 2 row)) answers result)
      (dolist (text subjects)
        (let ((hit (string-match pattern text)))
          (setq answers (cons (list hit (and hit (match-data t))) answers))))
      (princ (format "RT-RESULT %d %S %S\n" index (car row) (nreverse answers)))
      (garbage-collect)
      (rt-request #x43540001)
      (dotimes (_ count)
        (dolist (text subjects) (setq result (string-match pattern text))))
      (garbage-collect)
      (princ (format "RT-CALLS %d %d\n" index (* count (length subjects))))
      (rt-request #x43540000))
    (setq index (1+ index))))
(princ "RT-SEGMENTS-DONE\n")
nil
