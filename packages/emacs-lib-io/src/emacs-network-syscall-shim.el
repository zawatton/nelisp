;;; emacs-network-syscall-shim.el --- nl-ffi-* shim over syscall-direct -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; M14 server/emacsclient lane — the K1 network stack
;; (`emacs-network-ffi.el' / `emacs-process-events.el' /
;; `emacs-eventloop.el' / `emacs-server-polyfills.el') was written
;; against the Rust build-tool's `nl-ffi-call' libffi primitive, which
;; the pure-elisp standalone reader no longer ships.  The current
;; reader exposes `syscall-direct' + `alloc-bytes' +
;; `ptr-read-u64'/`ptr-write-u64' instead (the same surface the
;; nelisp-gui X11 editor is compiled against), which is enough to
;; re-create the small nl-ffi-* surface those modules consume:
;;
;;   nl-ffi-malloc / nl-ffi-free
;;   nl-ffi-read-i16 / nl-ffi-read-i32 / nl-ffi-read-bytes
;;   nl-ffi-write-i16 / nl-ffi-write-i32
;;   nl-ffi-write-bytes / nl-ffi-write-bytes-at
;;   nl-ffi-call  (libc-name -> syscall dispatch)
;;
;; Load this file BEFORE the K1 modules; after it loads,
;; `(fboundp 'nl-ffi-call)' is true, so every existing
;; standalone-detection gate in the K1 stack works unchanged.
;;
;; Scope and omissions (documented once):
;; - Linux x86_64 syscall numbers only (this is the only standalone
;;   reader target today).
;; - `nl-ffi-free' is a no-op: `alloc-bytes' memory is arena-owned by
;;   the reader.  Buffers are small (sockaddr/pollfd/recv chunks).
;; - errno emulation: `syscall-direct' returns -errno directly; the
;;   shim stores it in a 4-byte buffer whose address
;;   "__errno_location" returns, so the unmodified
;;   `emacs-network-ffi--errno' memcpy dance reads the right value.
;; - byte access is read-modify-write over unaligned u64 words; every
;;   allocation gets 8 slack bytes so the tail bytes stay in bounds.
;; - `nl-ffi-read-bytes' builds the Lisp string per byte — fine for
;;   the small line-oriented emacsclient protocol, not tuned for bulk.

;;; Code:

(when (and (not (fboundp 'nl-ffi-call))
           (fboundp 'syscall-direct)
           (fboundp 'alloc-bytes))

  (defvar nl-ffi-shim--errno-buf nil
    "4-byte buffer holding the last syscall errno (libc emulation).")

  (defun nl-ffi-shim--zero (ptr bytes)
    "Zero BYTES bytes at PTR (alloc-bytes does not guarantee zero-init)."
    (let ((i 0))
      (while (< i bytes)
        (ptr-write-u64 ptr i 0)
        (setq i (+ i 8)))))

  (defun nl-ffi-malloc (n)
    "Allocate N zeroed bytes (+8 slack for unaligned u64 tail access)."
    (let ((ptr (alloc-bytes (+ n 8) 8)))
      (nl-ffi-shim--zero ptr (+ n 8))
      ptr))

  (defun nl-ffi-free (_ptr)
    "No-op: arena-owned memory."
    nil)

  (defun nl-ffi-shim--peek-u8 (ptr off)
    (logand (ptr-read-u64 ptr off) 255))

  (defun nl-ffi-shim--poke-u8 (ptr off val)
    (ptr-write-u64 ptr off
                   (logior (logand (ptr-read-u64 ptr off) -256)
                           (logand val 255))))

  (defun nl-ffi-read-i16 (ptr off)
    (logand (ptr-read-u64 ptr off) 65535))

  (defun nl-ffi-read-i32 (ptr off)
    (logand (ptr-read-u64 ptr off) 4294967295))

  (defun nl-ffi-write-i16 (ptr off val)
    (ptr-write-u64 ptr off
                   (logior (logand (ptr-read-u64 ptr off) -65536)
                           (logand val 65535))))

  (defun nl-ffi-write-i32 (ptr off val)
    (ptr-write-u64 ptr off
                   (logior (logand (ptr-read-u64 ptr off) -4294967296)
                           (logand val 4294967295))))

  (defun nl-ffi-write-bytes-at (ptr off str)
    "Write STR's bytes at PTR+OFF (no trailing NUL; buffer pre-zeroed)."
    (let ((i 0)
          (n (length str)))
      (while (< i n)
        (nl-ffi-shim--poke-u8 ptr (+ off i) (aref str i))
        (setq i (1+ i)))))

  (defun nl-ffi-write-bytes (ptr str)
    (nl-ffi-write-bytes-at ptr 0 str))

  (defun nl-ffi-read-bytes (ptr n)
    "Read N bytes at PTR into a Lisp string."
    (let ((out "")
          (i 0))
      (while (< i n)
        (setq out (concat out (char-to-string (nl-ffi-shim--peek-u8 ptr i))))
        (setq i (1+ i)))
      out))

  (defun nl-ffi-shim--cstr (str)
    "Marshal STR to a NUL-terminated C string buffer; return the pointer."
    (let ((buf (nl-ffi-malloc (1+ (length str)))))
      (nl-ffi-write-bytes-at buf 0 str)
      buf))

  (defun nl-ffi-shim--ret (rc)
    "Map a raw syscall result to libc semantics (-1 + errno on failure)."
    (if (and (integerp rc) (< rc 0))
        (progn
          (unless nl-ffi-shim--errno-buf
            (setq nl-ffi-shim--errno-buf (nl-ffi-malloc 4)))
          (nl-ffi-write-i32 nl-ffi-shim--errno-buf 0 (- rc))
          -1)
      rc))

  (defun nl-ffi-shim--inet-pton (host out)
    "Pure-elisp inet_pton(AF_INET): parse dotted-quad HOST into OUT.
Writes the 4 network-order bytes; returns 1 on success, 0 on bad input."
    (let ((parts nil)
          (cur 0)
          (digits 0)
          (i 0)
          (n (length host))
          (ok t))
      (while (< i n)
        (let ((c (aref host i)))
          (cond
           ((and (>= c ?0) (<= c ?9))
            (setq cur (+ (* cur 10) (- c ?0)))
            (setq digits (1+ digits))
            (when (> cur 255) (setq ok nil)))
           ((= c ?.)
            (if (zerop digits) (setq ok nil)
              (push cur parts)
              (setq cur 0 digits 0)))
           (t (setq ok nil))))
        (setq i (1+ i)))
      (if (zerop digits) (setq ok nil) (push cur parts))
      (if (or (not ok) (/= (length parts) 4))
          0
        (let ((bytes (nreverse parts))
              (j 0))
          (while bytes
            (nl-ffi-shim--poke-u8 out j (car bytes))
            (setq bytes (cdr bytes))
            (setq j (1+ j)))
          1))))

  (defun nl-ffi-shim--inet-pton6 (host out)
    "Pure-elisp inet_pton(AF_INET6): parse HOST into 16 network-order bytes at OUT.
Returns 1 on success, 0 on bad input.  Handles `::' zero-run compression
\(e.g. \"::1\", \"::\", \"fe80::1\", full 8-group form).  IPv4-mapped tails
\(\"::ffff:1.2.3.4\") are not handled (not needed by the K1 stack)."
    (let ((ok t) (groups nil))
      (condition-case nil
          (let ((dbl (and (>= (length host) 2)
                          (let ((i 0) (n (length host)) (hit nil))
                            (while (and (not hit) (< i (1- n)))
                              (when (and (= (aref host i) ?:)
                                         (= (aref host (1+ i)) ?:))
                                (setq hit i))
                              (setq i (1+ i)))
                            hit))))
            (if dbl
                (let* ((head (substring host 0 dbl))
                       (tail (substring host (+ dbl 2)))
                       (hg (if (string= head "") nil (nl-ffi-shim--split head ?:)))
                       (tg (if (string= tail "") nil (nl-ffi-shim--split tail ?:)))
                       (nz (- 8 (length hg) (length tg))))
                  (if (< nz 0)
                      (setq ok nil)
                    (setq groups (append hg (make-list nz "0") tg))))
              (setq groups (nl-ffi-shim--split host ?:))))
        (error (setq ok nil)))
      (if (or (not ok) (/= (length groups) 8))
          0
        (let ((j 0) (rc 1))
          (dolist (g groups)
            (if (or (string= g "") (> (length g) 4))
                (setq rc 0)
              (let ((v (nl-ffi-shim--parse-hex16 g)))
                (if (null v)
                    (setq rc 0)
                  (nl-ffi-shim--poke-u8 out j (logand 255 (ash v -8)))
                  (nl-ffi-shim--poke-u8 out (1+ j) (logand 255 v)))))
            (setq j (+ j 2)))
          rc))))

  (defun nl-ffi-shim--split (s sep-char)
    "Split string S on the single character SEP-CHAR; return a list of parts."
    (let ((parts nil) (cur "") (i 0) (n (length s)))
      (while (< i n)
        (let ((c (aref s i)))
          (if (= c sep-char)
              (progn (push cur parts) (setq cur ""))
            (setq cur (concat cur (char-to-string c)))))
        (setq i (1+ i)))
      (push cur parts)
      (nreverse parts)))

  (defun nl-ffi-shim--parse-hex16 (g)
    "Parse a 1..4 char hex group G into 0..65535, or nil if non-hex."
    (let ((v 0) (i 0) (n (length g)) (ok t))
      (while (and ok (< i n))
        (let* ((c (aref g i))
               (d (cond ((and (>= c ?0) (<= c ?9)) (- c ?0))
                        ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
                        ((and (>= c ?A) (<= c ?F)) (+ 10 (- c ?A)))
                        (t nil))))
          (if d (setq v (+ (* v 16) d)) (setq ok nil)))
        (setq i (1+ i)))
      (and ok v)))

  (defconst nl-ffi-shim--syscalls
    '(("read" . 0) ("write" . 1) ("open" . 2) ("close" . 3)
      ("poll" . 7) ("ioctl" . 16) ("pipe" . 22) ("dup2" . 33) ("getpid" . 39)
      ("socket" . 41) ("connect" . 42) ("accept" . 43)
      ("sendto" . 44) ("recvfrom" . 45)
      ("bind" . 49) ("listen" . 50) ("getsockname" . 51) ("setsockopt" . 54)
      ("wait4" . 61) ("fcntl" . 72) ("getuid" . 102) ("setsid" . 112)
      ("pipe2" . 293))
    "libc function name -> Linux x86_64 syscall number (direct args).
Doc 06 C3 added open/ioctl/dup2/setsid/getpid for PTY support.
Doc 06 D2 added sendto/recvfrom/getsockname for datagram + IPv6.
Doc 06 C1 added pipe/pipe2 for async pipe-subprocess fds.
Doc 06 C2 added wait4 for SIGCHLD-fallback child reaping.")

  (defun nl-ffi-call (_lib func _sig &rest args)
    "Shim: dispatch libc FUNC to `syscall-direct' (x86_64).
_LIB and _SIG are accepted for nl-ffi-call compatibility and ignored.
String arguments are marshalled to NUL-terminated C buffers."
    (cond
     ;; recv/send are not direct syscalls on x86_64 — route through
     ;; recvfrom(45) / sendto(44) with NULL peer address.
     ((equal func "recv")
      (nl-ffi-shim--ret
       (syscall-direct 45 (nth 0 args) (nth 1 args) (nth 2 args)
                       (or (nth 3 args) 0) 0 0)))
     ((equal func "send")
      (nl-ffi-shim--ret
       (syscall-direct 44 (nth 0 args) (nth 1 args) (nth 2 args)
                       (or (nth 3 args) 0) 0 0)))
     ((equal func "unlink")
      (nl-ffi-shim--ret
       (if (fboundp 'nelisp--syscall-path)
           (nelisp--syscall-path 87 (nth 0 args))
         (syscall-direct 87 (nl-ffi-shim--cstr (nth 0 args)) 0 0 0 0 0))))
     ((equal func "mkdir")
      (nl-ffi-shim--ret
       (if (fboundp 'nelisp--syscall-path-int)
           (nelisp--syscall-path-int 83 (nth 0 args) (or (nth 1 args) 448))
         (syscall-direct 83 (nl-ffi-shim--cstr (nth 0 args))
                         (or (nth 1 args) 448) 0 0 0 0))))
     ((equal func "access")
      (nl-ffi-shim--ret
       (if (fboundp 'nelisp--syscall-path-int)
           (nelisp--syscall-path-int 21 (nth 0 args) (or (nth 1 args) 0))
         (syscall-direct 21 (nl-ffi-shim--cstr (nth 0 args))
                         (or (nth 1 args) 0) 0 0 0 0))))
     ((equal func "inet_pton")
      ;; args: family host-string out-ptr.  family 10 = AF_INET6.
      (if (eql (nth 0 args) 10)
          (nl-ffi-shim--inet-pton6 (nth 1 args) (nth 2 args))
        (nl-ffi-shim--inet-pton (nth 1 args) (nth 2 args))))
     ((equal func "usleep")
      ;; usleep(usec) is not a syscall; emulate via poll(NULL, 0, usec/1000ms),
      ;; which sleeps for the timeout and returns 0 (Doc 06 C1 — fixes the
      ;; no-fds sleep path used by sit-for / accept-process-output).
      (nl-ffi-shim--ret
       (syscall-direct 7 0 0 (/ (or (nth 0 args) 0) 1000) 0 0 0)))
     ((equal func "__errno_location")
      (unless nl-ffi-shim--errno-buf
        (setq nl-ffi-shim--errno-buf (nl-ffi-malloc 4)))
      nl-ffi-shim--errno-buf)
     ((equal func "__error")
      (unless nl-ffi-shim--errno-buf
        (setq nl-ffi-shim--errno-buf (nl-ffi-malloc 4)))
      nl-ffi-shim--errno-buf)
     ((equal func "memcpy")
      ;; args: dst src n — elisp byte copy
      (let ((dst (nth 0 args))
            (src (nth 1 args))
            (n (nth 2 args))
            (i 0))
        (while (< i n)
          (nl-ffi-shim--poke-u8 dst i (nl-ffi-shim--peek-u8 src i))
          (setq i (1+ i)))
        dst))
     (t
      (let ((nr (cdr (assoc func nl-ffi-shim--syscalls))))
        (unless nr
          (error "nl-ffi shim: unsupported libc function %s" func))
        (let ((a (mapcar (lambda (arg) (if (stringp arg)
                                           (nl-ffi-shim--cstr arg)
                                         (or arg 0)))
                         args)))
          (nl-ffi-shim--ret
           (syscall-direct nr
                           (or (nth 0 a) 0) (or (nth 1 a) 0)
                           (or (nth 2 a) 0) (or (nth 3 a) 0)
                           (or (nth 4 a) 0) (or (nth 5 a) 0))))))))
  nil)

;; Small numeric polyfills the K1 stack touches but the pure-elisp
;; standalone reader does not ship.  `truncate' is a PHANTOM builtin
;; there — `(fboundp 'truncate)' is t yet calling it errors — so the
;; definition cannot be fboundp-gated; instead the whole block is
;; gated on the standalone syscall surface, which host Emacs lacks.
(when (and (fboundp 'syscall-direct)
           (fboundp 'alloc-bytes))
  (defun /= (a b)
    "Reader polyfill: only = < > <= >= ship as numeric builtins."
    (not (= a b)))
  (defmacro declare (&rest _specs)
    "Reader polyfill: in host Emacs `declare' inside a defun body is
consumed by the defun machinery; the reader's plain defun executes
the body, so without this no-op every dash/s/ht style function that
carries a declare form aborts at call time."
    nil)
  (defmacro ignore-errors (&rest body)
    "Reader polyfill: the macro is absent and vendor server.el
relies on it (`condition-case' itself works)."
    `(condition-case nil (progn ,@body) (error nil)))
  (defun functionp (f)
    "Reader polyfill: the builtin returns nil for closures/lambdas,
which silently skips every process filter/sentinel dispatch.  Accept
symbols with function bindings and (closure ...) / (lambda ...) forms."
    (cond
     ((null f) nil)
     ((symbolp f) (fboundp f))
     ((consp f) (if (memq (car f) '(lambda closure builtin)) t nil))
     (t nil)))
  (defun string-bytes (s)
    "Byte length of S (reader polyfill: strings are raw byte arrays,
so `length' already counts bytes).  `string-bytes' is a phantom
builtin on the standalone reader — fboundp t, calling errors."
    (length s))
  (defun truncate (x &optional divisor)
    "Integer truncation toward zero (reader polyfill)."
    (when divisor (setq x (/ x divisor)))
    (if (integerp x)
        x
      (let* ((s (number-to-string x))
             (i 0)
             (n (length s))
             (out ""))
        (while (and (< i n) (/= (aref s i) ?.))
          (setq out (concat out (char-to-string (aref s i))))
          (setq i (1+ i)))
        (string-to-number out)))))

(provide 'emacs-network-syscall-shim)

;;; emacs-network-syscall-shim.el ends here
