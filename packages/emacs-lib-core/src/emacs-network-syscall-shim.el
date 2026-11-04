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
;;   emacs-network-syscall-shim--call (network-local syscall dispatch)
;;
;; This is a network-local compatibility adapter.  The native
;; `nl-ffi-call' fixed table remains authoritative for other callers.
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

(let ((helpers '(nl-ffi-malloc nl-ffi-free nl-ffi-read-i16
                 nl-ffi-read-i32 nl-ffi-read-bytes nl-ffi-write-i16
                 nl-ffi-write-i32 nl-ffi-write-bytes nl-ffi-write-bytes-at))
      (present 0))
  (dolist (name helpers) (when (fboundp name) (setq present (1+ present))))
  (when (and (eq system-type 'gnu/linux)
             (boundp 'system-configuration)
             (stringp system-configuration)
             (string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration)
             (fboundp 'syscall-direct) (fboundp 'alloc-bytes)
             (fboundp 'ptr-read-u64) (fboundp 'ptr-write-u64)
             (fboundp 'ptr-write-u8)
             (or (featurep 'nl-ffi-memory)
                 (require 'nl-ffi-memory nil t))
             (or (not (fboundp 'nl-ffi-call))
                 (eq (type-of (symbol-function 'nl-ffi-call)) 'subr))
             (> present 0) (< present (length helpers)))
    (error "nl-ffi shim: refusing partial buffer-helper ownership set (%d/%d)"
           present (length helpers))))

(when (and (eq system-type 'gnu/linux)
           (boundp 'system-configuration)
           (stringp system-configuration)
           (string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration)
           (fboundp 'syscall-direct)
           (fboundp 'alloc-bytes)
           (fboundp 'ptr-read-u64) (fboundp 'ptr-write-u64)
           (fboundp 'ptr-write-u8)
           (or (featurep 'nl-ffi-memory)
               (require 'nl-ffi-memory nil t))
           (or (not (fboundp 'nl-ffi-call))
               (eq (type-of (symbol-function 'nl-ffi-call)) 'subr)))

  (defvar emacs-network-syscall-shim--active-p nil
    "Non-nil only when the checked NeLisp Linux x86_64 adapter is active.")
  (setq emacs-network-syscall-shim--active-p t)

  (defvar nl-ffi-shim--delegate-buffer-helpers-p
    (let ((names '(nl-ffi-malloc nl-ffi-free nl-ffi-read-i16 nl-ffi-read-i32
                   nl-ffi-read-bytes nl-ffi-write-i16 nl-ffi-write-i32
                   nl-ffi-write-bytes nl-ffi-write-bytes-at))
          (count 0))
      (dolist (name names) (when (fboundp name) (setq count (1+ count))))
      (= count (length names)))
    "Whether a complete preexisting buffer API owns compatibility memory.")

  (defvar nl-ffi-shim--errno-buf nil
    "4-byte buffer holding the last syscall errno (libc emulation).")

  (defvar nl-ffi-shim--owners nil
    "Private live mmap owners backing the network compatibility buffers.")

  (defun nl-ffi-shim--owner-size (ptr)
    (let ((entry (assoc ptr nl-ffi-shim--owners)))
      (unless entry (error "nl-ffi shim: pointer is not a live owned buffer"))
      (cadr entry)))

  (unless (fboundp 'nl-ffi-malloc)
    (defun nl-ffi-malloc (n)
      "Allocate zeroed, externally owned network buffer with tail slack."
      (unless (require 'nl-ffi-memory nil t)
        (error "nl-ffi shim: nl-ffi-memory owner API is required"))
      (unless (and (integerp n) (>= n 0))
        (error "nl-ffi shim: invalid allocation size %S" n))
      (let* ((size (+ n 8)) (owner (nl-ffi-memory-allocate size))
             (ptr (nl-ffi-memory-address owner)))
        (push (cons ptr (cons size owner)) nl-ffi-shim--owners)
        ptr)))

  (unless (fboundp 'nl-ffi-free)
    (defun nl-ffi-free (ptr)
      "Release a network buffer's matching mmap owner exactly once."
      (let ((entry (assoc ptr nl-ffi-shim--owners)))
        (unless entry (error "nl-ffi shim: free of unknown buffer"))
        (nl-ffi-memory-release (cddr entry))
        (setq nl-ffi-shim--owners (delq entry nl-ffi-shim--owners))
        t)))

  (defun nl-ffi-shim--check-range (ptr off width)
    (let ((size (nl-ffi-shim--owner-size ptr)))
      (unless (and (integerp off) (>= off 0) (<= (+ off width) size))
        (error "nl-ffi shim: buffer access out of bounds"))))

  (defun nl-ffi-shim--zero (ptr bytes)
    "Zero BYTES bytes at PTR (alloc-bytes does not guarantee zero-init)."
    (let ((i 0))
      (while (< i bytes)
        (ptr-write-u64 ptr i 0)
        (setq i (+ i 8)))))

  (defun nl-ffi-shim--peek-u8 (ptr off)
    (cond ((assoc ptr nl-ffi-shim--owners)
           (nl-ffi-shim--check-range ptr off 8))
          (nl-ffi-shim--delegate-buffer-helpers-p nil)
          (t (error "nl-ffi shim: pointer is not a live owned buffer")))
    (logand (ptr-read-u64 ptr off) 255))

  (defun nl-ffi-shim--poke-u8 (ptr off val)
    (cond ((assoc ptr nl-ffi-shim--owners)
           (nl-ffi-shim--check-range ptr off 8))
          (nl-ffi-shim--delegate-buffer-helpers-p nil)
          (t (error "nl-ffi shim: pointer is not a live owned buffer")))
    (ptr-write-u64 ptr off
                   (logior (logand (ptr-read-u64 ptr off) -256)
                           (logand val 255))))

  (unless (fboundp 'nl-ffi-read-i16)
    (defun nl-ffi-read-i16 (ptr off)
      (nl-ffi-shim--check-range ptr off 8)
      (logand (ptr-read-u64 ptr off) 65535)))

  (unless (fboundp 'nl-ffi-read-i32)
    (defun nl-ffi-read-i32 (ptr off)
      (nl-ffi-shim--check-range ptr off 8)
      (logand (ptr-read-u64 ptr off) 4294967295)))

  (unless (fboundp 'nl-ffi-write-i16)
    (defun nl-ffi-write-i16 (ptr off val)
      (nl-ffi-shim--check-range ptr off 8)
      (ptr-write-u64 ptr off
                     (logior (logand (ptr-read-u64 ptr off) -65536)
                             (logand val 65535)))))

  (unless (fboundp 'nl-ffi-write-i32)
    (defun nl-ffi-write-i32 (ptr off val)
      (nl-ffi-shim--check-range ptr off 8)
      (ptr-write-u64 ptr off
                     (logior (logand (ptr-read-u64 ptr off) -4294967296)
                             (logand val 4294967295)))))

  (unless (fboundp 'nl-ffi-write-bytes-at)
    (defun nl-ffi-write-bytes-at (ptr off str)
    "Write STR's bytes at PTR+OFF (no trailing NUL; buffer pre-zeroed)."
    (let ((i 0)
          (n (length str)))
      (while (< i n)
        (nl-ffi-shim--poke-u8 ptr (+ off i) (aref str i))
        (setq i (1+ i))))))

  (unless (fboundp 'nl-ffi-write-bytes)
    (defun nl-ffi-write-bytes (ptr str)
      (nl-ffi-write-bytes-at ptr 0 str)))

  (unless (fboundp 'nl-ffi-read-bytes)
    (defun nl-ffi-read-bytes (ptr n)
    "Read N bytes at PTR into a Lisp string."
    (let ((out "")
          (i 0))
      (while (< i n)
        (setq out (concat out (char-to-string (nl-ffi-shim--peek-u8 ptr i))))
        (setq i (1+ i)))
      out)))

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

  (defun emacs-network-syscall-shim--supports-p (func)
    "Return non-nil when network adapter supports libc FUNC."
    (or (assoc func nl-ffi-shim--syscalls)
        (member func '("recv" "send" "unlink" "mkdir" "access"
                       "inet_pton" "usleep" "__errno_location"
                       "__error" "memcpy"))))

  (defun emacs-network-syscall-shim--call (func &rest args)
    "Dispatch supported libc FUNC to the Linux x86_64 syscall adapter."
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
  (unless (fboundp 'nl-ffi-call)
  (defun nl-ffi-call (_lib func _sig &rest args)
      "Legacy ABI wrapper installed only when no native backend exists."
      (apply #'emacs-network-syscall-shim--call func args)))
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
  ;; NeLisp v1.1.0 (Doc 200) ships a real `string-bytes' that counts
  ;; encoded bytes, so the polyfill below would now under-count every
  ;; multibyte string it is asked about: a three-character Japanese
  ;; string is 9 bytes to the builtin and 3 to the polyfill.  Older
  ;; readers ship `string-bytes' as a PHANTOM builtin -- fboundp is t
  ;; yet calling it errors -- which is why this cannot be fboundp-gated.
  ;; Call it: keep the polyfill only for a reader that does not answer.
  (unless (condition-case nil (progn (string-bytes "a") t) (error nil))
    (defun string-bytes (s)
      "Byte length of S (reader polyfill for readers whose `string-bytes'
is a phantom builtin — fboundp t, calling errors).  Strings are raw
byte arrays there, so `length' already counts bytes."
      (length s)))
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
