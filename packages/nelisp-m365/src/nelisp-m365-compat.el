;;; nelisp-m365-compat.el --- Substrate shims for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The NeLisp standalone runtime implements a subset of the Emacs Lisp
;; surface, and several primitives this package would otherwise reach
;; for are absent or return a stub value.  Rather than scatter `fboundp'
;; guards through the Graph client, every gap is closed once here.
;;
;; Measured on nelisp 2026-08-19 (Linux ELF) and 2026-08-13 (Windows PE):
;;
;;   float-time             Linux: real clock.  Windows: returns nil.
;;   format-time-string     both: returns nil (stub).
;;   match-beginning/end    both: return nil, so regexp capture groups
;;                          are unreadable.  Only `string-match' (index)
;;                          and `string-search' (literal index) work.
;;   sit-for                both: returns t without sleeping.
;;   getenv                 Linux: nil for every name.  Windows: works.
;;   process-environment    Linux: nil.
;;   command-line-args      Linux: nil, so the process cannot read its
;;                          own argv; configuration arrives through a
;;                          generated bootstrap file instead.
;;   file-exists-p          Linux: works.  Windows: returns nil always.
;;   set-file-modes         Linux: works.  Windows: signals errno -38.
;;   make-network-process   both: void.  All HTTP goes through curl.
;;
;; A second hazard shapes the style here: calling a builtin that the
;; runtime declares but does not implement aborts the whole form without
;; signalling, and `condition-case' cannot catch it.  So this file never
;; probes a primitive by calling it inside a handler; it either uses one
;; from the verified-working set, or checks the return value of a
;; primitive already known to return a stub rather than abort.

;;; Code:

;; Under a regular Emacs the JSON functions live in `json'; a batch -Q
;; session has not loaded it.  On the standalone runtime `require' is a
;; silent no-op for a missing feature, which is harmless here because
;; every use is guarded by `fboundp'.
(require 'json nil t)

(defconst nelisp-m365-compat-version "0.1.0"
  "Version of the nelisp-m365 substrate shim layer.")

;;; File predicates ---------------------------------------------------

(defun nelisp-m365-compat--file-attributes (path)
  "Return `file-attributes' for PATH, or nil when unavailable.
Used instead of `file-exists-p', which is a stub on the Windows build."
  (and (fboundp 'file-attributes)
       (file-attributes path)))

(defun nelisp-m365-compat-exists-p (path)
  "Return non-nil when PATH exists.
`file-exists-p' is a stub on the Windows build, but `file-attributes'
returns nil for a missing path on both builds."
  (and (nelisp-m365-compat--file-attributes path) t))

(defun nelisp-m365-compat-directory-p (path)
  "Return non-nil when PATH names an existing directory.
The standalone runtime fills the whole `file-attributes' list with nil
placeholders — including the leading directory flag — so that field
cannot be used.  `file-directory-p' is accurate."
  (and (fboundp 'file-directory-p)
       (file-directory-p path)
       t))

(defun nelisp-m365-compat-file-readable-p (path)
  "Return non-nil when PATH names an existing non-directory file."
  (and (nelisp-m365-compat-exists-p path)
       (not (nelisp-m365-compat-directory-p path))))

;;; Platform ----------------------------------------------------------

(defvar nelisp-m365-compat--windows nil
  "Cached result of `nelisp-m365-compat-windows-p'.")

(defvar nelisp-m365-compat--windows-known nil
  "Non-nil once `nelisp-m365-compat--windows' has been computed.")

(defun nelisp-m365-compat-windows-p ()
  "Return non-nil when running on Windows.
The standalone runtime reports `system-type' as gnu/linux even on the
Windows PE build, and every filesystem predicate is a stub there, so
neither can be used.  Running a Windows-only program is the one probe
that answers correctly on both builds."
  (unless nelisp-m365-compat--windows-known
    (setq nelisp-m365-compat--windows-known t)
    (setq nelisp-m365-compat--windows
          (let ((res (nelisp-m365-compat-run-program
                      (list "cmd.exe" "/c" "exit 0"))))
            (and res (equal (car res) 0)))))
  nelisp-m365-compat--windows)

;;; Subprocesses ------------------------------------------------------

(defun nelisp-m365-compat-run-program (argv &optional stderr-file)
  "Run ARGV and return (EXIT-CODE . STDOUT-STRING).
ARGV is a list whose head is the program.  STDERR-FILE, when given,
receives standard error; otherwise it is discarded.  Return nil when
ARGV is empty."
  (when argv
    (with-temp-buffer
      ;; A missing program yields exit 1 on the standalone runtime but
      ;; signals under a regular Emacs; normalise both to a non-zero
      ;; exit so callers can probe candidates uniformly.
      (let* ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix)
            (code (condition-case nil
                      (apply #'call-process
                             (car argv) nil
                             (if stderr-file (list t stderr-file) t)
                             nil
                             (cdr argv))
                    (error 127))))
        (cons code (buffer-string))))))

(defun nelisp-m365-compat-run-program-to-string (argv)
  "Run ARGV and return its stdout, or nil when it exits non-zero."
  (let ((res (nelisp-m365-compat-run-program argv)))
    (and res (equal (car res) 0) (cdr res))))

;;; File contents -----------------------------------------------------

(defun nelisp-m365-compat-read-file (path)
  "Return the contents of PATH as a string, or nil when absent or empty.
`insert-file-contents' returns an empty buffer rather than signalling
for a missing path on the standalone runtime, and the Windows build has
no working existence predicate, so absent and empty are deliberately
treated the same.  Every caller here stores non-empty content."
  (let ((text (condition-case nil
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))
                (error nil))))
    (and text (not (equal text "")) text)))

(defun nelisp-m365-compat--powershell-string (text)
  "Quote TEXT as a literal PowerShell string."
  (concat "'" (string-join (nelisp-m365-compat-split-all text "'") "''") "'"))

(defun nelisp-m365-compat--protect-file (path)
  "Replace PATH's Windows ACL with access for only the current user.
Fail closed: a token must never be written when protection fails."
  (let ((res (nelisp-m365-compat-run-program
              (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command"
                    (concat
                     "$ErrorActionPreference='Stop';"
                     "$sid=[Security.Principal.WindowsIdentity]::GetCurrent().User;"
                     "$acl=New-Object Security.AccessControl.FileSecurity;"
                     "$acl.SetOwner($sid);$acl.SetAccessRuleProtection($true,$false);"
                     "$acl.AddAccessRule((New-Object Security.AccessControl.FileSystemAccessRule($sid,'FullControl','Allow')));"
                     "[IO.File]::SetAccessControl("
                     (nelisp-m365-compat--powershell-string path) ",$acl)")))))
    (unless (and res (equal (car res) 0))
      (error "Cannot restrict token cache ACL"))))

(defun nelisp-m365-compat-write-file (path text &optional private)
  "Write TEXT to PATH, creating parent directories as needed.
With PRIVATE non-nil, restrict the file to the current user before
writing secrets, using a Windows ACL or Unix mode 0600."
  (let ((dir (file-name-directory path)))
    (when (and dir (not (nelisp-m365-compat-directory-p dir)))
      (make-directory dir t)))
  (when private
    (unless (nelisp-m365-compat-exists-p path)
      (write-region "" nil path))
    (if (nelisp-m365-compat-windows-p)
        (nelisp-m365-compat--protect-file path)
      (set-file-modes path 384)))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path))
  path)

;;; Clock -------------------------------------------------------------

(defun nelisp-m365-compat--shell-epoch ()
  "Return the current Unix time by asking the OS, or nil on failure.
Fallback for builds where `float-time' returns nil."
  (let ((out (nelisp-m365-compat-run-program-to-string
              (if (nelisp-m365-compat-windows-p)
                  (list "powershell.exe" "-NoProfile" "-NonInteractive"
                        "-Command" "[DateTimeOffset]::UtcNow.ToUnixTimeSeconds()")
                (list "/bin/sh" "-c" "date +%s")))))
    (and out
         (let ((n (string-to-number (string-trim out))))
           (and (> n 0) n)))))

(defun nelisp-m365-compat-now ()
  "Return the current Unix time in whole seconds as an integer.
Prefer `float-time'; fall back to the OS when that primitive is a stub.

The result is truncated rather than left as a float because
`nelisp-json-encode' rejects floats outright -- it reports them as NaN
or infinity -- and these timestamps are persisted in the token cache.
Second resolution is ample for token expiry and poll deadlines."
  (let ((v (float-time)))
    (truncate
     (if (numberp v) v (or (nelisp-m365-compat--shell-epoch) 0)))))

;;; Calendar ----------------------------------------------------------

(defun nelisp-m365-compat--civil-from-days (z)
  "Return (YEAR MONTH DAY) for Z days since the Unix epoch.
Hinnant's civil-from-days.  This package only feeds it post-1970 values,
so truncating integer division matches the floor division the algorithm
specifies."
  (let* ((z (+ z 719468))
         (era (/ z 146097))
         (doe (- z (* era 146097)))
         (yoe (/ (- (+ doe (/ doe 36524))
                    (+ (/ doe 1460) (/ doe 146096)))
                 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (/ yoe 4) (- (/ yoe 100)))))
         (mp (/ (+ (* 5 doy) 2) 153))
         (d (+ (- doy (/ (+ (* 153 mp) 2) 5)) 1))
         (m (+ mp (if (< mp 10) 3 -9))))
    (list (if (<= m 2) (1+ y) y) m d)))

(defun nelisp-m365-compat-iso8601-utc (&optional epoch)
  "Return EPOCH (default now) as an ISO 8601 UTC timestamp string.
`format-time-string' is a stub on the standalone runtime, so the
calendar conversion is done here."
  (let* ((secs (truncate (or epoch (nelisp-m365-compat-now))))
         (days (/ secs 86400))
         (rem (- secs (* days 86400)))
         (ymd (nelisp-m365-compat--civil-from-days days))
         (hh (/ rem 3600))
         (mm (/ (- rem (* hh 3600)) 60))
         (ss (- rem (* hh 3600) (* mm 60))))
    (format "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (nth 0 ymd) (nth 1 ymd) (nth 2 ymd) hh mm ss)))

;;; Sleep -------------------------------------------------------------

(defun nelisp-m365-compat-sleep (seconds)
  "Block for SECONDS.
`sit-for' returns immediately on the standalone runtime, so delegate to
the OS."
  (if (nelisp-m365-compat-windows-p)
      (nelisp-m365-compat-run-program
       (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command"
             (format "Start-Sleep -Milliseconds %d"
                     (truncate (* 1000 seconds)))))
    (nelisp-m365-compat-run-program
     (list "/bin/sleep" (format "%s" seconds))))
  nil)

(defun nelisp-m365-compat-base64-file (path)
  "Return the contents of PATH base64-encoded on a single line, or nil.

Encoded by the OS rather than in Lisp: every string in this runtime is
UTF-8, so reading a binary file into one corrupts any byte above 127.
The base64 that comes back is ASCII and therefore safe to carry."
  (let ((res (nelisp-m365-compat-run-program
              (if (nelisp-m365-compat-windows-p)
                  (list "powershell.exe" "-NoProfile" "-NonInteractive" "-Command"
                        (concat "[Convert]::ToBase64String([IO.File]::ReadAllBytes("
                                (nelisp-m365-compat--powershell-string path) "))"))
                (list "/bin/sh" "-c"
                      (concat "base64 -w0 -- '" path "'"))))))
    (and res (equal (car res) 0)
         (let ((text (string-trim (cdr res))))
           (and (not (equal text "")) text)))))

(defun nelisp-m365-compat-file-size (path)
  "Return the size of PATH in bytes, or nil when it cannot be read."
  (let ((attrs (nelisp-m365-compat--file-attributes path)))
    (and attrs
         (let ((size (nth 7 attrs)))
           (and (numberp size) size)))))

;;; Executable lookup -------------------------------------------------

(defconst nelisp-m365-compat--curl-candidates
  '("/usr/bin/curl"
    "/bin/curl"
    "/usr/local/bin/curl"
    "/opt/homebrew/bin/curl"
    "C:/Windows/System32/curl.exe"
    "C:/msys64/usr/bin/curl.exe"
    "C:/Program Files/Git/mingw64/bin/curl.exe")
  "Absolute paths probed for curl.
Neither `executable-find' nor `nelisp-sys-executable-find' appends the
Windows `.exe' suffix, and `getenv' returns nil for PATH on the Linux
build, so a PATH search is not usable here.")

(defvar nelisp-m365-compat--curl-cache nil
  "Cached result of `nelisp-m365-compat-curl-program'.")

(defun nelisp-m365-compat-curl-program ()
  "Return an absolute path to curl, or nil when none is installed.
Each candidate is probed by running it, not by asking whether the file
exists: every filesystem predicate is a stub on the Windows build, so a
successful `curl --version' is the only portable evidence."
  (or nelisp-m365-compat--curl-cache
      (setq nelisp-m365-compat--curl-cache
            (let ((found nil)
                  (rest nelisp-m365-compat--curl-candidates))
              (while (and rest (not found))
                (let ((res (nelisp-m365-compat-run-program
                            (list (car rest) "--version"))))
                  (when (and res (equal (car res) 0))
                    (setq found (car rest))))
                (setq rest (cdr rest)))
              found))))

;;; String helpers ----------------------------------------------------

;; `match-beginning', `match-end' and `match-data' all return nil on the
;; standalone runtime, so regexp capture groups cannot be read back.
;; Everything below is built from `string-search' (literal index) and
;; `substring', both of which are verified working.

(defconst nelisp-m365-compat-scan-window 512
  "Characters examined per slice by `nelisp-m365-compat-find'.
Small enough that a scan over a large document stays roughly linear,
large enough that the per-slice overhead stays negligible.")

(defun nelisp-m365-compat-find (needle haystack &optional start)
  "Return the index of NEEDLE in HAYSTACK at or after START, or nil.

Searches in bounded slices rather than calling `string-search' with a
START argument.  That argument allocates in proportion to the *remaining*
haystack on this runtime -- about 150 kB per call on a 7 KB input -- so
a scan that walks a document tag by tag ends up quadratic in the
document size.  Slicing caps the cost of each individual search.

Consecutive slices overlap by one character less than NEEDLE, so a match
straddling a slice boundary is still found."
  (let* ((len (length haystack))
         (nlen (length needle))
         (pos (or start 0))
         (found nil)
         (done nil))
    (if (or (= nlen 0) (> nlen len))
        nil
      (while (not done)
        (if (> (+ pos nlen) len)
            (setq done t)
          (let* ((end (min len (+ pos nelisp-m365-compat-scan-window)))
                 (idx (string-search needle (substring haystack pos end))))
            (cond
             (idx (setq found (+ pos idx))
                  (setq done t))
             ((>= end len) (setq done t))
             ;; Step back by nlen-1 so a straddling match is not missed.
             (t (setq pos (- end (1- nlen))))))))
      found)))

(defun nelisp-m365-compat-split-once (string separator)
  "Split STRING at the first occurrence of literal SEPARATOR.
Return (BEFORE . AFTER), or nil when SEPARATOR does not occur."
  (let ((idx (nelisp-m365-compat-find separator string)))
    (and idx
         (cons (substring string 0 idx)
               (substring string (+ idx (length separator)))))))

(defun nelisp-m365-compat-split-all (string separator)
  "Split STRING on every occurrence of literal SEPARATOR.
Return a list of the pieces, including empty ones.

Walks with absolute indices rather than repeatedly re-slicing the
remainder: the obvious `(setq rest (substring rest ...))' loop copies
the tail once per piece, which is quadratic in the input."
  (let ((parts nil)
        (pos 0)
        (len (length string))
        (slen (length separator))
        (done nil))
    (while (not done)
      (let ((idx (nelisp-m365-compat-find separator string pos)))
        (if idx
            (progn
              (push (substring string pos idx) parts)
              (setq pos (+ idx slen)))
          (push (substring string pos len) parts)
          (setq done t))))
    (nreverse parts)))

(defconst nelisp-m365-compat--unreserved
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"
  "Characters RFC 3986 leaves unescaped in a percent-encoded segment.")

(defun nelisp-m365-compat-string-to-utf8-bytes (string)
  "Return STRING as a list of UTF-8 byte values.
The standalone runtime stores every string as UTF-8 internally but
exposes it as characters, and `encode-coding-string' is not available,
so the encoding is done here."
  (let ((bytes nil))
    (dolist (c (string-to-list string))
      (cond
       ((< c #x80)
        (push c bytes))
       ((< c #x800)
        (push (logior #xC0 (ash c -6)) bytes)
        (push (logior #x80 (logand c #x3F)) bytes))
       ((< c #x10000)
        (push (logior #xE0 (ash c -12)) bytes)
        (push (logior #x80 (logand (ash c -6) #x3F)) bytes)
        (push (logior #x80 (logand c #x3F)) bytes))
       (t
        (push (logior #xF0 (ash c -18)) bytes)
        (push (logior #x80 (logand (ash c -12) #x3F)) bytes)
        (push (logior #x80 (logand (ash c -6) #x3F)) bytes)
        (push (logior #x80 (logand c #x3F)) bytes))))
    (nreverse bytes)))

(defun nelisp-m365-compat-url-encode (string)
  "Percent-encode STRING per RFC 3986.
`url-hexify-string' is not available on the standalone runtime.  The
input is encoded as UTF-8 before escaping, so non-ASCII text round-trips
correctly."
  (let ((out nil))
    (dolist (b (nelisp-m365-compat-string-to-utf8-bytes string))
      (let ((ch (and (< b 128) (char-to-string b))))
        (push (if (and ch (string-search ch nelisp-m365-compat--unreserved))
                  ch
                (format "%%%02X" b))
              out)))
    (apply #'concat (nreverse out))))

(defun nelisp-m365-compat-form-encode (pairs)
  "Return PAIRS as an application/x-www-form-urlencoded body.
PAIRS is an alist of (NAME . VALUE) strings."
  (let ((parts nil))
    (dolist (pair pairs)
      (push (concat (nelisp-m365-compat-url-encode (car pair))
                    "="
                    (nelisp-m365-compat-url-encode (cdr pair)))
            parts))
    (string-join (nreverse parts) "&")))

;;; JSON --------------------------------------------------------------

(defun nelisp-m365-compat-json-parse (string)
  "Parse JSON STRING into an alist tree.
Uses `nelisp-json' on the standalone runtime and the host
`json-parse-string' under a regular Emacs, so the same code runs on
both.

Both back ends are pinned to the same representation, chosen so that
anything parsed here can be handed straight back to
`nelisp-m365-compat-json-encode':

  `:array-type vector'  keeps arrays distinguishable from objects.
        Parsing arrays as lists looks tidier but is unencodable: the
        encoder reads any list as an alist, so a Graph field like a
        contact's `emailAddresses' -- an array of objects -- comes back
        as a list of alists and fails with \"invalid JSON object key\".
  `:null-object nil'    avoids the keyword `:null', which the encoder
        rejects outright, and which appears in almost every Graph row.
  `:false-object :json-false'  is the value the encoder emits as false."
  (cond
   ((fboundp 'nelisp-json-parse-string)
    (nelisp-json-parse-string string :object-type 'alist :array-type 'vector
                              :null-object nil :false-object :json-false))
   ((fboundp 'json-parse-string)
    ;; The host parser spells the same choice `array', not `vector', and
    ;; it interns object keys as symbols where `nelisp-json' leaves them
    ;; as strings.  Normalise to strings so `assoc' lookups written for
    ;; the runtime also work under a regular Emacs.
    (nelisp-m365-compat--stringify-keys
     (json-parse-string string :object-type 'alist :array-type 'array
                        :null-object nil :false-object :json-false)))
   (t (error "nelisp-m365: no JSON parser available"))))

(defun nelisp-m365-compat--stringify-keys (value)
  "Return VALUE with every alist key coerced from a symbol to a string.
Objects are alists and arrays are vectors after parsing, so a cons cell
is unambiguously an object entry."
  (cond
   ((vectorp value)
    (apply #'vector (mapcar #'nelisp-m365-compat--stringify-keys
                            (append value nil))))
   ((consp value)
    (mapcar (lambda (cell)
              (if (consp cell)
                  (cons (if (symbolp (car cell))
                            (symbol-name (car cell))
                          (car cell))
                        (nelisp-m365-compat--stringify-keys (cdr cell)))
                (nelisp-m365-compat--stringify-keys cell)))
            value))
   (t value)))

(defun nelisp-m365-compat-to-list (sequence)
  "Return SEQUENCE as a list, accepting a vector or a list.
Parsed JSON arrays are vectors; this is how the rest of the package
iterates them without caring which it was handed."
  (append sequence nil))

(defun nelisp-m365-compat-json-encode (value)
  "Encode VALUE as a JSON string.

Encoding rules that hold on both back ends, and that every caller in
this package must follow:

  object   an alist of (STRING . VALUE); an empty object is a hash table
           because nil would encode as null
  array    a *vector*.  A list is not usable: `nelisp-json-encode' reads
           a list as an alist, so (\"a\" \"b\") encodes as {\"a\":\"b\"},
           and a list of alists aborts the encoder outright
  boolean  t for true, :json-false for false.  nil is null, not false"
  (cond
   ((fboundp 'nelisp-json-encode) (nelisp-json-encode value))
   ((fboundp 'json-encode) (json-encode value))
   (t (error "nelisp-m365: no JSON encoder available"))))

(defun nelisp-m365-compat-ascii-p (string)
  "Return non-nil when STRING contains no character above U+007F.
Compares the character count with the byte count, which is O(1) on both
back ends and avoids walking a multi-megabyte payload."
  (if (fboundp 'string-bytes)
      (= (length string) (string-bytes string))
    (not (let ((found nil))
           (dolist (c (string-to-list string) found)
             (when (> c 127) (setq found t)))))))

(defun nelisp-m365-compat-escape-non-ascii (string)
  "Return STRING with every non-ASCII character as a JSON \\uXXXX escape.

Needed because the runtime's `write-region' is a stub that compares the
number of *bytes* it wrote against the number of *characters* it was
given, and signals when they differ -- so any file containing Japanese
fails to write at all.  Escaping first makes the payload pure ASCII, and
JSON says a \\uXXXX escape means exactly the character it names, so the
server sees the same text either way."
  (if (nelisp-m365-compat-ascii-p string)
      string
    (let ((out nil))
      (dolist (c (string-to-list string))
        (cond
         ((< c 128) (push (char-to-string c) out))
         ((< c #x10000) (push (format "\\u%04X" c) out))
         (t
          ;; Outside the BMP: JSON has no \U, so emit a surrogate pair.
          (let* ((v (- c #x10000))
                 (hi (+ #xD800 (ash v -10)))
                 (lo (+ #xDC00 (logand v #x3FF))))
            (push (format "\\u%04X\\u%04X" hi lo) out)))))
      (apply #'concat (nreverse out)))))

(defun nelisp-m365-compat-json-encode-ascii (value)
  "Encode VALUE as JSON with every non-ASCII character escaped."
  (nelisp-m365-compat-escape-non-ascii
   (nelisp-m365-compat-json-encode value)))

(defun nelisp-m365-compat-json-array (list)
  "Return LIST as a vector, ready to encode as a JSON array.
Parsed JSON arrives as lists, so anything round-tripping back out has to
pass through here first."
  (if (vectorp list) list (apply #'vector (or list nil))))

(defun nelisp-m365-compat-json-object ()
  "Return a fresh value that encodes as an empty JSON object."
  (make-hash-table :test 'equal))

(defun nelisp-m365-compat-json-bool (value)
  "Return VALUE as a JSON boolean: t stays t, nil becomes :json-false."
  (if value t :json-false))

(provide 'nelisp-m365-compat)

;;; nelisp-m365-compat.el ends here
