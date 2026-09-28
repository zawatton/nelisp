;;; nelisp-eln-registration-executable-regions-test.el --- whole-file executable-region authentication -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Exercises `nelisp-eln-registration--validate-executable-regions' and its
;; helpers purely through mocks -- never by physically corrupting a real
;; .eln and opening it. That was this suite's first design, and it
;; segfaulted the whole test process twice while being written: opening a
;; file with one flipped byte in `.init' crashed immediately (`_init' is
;; the ELF DT_INIT entry, called by `nl-ffi--dlopen' itself, before any
;; Lisp -- this file's or anyone else's -- ever runs), and a flipped byte
;; in `.plt.got' crashed the same way. Both are exactly the file-level
;; commentary's "ordering limit" in `lisp/nelisp-eln-registration.el' made
;; concrete: this validation cannot run before `dlopen' already has. A
;; genuine one-byte-flip regression test for those two regions is not
;; something this test suite can safely perform in-process; the crash
;; corpus gate's own subprocess-per-check isolation (one artifact/check
;; pair per process, a crash reported as CRASH and never aborting the
;; whole run) is the only place that is safe, and it corrupts only
;; `.text' (see tools/nelisp-eln-crash-corpus-gate.sh), never `.init' or
;; `.plt.got' directly.
;;
;; Runs under plain `emacs --batch': every mock here is pure Lisp
;; arithmetic on constructed byte strings, no FFI primitive involved.
;;
;; S7.8 follow-up: `nelisp-eln-registration--validate-preopen' (and the
;; `.dynamic'/CRT/`.init'/`.plt'/`.plt.got'/`.fini' checks it runs) reads
;; only the plain on-file BYTES `nelisp-eln-system-loader-open' already
;; has before it ever calls `nl-ffi--dlopen' -- no handle, no live
;; memory, no FFI. That means the .init/.plt.got flip-then-validate
;; regression this file's header explains it could not safely do through
;; the OLD (post-open) validators is now safe here: read a genuine file's
;; bytes into a string, flip one byte in memory, call the validator
;; directly. Nothing is ever written to disk or opened.

(defun nelisp-eln-exec-regions-test--file-bytes (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun nelisp-eln-exec-regions-test--flipped (bytes offset)
  (let ((copy (copy-sequence bytes)))
    (aset copy offset (logxor (aref copy offset) #xff))
    copy))

(defun nelisp-eln-exec-regions-test--preopen-admitted-p (bytes)
  (condition-case nil
      (progn (nelisp-eln-registration--validate-preopen bytes) t)
    (nelisp-eln-registration-error nil)))

(ert-deftest nelisp-eln-registration-preopen-admits-genuine-file ()
  (let ((eln (getenv "NELISP_ELN_EXEC_REGIONS_ELN")))
    (skip-unless (and eln (file-readable-p eln)))
    (should (nelisp-eln-exec-regions-test--preopen-admitted-p
             (nelisp-eln-exec-regions-test--file-bytes eln)))))

(ert-deftest nelisp-eln-registration-preopen-admits-self-emitted-file ()
  (let ((self (getenv "NELISP_ELN_EXEC_REGIONS_SELF")))
    (skip-unless (and self (file-readable-p self)))
    (should (nelisp-eln-exec-regions-test--preopen-admitted-p
             (nelisp-eln-exec-regions-test--file-bytes self)))))

(ert-deftest nelisp-eln-registration-preopen-rejects-init-flip ()
  ;; The exact scenario that segfaulted this suite's first design when
  ;; tested through the real loader: now caught from plain bytes, before
  ;; any `dlopen' of anything -- genuine or tampered -- would ever happen.
  (let ((eln (getenv "NELISP_ELN_EXEC_REGIONS_ELN")))
    (skip-unless (and eln (file-readable-p eln)))
    (should-not
     (nelisp-eln-exec-regions-test--preopen-admitted-p
      (nelisp-eln-exec-regions-test--flipped
       (nelisp-eln-exec-regions-test--file-bytes eln) #x1000)))))

(ert-deftest nelisp-eln-registration-preopen-rejects-plt-got-flip ()
  (let ((eln (getenv "NELISP_ELN_EXEC_REGIONS_ELN")))
    (skip-unless (and eln (file-readable-p eln)))
    (should-not
     (nelisp-eln-exec-regions-test--preopen-admitted-p
      (nelisp-eln-exec-regions-test--flipped
       (nelisp-eln-exec-regions-test--file-bytes eln) #x1030)))))

(ert-deftest nelisp-eln-registration-preopen-rejects-init-array-target-flip ()
  ;; The RELA entry writing INIT_ARRAY's slot, not the slot's own static
  ;; file content, is what `dlopen' actually uses (confirmed against a
  ;; genuine artifact: `readelf -r' shows an R_X86_64_RELATIVE targeting
  ;; INIT_ARRAY with frame_dummy's address as its addend). Flip a byte in
  ;; that addend -- not in the slot itself -- and this must still reject.
  (let* ((eln (getenv "NELISP_ELN_EXEC_REGIONS_ELN")))
    (skip-unless (and eln (file-readable-p eln)))
    (let* ((bytes (nelisp-eln-exec-regions-test--file-bytes eln))
           (rela (nelisp-eln-registration--elf-section-in-bytes
                  bytes ".rela.dyn"))
           (relocations (nelisp-eln-registration--parse-rela-entries
                        bytes ".rela.dyn"))
           (dyn (nelisp-eln-registration--parse-dynamic-entries bytes))
           (init-array
            (car (nelisp-eln-registration--dynamic-values
                  dyn nelisp-eln-registration--dt-init-array))))
      ;; Find this artifact's own R_X86_64_RELATIVE entry targeting
      ;; DT_INIT_ARRAY within `.rela.dyn', and flip a byte in its addend
      ;; field (offset+16 within that 24-byte entry).
      (let ((off (nth 1 rela)) (found nil))
        (dolist (r relocations)
          (when (and (not found) (= (nth 0 r) init-array))
            (setq found off))
          (setq off (+ off 24)))
        (should found)
        (should-not
         (nelisp-eln-exec-regions-test--preopen-admitted-p
          (nelisp-eln-exec-regions-test--flipped bytes (+ found 16))))))))

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-exec-regions-test--base #x555555555000
  "An arbitrary but realistic runtime bias for every mocked address.")

(defun nelisp-eln-exec-regions-test--sections (overrides)
  "Return a `nelisp-eln-registration--elf-section' mock plist.
OVERRIDES maps a section name to its own (ADDR . SIZE); a section not
present in OVERRIDES is absent (mocks `nil', matching a real ELF that
has no such section)."
  (lambda (_handle name) (cdr (assoc name overrides))))

(defmacro nelisp-eln-exec-regions-test--with-mocks
    (sections raw-bytes-alist &rest body)
  "Run BODY with `-elf-section'/`-raw-read-bytes'/`-state' mocked.
SECTIONS is an alist as `nelisp-eln-exec-regions-test--sections' takes.
RAW-BYTES-ALIST maps an exact (ADDRESS . LENGTH) call to the byte string
`nelisp-eln-registration--raw-read-bytes' should return for it; any call
not listed signals, matching an address this test never expected to be
read."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
              (lambda (_handle) (list :bias nelisp-eln-exec-regions-test--base)))
             ((symbol-function 'nelisp-eln-registration--elf-section)
              (nelisp-eln-exec-regions-test--sections ,sections))
             ((symbol-function 'nelisp-eln-registration--raw-read-bytes)
              (lambda (_handle address length)
                (let ((entry (assoc (cons address length) ,raw-bytes-alist)))
                  (unless entry
                    (error "unexpected raw-read-bytes call: %S %S"
                           address length))
                  (cdr entry)))))
     ,@body))

(defun nelisp-eln-exec-regions-test--caar-crt-bytes ()
  "Return a genuine, untampered CRT stub block: the fixed template with
its six holes filled in with real, mutually-consistent displacements."
  (let ((bytes (copy-sequence
                nelisp-eln-registration--crt-stub-template))
        ;; __TMC_END__ at base+0x1000 (arbitrary but shared by all four);
        ;; completed.0 at base+0x2000 (shared by both), each computed back
        ;; into its own hole's `target = base+offset+4+disp' form.
        (tmc-end (+ nelisp-eln-exec-regions-test--base #x1000))
        (completed (+ nelisp-eln-exec-regions-test--base #x2000)))
    (dolist (offset '(3 10 51 58))
      (let ((disp (- tmc-end nelisp-eln-exec-regions-test--base offset 4)))
        (aset bytes offset (logand disp #xff))
        (aset bytes (1+ offset) (logand (ash disp -8) #xff))
        (aset bytes (+ offset 2) (logand (ash disp -16) #xff))
        (aset bytes (+ offset 3) (logand (ash disp -24) #xff))))
    (dolist (offset '(118 158))
      (let ((disp (- completed nelisp-eln-exec-regions-test--base offset 4)))
        (aset bytes offset (logand disp #xff))
        (aset bytes (1+ offset) (logand (ash disp -8) #xff))
        (aset bytes (+ offset 2) (logand (ash disp -16) #xff))
        (aset bytes (+ offset 3) (logand (ash disp -24) #xff))))
    bytes))

(ert-deftest nelisp-eln-registration-exec-regions-admits-genuine-layout ()
  (let* ((crt-addr (+ nelisp-eln-exec-regions-test--base #x1040))
         (leaf-addr (+ nelisp-eln-exec-regions-test--base #x1100))
         (top-addr (+ nelisp-eln-exec-regions-test--base #x1150))
         (crt-bytes (nelisp-eln-exec-regions-test--caar-crt-bytes))
         (sections
          (list (cons ".init" (cons #x1000 (length nelisp-eln-registration--init-template)))
                (cons ".plt" (cons #x1020 (length nelisp-eln-registration--plt-template)))
                (cons ".plt.got" (cons #x1030 (length nelisp-eln-registration--plt-got-template)))
                (cons ".fini" (cons #x2184 (length nelisp-eln-registration--fini-template)))
                (cons ".text" (cons #x1040 371))))
         (raw-bytes
          (list (cons (cons (+ nelisp-eln-exec-regions-test--base #x1000)
                             (length nelisp-eln-registration--init-template))
                      nelisp-eln-registration--init-template)
                (cons (cons (+ nelisp-eln-exec-regions-test--base #x1020)
                             (length nelisp-eln-registration--plt-template))
                      nelisp-eln-registration--plt-template)
                (cons (cons (+ nelisp-eln-exec-regions-test--base #x1030)
                             (length nelisp-eln-registration--plt-got-template))
                      nelisp-eln-registration--plt-got-template)
                (cons (cons (+ nelisp-eln-exec-regions-test--base #x2184)
                             (length nelisp-eln-registration--fini-template))
                      nelisp-eln-registration--fini-template)
                (cons (cons crt-addr (length nelisp-eln-registration--crt-stub-template))
                      crt-bytes)
                ;; The 5-byte gap between the leaf (75 bytes, starting
                ;; right after the CRT block) and top_level_run.
                (cons (cons (+ leaf-addr 75) 5)
                      (unibyte-string #x0f #x1f #x44 #x00 #x00)))))
    (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
      ;; Validates by signaling on failure; success returns an
      ;; unspecified value, so the assertion is "does not signal".
      (should (progn (nelisp-eln-registration--validate-executable-regions
                       'handle (list (cons top-addr 99) (cons leaf-addr 75)))
                      t)))))

(ert-deftest nelisp-eln-registration-exec-regions-admits-self-emitted-layout ()
  ;; No .init/.plt/.plt.got/.fini, no CRT stubs; a single small 0x90
  ;; padding run between the leaf and top_level_run.
  (let* ((leaf-addr (+ nelisp-eln-exec-regions-test--base #x1000))
         (top-addr (+ nelisp-eln-exec-regions-test--base #x1010))
         (sections (list (cons ".text" (cons #x1000 93))))
         (raw-bytes
          (list (cons (cons (+ leaf-addr 6) 10) (make-string 10 ?\x90)))))
    (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
      (should (progn (nelisp-eln-registration--validate-executable-regions
                       'handle (list (cons top-addr 77) (cons leaf-addr 6)))
                      t)))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-tampered-fini ()
  (let* ((sections (list (cons ".fini" (cons #x2184 9))))
         (bad-fini (copy-sequence nelisp-eln-registration--fini-template)))
    (aset bad-fini 4 (logxor (aref bad-fini 4) #xff))
    (let ((raw-bytes
           (list (cons (cons (+ nelisp-eln-exec-regions-test--base #x2184) 9)
                       bad-fini))))
      (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
        (should-not
         (nelisp-eln-registration--validate-fixed-section
          'handle ".fini" nelisp-eln-registration--fini-template))))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-tampered-plt-got ()
  (let* ((sections (list (cons ".plt.got" (cons #x1030 8))))
         (bad (copy-sequence nelisp-eln-registration--plt-got-template)))
    (aset bad 0 (logxor (aref bad 0) #xff))
    (let ((raw-bytes
           (list (cons (cons (+ nelisp-eln-exec-regions-test--base #x1030) 8)
                       bad))))
      (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
        (should-not
         (nelisp-eln-registration--validate-fixed-section
          'handle ".plt.got" nelisp-eln-registration--plt-got-template))))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-crt-fixed-byte-flip ()
  (let* ((addr (+ nelisp-eln-exec-regions-test--base #x1040))
         (bytes (nelisp-eln-exec-regions-test--caar-crt-bytes)))
    ;; Byte 112 opens __do_global_dtors_aux's `endbr64' -- fixed, no hole.
    (aset bytes 112 (logxor (aref bytes 112) #xff))
    (let ((sections (list (cons ".init" (cons #x1000 1)) (cons ".text" (cons #x1040 192))))
          (raw-bytes
           (list (cons (cons addr (length nelisp-eln-registration--crt-stub-template))
                       bytes))))
      (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
        (should-not (nelisp-eln-registration--validate-crt-stubs 'handle))))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-crt-hole-disagreement ()
  ;; Genuine bytes, but one of the four __TMC_END__ references is altered
  ;; to point somewhere else: the template still matches byte-for-byte
  ;; (holes are holes), but the four references no longer agree.
  (let* ((addr (+ nelisp-eln-exec-regions-test--base #x1040))
         (bytes (nelisp-eln-exec-regions-test--caar-crt-bytes)))
    (aset bytes 3 (logxor (aref bytes 3) 1))
    (let ((sections (list (cons ".init" (cons #x1000 1)) (cons ".text" (cons #x1040 192))))
          (raw-bytes
           (list (cons (cons addr (length nelisp-eln-registration--crt-stub-template))
                       bytes))))
      (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
        (should-not (nelisp-eln-registration--validate-crt-stubs 'handle))))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-non-padding-gap ()
  (let* ((leaf-addr (+ nelisp-eln-exec-regions-test--base #x1000))
         (top-addr (+ nelisp-eln-exec-regions-test--base #x1010))
         (sections (list (cons ".text" (cons #x1000 93))))
         ;; A five-byte gap that is not any admitted NOP form.
         (raw-bytes
          (list (cons (cons (+ leaf-addr 6) 10)
                      (unibyte-string 1 2 3 4 5 6 7 8 9 10)))))
    (nelisp-eln-exec-regions-test--with-mocks sections raw-bytes
      (should-not
       (nelisp-eln-registration--validate-text-padding
        'handle (list (cons top-addr 77) (cons leaf-addr 6)))))))

(ert-deftest nelisp-eln-registration-exec-regions-rejects-overlapping-ranges ()
  (let ((sections (list (cons ".text" (cons #x1000 200)))))
    (nelisp-eln-exec-regions-test--with-mocks sections nil
      (should-not
       (nelisp-eln-registration--validate-text-padding
        'handle
        (list (cons (+ nelisp-eln-exec-regions-test--base #x1000) 50)
              (cons (+ nelisp-eln-exec-regions-test--base #x1010) 50)))))))

(provide 'nelisp-eln-registration-executable-regions-test)

;;; nelisp-eln-registration-executable-regions-test.el ends here
