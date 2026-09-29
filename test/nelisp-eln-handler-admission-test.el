;;; nelisp-eln-handler-admission-test.el --- Doc 210 S8.2 _setjmp admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host tests (no NeLisp binary) for the one PLT surface Doc 210 admits: an
;; undefined `_setjmp@GLIBC_2.2.5' JUMP_SLOT, in an artifact whose native
;; bodies are declared exact templates.  The pinned genuine probe
;; (test/fixtures/eln-handler/s8-handler-probe.eln, compiled by host GNU
;; Emacs 31.1 `native-compile' from s8-handler-probe.el) is admitted; every
;; mutation named by the criterion -- renamed symbol, a second relocation,
;; an extra JUMP_SLOT, an extra or different DT_NEEDED, any other undefined
;; PLT symbol, an undeclared or altered body -- is refused, both by the
;; surface predicate and by the pre-dlopen validator.  Each negative is
;; paired with the unmutated positive so it cannot pass vacuously.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-handler-admission-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst nelisp-eln-handler-admission-test--eln
  (expand-file-name "fixtures/eln-handler/s8-handler-probe.eln"
                    nelisp-eln-handler-admission-test--dir))

(defconst nelisp-eln-handler-admission-test--eln-sha256
  "f46be5f57f392f5a5aa30f4b650d24cc0e0e6414f7e02655527a7224a134abbe")

(defconst nelisp-eln-handler-admission-test--body-sha256
  "98455081bbd8a42a5ec6fab0ceb44ace64765cef35c003fcd9a4851ddbe2cd5a"
  "Digest of the probe's one native body `F..._s8_handler_probe_0'.")

(defun nelisp-eln-handler-admission-test--bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-handler-admission-test--eln)
    (buffer-string)))

(defun nelisp-eln-handler-admission-test--mutate (bytes fn)
  "Return a copy of BYTES after calling FN on it (FN mutates with `aset')."
  (let ((copy (copy-sequence bytes)))
    (funcall fn copy)
    copy))

(defun nelisp-eln-handler-admission-test--replace (copy old new)
  "Replace every occurrence of the equal-length string OLD by NEW in COPY."
  (should (= (length old) (length new)))
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote old) copy start)
      (dotimes (i (length new)) (aset copy (+ (match-beginning 0) i) (aref new i)))
      (setq start (match-end 0) count (1+ count)))
    (should (> count 0))))

(defun nelisp-eln-handler-admission-test--u64 (bytes offset)
  (nelisp-eln-registration--read-u64-le bytes offset))

(defun nelisp-eln-handler-admission-test--set-u64 (copy offset value)
  (dotimes (i 8) (aset copy (+ offset i) (logand (ash value (* -8 i)) #xff))))

(defun nelisp-eln-handler-admission-test--section-header (bytes name)
  "Return the file offset of section NAME's header in BYTES."
  (let* ((section (nelisp-eln-registration--elf-section-in-bytes bytes name))
         (shoff (nelisp-eln-handler-admission-test--u64 bytes #x28))
         (count (nelisp-eln-registration--read-u16-le bytes #x3c))
         (found nil))
    (dotimes (i count)
      (let ((base (+ shoff (* i 64))))
        (when (and (= (nelisp-eln-handler-admission-test--u64 bytes (+ base 16))
                      (nth 0 section))
                   (= (nelisp-eln-handler-admission-test--u64 bytes (+ base 24))
                      (nth 1 section))
                   (= (nelisp-eln-handler-admission-test--u64 bytes (+ base 32))
                      (nth 2 section)))
          (setq found base))))
    (should found)
    found))

(defmacro nelisp-eln-handler-admission-test--declared (&rest body)
  `(let ((nelisp-eln-registration--setjmp-declared-body-sha256s
          (list nelisp-eln-handler-admission-test--body-sha256)))
     ,@body))

(defun nelisp-eln-handler-admission-test--refused-p (bytes)
  "True when BYTES, with the probe body declared, is refused by both the
surface predicate and the pre-dlopen validator."
  (nelisp-eln-handler-admission-test--declared
   (and (not (nelisp-eln-registration--setjmp-surface-admitted-p bytes))
        (condition-case nil
            (progn (nelisp-eln-registration--validate-preopen bytes) nil)
          (nelisp-eln-registration-error t)))))

(ert-deftest nelisp-eln-handler-admission-fixture-pinned ()
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (should (equal (secure-hash 'sha256 bytes)
                   nelisp-eln-handler-admission-test--eln-sha256))))

(ert-deftest nelisp-eln-handler-admission-positive ()
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (should (nelisp-eln-registration--setjmp-plt-surface-p bytes))
    (nelisp-eln-handler-admission-test--declared
     (should (nelisp-eln-registration--setjmp-surface-admitted-p bytes))
     (should (null (nelisp-eln-registration--validate-preopen bytes))))))

(ert-deftest nelisp-eln-handler-admission-undeclared-body-refused ()
  "Production declares no handler-bearing body: the surface alone admits nothing."
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (should (nelisp-eln-registration--setjmp-plt-surface-p bytes))
    ;; Production (Doc 210 S10) declares exactly the `byte-compile-form' body;
    ;; the S8 probe body is not in the list.
    (should (equal (length nelisp-eln-registration--setjmp-declared-body-sha256s) 1))
    (should-not (member nelisp-eln-handler-admission-test--body-sha256
                        nelisp-eln-registration--setjmp-declared-body-sha256s))
    (should-not (nelisp-eln-registration--setjmp-surface-admitted-p bytes))
    (should-error (nelisp-eln-registration--validate-preopen bytes)
                  :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-handler-admission-body-mutation-refused ()
  (let* ((bytes (nelisp-eln-handler-admission-test--bytes))
         (text (nelisp-eln-registration--elf-section-in-bytes bytes ".text"))
         (body (+ (nth 1 text) (- #x1110 (nth 0 text)))))
    ;; The `mov $1,%esi' type immediate of the push_handler call (offset 3 of
    ;; the body's third instruction pair): any single changed body byte.
    (dolist (delta '(0 3 #x2f #x30 #x33 #x60))
      (should
       (nelisp-eln-handler-admission-test--refused-p
        (nelisp-eln-handler-admission-test--mutate
         bytes (lambda (copy)
                 (aset copy (+ body delta) (logxor (aref copy (+ body delta)) 1)))))))))

(ert-deftest nelisp-eln-handler-admission-renamed-symbol-refused ()
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (dolist (name '("_setjmq" "_lngjmp" "_syst3m"))
      (should
       (nelisp-eln-handler-admission-test--refused-p
        (nelisp-eln-handler-admission-test--mutate
         bytes (lambda (copy)
                 (nelisp-eln-handler-admission-test--replace
                  copy "_setjmp" name))))))))

(ert-deftest nelisp-eln-handler-admission-wrong-version-refused ()
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--replace
                copy "GLIBC_2.2.5" "GLIBC_2.2.6")))))))

(ert-deftest nelisp-eln-handler-admission-second-reloc-refused ()
  "A `.rela.plt' (and DT_PLTRELSZ) that grows to a second relocation."
  (let* ((bytes (nelisp-eln-handler-admission-test--bytes))
         (header (nelisp-eln-handler-admission-test--section-header
                  bytes ".rela.plt")))
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--set-u64
                copy (+ header 32) 48)))))))

(ert-deftest nelisp-eln-handler-admission-extra-jump-slot-refused ()
  "A `.rela.dyn' entry turned into a second JUMP_SLOT naming `_setjmp'."
  (let* ((bytes (nelisp-eln-handler-admission-test--bytes))
         (rela (nelisp-eln-registration--elf-section-in-bytes bytes ".rela.dyn"))
         (plt-rela (nelisp-eln-registration--elf-section-in-bytes
                    bytes ".rela.plt"))
         (setjmp-index (ash (nelisp-eln-handler-admission-test--u64
                             bytes (+ (nth 1 plt-rela) 8))
                            -32))
         (target nil))
    (let ((off (nth 1 rela)))
      (while (and (not target) (< off (+ (nth 1 rela) (nth 2 rela))))
        (when (= (logand (nelisp-eln-handler-admission-test--u64 bytes (+ off 8))
                         #xffffffff)
                 8)
          (setq target off))
        (setq off (+ off 24))))
    (should target)
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--set-u64
                copy (+ target 8) (logior (ash setjmp-index 32) 7))))))))

(ert-deftest nelisp-eln-handler-admission-libm-needed-refused ()
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--replace
                copy "libc.so.6" "libm.so.6")))))))

(ert-deftest nelisp-eln-handler-admission-extra-needed-refused ()
  "A second DT_NEEDED naming the same allowed library (DT_GNU_HASH rewritten)."
  (let* ((bytes (nelisp-eln-handler-admission-test--bytes))
         (dyn (nelisp-eln-registration--elf-section-in-bytes bytes ".dynamic"))
         (needed nil) (victim nil))
    (let ((off (nth 1 dyn)))
      (while (< off (+ (nth 1 dyn) (nth 2 dyn)))
        (let ((tag (nelisp-eln-handler-admission-test--u64 bytes off)))
          (when (= tag 1)
            (setq needed (nelisp-eln-handler-admission-test--u64 bytes (+ off 8))))
          (when (= tag #x6ffffef5) (setq victim off)))
        (setq off (+ off 16))))
    (should (and needed victim))
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--set-u64 copy victim 1)
               (nelisp-eln-handler-admission-test--set-u64
                copy (+ victim 8) needed)))))))

(ert-deftest nelisp-eln-handler-admission-other-undefined-symbol-refused ()
  "Any undefined symbol beyond `_setjmp' and the four weak hooks."
  (let ((bytes (nelisp-eln-handler-admission-test--bytes)))
    (dolist (pair '(("__cxa_finalize" . "__cxa_finalizf")
                    ("__gmon_start__" . "__gmon_startx_")))
      (should
       (nelisp-eln-handler-admission-test--refused-p
        (nelisp-eln-handler-admission-test--mutate
         bytes (lambda (copy)
                 (nelisp-eln-handler-admission-test--replace
                  copy (car pair) (cdr pair)))))))))

(ert-deftest nelisp-eln-handler-admission-lazy-got-word-refused ()
  "The fourth `.got.plt' word must still point at the entry's own `push'."
  (let* ((bytes (nelisp-eln-handler-admission-test--bytes))
         (got (nelisp-eln-registration--elf-section-in-bytes bytes ".got.plt")))
    (should
     (nelisp-eln-handler-admission-test--refused-p
      (nelisp-eln-handler-admission-test--mutate
       bytes (lambda (copy)
               (nelisp-eln-handler-admission-test--set-u64
                copy (+ (nth 1 got) 24) #x1000)))))))

(ert-deftest nelisp-eln-handler-admission-positive-control-still-passes ()
  "The unmutated probe still passes after all mutations above (the helper
never mutates its input, so the negatives are not vacuous)."
  (nelisp-eln-handler-admission-test--declared
   (should (nelisp-eln-registration--setjmp-surface-admitted-p
            (nelisp-eln-handler-admission-test--bytes)))))

(provide 'nelisp-eln-handler-admission-test)

;;; nelisp-eln-handler-admission-test.el ends here
