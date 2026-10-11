;;; nelisp-native-template-stencils.el --- Authenticated fragment ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-native-load)
(require 'nelisp-native-funcall-v2)
(require 'nelisp-native-frame-v2)
(require 'nelisp-native-template-pin)
(declare-function syscall-direct "nelisp-standalone" (number a b c d e f))
(defconst nelisp-native-template-stencil-version "template-x86_64-sysv-v4")
(defconst nelisp-native-template-hole-grammar '(imm32 disp32 rel32 import-rel32))
(defconst nelisp-native-template-fragment-abi
  '(:persistent (r12 r13 rbx r14) :scratch (r15) :clobbers (rax rcx rdx rdi rsi r8 r9 r10 r11 flags)
    :slot-bytes 32 :call-alignment 16 :body-stack-delta 0
    :callee-saved (rbp rbx r12 r13 r14 r15) :exits (bad epilogue)
    :values rooted :pointers authenticated-nonmoving-root-bank :root-bounds certificate-and-wrapper))
(defvar nelisp-native-template--library-path
  (expand-file-name "../templates/nelisp-native-template.nelst"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defconst nelisp-native-template-win64-fragment-abi
  '(:persistent (r12 r13 rbx r14) :scratch (r15)
    :clobbers (rax rcx rdx r8 r9 r10 r11 flags)
    :slot-bytes 32 :call-alignment 16 :body-stack-delta 0 :shadow-space 32
    :callee-saved (rbp rbx rdi rsi r12 r13 r14 r15 xmm6 xmm7 xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15)
    :exits (bad epilogue) :values rooted
    :pointers authenticated-nonmoving-root-bank :root-bounds certificate-and-wrapper))
(defvar nelisp-native-template--selected-target nil)
(defun nelisp-native-template-select-target ()
  "Select a pinned stencil ABI from the runtime target, before any copying.
A process cannot change targets after selecting its library or cache identity."
  (let ((target (nelisp-native-load--target-v2)))
    (unless (and (eq (plist-get target :arch) 'x86_64)
                 (or (nelisp-native-load--windows-p)
                     (and (eq system-type 'gnu/linux)
                          (string-match-p "x86_64" system-configuration))))
      (error "Template target unsupported: %S" target))
    (if nelisp-native-template--selected-target
        (unless (equal target nelisp-native-template--selected-target)
          (error "Template selected target changed"))
      (when (eq (plist-get target :calling-convention) 'win64)
        (require 'nelisp-native-template-win64-pin)
        (setq nelisp-native-template-stencil-version "template-x86_64-win64-v1"
              nelisp-native-template-fragment-abi nelisp-native-template-win64-fragment-abi
              nelisp-native-template-library-sha256 nelisp-native-template-win64-library-sha256
              nelisp-native-template-library-source-key nelisp-native-template-win64-library-source-key
              nelisp-native-template-library-inventory nelisp-native-template-win64-library-inventory
              nelisp-native-template--library-path
              (expand-file-name "nelisp-native-template-win64.nelst"
                                (file-name-directory nelisp-native-template--library-path))))
      (setq nelisp-native-template--selected-target target))))
(defvar nelisp-native-template--snapshot nil)
(defvar nelisp-native-template--copy-count 0)
(defvar nelisp-native-template--library-check-count 0)
(defun nelisp-native-template--native-printable-p (value)
  "Bound inert data before rendering. Representation is checked by roundtrip."
  (let ((todo (list (cons value 0))) (budget 20000) (valid t))
    (while (and todo valid (> budget 0))
      (let* ((item (pop todo)) (node (car item)) (depth (cdr item)))
        (setq budget (1- budget))
        (cond
         ((> depth 256) (setq valid nil))
         ((or (null node) (symbolp node) (numberp node)) nil)
         ((stringp node) (unless (<= (length node) 1048576) (setq valid nil)))
         ((consp node)
          (push (cons (cdr node) depth) todo) (push (cons (car node) (1+ depth)) todo))
         ((vectorp node)
          (if (> (length node) 4096) (setq valid nil)
            (dotimes (i (length node)) (push (cons (aref node i) (1+ depth)) todo))))
         (t (setq valid nil)))))
    (and valid (null todo))))
(let ((native-printer (and (fboundp 'nelisp--repr) (symbol-function 'nelisp--repr)))
      (parser (symbol-function 'read-from-string)) (same (symbol-function 'equal)))
  (defun nelisp-native-template--print (value)
    "Print complete proof data with a checked native representation.
One bounded reader roundtrip proves that the fast printer preserved every
field and string. This is serialization checking, not semantic validation."
    (unless (nelisp-native-template--native-printable-p value)
      (error "Template printable data bound refused"))
    (or (and native-printer
             (condition-case nil
                 (let* ((bytes (funcall native-printer value))
                        (parsed (let ((read-circle nil)) (funcall parser bytes))))
                   (and (= (cdr parsed) (length bytes))
                        (funcall same value (car parsed)) bytes))
               (error nil)))
        (let ((print-length nil) (print-level nil) (print-circle nil)
              (print-quoted t) (print-escape-newlines t) (print-escape-nonascii t)
              (print-escape-multibyte t)) (prin1-to-string value)))))
(defun nelisp-native-template--hash (value)
  (secure-hash 'sha256 (nelisp-native-template--print value)))
(defun nelisp-native-template-stencil-abi ()
  "Address-free library ABI; source closure is pinned separately by the generator."
  (nelisp-native-template-select-target)
  (list nelisp-native-template-stencil-version nelisp-native-template-hole-grammar
        nelisp-native-template-fragment-abi (nelisp-native-load--runtime-abi-v2)
        nelisp-native-load-raw-layout-id-v2 nelisp-native-load-raw-supported-arch
        nelisp-native-funcall-v2-version (nelisp-native-funcall-v2-descriptor)
        (nelisp-native-frame-v2-descriptor)
        nelisp-native-load-raw-v2-import-contract-version))
(defun nelisp-native-template-check-holes (fragment)
  "Refuse malformed, overlapping or opcode-changing fixed-width holes."
  (let ((bytes (plist-get fragment :bytes)) (end 0))
    (unless (and (stringp bytes) (not (multibyte-string-p bytes))
                 (< 0 (length bytes) 4096)
                 (nelisp-native-load--trusted-list-p (plist-get fragment :holes)))
      (error "Template fragment shape refused"))
    (dolist (hole (plist-get fragment :holes))
      (let ((offset (plist-get hole :offset)) (kind (plist-get hole :kind)))
        (unless (and (nelisp-native-load--trusted-list-p hole)
                     (symbolp (plist-get hole :name))
                     (memq kind nelisp-native-template-hole-grammar)
                     (eql (plist-get hole :width) 4) (eq (plist-get hole :signed) t)
                     (equal (plist-get hole :mask) '(0 0 0 0))
                     (integerp offset) (<= end offset) (<= (+ offset 4) (length bytes)))
          (error "Template hole grammar or overlap refused"))
        (dotimes (i 4)
          (unless (= 0 (aref bytes (+ offset i))) (error "Template hole mask refused")))
        (setq end (+ offset 4))))
    t))
(defun nelisp-native-template--read-library (snapshot)
  (let* ((read-circle nil) (parsed (read-from-string snapshot)))
    (unless (string-match-p "\\`[ \n\r\t]*\\'" (substring snapshot (cdr parsed)))
      (error "Trailing stencil library data")) (car parsed)))
(defun nelisp-native-template-open-library ()
  "Read one bounded private snapshot; authenticate before any fragment copy."
  (nelisp-native-template-select-target)
  (require 'nelisp-native-template-pin)
  (unless nelisp-native-template--snapshot
    ;; The bytes are authenticated by the pinned sha256 of a private snapshot,
    ;; so only world-writable files are refused here: a git checkout under the
    ;; common umask 002 leaves the library group-writable.
    (let* ((attrs (file-attributes nelisp-native-template--library-path 'integer))
           (mode (file-modes nelisp-native-template--library-path))
           (bytes (and attrs (nth 7 attrs))))
      (unless (and attrs (not (car attrs)) (not (file-symlink-p nelisp-native-template--library-path))
                   (integerp bytes) (< 0 bytes 65536)
                   ;; Windows checkout files use a pinned digest rather than
                   ;; POSIX uid/mode emulation. Cache publication still requires
                   ;; the common protected-DACL and handle checks.
                   (or (nelisp-native-load--windows-p)
                       (and mode (= 0 (logand mode #o002))
                            (eql (nth 2 attrs) (if (fboundp 'user-uid) (user-uid)
                                                (syscall-direct 102 0 0 0 0 0 0))))))
        (error "Stencil library ownership, permissions or bound refused"))
      (let ((snapshot (with-temp-buffer (set-buffer-multibyte nil)
                                       (insert-file-contents-literally nelisp-native-template--library-path nil 0 65537)
                                       (buffer-string))))
        (unless (and (= bytes (length snapshot))
                     (equal (secure-hash 'sha256 snapshot) nelisp-native-template-library-sha256))
          (error "Stencil library pinned digest refused"))
        (let ((library (nelisp-native-template--read-library snapshot)))
          (unless (and (equal (plist-get library :abi) (nelisp-native-template-stencil-abi))
                       (equal (plist-get library :source-key) nelisp-native-template-library-source-key)
                       (equal (plist-get library :opcode-inventory) nelisp-native-template-library-inventory)
                       (equal (mapcar #'car (plist-get library :fragments))
                              '(prologue copy call poll fixnum-add fixnum-sub fixnum-mul fixnum-inc fixnum-dec fixnum-neg fixnum-eq fixnum-lt fixnum-gt fixnum-le fixnum-ge frame status-save status-restore status-branch nil-branch nonnull-branch jump switch return bad epilogue)))
            (error "Stencil library stale ABI/key refused"))
          (dolist (fragment (plist-get library :fragments))
            (unless (equal (plist-get (cdr fragment) :abi) nelisp-native-template-fragment-abi)
              (error "Stencil fragment ABI refused"))
            (nelisp-native-template-check-holes (cdr fragment))))
        ;; No mutable object is returned from this capture. Reparse gives each
        ;; compiler its own private copy; changed disk bytes are never reopened.
        (setq nelisp-native-template--snapshot
              (let ((frozen snapshot)) (lambda () (nelisp-native-template--read-library frozen))))
        (setq nelisp-native-template--library-check-count (1+ nelisp-native-template--library-check-count)))))
  (funcall nelisp-native-template--snapshot))
(defun nelisp-native-template-patch32 (bytes offset value)
  "Write a signed, fixed-width imm/disp/rel32 to a fresh function buffer."
  (unless (and (integerp value) (<= -2147483648 value 2147483647)
               (integerp offset) (<= 0 offset) (<= (+ offset 4) (length bytes)))
    (error "Template signed patch overflow/bounds refused"))
  (aset bytes offset (logand 255 value))
  (aset bytes (+ offset 1) (logand 255 (ash value -8)))
  (aset bytes (+ offset 2) (logand 255 (ash value -16)))
  (aset bytes (+ offset 3) (logand 255 (ash value -24))))
(provide 'nelisp-native-template-stencils)
