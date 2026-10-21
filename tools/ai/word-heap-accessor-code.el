;;; word-heap-accessor-code.el --- S1 accessor machine-code equality -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'json)
(require 'nelisp-aot-compiler)

(defun word-heap-accessor-code (output)
  "Write actual old/new code comparisons and rejecting controls to OUTPUT.
Compile x86-64 SysV, x86-64 Win64 and AArch64; execute no foreign binary.
Compare text, relocation and symbol data, not just successful compilation."
  (let ((max-lisp-eval-depth 40000) results)
    (dolist (target '((x86_64 elf) (x86_64 coff) (aarch64 elf)))
      (let ((arch (car target)) (format (cadr target)) (count 0))
        ;; Independent old-ABI fixtures: do not derive the expected program
        ;; by invoking the accessor lowerer under test.
        (dolist (entry '((heap-read-u8 ptr-read-u8) (heap-read-u16 ptr-read-u16)
                         (heap-read-u32 ptr-read-u32) (heap-read-u64 ptr-read-u64)
                         (heap-read-s8 ptr-read-s8) (heap-read-s16 ptr-read-s16)
                         (heap-read-s32 ptr-read-s32) (heap-read-s64 ptr-read-s64)
                         (heap-native-read-u8 nelisp_ptr_read_u8)
                         (heap-native-read-u64 nelisp_ptr_read_u64)
                         (heap-extern-read-u8 extern-call nelisp_ptr_read_u8)
                         (heap-extern-read-u64 extern-call nelisp_ptr_read_u64)
                         (heap-sexp-tag-byte ptr-read-u8 0)
                         (heap-sexp-tag-word ptr-read-u64 0)
                         (heap-sexp-payload ptr-read-u64 8)
                         (heap-sexp-data ptr-read-u64 16)
                         (heap-sexp-length ptr-read-u64 24)))
          (let* ((name (car entry))
                 (field (numberp (caddr entry)))
                 ;; An offset producer call makes evaluation order/count
                 ;; observable in the code, instead of testing constants only.
                 (new `(defun wh_accessor_probe (p offset)
                         (,name (wh_identity p)
                                ,@(unless field '((wh_identity offset))))))
                 (old `(defun wh_accessor_probe (p offset)
                         ,(if field
                              (list (cadr entry) '(wh_identity p) (caddr entry))
                            (append (cdr entry) '((wh_identity p) (wh_identity offset))))))
                 (before (nelisp-aot-compile-to-link-unit old :arch arch :format format))
                 (after (nelisp-aot-compile-to-link-unit new :arch arch :format format)))
            (unless (and (> (length (plist-get before :text)) 0)
                         (equal before after))
              (error "Accessor changes emitted unit: %S %S" target name))
            (setq count (1+ count))))
        (let* ((old '(defun wh_mutation (p) (ptr-read-u64 p 8)))
               (broken '(defun wh_mutation (p) (heap-read-u64 p 16)))
               (before (nelisp-aot-compile-to-link-unit old :arch arch :format format))
               (after (nelisp-aot-compile-to-link-unit broken :arch arch :format format)))
          (when (equal (plist-get before :text) (plist-get after :text))
            (error "Code comparison missed changed payload offset: %S" target)))
        (let* ((before (nelisp-aot-compile-to-link-unit
                        '(defun wh_mutation (p) (ptr-read-u64 p 8)) :arch arch :format format))
               (nelisp-heap-accessors-sexp-fields '((heap-sexp-payload ptr-read-u64 16)))
               (after (nelisp-aot-compile-to-link-unit
                       '(defun wh_mutation (p) (heap-sexp-payload p)) :arch arch :format format)))
          (when (equal (plist-get before :text) (plist-get after :text))
            (error "Code comparison missed broken accessor lowering: %S" target)))
        (push `((arch . ,(symbol-name arch)) (format . ,(symbol-name format))
                (cases . ,count) (identical_units . t) (changed_offset_rejected . t)
                (broken_lowerer_rejected . t)) results)))
    ;; Source lowering must not reinterpret list tails or quoted Lisp data.
    (unless (equal (nelisp-heap-accessors-lower
                    '(foo heap-read-u64 (quote (heap-read-u8 p 0))))
                   '(foo heap-read-u64 (quote (heap-read-u8 p 0))))
      (error "Accessor lowering rewrites literal data"))
    (with-temp-file output
      (insert (json-encode (vconcat (nreverse results))) "\n"))
    (princ "PASS accessor code: 51 nonempty identical units, 6 rejecting mutations; literal data preserved\n")))

(provide 'word-heap-accessor-code)
;;; word-heap-accessor-code.el ends here
