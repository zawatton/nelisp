;;; word-object-fields.el --- Inspect owned field WORDs -*- lexical-binding: t; -*-
;; Run through tools/ai/abi-cons-check.py inspect BIN. The launcher resolves
;; the pin bridge indices from this checkout, without loading the compiler
;; and native-loader Lisp into the process being measured.
(defun wof--field (word)
  (if (= (logand word 1) 1) (list 'immediate word)
    (list 'box 'tag (ptr-read-u64 word 0)
          'bytes (logand (ptr-read-u64 (- word 8) 0) 4294967288))))
(let* ((begin (string-to-number (or (getenv "ABI_PIN_BEGIN") (error "Use the inspect launcher"))))
       (end (string-to-number (or (getenv "ABI_PIN_END") (error "Use the inspect launcher"))))
       (env (nelisp--native-env))
       (ticket (ptr-call (nelisp--native-symbol-addr begin) env 0 0 0 0 0)))
  (dolist (object (list (cons nil nil) (make-marker) (record 'word-type nil 1 nil)))
    (let* ((slot (nelisp--native-pin-copy env ticket object))
           (tag (ptr-read-u64 slot 0)) (box (ptr-read-u64 slot 8)))
      (princ (format "WORD-FIELDS %S tag=%d payload-bytes=%d\n" object tag
                     (logand (ptr-read-u64 (- box 8) 0) 4294967288)))
      (if (= tag 7)
          (princ (format "FIELDS %S %S\n" (wof--field (ptr-read-u64 box 0))
                         (wof--field (ptr-read-u64 box 8))))
        (let ((data (ptr-read-u64 box 32)) (count (ptr-read-u64 box 48)))
          (princ (format "DATA bytes=%d\n" (logand (ptr-read-u64 (- data 8) 0) 4294967288)))
          (dotimes (i count)
            (princ (format "FIELD %d %S\n" i (wof--field (ptr-read-u64 data (* i 8))))))))))
  (ptr-call (nelisp--native-symbol-addr end) env ticket 0 0 0 0))
(princ "WORD-FIELDS-DONE\n")
