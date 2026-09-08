;;; nelisp-t91-float-oracle-build.el --- temporary bit-word build -*- lexical-binding: t; -*-

;; This recipe is driven by nelisp-t91-float-oracle.py.  The production
;; parser is renamed so the reader's own nl_str_to_float entry can expose one
;; u32 word at a time; emitting hi and lo separately avoids the standalone
;; integer-width limitation for a 64-bit value.

(setq load-prefer-newer t)
(require 'nelisp-cc-evalport-str-to-float)
(require 'nelisp-standalone-build)

(let ((xs (cdr nelisp-cc-evalport-str-to-float--source)))
  (while xs
    (let ((form (car xs)))
      (when (and (consp form)
                 (eq (car form) 'defun)
                 (eq (nth 1 form) 'nl_str_to_float))
        (setf (nth 1 form) 'nlf_t91_oracle_production)))
    (setq xs (cdr xs))))

(let ((mode (getenv "NELISP_T91_WORD")))
  (setq nelisp-cc-evalport-str-to-float--source
        (append nelisp-cc-evalport-str-to-float--source
                `((defun nl_str_to_float (bytes_ptr len slot)
                    (seq (nlf_t91_oracle_production bytes_ptr len slot)
                         (sexp-int-make
                          slot (ptr-read-u32 slot
                                              ,(if (string= mode "lo") 8 12)))
                         1))))))

(nelisp-standalone-build-reader)
