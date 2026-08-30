;;; nelisp-cc-evalport-nonenv-mut-str-set-cp.el --- lowered -*- lexical-binding: t; -*-
;;; Code:
(defconst nelisp-cc-evalport-nonenv-mut-str-set-cp--source
  '(seq
    (defun nl_msscp_write_int_out (out val-cp)
      (seq
       (ptr-write-u64 out 0 2)
       (ptr-write-u64 (+ out 8) 0 val-cp)
       0))
    (defun nl_msscp_unibyte_write (arg idx val-cp)
      (let* ((nlstr (ptr-read-u64 (+ arg 8) 0))
             (data (ptr-read-u64 (+ nlstr 8) 0))
             (len (ptr-read-u64 (+ nlstr 16) 0)))
        (if (if (< val-cp 0) 1 (if (> val-cp 255) 1 0))
            1
          (if (>= idx len)
              1
            (seq (nelisp_ptr_write_u8 data idx val-cp) 0)))))
    (defun nl_msscp_multibyte_find_and_write
        (arg byte-idx char-idx idx val-cp scratch)
      (let* ((ok (extern-call nl_str_codepoint_at
                              arg byte-idx scratch (+ scratch 8))))
        (if (= ok 0)
            1
          (let* ((old-cp (ptr-read-u64 scratch 0))
                 (width (ptr-read-u64 (+ scratch 8) 0)))
            (if (= char-idx idx)
                (if (>= old-cp 128)
                    1
                  (let* ((nlstr (ptr-read-u64 (+ arg 8) 0))
                         (data (ptr-read-u64 (+ nlstr 8) 0)))
                    (seq
                     (nelisp_ptr_write_u8 data byte-idx val-cp)
                     0)))
              (nl_msscp_multibyte_find_and_write
               arg (+ byte-idx width) (+ char-idx 1) idx val-cp scratch))))))
    (defun nl_msscp_multibyte_write (arg idx val-cp)
      (if (if (< val-cp 0) 1 (if (> val-cp 127) 1 0))
          1
        (let* ((char-count (extern-call nl_str_char_count arg)))
          (if (>= idx char-count)
              1
            (let* ((scratch (alloc-bytes 16 8)))
              (nl_msscp_multibyte_find_and_write
               arg 0 0 idx val-cp scratch))))))
    (defun nl_mut_str_set_codepoint_raw (arg idx val-cp out)
      (if (< idx 0)
          1
        (let* ((tag (nelisp_ptr_read_u8 arg 0))
               (rc (if (= tag 15)
                       (nl_msscp_unibyte_write arg idx val-cp)
                     (if (= tag 6)
                         (nl_msscp_multibyte_write arg idx val-cp)
                       1))))
          (if (= rc 0)
              (nl_msscp_write_int_out out val-cp)
            1))))))
(provide (quote nelisp-cc-evalport-nonenv-mut-str-set-cp))
