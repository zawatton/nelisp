;;; par-elc-stream-boundaries.el --- Incremental .elc read/eval boundaries -*- lexical-binding: t; -*-

(defvar par-elc-stream-boundaries--seen nil)
(defvar par-elc-stream-boundaries--constant nil)
(defvar par-elc-stream-boundaries--byte8 nil)

(let ((file (make-temp-file "par-elc-stream-boundaries-" nil ".elc"))
      (header ";ELC\037\0\0\0\n;;; in Emacs version 31.1\n")
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (concat header
                 "(push (intern-soft \"par-elc-stream--not-read-yet\") par-elc-stream-boundaries--seen)\n"
                 "(garbage-collect)\n"
                 "(setq par-elc-stream-boundaries--constant 'par-elc-stream--not-read-yet)\n"
                 "(defalias 'par-elc-stream-boundaries--fn #[0 \"\\300\\207\" [#1=[a b] #1#] 1])\n"
                 "(garbage-collect)\n"
                 "(push (eq par-elc-stream-boundaries--constant (intern-soft \"par-elc-stream--not-read-yet\")) par-elc-stream-boundaries--seen)\n"
                 "(push (par-elc-stream-boundaries--fn) par-elc-stream-boundaries--seen)\n"
                 "(setq par-elc-stream-boundaries--byte8 #(\""
                 (unibyte-string #xc1 #xbf #xc0 #x81 #xc3 #xa9 #xc1 #x80 #xc0 #xbf)
                 "\" 0 1 (byte8-test yes)))\n"
                 "#@00 (push 'late par-elc-stream-boundaries--seen)\n")
         nil file nil 'silent)
        (load file nil t t)
        (prin1 (cons 'load-boundaries (nreverse par-elc-stream-boundaries--seen)))
        (terpri)
        (let ((constants (aref (symbol-function 'par-elc-stream-boundaries--fn) 2)))
          (prin1 (list 'shared-constant (eq (aref constants 0) (aref constants 1))
                       (eq (par-elc-stream-boundaries--fn) (aref constants 0)))))
        (terpri)
        (prin1 (list 'byte8-fallback
                     (string-to-list par-elc-stream-boundaries--byte8)
                     (get-text-property 0 'byte8-test
                                        par-elc-stream-boundaries--byte8)))
        (terpri))
    (delete-file file)))
