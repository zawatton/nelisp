;;; par-init-elc-subtable.el --- Compiled char-table payloads -*- lexical-binding: t; -*-

(defvar par-init-elc-subtable)

(let* ((file (make-temp-file "par-init-elc-subtable-" nil ".elc"))
       (nil-slots (lambda (count) (mapconcat #'identity (make-list count "nil") " ")))
       (small-table (format "#^[inner nil parity-subtable nil %s]" (funcall nil-slots 64)))
       (plain (format "#^^[3 0 \"é😀\" \"xé\\377\" [0 1 2] %s]"
                      (funcall nil-slots 125)))
       (complex (format "#^^[3 0 %s #1=(one two) #1# %s]"
                        small-table (funcall nil-slots 125)))
       (top (lambda (ascii)
              (format "#^[outer-default nil parity-subtable %s %s]"
                      ascii (funcall nil-slots 64))))
       (header ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n")
       (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (encode-coding-string
          (format "%s(setq par-init-elc-subtable (list %s %s))\n"
                  header (funcall top plain) (funcall top complex))
          'utf-8)
         nil file nil 'silent)
        (load file nil t t)
        (let ((table (car par-init-elc-subtable)))
          (prin1 (list 'plain-subtable
                       (aref table 0)
                       (string-to-list (aref table 1))
                       (aref table 2)
                       (aref table 65)
                       (aref table 128)))
          (terpri))
        (let* ((table (cadr par-init-elc-subtable))
               (nested (aref table 0)))
          (prin1 (list 'nested-subtable
                       (char-table-p nested)
                       (and (char-table-p nested) (aref nested 300))
                       (aref table 1)
                       (eq (aref table 1) (aref table 2))))
          (terpri)))
    (delete-file file)))
