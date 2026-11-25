;;; par-elc-load.el --- GNU 31.1 compiled Lisp loading parity -*- lexical-binding: t; -*-

;; Set NELISP_GNU_LISP to the GNU Emacs 31.1 lisp directory when running
;; this row on NeLisp.  Run with GNU Emacs -Q --batch -l to regenerate .gnu.

(defvar lisp-directory)
(defvar documentation-dynamic-reload)
(defvar par-elc--table)
(defvar par-elc--encoding)

(defun par-elc--report (name value)
  (prin1 (list name value))
  (terpri))

(defmacro par-elc--check (name &rest body)
  `(par-elc--report ,name (condition-case err (progn ,@body)
                            (error (list :error (car err))))))

(defun par-elc--fixture (path)
  "Write a compiled-format fixture with byte-offset docs and typed literals."
  (let* ((header ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n")
         (first "日本\001\001\0010\001_")
         (second "#@4 literal")
         (block (concat " " first "\037" second "\037"))
         (prefix (format "%s#@%d" header (string-bytes block)))
         (first-pos (1+ (string-bytes prefix)))
         (second-pos (+ first-pos (string-bytes first) 1))
         (nil-slots (lambda (n) (mapconcat #'identity (make-list n "nil") " ")))
         (parent (format "#^[parent-default nil parity-table parent-ascii %s]"
                         (funcall nil-slots 64)))
         ;; Like an actual compiled table, ASCII can share a labelled subtable
         ;; with the tree.  Here it is deliberately distinct to test both paths.
         (ascii (format "#^^[3 0 %s ascii %s]"
                        (funcall nil-slots 65) (funcall nil-slots 62)))
         ;; This ordinary vector happens to have the shape of a subtable.
         (leaf (format "[1 65536 %s]" (funcall nil-slots 16)))
         (body (concat
                (format "\n(setq par-elc--table #^[nil %s parity-table %s non-ascii %s %s extra \"日本\"])\n"
                        parent ascii leaf (funcall nil-slots 62))
                ;; #(...) forces the reference reader on the fixed dump.
                "(setq par-elc--encoding (list \"é日本😀\" ?é ?日 ?😀 #(\"é\" 0 1 (par-elc t)) \"xé\\377\"))\n"
                (format "(put 'par-elc--doc 'variable-documentation '(#$ . %d))\n"
                        first-pos)
                (format "(put 'par-elc--packed 'variable-documentation '(#$ . %d))\n"
                        second-pos)
                "#@00discard (this\n"))
         (coding-system-for-write 'no-conversion))
    (write-region (encode-coding-string (concat prefix block body) 'utf-8)
                  nil path nil 'silent)
    (list first-pos second-pos)))

(let ((root (or (getenv "NELISP_GNU_LISP") lisp-directory)))
  (par-elc--check 'ring
    (load (expand-file-name "emacs-lisp/ring.elc" root) nil t)
    (let ((ring (make-ring 3)))
      (ring-insert ring 'first)
      (ring-insert ring 'second)
      (ring-elements ring)))
  (par-elc--check 'ring-doc (documentation 'ring-p))
  (par-elc--check 'ring-insert-doc (documentation 'ring-insert t))
  (par-elc--check 'org-macs
    (load (expand-file-name "org/org-macs.elc" root) nil t)
    (org-uniquify '(x y x z y)))
  (par-elc--check 'org-function-doc (documentation 'org-assert-version t))
  (par-elc--check 'org-variable-doc
    (documentation-property 'org--inhibit-version-check 'variable-documentation t))
  (par-elc--check 'char-fold
    (load (expand-file-name "char-fold.elc" root) nil t)
    (list (char-table-p char-fold-table)
          (char-table-subtype char-fold-table)
          (char-table-range char-fold-table nil)
          (char-table-parent char-fold-table)
          (aref (char-table-extra-slot char-fold-table 0) #x300)
          (mapcar (lambda (char) (aref char-fold-table char))
                  (number-sequence 0 127))
          (mapcar (lambda (char) (cons char (aref char-fold-table char)))
                  '(128 129 233 255 256 257 767 768 769 1023 1024
                    4095 4096 4097 12354 19968 55295 55296 65535
                    65536 65537 128512 1114111 4194303))))
  (par-elc--check 'char-fold-regexp (char-fold-to-regexp "A"))
  (par-elc--check 'cl-lib-symbol-file
    ;; cl-lib loads cl-loaddefs in the middle of the file before defining
    ;; a generic method. Earlier definitions still belong to cl-lib.elc.
    (let ((file (expand-file-name "emacs-lisp/cl-lib.elc" root)))
      (load file nil t t)
      (equal (symbol-file 'cl-pushnew 'defun) file))))

(let ((file (make-temp-file "par-elc-load-" nil ".elc"))
      (documentation-dynamic-reload nil))
  (unwind-protect
      (let ((positions (par-elc--fixture file)))
        (par-elc--check 'fixture-load (load file nil t))
        (par-elc--check 'fixture-table
          (list (char-table-subtype par-elc--table)
                (char-table-range par-elc--table nil)
                (char-table-range (char-table-parent par-elc--table) nil)
                (char-table-extra-slot par-elc--table 0)
                (char-table-extra-slot par-elc--table 1)
                (mapcar (lambda (char) (aref par-elc--table char))
                        '(0 64 65 66 127 128 255 256 65535
                          65536 131071 131072 4194303))))
        (par-elc--check 'fixture-encoding
          (append (list (nth 0 par-elc--encoding) (nth 1 par-elc--encoding)
                        (nth 2 par-elc--encoding) (nth 3 par-elc--encoding))
                  (list (get-text-property 0 'par-elc (nth 4 par-elc--encoding))
                        (string-to-list (nth 5 par-elc--encoding)))))
        (par-elc--check 'fixture-doc
          (string-to-list (documentation-property 'par-elc--doc 'variable-documentation t)))
        (par-elc--check 'fixture-relative-doc
          (let ((lisp-directory (file-name-directory file)))
            (put 'par-elc--packed 'variable-documentation
                 (cons (file-name-nondirectory file) (- (cadr positions))))
            (documentation-property 'par-elc--packed 'variable-documentation t)))
        (par-elc--check 'fixture-stale-doc
          (put 'par-elc--doc 'variable-documentation (cons file (1+ (car positions))))
          (documentation-property 'par-elc--doc 'variable-documentation t)))
    (delete-file file)))

;; The byte8 in a skipped block occupies two bytes on disk.  Inside
;; literals it widens the string only when there is a multibyte character.
(defvar par-elc--after-byte8 nil)
(let ((file (make-temp-file "par-elc-byte8-" nil ".elc"))
      (compressed (make-temp-file "par-elc-byte8-" nil ".elc.gz"))
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (concat ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n"
                 "#@4 " (unibyte-string 192 128 31)
                 "(setq par-elc--after-byte8 (list (string-to-list \""
                 (unibyte-string 193 191) "\") (string-to-list \""
                 (unibyte-string 195 169 193 191) "\")))\n")
         nil file nil 'silent)
        (par-elc--check 'fixture-byte8-offset
          (list (load file nil t t) par-elc--after-byte8))
        (par-elc--check 'fixture-byte8-compressed
          ;; A deterministic gzip of the exact compiled bytes above.
          (write-region
           (base64-decode-string
            (concat
             "H4sIAAAAAAAC/7N29XGWZ2Bg4LK2tlZwzs8tyMxJTQFzMvMUXHMTk4sV"
             "ylKLijPz8xSMDfUMuZQdTBQONMhrFKeWFCoUJBbppuYk6+omppWkFukm"
             "VZakWiho5GQWlyhoFJcUZeal65bk64L5Sgf3K2liih5eCRLX1OQCAP7i"
             "2pqIAAAA"))
           nil compressed nil 'silent)
          (setq par-elc--after-byte8 nil)
          (list (load compressed nil t t) par-elc--after-byte8)))
    (delete-file file)
    (delete-file compressed)))

(defvar par-elc--operand-effect nil)
(let ((file (make-temp-file "par-elc-operands-" nil ".elc"))
      (header ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n")
      (literal "(byte-code \"\\300\\207\" [17] 1)")
      (original (symbol-function 'byte-code)))
  (unwind-protect
      (progn
        (par-elc--check 'fixture-byte-code-operand
          (write-region
           (concat header "(byte-code (progn (setq par-elc--operand-effect 42) "
                   "\"\\300\\207\") [17] 1)\n" literal "\n")
           nil file nil 'silent)
          (list (load file nil t t) par-elc--operand-effect))
        (par-elc--check 'fixture-byte-code-rebound
          (fset 'byte-code (lambda (&rest args)
                             (setq par-elc--operand-effect (length args))))
          (write-region (concat header literal) nil file nil 'silent)
          (list (load file nil t t) par-elc--operand-effect)))
    (fset 'byte-code original)
    (delete-file file)))

(defvar par-elc--header-effect nil)
(let ((file (make-temp-file "par-elc-header-" nil ".elc")))
  (unwind-protect
      (progn
        (par-elc--check 'fixture-empty-header (load file nil t t))
        (par-elc--check 'fixture-unsafe-header
          (write-region "(setq par-elc--header-effect t)\n"
                        nil file nil 'silent)
          (list (condition-case err (load file nil t t)
                  (error (car err)))
                par-elc--header-effect))
        (par-elc--check 'fixture-old-header
          (write-region ";ELC\001\n;;; in Emacs version 31.1\n(setq par-elc--header-effect t)\n"
                        nil file nil 'silent)
          (list (load file nil t t) par-elc--header-effect)))
    (delete-file file)))

(defvar par-elc--header-detected nil)
(let ((file (make-temp-file "par-elc-content-" nil ".sample"))
      (coding-system-for-write 'no-conversion))
  (unwind-protect
      (par-elc--check 'fixture-compiled-content
        ;; The compiler comment identifies the format even without ;ELC.
        (write-region
         (concat ";file-without-magic\n;;; in Emacs version 31.1\n#@4 "
                 (unibyte-string 192 128 31)
                 "(setq par-elc--header-detected (string-to-list \""
                 (unibyte-string 193 191) "\"))\n")
         nil file nil 'silent)
        (list (load file nil t t) par-elc--header-detected))
    (delete-file file)))
