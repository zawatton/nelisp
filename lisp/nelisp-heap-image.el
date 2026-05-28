;;; nelisp-heap-image.el --- Heap image graph codec in Elisp  -*- lexical-binding: t; byte-compile-warnings: (not unresolved); -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; heap-v0 is the first non-source-replay image layer.  It serializes an
;; explicit root set as an object graph with numeric object ids, so cons,
;; vector, and record identity can be reconstructed without evaluating the
;; original source forms again.
;;
;; This module intentionally stays in Elisp.  A future cold loader can use
;; the same format before `Env::new_global' falls back to source boot; the
;; only non-Elisp responsibility should be byte input plus root installation.

;;; Code:

(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (type-tag &rest slots)
    "Host-Emacs compatibility record used by heap image tests."
    (apply 'vector type-tag slots))
  (defun nelisp--record-length (obj)
    "Host-Emacs compatibility record length for OBJ."
    (1- (length obj)))
  (defun nelisp--record-type (obj)
    "Host-Emacs compatibility record type for OBJ."
    (aref obj 0))
  (defun nelisp--record-ref (obj index)
    "Host-Emacs compatibility record slot read from OBJ at INDEX."
    (aref obj (1+ index)))
  (defun nelisp--record-set (obj index value)
    "Host-Emacs compatibility record slot write to OBJ at INDEX."
    (aset obj (1+ index) value)))

(defconst nelisp-heap-image--magic ";;; nelisp-heap-image heap-v0\n"
  "Magic header for heap-v0 S-expression image files.")

(defconst nelisp-heap-image--line-magic ";;; nelisp-heap-image heap-v0-lines\n"
  "Magic header for heap-v0 line image files.")

(defconst nelisp-heap-image--counted-line-magic
  ";;; nelisp-heap-image heap-v0-lines-counted\n"
  "Magic header for heap-v0 line image files with count metadata.")

(defconst nelisp-heap-image--binary-magic
  ";;; nelisp-heap-image heap-v0-bin\n"
  "Magic header for heap-v0 binary image files.")

(defconst nelisp-heap-image--binary-ref-nil 0)
(defconst nelisp-heap-image--binary-ref-t 1)
(defconst nelisp-heap-image--binary-ref-int 2)
(defconst nelisp-heap-image--binary-ref-float 3)
(defconst nelisp-heap-image--binary-ref-str 4)
(defconst nelisp-heap-image--binary-ref-symbol 5)
(defconst nelisp-heap-image--binary-ref-object 6)

(defconst nelisp-heap-image--binary-obj-cons 1)
(defconst nelisp-heap-image--binary-obj-vector 2)
(defconst nelisp-heap-image--binary-obj-record 3)
(defconst nelisp-heap-image--binary-obj-mut-str 4)
(defconst nelisp-heap-image--binary-obj-cell 5)
(defconst nelisp-heap-image--binary-obj-bool-vector 6)
(defconst nelisp-heap-image--binary-obj-char-table 7)

(defconst nelisp-heap-image--tag 'nelisp-heap-image-v0
  "Top-level data tag for heap-v0 images.")

(defconst nelisp-heap-image--env-root-manifest
  '(("globals_record" env-globals-record)
    ("frames_record" env-frames-record)
    ("unbound_marker" env-unbound-marker)
    ("use_elisp_apply" env-use-elisp-apply)
    ("max_recursion" env-max-recursion))
  "Pure-Elisp manifest for the Env roots stored in heap-v0 images.

The first element is the stable image root name.  The second element is
a cold-loader accessor token implemented by the minimal Rust bridge
while dumping or restoring an Env.")

(defun nelisp-heap-image-env-root-manifest ()
  "Return the heap-v0 Env root manifest."
  nelisp-heap-image--env-root-manifest)

(defun nelisp-heap-image--nil-list (n)
  "Return a list of N nil values."
  (let ((out nil))
    (while (> n 0)
      (setq out (cons nil out))
      (setq n (1- n)))
    out))

(defun nelisp-heap-image--immediate-ref (obj)
  "Return an immediate reference for OBJ, or nil if OBJ needs an id."
  (cond
   ((null obj) '(imm nil))
   ((eq obj t) '(imm t))
   ((integerp obj) (list 'imm 'int obj))
   ((floatp obj) (list 'imm 'float obj))
   ((stringp obj) (list 'imm 'str obj))
   ((symbolp obj) (list 'imm 'sym (symbol-name obj)))
   (t nil)))

(defun nelisp-heap-image--alloc-kind (obj)
  "Return heap-v0 object kind for OBJ, or signal if unsupported."
  (cond
   ((consp obj) 'cons)
   ((vectorp obj) 'vector)
   ((recordp obj) 'record)
   (t (signal 'wrong-type-argument (list 'nelisp-heap-image-object obj)))))

(defun nelisp-heap-image--encode-ref (obj seen objects next-id-cell)
  "Encode OBJ to a heap-v0 ref.
SEEN maps object identity to ids, OBJECTS is a cons cell whose cdr is
the reversed object descriptor list, and NEXT-ID-CELL is a cons cell
holding the next integer id in its car."
  (or (nelisp-heap-image--immediate-ref obj)
      (let ((existing (gethash obj seen nil)))
        (if existing
            (list 'ref existing)
          (let* ((id (car next-id-cell))
                 (kind (nelisp-heap-image--alloc-kind obj)))
            (setcar next-id-cell (1+ id))
            (puthash obj id seen)
            (let ((desc
                   (cond
                    ((eq kind 'cons)
                     (list id 'cons
                           (nelisp-heap-image--encode-ref
                            (car obj) seen objects next-id-cell)
                           (nelisp-heap-image--encode-ref
                            (cdr obj) seen objects next-id-cell)))
                    ((eq kind 'vector)
                     (let ((refs nil)
                           (i 0)
                           (n (length obj)))
                       (while (< i n)
                         (setq refs
                               (cons (nelisp-heap-image--encode-ref
                                      (aref obj i) seen objects next-id-cell)
                                     refs))
                         (setq i (1+ i)))
                       (list id 'vector (nreverse refs))))
                    ((eq kind 'record)
                     (let ((refs nil)
                           (i 0)
                           (n (nelisp--record-length obj))
                           (type-tag (nelisp--record-type obj)))
                       (while (< i n)
                         (setq refs
                               (cons (nelisp-heap-image--encode-ref
                                      (nelisp--record-ref obj i)
                                      seen objects next-id-cell)
                                     refs))
                         (setq i (1+ i)))
                       (list id 'record
                             (nelisp-heap-image--encode-ref
                              type-tag seen objects next-id-cell)
                             (nreverse refs))))
                    (t (signal 'error (list "unknown heap image kind"))))))
              (setcdr objects (cons desc (cdr objects)))
              (list 'ref id)))))))

(defun nelisp-heap-image-encode-roots (roots)
  "Encode ROOTS as a heap-v0 data form.
ROOTS is an alist of (NAME . VALUE), where NAME is a symbol or string.
Return a readable data form, not a source program."
  (let ((seen (make-hash-table :test 'eq))
        (objects (cons nil nil))
        (next-id-cell (cons 1 nil))
        (encoded-roots nil)
        (cur roots))
    (while cur
      (let* ((entry (car cur))
             (name (car entry))
             (value (cdr entry))
             (name-string (cond
                           ((symbolp name) (symbol-name name))
                           ((stringp name) name)
                           (t (signal 'wrong-type-argument
                                      (list 'symbol-or-string-p name))))))
        (setq encoded-roots
              (cons (cons name-string
                          (nelisp-heap-image--encode-ref
                           value seen objects next-id-cell))
                    encoded-roots)))
      (setq cur (cdr cur)))
    (list nelisp-heap-image--tag
          :version 0
          :roots (nreverse encoded-roots)
          :objects (nreverse (cdr objects)))))

(defun nelisp-heap-image-dump-string (roots)
  "Return ROOTS encoded as a heap-v0 image string."
  (nelisp-heap-image--binary-form
   (nelisp-heap-image-encode-roots roots)))

(defun nelisp-heap-image--strip-magic (source)
  "Return heap-v0 data payload from SOURCE, or signal on bad magic."
  (let ((n (length nelisp-heap-image--magic)))
    (unless (and (>= (length source) n)
                 (string= (substring source 0 n)
                          nelisp-heap-image--magic))
      (signal 'error (list "bad heap image magic")))
    (substring source n)))

(defun nelisp-heap-image--plist-get (plist key)
  "Small local `plist-get' to keep the codec substrate-independent."
  (let ((cur plist)
        (hit nil)
        (value nil))
    (while (and cur (not hit))
      (when (eq (car cur) key)
        (setq value (car (cdr cur))
              hit t))
      (setq cur (cdr (cdr cur))))
    value))

(defun nelisp-heap-image--hex-string (string)
  "Return hexadecimal representation of STRING's character bytes.
heap-v0-lines is intentionally ASCII-framed.  Current boot images only
need ASCII names and symbols; non-ASCII payloads should continue using
the S-expression form until NeLisp has a byte-level string codec here."
  (let ((i 0)
        (n (length string))
        (out ""))
    (while (< i n)
      (setq out (concat out (format "%02x" (aref string i))))
      (setq i (1+ i)))
    out))

(defun nelisp-heap-image--hex-decode (hex)
  "Decode HEX produced by `nelisp-heap-image--hex-string'."
  (let ((i 0)
        (n (length hex))
        (out ""))
    (unless (= (% n 2) 0)
      (signal 'error (list "odd-length heap image hex string")))
    (while (< i n)
      (setq out
            (concat out
                    (string
                     (string-to-number (substring hex i (+ i 2)) 16))))
      (setq i (+ i 2)))
    out))

(defun nelisp-heap-image--line-ref (ref)
  "Encode heap-v0 REF for the line image format."
  (let ((tag (car ref)))
    (cond
     ((eq tag 'ref)
      (concat "R" (number-to-string (car (cdr ref)))))
     ((eq tag 'imm)
      (let ((kind (car (cdr ref)))
            (value (car (cdr (cdr ref)))))
        (cond
         ((eq kind 'nil) "N")
         ((eq kind 't) "T")
         ((eq kind 'int) (concat "I" (number-to-string value)))
         ((eq kind 'str) (concat "S" (nelisp-heap-image--hex-string value)))
         ((eq kind 'sym) (concat "Y" (nelisp-heap-image--hex-string value)))
         ((eq kind 'float) (concat "F" (number-to-string value)))
         (t (signal 'error (list "unknown heap image immediate" kind))))))
     (t (signal 'error (list "malformed heap image ref" ref))))))

(defun nelisp-heap-image--line-ref-list (refs)
  "Encode REFS as a comma-separated heap-v0-lines field."
  (let ((out "")
        (cur refs)
        (first t))
    (while cur
      (unless first
        (setq out (concat out ",")))
      (setq out (concat out (nelisp-heap-image--line-ref (car cur))))
      (setq first nil
            cur (cdr cur)))
    out))

(defun nelisp-heap-image--line-object (desc)
  "Encode object DESC as one heap-v0-lines object line."
  (let ((id (nth 0 desc))
        (kind (nth 1 desc)))
    (cond
     ((eq kind 'cons)
      (concat "obj\t" (number-to-string id) "\tcons\t"
              (nelisp-heap-image--line-ref (nth 2 desc)) "\t"
              (nelisp-heap-image--line-ref (nth 3 desc))))
     ((eq kind 'vector)
      (concat "obj\t" (number-to-string id) "\tvector\t"
              (nelisp-heap-image--line-ref-list (nth 2 desc))))
     ((eq kind 'record)
      (concat "obj\t" (number-to-string id) "\trecord\t"
              (nelisp-heap-image--line-ref (nth 2 desc)) "\t"
              (nelisp-heap-image--line-ref-list (nth 3 desc))))
     (t (signal 'error (list "heap-v0-lines unsupported object" kind))))))

(defun nelisp-heap-image--byte (n)
  "Return one byte string for N."
  (string (logand n 255)))

(defun nelisp-heap-image--u32-le (n)
  "Return N as a little-endian unsigned 32-bit string."
  (concat (nelisp-heap-image--byte n)
          (nelisp-heap-image--byte (ash n -8))
          (nelisp-heap-image--byte (ash n -16))
          (nelisp-heap-image--byte (ash n -24))))

(defun nelisp-heap-image--u64-le (n)
  "Return N as a little-endian unsigned 64-bit string."
  (concat (nelisp-heap-image--byte n)
          (nelisp-heap-image--byte (ash n -8))
          (nelisp-heap-image--byte (ash n -16))
          (nelisp-heap-image--byte (ash n -24))
          (nelisp-heap-image--byte (ash n -32))
          (nelisp-heap-image--byte (ash n -40))
          (nelisp-heap-image--byte (ash n -48))
          (nelisp-heap-image--byte (ash n -56))))

(defun nelisp-heap-image--i64-le (n)
  "Return N as a little-endian signed 64-bit string."
  (when (< n 0)
    (setq n (+ n (ash 1 64))))
  (nelisp-heap-image--u64-le n))

(defun nelisp-heap-image--binary-string (string)
  "Encode STRING as length-prefixed heap-v0-bin bytes."
  (let ((bytes (if (multibyte-string-p string)
                   (encode-coding-string string 'utf-8 t)
                 string)))
    (concat (nelisp-heap-image--u64-le (length bytes)) bytes)))

(defun nelisp-heap-image--binary-ref (ref)
  "Encode heap-v0 REF for the binary image format."
  (let ((tag (car ref)))
    (cond
     ((eq tag 'ref)
      (concat (nelisp-heap-image--byte nelisp-heap-image--binary-ref-object)
              (nelisp-heap-image--i64-le (car (cdr ref)))))
     ((eq tag 'imm)
      (let ((kind (car (cdr ref)))
            (value (car (cdr (cdr ref)))))
        (cond
         ((eq kind 'nil)
          (nelisp-heap-image--byte nelisp-heap-image--binary-ref-nil))
         ((eq kind 't)
          (nelisp-heap-image--byte nelisp-heap-image--binary-ref-t))
         ((eq kind 'int)
          (concat (nelisp-heap-image--byte nelisp-heap-image--binary-ref-int)
                  (nelisp-heap-image--i64-le value)))
         ((eq kind 'str)
          (concat (nelisp-heap-image--byte nelisp-heap-image--binary-ref-str)
                  (nelisp-heap-image--binary-string value)))
         ((eq kind 'sym)
          (concat (nelisp-heap-image--byte nelisp-heap-image--binary-ref-symbol)
                  (nelisp-heap-image--binary-string value)))
         ((eq kind 'float)
          (concat (nelisp-heap-image--byte nelisp-heap-image--binary-ref-float)
                  (nelisp-heap-image--binary-string (number-to-string value))))
         (t (signal 'error (list "unknown heap image immediate" kind))))))
     (t (signal 'error (list "malformed heap image ref" ref))))))

(defun nelisp-heap-image--binary-ref-list (refs)
  "Encode REFS as heap-v0-bin length plus references."
  (let ((out (nelisp-heap-image--u64-le (length refs)))
        (cur refs))
    (while cur
      (setq out (concat out (nelisp-heap-image--binary-ref (car cur))))
      (setq cur (cdr cur)))
    out))

(defun nelisp-heap-image--binary-object (desc)
  "Encode object DESC as one heap-v0-bin object payload."
  (let ((kind (nth 1 desc)))
    (cond
     ((eq kind 'cons)
      (concat (nelisp-heap-image--byte nelisp-heap-image--binary-obj-cons)
              (nelisp-heap-image--binary-ref (nth 2 desc))
              (nelisp-heap-image--binary-ref (nth 3 desc))))
     ((eq kind 'vector)
      (concat (nelisp-heap-image--byte nelisp-heap-image--binary-obj-vector)
              (nelisp-heap-image--binary-ref-list (nth 2 desc))))
     ((eq kind 'record)
      (concat (nelisp-heap-image--byte nelisp-heap-image--binary-obj-record)
              (nelisp-heap-image--binary-ref (nth 2 desc))
              (nelisp-heap-image--binary-ref-list (nth 3 desc))))
     (t (signal 'error (list "heap-v0-bin unsupported object" kind))))))

(defun nelisp-heap-image--binary-form (form)
  "Encode heap-v0 FORM in binary image format."
  (let* ((plist (cdr form))
         (roots (nelisp-heap-image--plist-get plist :roots))
         (objects (nelisp-heap-image--plist-get plist :objects))
         (out (concat nelisp-heap-image--binary-magic
                      (nelisp-heap-image--u32-le 0)
                      (nelisp-heap-image--u64-le (length objects))
                      (nelisp-heap-image--u64-le (length roots))))
         (cur objects))
    (while cur
      (setq out
            (concat out
                    (nelisp-heap-image--i64-le (nth 0 (car cur)))
                    (nelisp-heap-image--binary-object (car cur))))
      (setq cur (cdr cur)))
    (setq cur roots)
    (while cur
      (let ((entry (car cur)))
        (setq out
              (concat out
                      (nelisp-heap-image--binary-string (car entry))
                      (nelisp-heap-image--binary-ref (cdr entry)))))
      (setq cur (cdr cur)))
    out))

(defun nelisp-heap-image--line-form (form)
  "Encode heap-v0 FORM in line image format."
  (let* ((plist (cdr form))
         (roots (nelisp-heap-image--plist-get plist :roots))
         (objects (nelisp-heap-image--plist-get plist :objects))
         (out (concat nelisp-heap-image--counted-line-magic
                      "version\t0\n"
                      "counts\t"
                      (number-to-string (length objects))
                      "\t"
                      (number-to-string (length roots))
                      "\n"))
         (cur objects))
    (while cur
      (setq out (concat out
                        (nelisp-heap-image--line-object (car cur))
                        "\n"))
      (setq cur (cdr cur)))
    (setq cur roots)
    (while cur
      (let ((entry (car cur)))
        (setq out
              (concat out
                      "root\t"
                      (nelisp-heap-image--hex-string (car entry))
                      "\t"
                      (nelisp-heap-image--line-ref (cdr entry))
                      "\n")))
      (setq cur (cdr cur)))
    out))

(defun nelisp-heap-image--parse-line-ref (token)
  "Decode heap-v0-lines TOKEN to a heap-v0 reference form."
  (let ((tag (aref token 0))
        (body (substring token 1)))
    (cond
     ((= tag ?N) '(imm nil))
     ((= tag ?T) '(imm t))
     ((= tag ?I) (list 'imm 'int (string-to-number body)))
     ((= tag ?F) (list 'imm 'float (string-to-number body)))
     ((= tag ?S) (list 'imm 'str (nelisp-heap-image--hex-decode body)))
     ((= tag ?Y) (list 'imm 'sym (nelisp-heap-image--hex-decode body)))
     ((= tag ?R) (list 'ref (string-to-number body)))
     (t (signal 'error (list "unknown heap image line ref" token))))))

(defun nelisp-heap-image--parse-line-ref-list (field)
  "Decode comma-separated FIELD to heap-v0 refs."
  (if (= (length field) 0)
      nil
    (let ((parts (split-string field ","))
          (out nil))
      (while parts
        (setq out
              (cons (nelisp-heap-image--parse-line-ref (car parts)) out))
        (setq parts (cdr parts)))
      (nreverse out))))

(defun nelisp-heap-image--line-source-to-form (source)
  "Decode heap-v0-lines SOURCE to the canonical S-expression image form."
  (let* ((magic (if (and (>= (length source)
                            (length nelisp-heap-image--counted-line-magic))
                       (string= (substring source 0
                                           (length nelisp-heap-image--counted-line-magic))
                                nelisp-heap-image--counted-line-magic))
                  nelisp-heap-image--counted-line-magic
                nelisp-heap-image--line-magic))
         (payload (substring source (length magic)))
         (lines (split-string payload "\n" t))
         (roots nil)
         (objects nil))
    (while lines
      (let ((parts (split-string (car lines) "\t")))
        (cond
         ((equal (car parts) "version") nil)
         ((equal (car parts) "counts") nil)
         ((equal (car parts) "root")
          (setq roots
                (cons (cons (nelisp-heap-image--hex-decode (nth 1 parts))
                            (nelisp-heap-image--parse-line-ref (nth 2 parts)))
                      roots)))
         ((equal (car parts) "obj")
          (let ((id (string-to-number (nth 1 parts)))
                (kind (nth 2 parts)))
            (setq objects
                  (cons
                   (cond
                    ((equal kind "cons")
                     (list id 'cons
                           (nelisp-heap-image--parse-line-ref (nth 3 parts))
                           (nelisp-heap-image--parse-line-ref (nth 4 parts))))
                    ((equal kind "vector")
                     (list id 'vector
                           (nelisp-heap-image--parse-line-ref-list (nth 3 parts))))
                    ((equal kind "record")
                     (list id 'record
                           (nelisp-heap-image--parse-line-ref (nth 3 parts))
                           (nelisp-heap-image--parse-line-ref-list (nth 4 parts))))
                    (t (signal 'error
                               (list "unsupported heap image line object" kind))))
                   objects))))
         (t (signal 'error (list "malformed heap image line" (car lines))))))
      (setq lines (cdr lines)))
    (list nelisp-heap-image--tag
          :version 0
          :roots (nreverse roots)
          :objects (nreverse objects))))

(defun nelisp-heap-image--binary-read-u8 (source pos-cell)
  "Read one byte from SOURCE at POS-CELL."
  (let ((pos (car pos-cell)))
    (unless (< pos (length source))
      (signal 'error (list "truncated heap-v0-bin image")))
    (setcar pos-cell (1+ pos))
    (aref source pos)))

(defun nelisp-heap-image--binary-read-u32 (source pos-cell)
  "Read little-endian u32 from SOURCE at POS-CELL."
  (let ((i 0)
        (shift 0)
        (out 0))
    (while (< i 4)
      (setq out (+ out (ash (nelisp-heap-image--binary-read-u8 source pos-cell)
                            shift)))
      (setq i (1+ i)
            shift (+ shift 8)))
    out))

(defun nelisp-heap-image--binary-read-u64 (source pos-cell)
  "Read little-endian u64 from SOURCE at POS-CELL."
  (let ((i 0)
        (shift 0)
        (out 0))
    (while (< i 8)
      (setq out (+ out (ash (nelisp-heap-image--binary-read-u8 source pos-cell)
                            shift)))
      (setq i (1+ i)
            shift (+ shift 8)))
    out))

(defun nelisp-heap-image--binary-read-i64 (source pos-cell)
  "Read little-endian i64 from SOURCE at POS-CELL."
  (let ((n (nelisp-heap-image--binary-read-u64 source pos-cell)))
    (if (>= n (ash 1 63))
        (- n (ash 1 64))
      n)))

(defun nelisp-heap-image--binary-read-string (source pos-cell)
  "Read a heap-v0-bin length-prefixed string from SOURCE at POS-CELL."
  (let* ((len (nelisp-heap-image--binary-read-u64 source pos-cell))
         (beg (car pos-cell))
         (end (+ beg len)))
    (when (> end (length source))
      (signal 'error (list "truncated heap-v0-bin string")))
    (setcar pos-cell end)
    (substring source beg end)))

(defun nelisp-heap-image--binary-read-ref (source pos-cell)
  "Read a heap-v0-bin reference from SOURCE at POS-CELL."
  (let ((tag (nelisp-heap-image--binary-read-u8 source pos-cell)))
    (cond
     ((= tag nelisp-heap-image--binary-ref-nil) '(imm nil))
     ((= tag nelisp-heap-image--binary-ref-t) '(imm t))
     ((= tag nelisp-heap-image--binary-ref-int)
      (list 'imm 'int
            (nelisp-heap-image--binary-read-i64 source pos-cell)))
     ((= tag nelisp-heap-image--binary-ref-float)
      (list 'imm 'float
            (string-to-number
             (nelisp-heap-image--binary-read-string source pos-cell))))
     ((= tag nelisp-heap-image--binary-ref-str)
      (list 'imm 'str
            (nelisp-heap-image--binary-read-string source pos-cell)))
     ((= tag nelisp-heap-image--binary-ref-symbol)
      (list 'imm 'sym
            (nelisp-heap-image--binary-read-string source pos-cell)))
     ((= tag nelisp-heap-image--binary-ref-object)
      (list 'ref
            (nelisp-heap-image--binary-read-i64 source pos-cell)))
     (t (signal 'error (list "unknown heap-v0-bin ref tag" tag))))))

(defun nelisp-heap-image--binary-read-ref-list (source pos-cell)
  "Read a heap-v0-bin reference vector from SOURCE at POS-CELL."
  (let ((n (nelisp-heap-image--binary-read-u64 source pos-cell))
        (out nil))
    (while (> n 0)
      (setq out
            (cons (nelisp-heap-image--binary-read-ref source pos-cell)
                  out))
      (setq n (1- n)))
    (nreverse out)))

(defun nelisp-heap-image--binary-read-object (source pos-cell id)
  "Read one heap-v0-bin object with ID from SOURCE at POS-CELL."
  (let ((kind (nelisp-heap-image--binary-read-u8 source pos-cell)))
    (cond
     ((= kind nelisp-heap-image--binary-obj-cons)
      (list id 'cons
            (nelisp-heap-image--binary-read-ref source pos-cell)
            (nelisp-heap-image--binary-read-ref source pos-cell)))
     ((= kind nelisp-heap-image--binary-obj-vector)
      (list id 'vector
            (nelisp-heap-image--binary-read-ref-list source pos-cell)))
     ((= kind nelisp-heap-image--binary-obj-record)
      (list id 'record
            (nelisp-heap-image--binary-read-ref source pos-cell)
            (nelisp-heap-image--binary-read-ref-list source pos-cell)))
     ((or (= kind nelisp-heap-image--binary-obj-mut-str)
          (= kind nelisp-heap-image--binary-obj-cell)
          (= kind nelisp-heap-image--binary-obj-bool-vector)
          (= kind nelisp-heap-image--binary-obj-char-table))
      (signal 'error (list "heap-v0-bin object kind is not available in pure Elisp yet" kind)))
     (t (signal 'error (list "unknown heap-v0-bin object kind" kind))))))

(defun nelisp-heap-image--binary-source-to-form (source)
  "Decode heap-v0-bin SOURCE to the canonical S-expression image form."
  (when (multibyte-string-p source)
    (setq source (encode-coding-string source 'utf-8 t)))
  (let* ((pos-cell (cons (length nelisp-heap-image--binary-magic) nil))
         (version (nelisp-heap-image--binary-read-u32 source pos-cell))
         (object-count (nelisp-heap-image--binary-read-u64 source pos-cell))
         (root-count (nelisp-heap-image--binary-read-u64 source pos-cell))
         (objects nil)
         (roots nil))
    (unless (= version 0)
      (signal 'error (list "unsupported heap-v0-bin version" version)))
    (while (> object-count 0)
      (let ((id (nelisp-heap-image--binary-read-i64 source pos-cell)))
        (setq objects
              (cons (nelisp-heap-image--binary-read-object source pos-cell id)
                    objects)))
      (setq object-count (1- object-count)))
    (while (> root-count 0)
      (let ((name (nelisp-heap-image--binary-read-string source pos-cell))
            (value (nelisp-heap-image--binary-read-ref source pos-cell)))
        (setq roots (cons (cons name value) roots)))
      (setq root-count (1- root-count)))
    (unless (= (car pos-cell) (length source))
      (signal 'error (list "trailing bytes in heap-v0-bin image")))
    (list nelisp-heap-image--tag
          :version 0
          :roots (nreverse roots)
          :objects (nreverse objects))))

(defun nelisp-heap-image--decode-immediate (ref)
  "Decode immediate REF."
  (let ((kind (car (cdr ref))))
    (cond
     ((eq kind 'nil) nil)
     ((eq kind 't) t)
     ((eq kind 'int) (car (cdr (cdr ref))))
     ((eq kind 'float) (car (cdr (cdr ref))))
     ((eq kind 'str) (car (cdr (cdr ref))))
     ((eq kind 'sym) (intern (car (cdr (cdr ref)))))
     (t (signal 'error (list "unknown heap image immediate" kind))))))

(defun nelisp-heap-image--decode-ref (ref table)
  "Resolve REF through TABLE."
  (cond
   ((and (consp ref) (eq (car ref) 'imm))
    (nelisp-heap-image--decode-immediate ref))
   ((and (consp ref) (eq (car ref) 'ref))
    (let ((obj (gethash (car (cdr ref)) table nil)))
      (if obj obj
        (signal 'error (list "unknown heap image object id"
                             (car (cdr ref)))))))
   (t (signal 'error (list "malformed heap image ref" ref)))))

(defun nelisp-heap-image--decode-record-placeholder (type-ref slot-refs table)
  "Create a record placeholder for TYPE-REF and SLOT-REFS."
  (let ((type-tag (nelisp-heap-image--decode-ref type-ref table)))
    (unless (symbolp type-tag)
      (signal 'error (list "record type tag must be a symbol" type-tag)))
    (apply 'nelisp--make-record
           type-tag
           (nelisp-heap-image--nil-list (length slot-refs)))))

(defun nelisp-heap-image-decode-form (form)
  "Decode heap-v0 FORM and return a root alist."
  (unless (and (consp form) (eq (car form) nelisp-heap-image--tag))
    (signal 'error (list "not a heap-v0 image form")))
  (let* ((plist (cdr form))
         (version (nelisp-heap-image--plist-get plist :version))
         (roots (nelisp-heap-image--plist-get plist :roots))
         (objects (nelisp-heap-image--plist-get plist :objects))
         (table (make-hash-table :test 'eql))
         (cur objects))
    (unless (= version 0)
      (signal 'error (list "unsupported heap image version" version)))
    ;; Pass 1: allocate placeholders, so cycles and sharing can resolve.
    (while cur
      (let* ((desc (car cur))
             (id (nth 0 desc))
             (kind (nth 1 desc))
             placeholder)
        (setq placeholder
              (cond
               ((eq kind 'cons) (cons nil nil))
               ((eq kind 'vector) (make-vector (length (nth 2 desc)) nil))
               ((eq kind 'record)
                (nelisp-heap-image--decode-record-placeholder
                 (nth 2 desc) (nth 3 desc) table))
               (t (signal 'error (list "unknown heap image object" kind)))))
        (puthash id placeholder table))
      (setq cur (cdr cur)))
    ;; Pass 2: fill placeholders.
    (setq cur objects)
    (while cur
      (let* ((desc (car cur))
             (obj (gethash (nth 0 desc) table))
             (kind (nth 1 desc)))
        (cond
         ((eq kind 'cons)
          (setcar obj (nelisp-heap-image--decode-ref (nth 2 desc) table))
          (setcdr obj (nelisp-heap-image--decode-ref (nth 3 desc) table)))
         ((eq kind 'vector)
          (let ((refs (nth 2 desc))
                (i 0))
            (while refs
              (aset obj i (nelisp-heap-image--decode-ref (car refs) table))
              (setq refs (cdr refs))
              (setq i (1+ i)))))
         ((eq kind 'record)
          (let ((refs (nth 3 desc))
                (i 0))
            (while refs
              (nelisp--record-set
               obj i (nelisp-heap-image--decode-ref (car refs) table))
              (setq refs (cdr refs))
              (setq i (1+ i)))))))
      (setq cur (cdr cur)))
    (let ((decoded-roots nil)
          (r roots))
      (while r
        (setq decoded-roots
              (cons (cons (car (car r))
                          (nelisp-heap-image--decode-ref (cdr (car r)) table))
                    decoded-roots))
        (setq r (cdr r)))
      (nreverse decoded-roots))))

(defun nelisp-heap-image-read-string (source)
  "Decode a heap-v0 image string SOURCE and return root alist."
  (nelisp-heap-image-decode-form
   (cond
    ((and (>= (length source) (length nelisp-heap-image--binary-magic))
          (string= (substring source 0 (length nelisp-heap-image--binary-magic))
                   nelisp-heap-image--binary-magic))
     (nelisp-heap-image--binary-source-to-form source))
    ((or (and (>= (length source) (length nelisp-heap-image--line-magic))
              (string= (substring source 0 (length nelisp-heap-image--line-magic))
                       nelisp-heap-image--line-magic))
         (and (>= (length source)
                  (length nelisp-heap-image--counted-line-magic))
              (string= (substring source 0
                                  (length nelisp-heap-image--counted-line-magic))
                       nelisp-heap-image--counted-line-magic)))
     (nelisp-heap-image--line-source-to-form source))
    (t
     (read (nelisp-heap-image--strip-magic source))))))

(defun nelisp-heap-image-write-roots (path roots)
  "Write ROOTS to PATH as a heap-v0 image."
  (unless (eq (nl-write-file path (nelisp-heap-image-dump-string roots)) t)
    (signal 'file-error (list "cannot write heap image" path)))
  t)

(defun nelisp-heap-image-read-roots (path)
  "Read heap-v0 roots from PATH."
  (let ((source (nelisp--syscall-read-file path)))
    (unless (stringp source)
      (signal 'file-error (list "cannot read heap image" path)))
    (nelisp-heap-image-read-string source)))

(provide 'nelisp-heap-image)

;;; nelisp-heap-image.el ends here
