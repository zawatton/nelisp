;;; nelisp-com-wstr-lib.el --- heap UTF-16 buffer ("wbuf") library for AOT code  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stage S3a-1 of the native TSF/COM DLL plan.  `nelisp-com-wstr-lib-forms'
;; returns restricted-subset AOT defuns (wbuf_*) operating on growable
;; UTF-16 buffers allocated from the process heap:
;;
;;   struct wbuf { ptr ptr; u64 len; u64 cap; }     (24 bytes, zeroed)
;;
;; `len' and `cap' count UTF-16 code units.  The data block always holds
;; cap+1 units and a NUL follows the last unit, so `ptr' is also a valid
;; C wide string.  Every mutator validates the header first (non-null wbuf,
;; ptr, len <= cap <= WBUF_MAX) and returns an HRESULT: 0, E_INVALIDARG,
;; E_POINTER or E_OUTOFMEMORY.  Readers return 0 / 65536 sentinels instead
;; of touching memory out of bounds.  Growth uses HeapReAlloc.
;;
;; Requires imports `nelisp-com-wstr-lib-imports' (kernel32 heap calls).
;; Literals come from `nelisp-com-wstr-blob' / `nelisp-com-ascii-blob'.

;;; Code:

(require 'cl-lib)
(require 'nelisp-com-dsl)

(defconst nelisp-com-wstr-lib-imports
  '(("kernel32.dll" "GetProcessHeap" "HeapAlloc" "HeapFree" "HeapReAlloc"))
  "Imports needed by `nelisp-com-wstr-lib-forms'.")

(defconst nelisp-com-wbuf-max 268435456
  "Largest capacity in UTF-16 units a wbuf may grow to.")

(nelisp-com-defstruct wbuf (ptr ptr) (len u64) (cap u64))

(defun nelisp-com-ascii-blob (name string)
  "Return a rodata `data-blob' NAME holding the ASCII STRING and a NUL."
  (unless (string-match-p "\\`[\0-\177]*\\'" string)
    (error "Not ASCII: %S" string))
  `(data-blob ,name ,(append (append string nil) '(0)) rodata))

(defun nelisp-com-wstr-lib-forms ()
  "Return the AOT forms of the wbuf library."
  (let* ((ptr (lambda (w) (nelisp-com-field w 'wbuf 'ptr)))
         (len (lambda (w) (nelisp-com-field w 'wbuf 'len)))
         (cap (lambda (w) (nelisp-com-field w 'wbuf 'cap)))
         (max nelisp-com-wbuf-max)
         (inv nelisp-com-e-invalidarg)
         (nomem nelisp-com-e-outofmemory)
         (ptrerr nelisp-com-e-pointer))
    `(;; 1 when the header is sane.
      (defun wbuf_valid (w)
        (if (= w 0) 0
          (if (= ,(funcall ptr 'w) 0) 0
            (if (> ,(funcall len 'w) ,(funcall cap 'w)) 0
              (if (> ,(funcall cap 'w) ,max) 0 1)))))
      ;; New empty buffer with room for CAP units; 0 on failure.
      (defun wbuf_new (cap)
        (let ((h (extern-call GetProcessHeap)))
          (if (> cap ,max) 0
            (let ((c (if (< cap 16) 16 cap))
                  (w (extern-call HeapAlloc h 8 24)))
              (if (= w 0) 0
                (let ((p (extern-call HeapAlloc h 8 (* 2 (+ c 1)))))
                  (if (= p 0) (seq (extern-call HeapFree h 0 w) 0)
                    (seq ,(nelisp-com-set-field 'w 'wbuf 'ptr 'p)
                         ,(nelisp-com-set-field 'w 'wbuf 'len 0)
                         ,(nelisp-com-set-field 'w 'wbuf 'cap 'c)
                         w))))))))
      (defun wbuf_free (w)
        (if (= w 0) 0
          (let ((h (extern-call GetProcessHeap)))
            (seq (if (= ,(funcall ptr 'w) 0) 0
                   (extern-call HeapFree h 0 ,(funcall ptr 'w)))
                 (extern-call HeapFree h 0 w)
                 0))))
      (defun wbuf_len (w) (if (= (wbuf_valid w) 0) 0 ,(funcall len 'w)))
      (defun wbuf_ptr (w) (if (= (wbuf_valid w) 0) 0 ,(funcall ptr 'w)))
      (defun wbuf_clear (w)
        (if (= (wbuf_valid w) 0) ,inv
          (seq ,(nelisp-com-set-field 'w 'wbuf 'len 0)
               (ptr-write-u16 ,(funcall ptr 'w) 0 0)
               0)))
      ;; Ensure room for EXTRA more units.
      (defun wbuf_reserve (w extra)
        (if (= (wbuf_valid w) 0) ,inv
          (let ((need (+ ,(funcall len 'w) extra)))
            (if (> need ,max) ,nomem
              (if (<= need ,(funcall cap 'w)) 0
                (let ((nc ,(funcall cap 'w)))
                  (seq (while (< nc need) (setq nc (* nc 2)))
                       (if (> nc ,max) (setq nc ,max) 0)
                       (let* ((h (extern-call GetProcessHeap))
                              (np (extern-call HeapReAlloc h 8 ,(funcall ptr 'w)
                                               (* 2 (+ nc 1)))))
                         (if (= np 0) ,nomem
                           (seq ,(nelisp-com-set-field 'w 'wbuf 'ptr 'np)
                                ,(nelisp-com-set-field 'w 'wbuf 'cap 'nc)
                                0))))))))))
      ;; Append one UTF-16 unit.
      (defun wbuf_append_char (w c)
        (if (> c 65535) ,inv
          ,(nelisp-com-try
            '((hr (wbuf_reserve w 1)))
            `(let ((n ,(funcall len 'w)))
               (seq (ptr-write-u16 (+ ,(funcall ptr 'w) (* 2 n)) 0 c)
                    (ptr-write-u16 (+ ,(funcall ptr 'w) (* 2 (+ n 1))) 0 0)
                    ,(nelisp-com-set-field 'w 'wbuf 'len '(+ n 1))
                    0)))))
      ;; Append N units from the UTF-16 array at P.
      (defun wbuf_append_wn (w p n)
        (if (= p 0) ,ptrerr
          ,(nelisp-com-try
            '((hr (wbuf_reserve w n)))
            `(let ((base ,(funcall len 'w)) (i 0))
               (seq (while (< i n)
                      (seq (ptr-write-u16 (+ ,(funcall ptr 'w)
                                             (* 2 (+ base i)))
                                          0
                                          (ptr-read-u16 (+ p (* 2 i)) 0))
                           (setq i (+ i 1))))
                    (ptr-write-u16 (+ ,(funcall ptr 'w) (* 2 (+ base n))) 0 0)
                    ,(nelisp-com-set-field 'w 'wbuf 'len '(+ base n))
                    0)))))
      (defun wbuf_wcslen (p)
        (let ((n 0))
          (seq (while (/= (ptr-read-u16 (+ p (* 2 n)) 0) 0) (setq n (+ n 1)))
               n)))
      ;; Append the NUL-terminated wide string P.
      (defun wbuf_append_w (w p)
        (if (= p 0) ,ptrerr (wbuf_append_wn w p (wbuf_wcslen p))))
      ;; Append the NUL-terminated 7-bit ASCII byte string P.  The string
      ;; is validated first, so a rejected string leaves W unchanged.
      (defun wbuf_append_ascii (w p)
        (if (= p 0) ,ptrerr
          (let ((i 0) (n 0))
            (seq (while (/= (ptr-read-u8 (+ p n) 0) 0) (setq n (+ n 1)))
                 (while (and (< i n) (< (ptr-read-u8 (+ p i) 0) 128))
                   (setq i (+ i 1)))
                 (if (< i n) ,inv
                   ,(nelisp-com-try
                     '((hr (wbuf_reserve w n)))
                     '(let ((j 0))
                        (seq (while (< j n)
                               (seq (wbuf_append_char w (ptr-read-u8 (+ p j) 0))
                                    (setq j (+ j 1))))
                             0))))))))
      ;; Append one Unicode scalar value (UTF-32 -> UTF-16).
      (defun wbuf_append_cp (w cp)
        (if (> cp 1114111) ,inv
          (if (and (>= cp 55296) (<= cp 57343)) ,inv
            (if (< cp 65536) (wbuf_append_char w cp)
              (let ((v (- cp 65536)))
                ,(nelisp-com-try
                  '((hr (wbuf_reserve w 2)))
                  '(seq (wbuf_append_char w (+ 55296 (/ v 1024)))
                        (wbuf_append_char w (+ 56320 (mod v 1024)))
                        0)))))))
      ;; Append N in decimal (N >= 0).
      (defun wbuf_append_u64 (w n)
        (if (< n 0) ,inv
          (let ((digits 1) (t0 n))
            (seq (while (>= t0 10) (seq (setq t0 (/ t0 10))
                                        (setq digits (+ digits 1))))
                 ,(nelisp-com-try
                   '((hr (wbuf_reserve w digits)))
                   `(let ((base ,(funcall len 'w)) (i (- digits 1)) (v n))
                      (seq (while (>= i 0)
                             (seq (ptr-write-u16
                                   (+ ,(funcall ptr 'w) (* 2 (+ base i))) 0
                                   (+ 48 (mod v 10)))
                                  (setq v (/ v 10))
                                  (setq i (- i 1))))
                           (ptr-write-u16
                            (+ ,(funcall ptr 'w) (* 2 (+ base digits))) 0 0)
                           ,(nelisp-com-set-field 'w 'wbuf 'len
                                                  '(+ base digits))
                           0)))))))
      ;; New buffer holding a copy of the wide string P; 0 on failure.
      (defun wbuf_from_w (p)
        (if (= p 0) 0
          (let ((w (wbuf_new (wbuf_wcslen p))))
            (if (= w 0) 0
              (if (= (wbuf_append_w w p) 0) w
                (seq (wbuf_free w) 0))))))
      ;; Unit at index I, or 65536 when out of range / invalid.
      (defun wbuf_get (w i)
        (if (= (wbuf_valid w) 0) 65536
          (if (or (< i 0) (>= i ,(funcall len 'w))) 65536
            (ptr-read-u16 (+ ,(funcall ptr 'w) (* 2 i)) 0))))
      ;; -1 / 0 / 1 like wcscmp.
      (defun wbuf_wcscmp (a b)
        (let ((i 0) (r 0))
          (seq (while (= r 0)
                 (let ((x (ptr-read-u16 (+ a (* 2 i)) 0))
                       (y (ptr-read-u16 (+ b (* 2 i)) 0)))
                   (if (< x y) (setq r (- 0 1))
                     (if (> x y) (setq r 1)
                       (if (= x 0) (setq r 2)
                         (setq i (+ i 1)))))))
               (if (= r 2) 0 r))))
      ;; 1 when the buffer equals the wide string P, else 0.
      (defun wbuf_eq_w (w p)
        (if (= (wbuf_valid w) 0) 0
          (if (= p 0) 0
            (if (= (wbuf_wcscmp ,(funcall ptr 'w) p) 0) 1 0))))
      ;; 1 when both buffers hold the same units, else 0.
      (defun wbuf_eq (a b)
        (if (= (wbuf_valid a) 0) 0
          (if (= (wbuf_valid b) 0) 0
            (if (/= ,(funcall len 'a) ,(funcall len 'b)) 0
              (let ((i 0) (same 1))
                (seq (while (and (= same 1) (< i ,(funcall len 'a)))
                       (seq (if (/= (ptr-read-u16 (+ ,(funcall ptr 'a) (* 2 i)) 0)
                                    (ptr-read-u16 (+ ,(funcall ptr 'b) (* 2 i)) 0))
                                (setq same 0) 0)
                            (setq i (+ i 1))))
                     same)))))))))

(provide 'nelisp-com-wstr-lib)
;;; nelisp-com-wstr-lib.el ends here
