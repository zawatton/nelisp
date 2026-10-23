;;; nelisp-aot-symbol-name-word-test.el --- the name operations over String WORD names -*- lexical-binding: t; -*-

;; Decision item A (tools/ai/symbols-name-word-progress.org, S2.2 and S2.3): a symbol object's name cell holds
;; the String WORD (GNU's u.s.name), and the grammar emitters that read a name -- `symbol-name-eq' (the one the
;; runtime build compiles; `nelisp-heap-accessors-lower' rewrites the others first), `sexp-name-eq', `str-len',
;; `str-bytes' and `str-byte-at' -- read it.
;;
;; Each test compiles a freestanding program with the AOT compiler's back end for the architecture and runs it:
;; x86-64 natively, aarch64 under qemu-aarch64 (user mode).  The program mmaps a page, builds the objects of one
;; case at a time (a String header at +1024 with its data at +1088, a 48-byte symbol cell at +256, legacy views
;; at +512), runs the operation and compares its answer; `_start' exits with 0, or the number of the first case
;; that answered something else.
;;
;; The String layout is docs/design/227's: header at WORD - 4, `size' at +0, `size_byte' at +8 (-1 for a
;; unibyte string, whose byte length is then `size' without its top bit: the collector's mark), the data pointer
;; at +24.  A symbol object WORD is its 48-byte cell + 6, the kind (4 interned, 16 uninterned) in the low byte
;; of the flags at +0 and the name cell at +8.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)
(require 'nelisp-static-linker)

(defconst nelisp-snw-test--qemu
  (or (executable-find "qemu-aarch64")
      (let ((local (expand-file-name "~/bin/qemu-aarch64")))
        (and (file-executable-p local) local))))

(defconst nelisp-snw-test--mark (- (ash 1 63) (ash 1 64)) "Bit 63 as a signed 64-bit integer.")

;; ----------------------------------------------------------------------------- building the objects
(defun nelisp-snw-test--bytes (hdr text)
  "Forms writing the bytes of the unibyte string TEXT at HDR + 64 (and a NUL after them)."
  (append (cl-loop for b across text for i from 0 collect `(ptr-write-u8 (+ ,hdr 64) ,i ,b))
          (list `(ptr-write-u8 (+ ,hdr 64) ,(length text) 0))))

(defun nelisp-snw-test--string (hdr text &optional unibyte marked)
  "Forms building a String at HDR holding TEXT, a string of bytes (UTF-8 when it is multibyte text).
UNIBYTE gives size_byte -1 and a byte length in `size'; MARKED sets the collector's mark bit in `size'."
  (let* ((bytes (if (multibyte-string-p text) (encode-coding-string text 'utf-8 t) text))
         (n (length bytes))
         (chars (if unibyte n (length (decode-coding-string bytes 'utf-8)))))
    (append `((ptr-write-u64 ,hdr 0 ,(if marked (+ chars nelisp-snw-test--mark) chars))
              (ptr-write-u64 ,hdr 8 ,(if unibyte -1 n))
              (ptr-write-u64 ,hdr 16 0)
              (ptr-write-u64 ,hdr 24 (+ ,hdr 64)))
            (nelisp-snw-test--bytes hdr bytes))))

(defun nelisp-snw-test--cell (cell kind name)
  "Forms building the 48-byte symbol cell at CELL: flags KIND, name cell NAME, void cells."
  `((ptr-write-u64 ,cell 0 ,kind) (ptr-write-u64 ,cell 8 ,name)
    (ptr-write-u64 ,cell 16 3) (ptr-write-u64 ,cell 24 3) (ptr-write-u64 ,cell 32 3)
    (ptr-write-u64 ,cell 40 0)))

(defun nelisp-snw-test--view (view tag data len)
  "Forms building the legacy 32-byte string view at VIEW: tag byte TAG, DATA at +16, LEN at +24."
  `((ptr-write-u64 ,view 0 ,tag) (ptr-write-u64 ,view 8 0)
    (ptr-write-u64 ,view 16 ,data) (ptr-write-u64 ,view 24 ,len)))

;; The addresses of one case, relative to the mmap base M.
(defconst nelisp-snw-test--str '(+ m 1024))
(defconst nelisp-snw-test--cell-addr '(+ m 256))
(defconst nelisp-snw-test--view-addr '(+ m 512))
(defconst nelisp-snw-test--word '(+ (+ m 1024) 4) "The WORD of the String at +1024.")
(defconst nelisp-snw-test--obj '(+ (+ m 256) 6) "The WORD of the symbol cell at +256.")

(defun nelisp-snw-test--obj-case (kind text op &optional unibyte marked)
  "Forms: a symbol cell of KIND whose name is the String TEXT, then the form OP (OBJ stands for its WORD)."
  (append (nelisp-snw-test--string nelisp-snw-test--str text unibyte marked)
          (nelisp-snw-test--cell nelisp-snw-test--cell-addr kind nelisp-snw-test--word)
          (list op)))

;; ----------------------------------------------------------------------------- the cases
;; Each case: (NAME EXPECTED FORMS...); the program runs the forms and compares the last one's value with EXPECTED.
(defun nelisp-snw-test--case-name-eq ()
  (let ((obj nelisp-snw-test--obj) (word nelisp-snw-test--word))
    (cl-flet ((o (kind text lit expect &optional unibyte marked)
                (list (format "symbol-name-eq kind %d %S against %S" kind text lit) expect
                      (nelisp-snw-test--obj-case kind text `(symbol-name-eq ,obj ,lit) unibyte marked))))
      (list
       (o 4 "abc" "abc" 1)
       (o 4 "abc" "abd" 0)
       (o 4 "abc" "ab" 0)
       (o 4 "abc" "abcd" 0)
       (o 16 "xyz" "xyz" 1 t)
       (o 16 "xyz" "xyz" 1 t t)
       (o 16 "xyz" "xyw" 0 t t)
       (o 4 "" "" 1)
       (o 4 "" "a" 0)
       (o 4 "abc" "" 0)
       (o 4 (make-string 300 ?a) (make-string 300 ?a) 1)
       (o 4 (make-string 300 ?a) (make-string 299 ?a) 0)
       (o 4 "é" "é" 1)
       (o 4 "é" "e" 0)
       (o 4 "\300\301" "\300\301" 1 t)))))

(defun nelisp-snw-test--case-name-eq-others ()
  (let ((obj nelisp-snw-test--obj) (word nelisp-snw-test--word) (view nelisp-snw-test--view-addr)
        (str nelisp-snw-test--str) (cell nelisp-snw-test--cell-addr))
    (list
     ;; the name cell is nil before it is filled
     (list "an object not yet named is not equal to anything" 0
           (append (nelisp-snw-test--cell cell 4 3) (list `(symbol-name-eq ,obj "abc"))))
     (list "an object not yet named is not equal to the empty name" 0
           (append (nelisp-snw-test--cell cell 4 3) (list `(symbol-name-eq ,obj ""))))
     ;; a vectorlike that is not a symbol is left alone, whatever its second word holds
     (list "a vectorlike of kind 8 whose second word is a String WORD" 0
           (append (nelisp-snw-test--string str "abc")
                   (nelisp-snw-test--cell cell 8 word)
                   (list `(symbol-name-eq ,obj "abc"))))
     ;; design A reads String WORDs only: a legacy view in the name cell is not a name
     (list "an object whose name cell holds a legacy string view is not read" 0
           (append (nelisp-snw-test--bytes str "abc")
                   (nelisp-snw-test--view view 5 `(+ ,str 64) 3)
                   (nelisp-snw-test--cell cell 4 view)
                   (list `(symbol-name-eq ,obj "abc"))))
     ;; legacy views
     (list "a legacy symbol view" 1
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 4 `(+ ,str 64) 3)
                   (list `(symbol-name-eq ,view "abc"))))
     (list "a legacy symbol view of another name" 0
           (append (nelisp-snw-test--bytes str "abd") (nelisp-snw-test--view view 4 `(+ ,str 64) 3)
                   (list `(symbol-name-eq ,view "abc"))))
     (list "a legacy string view is not a symbol" 0
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 5 `(+ ,str 64) 3)
                   (list `(symbol-name-eq ,view "abc"))))
     ;; not symbols at all
     (list "a bare String WORD is not a symbol" 0
           (append (nelisp-snw-test--string str "abc") (list `(symbol-name-eq ,word "abc"))))
     (list "a fixnum" 0 (list `(symbol-name-eq 21 "abc")))
     (list "nil" 0 (list `(symbol-name-eq 3 "nil")))
     (list "a direct cons WORD" 0
           (append (nelisp-snw-test--string str "abc") (list `(symbol-name-eq (+ ,str 2) "abc")))))))

(defun nelisp-snw-test--case-sexp-name-eq ()
  (let ((obj nelisp-snw-test--obj) (word nelisp-snw-test--word) (view nelisp-snw-test--view-addr)
        (str nelisp-snw-test--str) (cell nelisp-snw-test--cell-addr))
    (list
     (list "sexp-name-eq of an object, its name" 1 (nelisp-snw-test--obj-case 4 "abc" `(sexp-name-eq ,obj "abc")))
     (list "sexp-name-eq of an object, another name" 0 (nelisp-snw-test--obj-case 4 "abc" `(sexp-name-eq ,obj "abd")))
     (list "sexp-name-eq of an uninterned unibyte object" 1 (nelisp-snw-test--obj-case 16 "xyz" `(sexp-name-eq ,obj "xyz") t t))
     (list "sexp-name-eq of a bare String WORD" 1 (nelisp-snw-test--obj-case 4 "abc" `(sexp-name-eq ,word "abc")))
     (list "sexp-name-eq of a bare unibyte String WORD" 1
           (append (nelisp-snw-test--string str "xyz" t t) (list `(sexp-name-eq ,word "xyz"))))
     (list "sexp-name-eq of a bare String WORD, another length" 0
           (append (nelisp-snw-test--string str "xyz" t) (list `(sexp-name-eq ,word "xy"))))
     (list "sexp-name-eq of a legacy symbol view" 1
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 4 `(+ ,str 64) 3)
                   (list `(sexp-name-eq ,view "abc"))))
     (list "sexp-name-eq of a legacy string view" 1
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 5 `(+ ,str 64) 3)
                   (list `(sexp-name-eq ,view "abc"))))
     (list "sexp-name-eq of a legacy unibyte view" 1
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 14 `(+ ,str 64) 3)
                   (list `(sexp-name-eq ,view "abc"))))
     (list "sexp-name-eq of a legacy view of another tag" 0
           (append (nelisp-snw-test--bytes str "abc") (nelisp-snw-test--view view 7 `(+ ,str 64) 3)
                   (list `(sexp-name-eq ,view "abc"))))
     (list "sexp-name-eq of an object not yet named" 0
           (append (nelisp-snw-test--cell cell 4 3) (list `(sexp-name-eq ,obj "abc"))))
     (list "sexp-name-eq of a vectorlike of kind 8" 0
           (append (nelisp-snw-test--string str "abc") (nelisp-snw-test--cell cell 8 word)
                   (list `(sexp-name-eq ,obj "abc"))))
     (list "sexp-name-eq of a fixnum" 0 (list `(sexp-name-eq 21 "abc"))))))

(defun nelisp-snw-test--case-str-len ()
  (let ((obj nelisp-snw-test--obj) (word nelisp-snw-test--word) (view nelisp-snw-test--view-addr)
        (str nelisp-snw-test--str))
    (list
     (list "str-len of an object, multibyte" 3 (nelisp-snw-test--obj-case 4 "abc" `(str-len ,obj)))
     (list "str-len of an object, bytes of a multibyte name" 2 (nelisp-snw-test--obj-case 4 "é" `(str-len ,obj)))
     (list "str-len of an object, unibyte" 3 (nelisp-snw-test--obj-case 16 "xyz" `(str-len ,obj) t))
     (list "str-len of an object, unibyte with the mark bit" 3 (nelisp-snw-test--obj-case 16 "xyz" `(str-len ,obj) t t))
     (list "str-len of an object, empty" 0 (nelisp-snw-test--obj-case 4 "" `(str-len ,obj)))
     (list "str-len of an object, 300 bytes" 300 (nelisp-snw-test--obj-case 4 (make-string 300 ?q) `(str-len ,obj)))
     (list "str-len of a bare String WORD" 5 (append (nelisp-snw-test--string str "hello") (list `(str-len ,word))))
     (list "str-len of a bare unibyte String WORD" 4 (append (nelisp-snw-test--string str "four" t t) (list `(str-len ,word))))
     (list "str-len of a legacy view" 7
           (append (nelisp-snw-test--bytes str "sevenby") (nelisp-snw-test--view view 5 `(+ ,str 64) 7)
                   (list `(str-len ,view)))))))

(defun nelisp-snw-test--case-str-bytes ()
  (let ((obj nelisp-snw-test--obj) (word nelisp-snw-test--word) (view nelisp-snw-test--view-addr)
        (str nelisp-snw-test--str) (m 'm))
    (ignore m)
    (list
     (list "str-bytes of an object is the String's data" 1
           (nelisp-snw-test--obj-case 4 "abc" `(if (= (str-bytes ,obj) (+ ,str 64)) 1 0)))
     (list "str-bytes of an unibyte object is the String's data" 1
           (nelisp-snw-test--obj-case 16 "xyz" `(if (= (str-bytes ,obj) (+ ,str 64)) 1 0) t t))
     (list "str-bytes of a bare String WORD" 1
           (append (nelisp-snw-test--string str "abc") (list `(if (= (str-bytes ,word) (+ ,str 64)) 1 0))))
     (list "str-bytes of a legacy view is its +16" 1
           (append (nelisp-snw-test--view view 5 `(+ ,str 64) 3) (list `(if (= (str-bytes ,view) (+ ,str 64)) 1 0))))
     (list "str-byte-at of an object" 98 (nelisp-snw-test--obj-case 4 "abc" `(str-byte-at ,obj 1)))
     (list "str-byte-at of an object, last byte" 99 (nelisp-snw-test--obj-case 4 "abc" `(str-byte-at ,obj 2)))
     (list "str-byte-at of an unibyte object" 193 (nelisp-snw-test--obj-case 16 "\300\301\302" `(str-byte-at ,obj 1) t t))
     (list "str-byte-at of a bare String WORD" 108 (append (nelisp-snw-test--string str "hello") (list `(str-byte-at ,word 3))))
     (list "str-byte-at of a legacy view" 100
           (append (nelisp-snw-test--bytes str "abcd") (nelisp-snw-test--view view 14 `(+ ,str 64) 4)
                   (list `(str-byte-at ,view 3)))))))

;; ----------------------------------------------------------------------------- compiling and running
(defun nelisp-snw-test--program (arch cases)
  "The freestanding program of CASES for ARCH (x86_64 or aarch64)."
  (let* ((mmap (if (eq arch 'aarch64) 222 9))
         (exit (if (eq arch 'aarch64) 93 60))
         (defs nil) (checks nil) (i 0))
    (dolist (c cases)
      (setq i (1+ i))
      (let ((fn (intern (format "snw_case_%d" i))) (forms (nth 2 c)))
        (push `(defun ,fn (m) (seq ,@forms)) defs)
        (push `(if (= (,fn mem) ,(nth 1 c)) 0 (setq fail (if (= fail 0) ,i fail))) checks)))
    `(seq ,@(nreverse defs)
          (defun _start ()
            (let* ((mem (syscall-direct ,mmap 0 1048576 3 34 -1 0)) (fail 0))
              (seq ,@(nreverse checks)
                   (syscall-direct ,exit fail 0 0 0 0 0)))))))

(defvar nelisp-snw-test--reached nil "Calls of the emitters under test during the compile in progress, by function.")
(defvar nelisp-snw-test--last-reached nil "The counts of the last completed compile.")

(defun nelisp-snw-test--run (arch cases &optional lowered)
  "Compile CASES for ARCH, run the program, and answer its exit status (0: every case passed).
Unless LOWERED the heap-accessor lowering is off, so `sexp-name-eq' and the `str-' operations reach the emitters."
  (let* ((dir (make-temp-file "snw-name-word-" t))
         (file (expand-file-name (format "prog-%s" arch) dir))
         (nelisp-aot-compiler--object-hidden-boundary-symbols nil)
         (nelisp-standalone--target (if (eq arch 'aarch64) 'linux-aarch64 'linux-x86_64))
         (emitters (if (eq arch 'aarch64)
                       '(nelisp-aot-compiler--emit-symbol-name-eq-arm64 nelisp-aot-compiler--emit-sexp-name-eq-arm64
                         nelisp-aot-compiler--emit-str-len-arm64 nelisp-aot-compiler--emit-str-bytes-arm64
                         nelisp-aot-compiler--emit-str-byte-at-arm64)
                     '(nelisp-aot-compiler--emit-symbol-name-eq nelisp-aot-compiler--emit-sexp-name-eq
                       nelisp-aot-compiler--emit-str-len nelisp-aot-compiler--emit-str-bytes
                       nelisp-aot-compiler--emit-str-byte-at))))
    (setq nelisp-snw-test--reached nil)
    (unwind-protect
        (progn
          (dolist (e emitters) (advice-add e :around (apply-partially #'nelisp-snw-test--count-around e) '((name . nelisp-snw-test))))
          (let ((unit (if lowered
                          (nelisp-standalone--compile-to-unit "snw" (nelisp-snw-test--program arch cases))
                        (cl-letf (((symbol-function 'nelisp-heap-accessors-lower) #'identity))
                          (nelisp-standalone--compile-to-unit "snw" (nelisp-snw-test--program arch cases))))))
            (nelisp-link-units file (list unit) "_start" nil arch))
          (setq nelisp-snw-test--last-reached (copy-alist nelisp-snw-test--reached))
          (set-file-modes file #o755)
          (let ((status (if (eq arch 'aarch64)
                            (call-process nelisp-snw-test--qemu nil nil nil file)
                          (call-process file nil nil nil))))
            status))
      (dolist (e emitters) (advice-remove e 'nelisp-snw-test))
      (delete-directory dir t))))

(defun nelisp-snw-test--count-around (fn orig &rest args)
  (cl-incf (alist-get fn nelisp-snw-test--reached 0))
  (apply orig args))

(defun nelisp-snw-test--expect (arch cases &optional lowered)
  "Run CASES for ARCH and fail naming the first case that answered wrongly."
  (let ((status (nelisp-snw-test--run arch cases lowered)))
    (unless (eql status 0)
      (ert-fail (list (format "%s: case %S answered something else" arch status)
                      (and (integerp status) (<= 1 status (length cases)) (car (nth (1- status) cases))))))
    t))

;; ----------------------------------------------------------------------------- tests
(defmacro nelisp-snw-test--define (arch skip-form)
  (let ((p (lambda (suffix) (intern (format "nelisp-snw-%s/%s" arch suffix)))))
    `(progn
       (ert-deftest ,(funcall p "symbol-name-eq-string-word-names") ()
         ,skip-form
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-name-eq)))
       (ert-deftest ,(funcall p "symbol-name-eq-views-objects-and-immediates") ()
         ,skip-form
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-name-eq-others)))
       (ert-deftest ,(funcall p "sexp-name-eq-with-the-lowering-off") ()
         ,skip-form
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-sexp-name-eq)))
       (ert-deftest ,(funcall p "str-len-with-the-lowering-off") ()
         ,skip-form
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-str-len)))
       (ert-deftest ,(funcall p "str-bytes-and-str-byte-at-with-the-lowering-off") ()
         ,skip-form
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-str-bytes)))
       (ert-deftest ,(funcall p "lowered-build-still-reaches-the-symbol-name-eq-emitter") ()
         ,skip-form
         ;; The runtime build lowers str-len, str-eq and sexp-name-eq itself; symbol-name-eq is the one name
         ;; operation left to the machine emitter.  A build with the lowering on must still compile it there
         ;; (and must not call the others), and the program must still pass.
         (nelisp-snw-test--expect ',arch (nelisp-snw-test--case-name-eq) t)
         (should (> (or (alist-get ',(if (eq arch 'aarch64)
                                        'nelisp-aot-compiler--emit-symbol-name-eq-arm64
                                      'nelisp-aot-compiler--emit-symbol-name-eq)
                                    nelisp-snw-test--last-reached)
                        0) 0)))
       (ert-deftest ,(funcall p "the-harness-detects-a-wrong-answer") ()
         ,skip-form
         ;; Negative control: a program whose second case expects the wrong answer must exit with that case's number.
         (let ((cases (list (car (nelisp-snw-test--case-name-eq))
                            (list "wrong on purpose" 0
                                  (nelisp-snw-test--obj-case 4 "abc" `(symbol-name-eq ,nelisp-snw-test--obj "abc"))))))
           (should (equal 2 (nelisp-snw-test--run ',arch cases)))))
       (ert-deftest ,(funcall p "an-unmarked-and-a-marked-unibyte-name-agree") ()
         ,skip-form
         (nelisp-snw-test--expect
          ',arch
          (list (list "marked" 1 (nelisp-snw-test--obj-case 16 "mark" `(symbol-name-eq ,nelisp-snw-test--obj "mark") t t))
                (list "unmarked" 1 (nelisp-snw-test--obj-case 16 "mark" `(symbol-name-eq ,nelisp-snw-test--obj "mark") t))
                (list "marked length" 4 (nelisp-snw-test--obj-case 16 "mark" `(str-len ,nelisp-snw-test--obj) t t))))))))

(nelisp-snw-test--define x86_64 (skip-unless (eq system-type 'gnu/linux)))
(nelisp-snw-test--define aarch64 (skip-unless nelisp-snw-test--qemu))

(provide 'nelisp-aot-symbol-name-word-test)
;;; nelisp-aot-symbol-name-word-test.el ends here
