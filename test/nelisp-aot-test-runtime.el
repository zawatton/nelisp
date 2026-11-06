;;; nelisp-aot-test-runtime.el --- C stand-ins for AOT native test harnesses  -*- lexical-binding: t; -*-

;;; Commentary:

;; Since c552d6bbb `sexp-tag' lowers to an extern call of the runtime
;; helper `nl_val_tag'.  Self-contained native test units that link with
;; gcc must therefore define it.  The C text below mirrors the
;; `nl_val_tag' defun in scripts/nelisp-standalone-build.el (Doc 216 WORD
;; tag dispatch) and uses only builtin types so it also links under
;; -nostdlib.

;;; Code:

(require 'cl-lib)

(defconst nelisp-aot-test-runtime-nl-val-tag-c
  (concat
   "unsigned long long nl_val_tag(unsigned long long v){"
   "if((v&7)==0)return *(unsigned char*)v;"
   "if((v&1)==0){"
   "if((v&7)==4)return *(long long*)(v-4+8)<0?14:5;"
   "if((v&7)==6)return *(unsigned char*)(v-6)&63;"
   "if((v&7)==2)return 7;"
   "return *(unsigned char*)v;}"
   "return (v&3)==1?2:(v==3?0:1);}\n")
  "C source of an `nl_val_tag' stand-in for native test harnesses.")

(defconst nelisp-aot-test-runtime-nl-val-tag-masked-c
  (concat
   "unsigned long long nl_val_tag(unsigned long long v){"
   "if((v&7)==2)return 7;"
   "if((v&7)==6)return *(unsigned char*)(v-6)&63;"
   "if((v&1)==0)return *(unsigned char*)(v&~7ULL)&63;"
   "return (v&3)==1?2:(v==3?0:1);}\n")
  "Like `nelisp-aot-test-runtime-nl-val-tag-c' but masks every header read.
Mirrors the pre-c552d6bbb inline `sexp-tag' read, which the literal-word
fold tests exercise with header bytes whose top two bits are set.")

(defconst nelisp-aot-test-runtime-nl-val-tag-defun
  '(defun nl_val_tag (v)
     (if (= (logand v 7) 0) (heap-read-u8 v 0)
       (if (= (logand v 1) 0)
           (if (= (logand v 7) 4)
               (if (< (heap-read-u64 (- v 4) 8) 0) 14 5)
             (if (= (logand v 7) 6) (logand (heap-read-u8 (- v 6) 0) 63)
               (if (= (logand v 7) 2) 7
                 (heap-read-u8 v 0))))
         (if (= (logand v 3) 1) 2 (if (= v 3) 0 1)))))
  "Lisp source of an `nl_val_tag' stand-in for AOT test sources.")

(defconst nelisp-aot-test-runtime-link-defuns
  (list nelisp-aot-test-runtime-nl-val-tag-defun
        '(defun nl_string_read64 (ptr offset) (ptr-read-u64 ptr offset))
        ;; Mirrors the defun in scripts/nelisp-standalone-build.el.
        '(defun nl_bignum_write_w (sign limb_ptr limb_count _pad)
           (let* ((box (alloc-bytes (+ 8 (logand (+ (* limb_count 4) 7) -8)) 8)) (i 0))
             (seq (ptr-write-u64 box 0 (+ 13 (+ (* sign 64) (* limb_count 256))))
                  (while (< i limb_count)
                    (seq (ptr-write-u32 box (+ 8 (* i 4)) (heap-read-u32 limb_ptr (* i 4)))
                         (setq i (+ i 1))))
                  (if (= (logand limb_count 1) 1)
                      (ptr-write-u32 box (+ 8 (* limb_count 4)) 0)
                    0)
                  (+ box 6)))))
  "Runtime helper defuns a self-contained native fixture must link.")

;;;; Production runtime closure ---------------------------------------

(defvar nelisp-aot-test-runtime--production-table nil
  "Hash table: defun name -> defun form, from the real standalone build.")

(defun nelisp-aot-test-runtime--collect-defuns (form table)
  "Record every `(defun NAME ...)' reachable inside FORM in TABLE."
  (when (consp form)
    (if (and (eq (car form) 'defun) (symbolp (cadr form)) (consp (cddr form)))
        (unless (gethash (cadr form) table)
          (puthash (cadr form) form table))
      (while (consp form)
        (nelisp-aot-test-runtime--collect-defuns (car form) table)
        (setq form (cdr form))))))

(defun nelisp-aot-test-runtime--production-table ()
  "Return a name -> defun table of the production runtime helpers.
The forms are the real definitions held by the source constants of
scripts/nelisp-standalone-build.el and the lisp/nelisp-cc-*.el modules it
links, never hand stubs.  Standalone constants win over module constants."
  (or nelisp-aot-test-runtime--production-table
      (let ((table (make-hash-table :test 'eq)) (standalone nil) (cc nil))
        (require 'nelisp-standalone-build)
        ;; Load every lisp/nelisp-cc-*.el module so that its `*-source'
        ;; constant is visible (some helpers, such as
        ;; `nelisp_env_lookup_value', live in modules the manifest omits).
        (let ((lisp-dir (file-name-directory (locate-library "nelisp-aot-compiler"))))
          (dolist (file (directory-files lisp-dir nil "\\`nelisp-cc-.*\\.el\\'"))
            (condition-case nil
                (require (intern (file-name-sans-extension file)) nil t)
              (error nil))))
        (mapatoms
         (lambda (sym)
           (let ((n (symbol-name sym)))
             (when (and (boundp sym) (consp (symbol-value sym)))
               (cond
                ((string-match-p
                  "\\`nelisp-standalone--.*\\(source\\|helpers\\)\\'" n)
                 (push sym standalone))
                ((string-match-p "\\`nelisp-cc-.*source\\'" n)
                 (push sym cc)))))))
        (dolist (sym (append (sort standalone #'string<) (sort cc #'string<)))
          (nelisp-aot-test-runtime--collect-defuns (symbol-value sym) table))
        ;; The reader build's own object manifest: (OBJECT FEATURE SOURCE-SYM).
        (when (boundp 'nelisp-standalone--manifest)
          (dolist (entry nelisp-standalone--manifest)
            (when (and (consp entry) (symbolp (nth 1 entry))
                       (symbolp (nth 2 entry)))
              (require (nth 1 entry) nil t)
              (when (boundp (nth 2 entry))
                (nelisp-aot-test-runtime--collect-defuns
                 (symbol-value (nth 2 entry)) table)))))
        ;; Zero-argument generators whose defuns depend on the active
        ;; target.  An explicit list: other `*-source' functions (the REPL
        ;; prelude) cost minutes to build and define no runtime helpers.
        (dolist (sym '(nelisp-standalone--eval-bt-extra-unit-source
                       nelisp-standalone--alloc-check-arena-forms))
          (when (fboundp sym)
            (nelisp-aot-test-runtime--collect-defuns
             (condition-case nil (funcall sym) (error nil)) table)))
        ;; Target-specific definitions (OS layer) come from the active
        ;; target's arena source; constants above win on conflicts.
        (nelisp-aot-test-runtime--collect-defuns
         (cdr (nelisp-standalone--target-arena-source)) table)
        ;; Raw machine code in the real build (no defun exists): it returns
        ;; the address of the stack-map table.  Stand in with a zeroed slot.
        (puthash 'nl_gc_scan_regs
                 '(defun nl_gc_scan_regs (top) 0)
                 table)
        (puthash 'nl_stackmap_base
                 '(defun nl_stackmap_base () (data-addr nl_stackmap_table))
                 table)
        (setq nelisp-aot-test-runtime--production-table table))))

(defun nelisp-aot-test-runtime--symbols (form acc)
  "Collect every symbol in FORM into hash table ACC."
  (while (consp form)
    (nelisp-aot-test-runtime--symbols (car form) acc)
    (setq form (cdr form)))
  (when (and form (symbolp form)) (puthash form t acc))
  acc)

(defun nelisp-aot-test-runtime-complete-unit (unit &optional keep-data)
  "Return UNIT, a `(seq FORM...)', plus the production helpers it needs.
Every symbol of UNIT that names a production runtime defun the unit does
not itself define is pulled in with its real definition, then the
transitive callees of those definitions.  A test double already defined
inside UNIT is kept and shadows the production body (its own callees are
not pulled in).  `sexp-tag' lowers to a call of `nl_val_tag' (c552d6bbb),
so that helper is required whenever it appears."
  (let* ((table (nelisp-aot-test-runtime--production-table))
         (defined (make-hash-table :test 'eq))
         (seen (make-hash-table :test 'eq))
         (extra nil))
    (let ((own (make-hash-table :test 'eq)))
      (nelisp-aot-test-runtime--collect-defuns (cdr unit) own)
      (maphash (lambda (k _v) (puthash k t defined)) own))
    (let ((pending nil))
      (cl-flet ((scan (form)
                  (let ((syms (nelisp-aot-test-runtime--symbols
                               form (make-hash-table :test 'eq))))
                    (when (gethash 'sexp-tag syms) (push 'nl_val_tag pending))
                    ;; Lowered by the compiler into runtime helper calls
                    ;; (b51fe66f4): WORD-aware heap reads and string views.
                    (dolist (r '(heap-read-u64 heap-read-u32 heap-read-u8))
                      (when (gethash r syms) (push 'nl_string_read64 pending)))
                    (maphash (lambda (k _v)
                               (when (and (gethash k table)
                                          (not (gethash k defined)))
                                 (push k pending)))
                             syms))))
        (scan (cdr unit))
        ;; Link-time probes: the compiler lowers to these helpers (WORD
        ;; payload extraction, boxed floats, byte-code word tags) without
        ;; the source naming them.
        (when (eq keep-data 'link)
          (dolist (r '(nl_word_payload nl_float_alloc wf_bytecode_w_tag))
            (when (and (gethash r table) (not (gethash r defined)))
              (push r pending))))
        (while pending
          (let ((name (pop pending)))
            (unless (or (gethash name seen) (gethash name defined))
              (puthash name t seen)
              (let ((def (gethash name table)))
                (when def
                  (push def extra)
                  (scan (cdddr def))))))))
      (let ((full (append (list 'seq)
                          (if (eq keep-data 'link)
                              (mapcar #'nelisp-aot-test-runtime--unlinked-data
                                      (nreverse extra))
                            (nreverse extra))
                          (cdr unit))))
        ;; KEEP-DATA: another link unit owns the `data-addr' symbols.
        (if keep-data full (nelisp-aot-test-runtime--bind-data full))))))


;; Data symbols the reader build's driver-owned BSS unit does not export.
(defun nelisp-aot-test-runtime--unlinked-data (form)
  "Replace `(data-addr NAME)' in pulled FORM by a constant for unexported NAME.
Used when the probe links against `nelisp-standalone--arena-base-slot-unit':
that unit supplies the driver globals; a pulled helper that mentions any other
runtime global is linked but never executed by these fixtures, so it only
needs a resolvable address."
  (let ((exported (mapcar (lambda (sym) (plist-get sym :name))
                          (plist-get (nelisp-standalone--arena-base-slot-unit)
                                     :symbols))))
    (cl-labels
        ((walk (x)
           (cond
            ((and (eq (car-safe x) 'data-addr) (symbolp (cadr x))
                  (not (member (symbol-name (cadr x)) exported)))
             (+ #x5f000000 (* 4096 (logand (sxhash-equal (cadr x)) 1023))))
            ((consp x)
             (let ((out nil))
               (while (consp x) (push (walk (car x)) out) (setq x (cdr x)))
               (nconc (nreverse out) x)))
            (t x))))
      (walk form))))

(defconst nelisp-aot-test-runtime-data-base #x60000000
  "Fixed address of the zeroed region standing in for the runtime BSS.")

(defconst nelisp-aot-test-runtime-heap-base #x70000000
  "Fixed address of the bump heap behind `nl_testrt_alloc'.")

(defconst nelisp-aot-test-runtime-alloc-defun
  `(defun nl_testrt_alloc (size align)
     ;; A freestanding executable has no allocator: `alloc-bytes' lowers to a
     ;; PLT call of `nl_alloc_bytes' that an executable cannot resolve.  Serve
     ;; it from a zeroed bump region instead (the production allocator needs
     ;; an initialised arena the probes do not build).
     (let* ((cur (heap-read-u64 ,(+ nelisp-aot-test-runtime-heap-base 0) 0))
            (base (if (= cur 0) ,(+ nelisp-aot-test-runtime-heap-base 4096) cur))
            (a (if (< align 8) 8 align))
            (p (* (/ (+ base (- a 1)) a) a)))
       (seq (ptr-write-u64 ,nelisp-aot-test-runtime-heap-base 0 (+ p size))
            p)))
  "Bump allocator used when the compiler lowers `alloc-bytes' to a PLT call.")

(defun nelisp-aot-test-runtime--bind-data (unit &optional lower-alloc)
  "Give every `(data-addr NAME)' in UNIT its own slot of a mapped region.
The real runtime links these symbols into BSS; a self-contained probe has
no BSS, so each name gets a 1 MiB zeroed slot of an anonymous MAP_FIXED
region mapped before the first non-defun form.  The raw-assembly helper
`nl_stackmap_base' is replaced by a function returning its slot."
  (let ((slots nil))
    (cl-labels
        ((slot (name)
           (let ((cell (assq name slots)))
             (unless cell
               (setq cell (cons name (length slots)))
               (setq slots (append slots (list cell))))
             (+ nelisp-aot-test-runtime-data-base (* (cdr cell) #x100000))))
         (walk (x)
           (cond
            ((and (eq (car-safe x) 'data-addr) (symbolp (cadr x))
                  (null (cddr x)))
             (slot (cadr x)))
            ((and lower-alloc (eq (car-safe x) 'alloc-bytes) (= (length x) 3))
             (list 'nl_testrt_alloc (walk (nth 1 x)) (walk (nth 2 x))))
            ((consp x)
             (let ((out nil))
               (while (consp x) (push (walk (car x)) out) (setq x (cdr x)))
               (nconc (nreverse out) x)))
            (t x))))
      (let* ((forms (mapcar #'walk (cdr unit)))
             (names (mapcar #'car slots)))
        (when lower-alloc (push nelisp-aot-test-runtime-alloc-defun forms)
              (setq slots (or slots (list (cons 'nl_testrt_none 0)))))
        (if (null slots)
            (cons 'seq forms)
          (let (before after (seen nil))
            (dolist (f forms)
              (if (or seen (not (eq (car-safe f) 'defun)))
                  (progn (setq seen t) (push f after))
                (push f before)))
            (ignore names)
            (append (list 'seq)
                    (nreverse before)
                    (list `(defun nl_testrt_map_data ()
                             (syscall-direct 9 ,nelisp-aot-test-runtime-data-base
                                             ,(* (length slots) #x100000)
                                             3 50 -1 0)
                             ,(if lower-alloc
                                  `(syscall-direct 9 ,nelisp-aot-test-runtime-heap-base
                                                   #x4000000 3 50 -1 0)
                                0))
                          '(nl_testrt_map_data))
                    (nreverse after))))))))

(defun nelisp-aot-test-runtime-compile-sexp (unit path &rest args)
  "Like `nelisp-aot-compile-sexp' on UNIT completed with production helpers.
See `nelisp-aot-test-runtime-complete-unit'; ARGS are passed through.
A helper the compiler itself lowers to (an unresolved call that appears
only after lowering) is added from the production table and the compile
retried."
  (let ((table (nelisp-aot-test-runtime--production-table))
        (unit (nelisp-aot-test-runtime-complete-unit unit t))
        (tries 0) (lower nil) (fresh nil) result done)
    (while (not done)
      (condition-case err
          (setq result (apply #'nelisp-aot-compile-sexp
                              (nelisp-aot-test-runtime--bind-data unit lower) path args)
                done t)
        (nelisp-aot-compiler-error
         (let* ((data (cdr err))
                (names
                 (cond
                  ((and (eq (car data) :not-value-expr)
                        (consp (cadr data)) (symbolp (car (cadr data))))
                   (list (car (cadr data))))
                  ((eq (car data) :unresolved-executable-relocations)
                   (mapcar (lambda (r) (intern (plist-get r :symbol)))
                           (cadr data)))))
                (defs (delq nil (mapcar (lambda (n) (gethash n table)) (delete-dups names)))))
           (when (and (member 'nl_alloc_bytes names) (not lower))
             (setq lower t fresh t))
           (setq defs (cl-remove 'nl_alloc_bytes defs :key #'cadr))
           ;; Never re-add a helper the unit already defines.
           (let ((own (make-hash-table :test 'eq)))
             (nelisp-aot-test-runtime--collect-defuns (cdr unit) own)
             (setq defs (cl-remove-if (lambda (d) (gethash (cadr d) own)) defs)))
           (if (and (or defs (prog1 fresh (setq fresh nil)))
                    (< (setq tries (1+ tries)) 40))
               (setq unit (nelisp-aot-test-runtime-complete-unit
                           (append (list 'seq) defs (cdr unit)) t))
             (signal (car err) (cdr err)))))))
    result))

(defun nelisp-aot-test-runtime-link-units (output name source others entry)
  "Compile SOURCE as unit NAME with the production closure and link it.
OTHERS are extra link units.  A helper that only the lowered code names
shows up as an unresolved link symbol; its production definition is added to
SOURCE and the link retried."
  (let ((table (nelisp-aot-test-runtime--production-table))
        (tries 0) done)
    (while (not done)
      (condition-case err
          (progn
            (nelisp-link-units
             output
             (cons (nelisp-standalone--compile-to-unit
                    name (nelisp-aot-test-runtime-complete-unit source 'link))
                   others)
             entry)
            (setq done t))
        (nelisp-link--unresolved-symbol
         (let* ((sym (and (stringp (car (cdr err))) (intern (car (cdr err)))))
                (def (and sym (gethash sym table))))
           (if (and def (< (setq tries (1+ tries)) 60))
               (setq source (append (list 'seq def) (cdr source)))
             (signal (car err) (cdr err)))))))
    output))

(provide 'nelisp-aot-test-runtime)
;;; nelisp-aot-test-runtime.el ends here
