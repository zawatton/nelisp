;;; nelisp-eln-s610-admission-test.el --- Doc 210 S10.1/S10.2 byte-compile-form admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host tests (no NeLisp binary) for the admission of the one handler-bearing
;; vendor body, `byte-compile-form' (gnu-byte-compile-form.eln, host GNU
;; Emacs 31.1 `native-compile'; pinned by sha256).
;;
;; S10.1 -- exact templates: the 7661-byte body (shape `compile-form-form',
;; including both `push_handler'/`_setjmp' regions, both guarded regions,
;; both landing pads and every `handlerlist' pop) and the 102-byte
;; `top_level_run' (the existing exact wide `gnu-require-subr' template) admit
;; the genuine artifact, and every single-byte mutation of the handler bytes
;; is refused.  "Refused" means the composite admission the loader performs is
;; false: the exact template no longer matches, or -- for a byte inside a
;; RIP-relative field -- the field no longer reaches the authenticated GOT
;; slot / `.plt' entry it must reach.
;;
;; S10.2 -- the frame-local rule (lisp/nelisp-eln-handler-frame.el) is
;; enforced on the actual bytes, independently of the template: a guarded
;; region with a local call, a call through a register or an unauthenticated
;; indirect call, a landing pad with any call, a store outside the frame, a
;; second pop or a runaway region is refused.  Each negative is paired with
;; the unmutated positive.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-tail-code)
(require 'nelisp-eln-handler-frame)

;; The mutation loops below run tens of thousands of admissions.  The libraries
;; are loaded from source (interpreted); byte-compile the functions under test
;; in memory so the whole S10.1 selector fits the ledger meter's 55 s cap.
;; This changes speed only, not the code verified (same definitions).
(let ((compiled 0))
  (mapatoms
   (lambda (symbol)
     (when (and (fboundp symbol)
                (string-match-p
                 "\\`nelisp-eln-\\(tail-code\\|leaf-code\\|handler-frame\\|registration\\)-"
                 (symbol-name symbol))
                (interpreted-function-p (symbol-function symbol)))
       (byte-compile symbol)
       (setq compiled (1+ compiled)))))
  (message "s610 admission test: byte-compiled %d interpreted functions" compiled))

(defconst nelisp-eln-s610-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-form/overlay/eln/31.1-ba35c031/gnu-byte-compile-form.eln"))

(defconst nelisp-eln-s610-test--eln-sha256
  "7109fe7ea0c4cd7b8b4cdaf20ce3e9cf3deea16f71361fcdba7c4f6c8889730d")

(defconst nelisp-eln-s610-test--body-sha256
  "477c67368d2d3fac3572b443814b909f766f6ce7b49c3fe8195b0804c7dbe0eb")

(defconst nelisp-eln-s610-test--body-symbol
  "F627974652d636f6d70696c652d666f726d_byte_compile_form_0")

(defvar nelisp-eln-s610-test--cache nil)

(defun nelisp-eln-s610-test--file-bytes ()
  (or (plist-get nelisp-eln-s610-test--cache :bytes)
      (let ((bytes (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally nelisp-eln-s610-test--eln)
                     (buffer-string))))
        (setq nelisp-eln-s610-test--cache
              (plist-put nelisp-eln-s610-test--cache :bytes bytes))
        bytes)))

(defun nelisp-eln-s610-test--function (name)
  "Return (VADDR FILE-OFFSET SIZE) of the dynsym function NAME."
  (let* ((bytes (nelisp-eln-s610-test--file-bytes))
         (text (nelisp-eln-registration--elf-section-in-bytes bytes ".text"))
         (dynsym (nelisp-eln-registration--elf-section-in-bytes bytes ".dynsym"))
         (found nil) (i 1))
    (while (and (not found) (< i (/ (nth 2 dynsym) 24)))
      (let ((entry (nelisp-eln-registration--dynsym-entry bytes i)))
        (when (equal (nth 0 entry) name)
          (setq found (list (nth 3 entry)
                            (+ (nth 1 text) (- (nth 3 entry) (nth 0 text)))
                            (nth 4 entry)))))
      (setq i (1+ i)))
    (should found)
    found))

(defun nelisp-eln-s610-test--body ()
  (let ((f (nelisp-eln-s610-test--function nelisp-eln-s610-test--body-symbol))
        (bytes (nelisp-eln-s610-test--file-bytes)))
    (list :vaddr (nth 0 f) :size (nth 2 f)
          :bytes (substring bytes (nth 1 f) (+ (nth 1 f) (nth 2 f)))
          :file-offset (nth 1 f))))

(defun nelisp-eln-s610-test--top-level ()
  (let ((f (nelisp-eln-s610-test--function "top_level_run"))
        (bytes (nelisp-eln-s610-test--file-bytes)))
    (substring bytes (nth 1 f) (+ (nth 1 f) (nth 2 f)))))

(defvar nelisp-eln-s610-test--genuine nil)

(defun nelisp-eln-s610-test--genuine ()
  "Facts about the genuine body every mutation is compared with."
  (or nelisp-eln-s610-test--genuine
      (let* ((body (nelisp-eln-s610-test--body))
             (bytes (plist-get body :bytes))
             (vaddr (plist-get body :vaddr))
             (r (nelisp-eln-tail-code-analyze-multi-import-call bytes vaddr)))
        (should r)
        (setq nelisp-eln-s610-test--genuine
              (list :vaddr vaddr :bytes bytes :analysis r
                    :regions (nelisp-eln-handler-frame-check
                              bytes (plist-get r :plt-call-offsets) '(1335 1334)
                              vaddr (plist-get r :current-thread-got)))))))

(defun nelisp-eln-s610-test--summary (r)
  "The authenticated facts of analysis R the loader relies on."
  (list :shape (plist-get r :shape)
        :imports (plist-get r :imports)
        :data (plist-get r :data-relocations)
        :swp (plist-get r :symbols-with-pos-got)
        :ct (plist-get r :current-thread-got)
        :plt (plist-get r :plt-calls)
        :plt-offsets (plist-get r :plt-call-offsets)
        :counter (plist-get r :module-counter-vaddr)))

(defun nelisp-eln-s610-test--admitted-p (bytes)
  "The composite admission of body BYTES the loader performs, host side.
The exact template must match, every GOT-relative field must reach the very
slot the genuine body reaches (the loader then authenticates those slots
against the artifact's root objects), every `call _setjmp@plt' must reach
the genuine `.plt' entry, and the frame-local rule must hold."
  (let* ((g (nelisp-eln-s610-test--genuine))
         (r (nelisp-eln-tail-code-analyze-multi-import-call
             bytes (plist-get g :vaddr))))
    (and r
         (equal (nelisp-eln-s610-test--summary r)
                (nelisp-eln-s610-test--summary (plist-get g :analysis)))
         (condition-case nil
             (nelisp-eln-handler-frame-check
              bytes (plist-get r :plt-call-offsets) '(1335 1334)
              (plist-get g :vaddr) (plist-get r :current-thread-got))
           (nelisp-eln-handler-frame-error nil)))))

(defun nelisp-eln-s610-test--handler-offsets ()
  "Every body offset that belongs to a handler region, sorted.
Per `_setjmp' site: the push sequence (the tag load, `mov $1,%esi',
`push_handler' call, `lea 0x40(%rax),%rdi', the PLT `call', `test' and the
`je'), the landing pad through its first branch, and the guarded region
through its `jmp'."
  (let ((g (nelisp-eln-s610-test--genuine)) (offsets nil))
    (dolist (region (plist-get g :regions))
      (let ((p (plist-get region :plt-call)))
        (cl-loop for k from (- (plist-get region :push-call) 24) below (+ p 12)
                 do (push k offsets))
        (cl-loop for k from (car (plist-get region :pad))
                 below (cdr (plist-get region :pad))
                 do (push k offsets))
        (cl-loop for k from (car (plist-get region :guarded))
                 below (cdr (plist-get region :guarded))
                 do (push k offsets))))
    (sort (delete-dups offsets) #'<)))

(defun nelisp-eln-s610-test--with-byte (bytes offset value)
  (let ((copy (copy-sequence bytes)))
    (aset copy offset value)
    copy))

;;; S10.1 -------------------------------------------------------------

(ert-deftest nelisp-eln-s610-fixture-pinned ()
  (should (file-exists-p nelisp-eln-s610-test--eln))
  (should (equal (secure-hash 'sha256 (nelisp-eln-s610-test--file-bytes))
                 nelisp-eln-s610-test--eln-sha256))
  (let ((body (nelisp-eln-s610-test--body)))
    (should (= (plist-get body :size) 7661))
    (should (equal (secure-hash 'sha256 (plist-get body :bytes))
                   nelisp-eln-s610-test--body-sha256))))

(ert-deftest nelisp-eln-s610-production-declares-the-body ()
  (should (member nelisp-eln-s610-test--body-sha256
                  nelisp-eln-registration--setjmp-declared-body-sha256s))
  (let ((bytes (nelisp-eln-s610-test--file-bytes)))
    (should (nelisp-eln-registration--setjmp-plt-surface-p bytes))
    (should (nelisp-eln-registration--setjmp-declared-p bytes))
    (should (nelisp-eln-registration--setjmp-surface-admitted-p bytes))
    (should (null (nelisp-eln-registration--validate-preopen bytes)))
    ;; Negative control: with the body undeclared the same file is refused
    ;; before dlopen.
    (let ((nelisp-eln-registration--setjmp-declared-body-sha256s nil))
      (should-not (nelisp-eln-registration--setjmp-surface-admitted-p bytes))
      (should-error (nelisp-eln-registration--validate-preopen bytes)
                    :type 'nelisp-eln-registration-error))))

(ert-deftest nelisp-eln-s610-body-template-admits-the-genuine-body ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (r (plist-get g :analysis)))
    (should (eq (plist-get r :shape) 'compile-form-form))
    (should (eq (plist-get r :safe) t))
    (should (equal (mapcar (lambda (i) (plist-get i :slot)) (plist-get r :imports))
                   '(12 1335 1119 1378 1205 945 1217 1354 1376 1220 4 0 7 1
                        1350 2 10 1334 1353 13 14)))
    (should (= (length (plist-get r :data-relocations)) 70))
    ;; both `call _setjmp@plt' reach the one PLT entry, at .plt + 16
    (should (equal (plist-get r :plt-calls) '(#x1030 #x1030)))
    (should (= (plist-get r :current-thread-got) #x4fb0))
    (should (= (plist-get r :symbols-with-pos-got) #x4fd0))
    (should (equal (plist-get r :handlers)
                   '(:guarded-slots (1335 1334) :push-slot 2
                                    :condition-case-type 1)))
    ;; the two regions
    (should (= (length (plist-get g :regions)) 2))
    (should (nelisp-eln-s610-test--admitted-p (plist-get g :bytes)))
    ;; the exact local helper next to the body
    (let* ((bytes (nelisp-eln-s610-test--file-bytes))
           (helper (plist-get r :helper))
           (f (nelisp-eln-s610-test--function nelisp-eln-s610-test--body-symbol))
           (back (plist-get helper :back))
           (start (- (nth 1 f) back))
           (result (nelisp-eln-tail-code-analyze-helper
                    (substring bytes start (nth 1 f)) helper (nth 0 f))))
      (should result)
      (should (= (plist-get result :size) 80)))))

(ert-deftest nelisp-eln-s610-top-level-run-is-an-exact-template ()
  (let* ((actual (nelisp-eln-s610-test--top-level))
         (template nelisp-eln-registration--gnu-require-subr-wide-template)
         (holes nelisp-eln-registration--gnu-require-subr-wide-holes))
    (should (= (length actual) 102))
    (should (nelisp-eln-registration--match-holed-template
             actual template holes))
    ;; the registration arities: (FORM &optional FOR-EFFECT)
    (should (= (nelisp-eln-registration--gnu-arity actual 61) 2))
    (should (= (nelisp-eln-registration--gnu-arity actual 70) 1))
    ;; every byte outside the declared holes is fixed
    (let ((refused 0) (fixed 0))
      (dotimes (i (length actual))
        (unless (nelisp-eln-registration--offset-holed-p i holes)
          (setq fixed (1+ fixed))
          (dolist (mask '(#x01 #x02 #x04 #x08 #x10 #x20 #x40 #x80 #xff))
            (let ((copy (nelisp-eln-s610-test--with-byte
                         actual i (logxor (aref actual i) mask))))
              (should-not (nelisp-eln-registration--match-holed-template
                           copy template holes))
              (setq refused (1+ refused))))))
      (should (> fixed 60))
      (should (= refused (* 9 fixed))))))

(ert-deftest nelisp-eln-s610-arity-is-bound-to-the-registration ()
  "The body's shape declares (1 . 2); the registration must say the same.
The preflight admits a body only when the arity immediates of top_level_run
equal the shape's arity and minimum, so every other pair is refused."
  (require 'nelisp-eln-native-subr)
  (let* ((actual (nelisp-eln-s610-test--top-level))
         (r (plist-get (nelisp-eln-s610-test--genuine) :analysis))
         (arity (nelisp-eln-native-subr-multi-arity r))
         (min-arity (nelisp-eln-native-subr-multi-min-arity r))
         (admitted 0) (tried 0))
    (should (= arity 2))
    (should (= min-arity 1))
    ;; the immediates are GNU-encoded fixnums ((N << 2) | 2); try every
    ;; encoded arity 0..8 for both, plus raw bytes that are not fixnums
    (dolist (max-byte (append (mapcar (lambda (n) (logior (ash n 2) 2))
                                      (number-sequence 0 8))
                              nil))
      (dolist (min-byte (mapcar (lambda (n) (logior (ash n 2) 2))
                                (number-sequence 0 8)))
        (let ((copy (copy-sequence actual)))
          (aset copy 61 max-byte)
          (aset copy 70 min-byte)
          (setq tried (1+ tried))
          (when (and (nelisp-eln-registration--match-holed-template
                      copy nelisp-eln-registration--gnu-require-subr-wide-template
                      nelisp-eln-registration--gnu-require-subr-wide-holes)
                     (eql (nelisp-eln-registration--gnu-arity copy 61) arity)
                     (eql (nelisp-eln-registration--gnu-arity copy 70) min-arity))
            (setq admitted (1+ admitted))))))
    (should (= tried 81))
    ;; raw bytes that are not encoded fixnums decode to nothing
    (dolist (byte '(0 1 3 4 7 100 255))
      (let ((copy (copy-sequence actual)))
        (aset copy 61 byte)
        (should-not (condition-case nil
                        (nelisp-eln-registration--gnu-arity copy 61)
                      (error nil)))))
    ;; exactly the genuine pair
    (should (= admitted 1))))

(ert-deftest nelisp-eln-s610-every-single-byte-mutation-of-handler-bytes-refused ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (offsets (nelisp-eln-s610-test--handler-offsets))
         (count 0))
    (should (nelisp-eln-s610-test--admitted-p bytes))
    (should (> (length offsets) 250))
    (dolist (offset offsets)
      ;; every single-bit flip and the complement of the byte
      (dolist (mask '(#x01 #x02 #x04 #x08 #x10 #x20 #x40 #x80 #xff))
        (let ((mutated (nelisp-eln-s610-test--with-byte
                        bytes offset (logxor (aref bytes offset) mask))))
          (when (nelisp-eln-s610-test--admitted-p mutated)
            (ert-fail (format "mutation of body offset %d (mask %#x) admitted"
                              offset mask)))
          (setq count (1+ count)))))
    (should (= count (* 9 (length offsets))))))

(ert-deftest nelisp-eln-s610-every-value-of-the-critical-handler-bytes-refused ()
  "All 255 other values of the bytes that decide which handler is pushed and
how the chain is popped: the `mov $1,%esi' immediate of both sites, the
`test %eax,%eax' pair and the three-instruction pop of every region."
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (critical nil))
    (dolist (region (plist-get g :regions))
      (let ((push-call (plist-get region :push-call))
            (p (plist-get region :plt-call)))
        ;; immediate bytes of `mov $1,%esi' (be 01 00 00 00)
        (cl-loop for k from (- push-call 24) below (- push-call 4)
                 when (and (= (aref bytes k) #xbe) (= (aref bytes (1+ k)) 1)
                           (= (aref bytes (+ k 2)) 0))
                 do (dotimes (j 5) (push (+ k j) critical)))
        (dotimes (j 4) (push (+ p 4 j) critical))
        ;; the lea 0x40 / the push_handler slot displacement
        (dotimes (j 4) (push (- p 5 (- j)) critical))
        (dotimes (j 3) (push (+ push-call j) critical)))
      ;; the pop triples: the last 12 bytes before the region's branch
      (let ((guarded-end (cdr (plist-get region :guarded)))
            (pad-end (cdr (plist-get region :pad))))
        (cl-loop for k from (- guarded-end 5 12) below (- guarded-end 5)
                 do (push k critical))
        ;; the pad's pop is the 12 bytes ending at the store
        (cl-loop for k from (- pad-end 22) below (- pad-end 8)
                 do (push k critical))))
    (setq critical (sort (delete-dups critical) #'<))
    (should (> (length critical) 60))
    (dolist (offset critical)
      (dotimes (value 256)
        (unless (= value (aref bytes offset))
          (when (nelisp-eln-s610-test--admitted-p
                 (nelisp-eln-s610-test--with-byte bytes offset value))
            ;; A RIP-relative field byte may be changed to a value whose
            ;; field still reaches the same slot only if the value is equal;
            ;; nothing else may be admitted.
            (ert-fail (format "critical offset %d value %#x admitted"
                              offset value))))))))

(ert-deftest nelisp-eln-s610-any-body-byte-mutation-refused-sampled ()
  "The template is exact over the whole body, not only the handler bytes: a
stride sample of every other offset (all nine mutations) is refused too."
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (handler (nelisp-eln-s610-test--handler-offsets))
         (checked 0))
    (cl-loop for offset from 0 below (length bytes) by 37
             unless (memq offset handler)
             do (dolist (mask '(#x01 #x10 #xff))
                  (let ((mutated (nelisp-eln-s610-test--with-byte
                                  bytes offset (logxor (aref bytes offset) mask))))
                    (when (nelisp-eln-s610-test--admitted-p mutated)
                      (ert-fail (format "body offset %d mask %#x admitted"
                                        offset mask)))
                    (setq checked (1+ checked)))))
    (should (> checked 400))))

(ert-deftest nelisp-eln-s610-truncated-or-extended-body-refused ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes)))
    (should (nelisp-eln-s610-test--admitted-p bytes))
    (should-not (nelisp-eln-s610-test--admitted-p
                 (substring bytes 0 (1- (length bytes)))))
    (should-not (nelisp-eln-s610-test--admitted-p
                 (concat bytes (unibyte-string 0))))))

(ert-deftest nelisp-eln-s610-rip-fields-must-reach-their-slots ()
  "A byte changed inside a RIP-relative field of a handler region does not
break the template (the field is a hole) but changes the slot it reaches, so
it is not admitted; the two `call _setjmp@plt' displacements likewise."
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (r (plist-get g :analysis))
         (checked 0))
    (dolist (p (plist-get r :plt-call-offsets))
      (dotimes (j 4)
        (let ((mutated (nelisp-eln-s610-test--with-byte
                        bytes (+ p j) (logxor (aref bytes (+ p j)) #x10))))
          ;; the shape still matches, but the reported target is not the PLT
          (let ((r2 (nelisp-eln-tail-code-analyze-multi-import-call
                     mutated (plist-get g :vaddr))))
            (should r2)
            (should-not (equal (plist-get r2 :plt-calls) '(#x1030 #x1030))))
          (should-not (nelisp-eln-s610-test--admitted-p mutated))
          (setq checked (1+ checked)))))
    (should (= checked 8))))

;;; S10.2 -------------------------------------------------------------

(defun nelisp-eln-s610-test--frame-check (bytes)
  (let* ((g (nelisp-eln-s610-test--genuine))
         (r (plist-get g :analysis)))
    (nelisp-eln-handler-frame-check
     bytes (plist-get r :plt-call-offsets) '(1335 1334)
     (plist-get g :vaddr) (plist-get r :current-thread-got))))

(defun nelisp-eln-s610-test--frame-reason (bytes)
  "The refusal reason of the frame rule for BYTES, or nil when it holds."
  (condition-case failure
      (progn (nelisp-eln-s610-test--frame-check bytes) nil)
    (nelisp-eln-handler-frame-error (cadr failure))))

(defun nelisp-eln-s610-test--patch (bytes offset list)
  (let ((copy (copy-sequence bytes)))
    (dolist (b list)
      (aset copy offset b)
      (setq offset (1+ offset)))
    copy))

(ert-deftest nelisp-eln-s610-frame-rule-accepts-the-genuine-body ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (regions (nelisp-eln-s610-test--frame-check (plist-get g :bytes))))
    (should (= (length regions) 2))
    (dolist (region regions)
      (should (< (car (plist-get region :guarded)) (cdr (plist-get region :guarded))))
      (should (< (car (plist-get region :pad)) (cdr (plist-get region :pad)))))
    ;; the guarded regions: exactly the Fsymbol_value / Fset pair
    (let ((bytes (plist-get g :bytes)))
      (dolist (region regions)
        (let* ((s (car (plist-get region :guarded)))
               (calls nil))
          (cl-loop for k from s below (cdr (plist-get region :guarded))
                   when (and (= (aref bytes k) #xff) (= (aref bytes (1+ k)) #x93))
                   do (push (/ (nelisp-eln-handler-frame--s32 bytes (+ k 2)) 8)
                            calls))
          (should (equal (nreverse calls) '(1335 1334))))))))

(ert-deftest nelisp-eln-s610-frame-rule-local-call-in-guarded-region-refused ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes)))
    (dolist (region (plist-get g :regions))
      (let* ((s (car (plist-get region :guarded)))
             ;; the first instruction of the region is a 5-byte
             ;; `mov d(%rsp),%rdi'; replace it by `call rel32'
             (mutated (nelisp-eln-s610-test--patch
                       bytes s '(#xe8 #x00 #x00 #x00 #x00))))
        (should (null (nelisp-eln-s610-test--frame-reason bytes)))
        (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                    'local-call-in-region))
        ;; and the exact template refuses the same body too
        (should-not (nelisp-eln-s610-test--admitted-p mutated))))))

(ert-deftest nelisp-eln-s610-frame-rule-unauthenticated-indirect-call-refused ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes)))
    (dolist (region (plist-get g :regions))
      (let* ((s (car (plist-get region :guarded)))
             (k (cl-loop for k from s below (cdr (plist-get region :guarded))
                         when (and (= (aref bytes k) #xff) (= (aref bytes (1+ k)) #x93))
                         return k)))
        (should k)
        ;; slot 1354 (`Fcar_safe') is authenticated for the body but not for
        ;; the guarded region; slot 1500 is authenticated nowhere
        (dolist (slot '(1354 1500 0 2))
          (let* ((disp (* 8 slot))
                 (mutated (nelisp-eln-s610-test--patch
                           bytes (+ k 2)
                           (list (logand disp #xff) (logand (ash disp -8) #xff)
                                 (logand (ash disp -16) #xff) 0))))
            (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                        'unauthenticated-indirect-call))
            (should-not (nelisp-eln-s610-test--admitted-p mutated))))
        ;; a call through a register (`call *%rax')
        (let ((mutated (nelisp-eln-s610-test--patch
                        bytes k '(#xff #xd0 #x90 #x90 #x90 #x90))))
          (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                      'unauthenticated-indirect-call)))
        ;; through a base register that is not the saved link table
        (let ((mutated (nelisp-eln-s610-test--patch
                        bytes k '(#xff #x95))))
          (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                      'unauthenticated-indirect-call)))))))

(ert-deftest nelisp-eln-s610-frame-rule-landing-pad-may-not-call ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes)))
    (dolist (region (plist-get g :regions))
      (let* ((pad (car (plist-get region :pad)))
             ;; the pad starts with a 7-byte RIP-relative load; replace it
             ;; by a 6-byte indirect call plus a nop
             (mutated (nelisp-eln-s610-test--patch
                       bytes pad '(#xff #x93 #xb8 #x29 #x00 #x00 #x90)))
             (rel (nelisp-eln-s610-test--patch
                   bytes pad '(#xe8 #x00 #x00 #x00 #x00 #x90 #x90))))
        (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                    'call-in-landing-pad))
        (should (eq (nelisp-eln-s610-test--frame-reason rel)
                    'local-call-in-region))))))

(ert-deftest nelisp-eln-s610-frame-rule-branch-store-and-runaway ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (region (car (plist-get g :regions)))
         (s (car (plist-get region :guarded)))
         (e (cdr (plist-get region :guarded))))
    ;; the final `jmp rel32' replaced by nops: the region never ends
    (should (memq (nelisp-eln-s610-test--frame-reason
                   (nelisp-eln-s610-test--patch
                    bytes (- e 5) '(#x90 #x90 #x90 #x90 #x90)))
                  '(unsupported-instruction pop-count runaway-region
                                            exit-outside-body)))
    ;; a conditional branch inside the guarded region
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes s '(#x74 #x00 #x90 #x90 #x90)))
                'branch-in-guarded-region))
    ;; a store through a base other than the frame: `mov %rax,0x50(%rdi)'
    ;; in place of the frame spill at the end of the Fsymbol_value call
    (let ((k (cl-loop for k from s below e
                      when (and (= (aref bytes k) #x48) (= (aref bytes (1+ k)) #x89)
                                (= (aref bytes (+ k 2)) #x44)
                                (= (aref bytes (+ k 3)) #x24))
                      return k)))
      (should k)
      (should (eq (nelisp-eln-s610-test--frame-reason
                   (nelisp-eln-s610-test--patch bytes k '(#x48 #x89 #x47 #x50 #x90)))
                  'store-outside-frame)))
    ;; the pop of the chain not read through current_thread_reloc: point the
    ;; first GOT load elsewhere (the freloc GOT slot)
    (let* ((pop-load (cl-loop for k from s below e
                              when (and (= (aref bytes k) #x48) (= (aref bytes (1+ k)) #x8b)
                                        (= (aref bytes (+ k 2)) #x15))
                              return k)))
      (should pop-load)
      (let* ((next (+ pop-load 7))
             (target #x4fc8)
             (rel (- target (+ (plist-get g :vaddr) next)))
             (mutated (copy-sequence bytes)))
        (dotimes (j 4)
          (aset mutated (+ pop-load 3 j) (logand (ash rel (* -8 j)) #xff)))
        (should (eq (nelisp-eln-s610-test--frame-reason mutated)
                    'pop-not-through-current-thread))))
    ;; no pop at all: the store of the triple becomes a register move
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes (- e 5 4)
                                              '(#x48 #x89 #xca #x90)))
                'pop-count))))

(ert-deftest nelisp-eln-s610-frame-rule-push-sequence ()
  (let* ((g (nelisp-eln-s610-test--genuine))
         (bytes (plist-get g :bytes))
         (region (car (plist-get g :regions)))
         (p (plist-get region :plt-call))
         (push-call (plist-get region :push-call)))
    ;; a `push_handler' slot other than 2
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes (+ push-call 2) '(#x18)))
                'no-push-handler-call))
    ;; the `lea 0x40(%rax),%rdi' displacement
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes (- p 2) '(#x48)))
                'no-jmp-buffer-address))
    ;; the `test %eax,%eax; je' pair
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes (+ p 4) '(#x85 #xc9)))
                'no-setjmp-branch))
    ;; a call site that is not a `call rel32'
    (should (eq (nelisp-eln-s610-test--frame-reason
                 (nelisp-eln-s610-test--patch bytes (1- p) '(#xe9)))
                'plt-site-not-a-call))))

(provide 'nelisp-eln-s610-admission-test)

;;; nelisp-eln-s610-admission-test.el ends here
