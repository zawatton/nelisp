;;; nelisp-native-load-test.el --- pure parts of the .neln loader  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The loader executes only where mmap, `ptr-call' and the runtime
;; symbols exist -- the standalone reader, not host Emacs.  `make
;; neln-loader-test' covers that half by compiling artifacts and having
;; the reader call them.
;;
;; This covers everything that is arithmetic and parsing, where a
;; mistake is silent rather than a crash: the trampoline encoding, the
;; symbol-index contract, the artifact reader, the pre-flight check, and
;; the calling-convention derivation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-artifact)
(load (expand-file-name "lisp/nelisp-native-load.el") nil t)

(defun nelisp-native-load-test--script (feature)
  "Load FEATURE from scripts/, once.

`scripts' is not on the test load path, so a plain `require' fails with
`file-missing' -- which ERT reports as the test failing, several
directories away from the reason."
  (unless (featurep feature)
    (load (expand-file-name (format "scripts/%s.el" feature)) nil t)))

(defun nelisp-native-load-test--linux-x86_64-p ()
  "Return non-nil when this host can build the x86_64 artifacts."
  (and (eq system-type 'gnu/linux)
       (stringp system-configuration)
       (string-match-p "x86_64\\|amd64" system-configuration)))

(defmacro nelisp-native-load-test--with-artifact (var source &rest body)
  "Compile SOURCE to a .neln, bind VAR to its path, and run BODY."
  (declare (indent 2))
  `(let ((dir (make-temp-file "nelisp-native-load-test-" t)))
     (unwind-protect
         (let ((el (expand-file-name "f.el" dir))
               (,var (expand-file-name "f.neln" dir)))
           (with-temp-file el (insert ,source "\n(provide 'f)\n"))
           (nelisp-artifact-compile-file el ,var nil nil nil nil nil 'neln)
           ,@body)
       (ignore-errors (delete-directory dir t)))))

;;;; Trampoline -------------------------------------------------------

(ert-deftest nelisp-native-load/trampoline-matches-the-build-time-generator ()
  "The loader's trampoline is byte-identical to the one baked at build time.

That generator is the known-good reference -- `--neln-selftest' enters
through it -- and the trampoline is the piece where a single wrong byte
is a jump to the wrong address rather than an error.  Covers both
displacement encodings: a slot at -128 or nearer takes the 8-bit form,
past that the 32-bit one, and the boundary moves with arity."
  (nelisp-native-load-test--script 'nelisp-standalone-build)
  (dolist (case '((0 . 17) (1 . 17) (1 . 18) (1 . 20) (3 . 17) (6 . 17) (6 . 32)))
    (let* ((arity (car case))
           (rt (cdr case))
           (meta (list :arity arity :rt-slot-count rt))
           (build (nelisp-standalone--native-trampoline-bytes meta))
           (mine (nelisp-native-load--trampoline arity rt)))
      (should (equal (plist-get mine :bytes) (plist-get build :bytes)))
      (should (equal (plist-get mine :imm64-offsets)
                     (plist-get build :imm64-offsets)))
      (should (= (nelisp-native-load--frame-bytes arity rt)
                 (nelisp-artifact--native-trampoline-frame-bytes meta))))))

(ert-deftest nelisp-native-load/trampoline-has-one-immediate-per-slot ()
  "There is an immediate for each boundary slot plus one for the entry.
Patching walks the offsets and the values together, so a mismatch would
put an address in a slot and a slot in the jump."
  (dolist (arity '(0 1 3 6))
    (let ((tramp (nelisp-native-load--trampoline arity 17)))
      (should (= (length (plist-get tramp :imm64-offsets))
                 (1+ nelisp-native-load-boundary-slots))))))

(ert-deftest nelisp-native-load/trampoline-refuses-arity-past-the-registers ()
  "Arity 7 has no register to read the seventh argument from."
  (should-error (nelisp-native-load--trampoline 7 17)))

;;;; Symbol index -----------------------------------------------------

(ert-deftest nelisp-native-load/bridgeable-list-matches-the-reader ()
  "The loader's symbol list is the reader's, in the same order.

The index is the whole contract: `nelisp--native-symbol-addr' selects
from a chain of `data-addr' forms fixed when the reader was built, and
an order that disagrees points every stub after the divergence at a
different function."
  (nelisp-native-load-test--script 'nelisp-standalone-build)
  (should (equal nelisp-native-load-bridgeable-symbols
                 nelisp-standalone--reader-neln-bridgeable-symbols)))

;;;; Sizing -----------------------------------------------------------

(ert-deftest nelisp-native-load/page-round-never-returns-zero ()
  "A zero-length mapping has no address to hand out."
  (should (= (nelisp-native-load--page-round 0) 4096))
  (should (= (nelisp-native-load--page-round 1) 4096))
  (should (= (nelisp-native-load--page-round 4096) 4096))
  (should (= (nelisp-native-load--page-round 4097) 8192)))

;;;; Artifact reading -------------------------------------------------

(ert-deftest nelisp-native-load/reads-a-real-artifact ()
  "The manifest parses and the defun metadata is found by name."
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (let* ((manifest (nelisp-native-load-manifest path))
           (native (plist-get manifest :native))
           (meta (nelisp-native-load--defun native "nlt-inc")))
      (should (eq (plist-get manifest :kind) 'neln))
      (should meta)
      (should (= (plist-get meta :arity) 1))
      (should (integerp (plist-get meta :body-offset)))
      (should-not (nelisp-native-load--defun native "nlt-absent")))))

(ert-deftest nelisp-native-load/text-decodes-to-the-recorded-length ()
  "The base64 text decodes to as many BYTES as the defun records.

This test used to assert `(= (length text) size)' and
`(= (--byte text 0) (aref text 0))', and its docstring said the point was
to pin `aref' per index rather than a byte walk.  That was pinning a
defect: `base64-decode-string' built each output byte with
`char-to-string', so a byte >= 128 came back as its two-byte UTF-8 form,
`length' happened to count those as one each, and `aref' happened to
read them back.  The pair only agreed because both were wrong in the
same direction.

With the decoder returning real bytes, `length' counts characters and
undercounts any payload with a high byte -- 1121 of 1152 for one
compiled object -- so the measure that matches `:size' is
`string-bytes', and the accessor that matches is `string-byte'."
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (let* ((native (plist-get (nelisp-native-load-manifest path) :native))
           (text (base64-decode-string (plist-get native :text-base64)))
           (meta (nelisp-native-load--defun native "nlt-inc")))
      (should (= (string-bytes text) (plist-get meta :size)))
      ;; `aref', not `string-byte': this test also runs under a host Emacs,
      ;; which has no `string-byte'.  On a unibyte string the two agree, and
      ;; `nelisp-native-load--byte' falls back to `aref' there for exactly
      ;; that reason.
      (should (= (nelisp-native-load--byte text 0) (aref text 0))))))

;;;; Pre-flight -------------------------------------------------------

(ert-deftest nelisp-native-load/check-passes-a-loadable-defun ()
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (should-not (nelisp-native-load-check
                 (nelisp-native-load-manifest path) "nlt-inc"))))

(ert-deftest nelisp-native-load/check-names-every-refusal ()
  "Refusals are reported before anything is mapped, and all at once."
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  ;; A name that is not in the artifact.
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (let ((problems (nelisp-native-load-check
                     (nelisp-native-load-manifest path) "nlt-absent")))
      (should (assq :no-such-defun problems))))
  ;; More parameters than the trampoline has registers.
  (nelisp-native-load-test--with-artifact
      path "(defun nlt-wide (a b c d e f g) (+ a (+ b (+ c (+ d (+ e (+ f g)))))))"
    (let ((problems (nelisp-native-load-check
                     (nelisp-native-load-manifest path) "nlt-wide")))
      (should (assq :arity-over-six problems))))
  ;; An extern with no bridge.  Built by hand rather than compiled: the
  ;; check has to reject the shape, and the compiler will not produce one
  ;; on demand.
  (let ((manifest (list :kind 'neln
                        :native (list :arch "x86_64"
                                      :extern-symbols '("nl_alloc_symbol"
                                                        "some_other_symbol")
                                      :defuns (list (list :name "f" :arity 1
                                                          :param-class 'gp
                                                          :offset 0
                                                          :body-offset 19
                                                          :rt-slot-count 17))))))
    (let ((problems (nelisp-native-load-check manifest "f")))
      (should (equal (assq :unbridgeable problems)
                     '(:unbridgeable "some_other_symbol")))))
  ;; A kind that is not neln.
  (should (assq :not-neln (nelisp-native-load-check '(:kind nelc) "f"))))

(ert-deftest nelisp-native-load/check-refuses-a-mismatched-artifact ()
  "Doc 142 section 6.4: reject on version or shape before mapping anything.

A mismatched bytecode artifact misbehaves; a mismatched native one is
machine code entered with the wrong frame layout, so these are refusals
rather than warnings.  `:format' and `:object-format' read back as
symbols while `:arch' reads back as a string -- comparing the wrong one
rejects every artifact, which is what a first cut of this did."
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (let ((manifest (nelisp-native-load-manifest path)))
      (should-not (nelisp-native-load-check manifest "nlt-inc"))
      (cl-flet ((tampered (key value &optional top)
                  ;; Copy far enough that the original manifest is intact.
                  (let ((m (copy-sequence manifest)))
                    (if top
                        (plist-put m key value)
                      (plist-put m :native
                                 (plist-put (copy-sequence
                                             (plist-get manifest :native))
                                            key value)))
                    (nelisp-native-load-check m "nlt-inc"))))
        (should (assq :artifact-format (tampered :format 'bogus-v9 t)))
        (should (assq :object-format (tampered :object-format 'elf-v0)))
        (should (assq :native-section-version
                      (tampered :native-section-version 99)))
        ;; A truncated or mangled text reaches the code page as a partial
        ;; function, which is a jump into whatever follows it.
        (should (assq :text-size-mismatch (tampered :text-size 1)))
        ;; The artifact hash.  Only checkable where the byte digest exists
        ;; -- `nelisp--sha256-bytes' is a reader builtin, so on host Emacs
        ;; the check is skipped rather than failing against a digest it
        ;; cannot compute.
        (if (fboundp 'nelisp--sha256-bytes)
            (should (assq :object-hash-mismatch
                          (tampered :object-sha256 "deadbeef")))
          (should-not (assq :object-hash-mismatch
                            (tampered :object-sha256 "deadbeef"))))))))

;;;; Calling convention -----------------------------------------------

(ert-deftest nelisp-native-load/abi-follows-the-dispatcher-externs ()
  "A DISPATCHER extern means the boxed boundary; other externs do not.

Measured on the two shapes: `add3', an extern-less `(+ a (+ b c))',
answers 6 from raw arguments and garbage from boxed ones, while `inc1'
answers through a Sexp only when its argument is one.

Taking any extern as boxed was wrong, and not harmlessly.  A defun that
allocates a vector carries `nl_alloc_vector' and friends without ever
delegating, and answers in a raw register:

  (defun c1 (n) (let ((v (vector 7 8 9)) (i 0)) (if (< i n) 111 222)))

read as boxed, its raw 111 was dereferenced as a Sexp pointer -- a fault
at address 111, surfacing in whatever the caller did with the result
rather than anywhere near the cause.  `(c1 0)' now answers 222."
  (should (eq (nelisp-native-load-abi '(:extern-symbols nil)) 'integer))
  ;; Allocation helpers alone: still the integer ABI.
  (should (eq (nelisp-native-load-abi
               '(:extern-symbols ("nl_alloc_vector" "nl_vector_set_slot")))
              'integer))
  (should (eq (nelisp-native-load-abi '(:extern-symbols ("nl_alloc_symbol")))
              'integer))
  ;; Either dispatcher: boxed.
  (should (eq (nelisp-native-load-abi
               '(:extern-symbols ("nl_alloc_symbol" "nelisp_aot_builtin_call1")))
              'boxed))
  (should (eq (nelisp-native-load-abi
               '(:extern-symbols ("nelisp_aot_builtin_calln")))
              'boxed))
  (skip-unless (nelisp-native-load-test--linux-x86_64-p))
  (nelisp-native-load-test--with-artifact path "(defun nlt-add (a b) (+ a b))"
    (should (eq (nelisp-native-load-abi
                 (plist-get (nelisp-native-load-manifest path) :native))
                'integer)))
  (nelisp-native-load-test--with-artifact path "(defun nlt-inc (x) (1+ x))"
    (should (eq (nelisp-native-load-abi
                 (plist-get (nelisp-native-load-manifest path) :native))
                'boxed))))

(ert-deftest nelisp-native-load/a-result-below-a-page-is-not-a-pointer ()
  "A raw result is not dereferenced just because the defun could delegate.

Neither signal settles this on its own.  A defun can carry a dispatcher
extern and still answer in a register:

  (defun u3 (n) (let ((m 3) (i 0)) (integerp n) (if (< i m) 111 222)))

delegates once, so the extern set says boxed, and its `:return-repr' is
`unknown', so the metadata declines to say.  Unboxing then dereferenced
address 111 -- a fault raised inside the caller's own `ptr-read-u64',
which is nowhere near the defun that produced it.

Nothing below the first page is a Sexp, so a result under that is the
value itself.  Pinned as a constant because the guard is a claim about
the address space, not a tuning knob."
  (should (= nelisp-native-load-page-bytes 4096))
  (should (> nelisp-native-load-page-bytes 222)))

;;;; Boxing -----------------------------------------------------------

(ert-deftest nelisp-native-load/string-bytes-refuses-past-one-byte ()
  "A character over 255 is more than one byte in the runtime's UTF-8.
Writing its low byte would hand over a different string than was asked
for, so this refuses rather than truncates."
  (should (equal (nelisp-native-load--string-bytes "abc") '(97 98 99)))
  (should (equal (nelisp-native-load--string-bytes "") nil))
  (should-error (nelisp-native-load--string-bytes "\u3042")))

;;;; Fixtures ---------------------------------------------------------

(ert-deftest nelisp-native-load/every-driver-case-has-a-fixture ()
  "The in-reader driver only calls artifacts the fixture list builds.
A missing one would fail as a load error rather than as a missing case."
  (nelisp-native-load-test--script 'nelisp-native-load-fixtures)
  (let ((driver (expand-file-name "test/nelisp-native-load-driver.el"))
        (called nil))
    (with-temp-buffer
      (insert-file-contents driver)
      (goto-char (point-min))
      (while (re-search-forward
              "nelisp-native-load-driver--case \"\\([^\"]+\\)\"" nil t)
        (push (match-string 1) called)))
    (should called)
    (dolist (name called)
      (should (assoc name nelisp-native-load-fixtures)))))

(provide 'nelisp-native-load-test)

;;; nelisp-native-load-test.el ends here
