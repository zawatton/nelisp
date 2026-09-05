;;; emacs-syntax-table-test.el --- ERT tests for the syntax-table MVP -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track R (2026-05-04) — coverage for the syntax class lookup
;; + the font-lock pre-pass that faces strings and line comments.

;;; Code:

(require 'ert)
(require 'emacs-syntax-table)
(require 'emacs-font-lock)
(require 'emacs-buffer)

;;;; --- helpers ---------------------------------------------------------------

(defmacro emacs-syntax-table-test--with-buffer (text &rest body)
  "Create a fresh buffer with TEXT loaded, bind it as `b'.

The buffer is the prefixed substrate's `nelisp-ec-generate-new-buffer'
(matches the existing emacs-font-lock-builtins-test pattern)."
  (declare (indent 1) (debug (form body)))
  `(let ((b (nelisp-ec-generate-new-buffer "*syntax-test*")))
     (unwind-protect
         (let ((nelisp-ec--current-buffer b))
           (nelisp-ec-insert ,text)
           ,@body)
       (when (fboundp 'nelisp-ec-kill-buffer)
         (nelisp-ec-kill-buffer b)))))

;;;; --- syntax-class lookup ---------------------------------------------------

(ert-deftest emacs-syntax-table-test/standard-classes ()
  (should (eq 'string-fence  (emacs-syntax-class-of ?\")))
  (should (eq 'escape        (emacs-syntax-class-of ?\\)))
  (should (eq 'comment-start (emacs-syntax-class-of ?\;)))
  (should (eq 'comment-end   (emacs-syntax-class-of ?\n)))
  (should (eq 'open          (emacs-syntax-class-of ?\()))
  (should (eq 'close         (emacs-syntax-class-of ?\))))
  (should (eq 'whitespace    (emacs-syntax-class-of ?\s)))
  ;; Default class for letters / digits = `word'.
  (should (eq 'word (emacs-syntax-class-of ?a)))
  (should (eq 'word (emacs-syntax-class-of ?0))))

(ert-deftest emacs-syntax-table-test/modify-entry ()
  (let ((tbl (make-hash-table :test 'eql)))
    (should (eq 'open (emacs-syntax-modify-entry ?\( 'open tbl)))
    (should (eq 'open (emacs-syntax-class-of ?\( tbl)))
    ;; nil class clears.
    (emacs-syntax-modify-entry ?\( nil tbl)
    (should (eq 'word (emacs-syntax-class-of ?\( tbl)))))

;;;; --- syntax-state-at -------------------------------------------------------

(ert-deftest emacs-syntax-table-test/state-at-start-of-buffer ()
  (emacs-syntax-table-test--with-buffer "abc"
    (should (eq 'code (emacs-syntax-state-at 1 b)))))

(ert-deftest emacs-syntax-table-test/state-at-inside-string ()
  (emacs-syntax-table-test--with-buffer "abc \"hello\" def"
    ;; Position 7 is inside "hello".
    (should (eq 'string (emacs-syntax-state-at 7 b)))))

(ert-deftest emacs-syntax-table-test/state-at-inside-comment ()
  (emacs-syntax-table-test--with-buffer "code ;; rest of line\nmore"
    ;; Position 12 is inside the comment.
    (should (eq 'comment (emacs-syntax-state-at 12 b)))))

(ert-deftest emacs-syntax-table-test/state-after-comment-newline ()
  (emacs-syntax-table-test--with-buffer "; cmt\nafter"
    ;; Position 7 (= start of "after") is back to code.
    (should (eq 'code (emacs-syntax-state-at 7 b)))))

(ert-deftest emacs-syntax-table-test/state-respects-escape-in-string ()
  (emacs-syntax-table-test--with-buffer "\"a\\\"b\" tail"
    ;; Position 5 is inside the string (= the b).  The escaped \"
    ;; must NOT close the string.
    (should (eq 'string (emacs-syntax-state-at 5 b)))
    ;; Position 8 is after the closing fence — back to code.
    (should (eq 'code (emacs-syntax-state-at 8 b)))))

;;;; --- font-lock pre-pass ----------------------------------------------------

(ert-deftest emacs-syntax-table-test/apply-faces-string-region ()
  (emacs-syntax-table-test--with-buffer "x \"hello\" y"
    (emacs-syntax-apply-faces-region 1 (1+ (length "x \"hello\" y")) b)
    ;; Position 3 = the opening `"', should be string-face.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 3 'face b)))
    ;; Position 9 = the closing `"', should be string-face.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 9 'face b)))
    ;; Position 1 = `x', should NOT have a face.
    (should-not (emacs-buffer-get-text-property 1 'face b))))

(ert-deftest emacs-syntax-table-test/apply-faces-comment-region ()
  (emacs-syntax-table-test--with-buffer "x ; cmt\ny"
    (emacs-syntax-apply-faces-region 1 (1+ (length "x ; cmt\ny")) b)
    ;; Position 3 = `;', should be comment-face.
    (should (eq 'font-lock-comment-face
                (emacs-buffer-get-text-property 3 'face b)))
    ;; Position 5 = inside the comment, should be comment-face.
    (should (eq 'font-lock-comment-face
                (emacs-buffer-get-text-property 5 'face b)))
    ;; Position 9 = `y' on the next line, should NOT.
    (should-not (emacs-buffer-get-text-property 9 'face b))))

(ert-deftest emacs-syntax-table-test/font-lock-keyword-loses-to-string ()
  "When a keyword regex would match inside a string literal, the
syntactic post-pass must overwrite the keyword face with the
string face."
  (emacs-syntax-table-test--with-buffer "before \"defun-inside\" after"
    (emacs-font-lock-add-keywords
     nil
     '(("defun-inside" (0 font-lock-keyword-face)))
     'set)
    (emacs-font-lock-fontify-region 1 (1+ (length "before \"defun-inside\" after")))
    ;; Position 9 = inside the string, was potentially keyword-face,
    ;; must be string-face after the post-pass.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 9 'face b)))))

(provide 'emacs-syntax-table-test)

;;; emacs-syntax-table-test.el ends here

;;;; --- upstream-compatible syntax-table layer ---------------------------------
;; Host Emacs' C builtins (char-syntax / make-syntax-table / standard-syntax-table
;; / string-to-syntax / modify-syntax-entry) shadow the install gates, so tests
;; exercise the prefixed `emacs-syntax-table-*' helpers and compare to host.

(ert-deftest emacs-syntax-table-test/char-syntax-matches-host-standard ()
  "Upstream char-syntax matches host on ASCII 0-127 under the standard table."
  (let ((std (emacs-syntax-table-standard)))
    (dotimes (i 128)
      (should (= (with-syntax-table (standard-syntax-table) (char-syntax i))
                 (emacs-syntax-table-char-syntax i std))))))

(ert-deftest emacs-syntax-table-test/char-syntax-non-ascii-word ()
  "Non-ASCII codepoints default to word syntax in the standard table."
  (let ((std (emacs-syntax-table-standard)))
    (should (= ?w (emacs-syntax-table-char-syntax #x65e5 std)))   ; CJK
    (should (= ?w (emacs-syntax-table-char-syntax #x3042 std))))) ; Hiragana

(ert-deftest emacs-syntax-table-test/string-to-syntax-matches-host ()
  "Upstream string-to-syntax parses basic descriptors like host."
  (dolist (d '("w" " " "." "(" ")" "\"" "()"))
    (should (equal (string-to-syntax d)
                   (emacs-syntax-table-string-to-syntax d)))))

(ert-deftest emacs-syntax-table-test/modify-syntax-entry-round-trip ()
  "modify-syntax-entry overrides entries (single char and range) over inheritance."
  (let ((tbl (emacs-syntax-table-make)))
    (should (= ?w (emacs-syntax-table-char-syntax ?a tbl)))
    (should (= ?. (emacs-syntax-table-char-syntax ?. tbl)))
    (emacs-syntax-table-modify-entry ?- "w" tbl)
    (should (= ?w (emacs-syntax-table-char-syntax ?- tbl)))
    (emacs-syntax-table-modify-entry ?a "_" tbl)
    (should (= ?_ (emacs-syntax-table-char-syntax ?a tbl)))
    (emacs-syntax-table-modify-entry (cons ?0 ?9) "." tbl)
    (should (= ?. (emacs-syntax-table-char-syntax ?5 tbl)))))

(ert-deftest emacs-syntax-table-test/make-syntax-table-isolates-from-standard ()
  "Edits to a derived table do not mutate the standard table."
  (let ((tbl (emacs-syntax-table-make)))
    (emacs-syntax-table-modify-entry ?a "." tbl)
    (should (= ?. (emacs-syntax-table-char-syntax ?a tbl)))
    (should (= ?w (emacs-syntax-table-char-syntax
                   ?a (emacs-syntax-table-standard))))))

;;; emacs-syntax-table-test.el upstream-layer tests end here

(ert-deftest emacs-syntax-table-test/parse-partial-sexp-matches-host ()
  "Upstream parse-partial-sexp state matches host on representative inputs.
The internal 11th state slot is ignored (compared via `butlast')."
  (dolist (c (list (list "(a (b) c)" nil)
                   (list "(((" nil)
                   (list "()" nil)
                   (list "(a \"bc" nil)
                   (list "ab\\" nil)
                   (list "(a \"s\" b)" nil)
                   (list "a ; comment" t)
                   (list "(foo (bar baz) \"q\" )" nil)))
    (let* ((text (nth 0 c))
           (comment (nth 1 c))
           (host (with-temp-buffer
                   (insert text)
                   (when comment
                     (modify-syntax-entry ?\; "<")
                     (modify-syntax-entry ?\n ">"))
                   (butlast (parse-partial-sexp 1 (1+ (length text))))))
           (mine (let ((b (nelisp-ec-generate-new-buffer "*ppss*")))
                   (unwind-protect
                       (let ((nelisp-ec--current-buffer b))
                         (nelisp-ec-insert text)
                         (let ((tbl (emacs-syntax-table-make)))
                           (when comment
                             (emacs-syntax-table-modify-entry ?\; "<" tbl)
                             (emacs-syntax-table-modify-entry ?\n ">" tbl))
                           (butlast (emacs-syntax-table-parse-partial-sexp
                                     1 (1+ (length text)) b tbl))))
                     (when (fboundp 'nelisp-ec-kill-buffer)
                       (nelisp-ec-kill-buffer b))))))
      (should (equal host mine)))))

(ert-deftest emacs-syntax-table-test/parse-partial-sexp-state-resumption ()
  "Chunked parse-partial-sexp with STATE equals a single whole-region parse."
  (let ((mk (lambda (text)
              (let ((b (nelisp-ec-generate-new-buffer "*p*")))
                (let ((nelisp-ec--current-buffer b)) (nelisp-ec-insert text))
                b))))
    (dolist (c (list (list "(a (b) c)" 5)
                     (list "(a \"bc\" d)" 6)    ; split inside the string
                     (list "(((x)))" 4)
                     (list "foo (bar baz)" 7)))
      (let* ((text (nth 0 c)) (split (nth 1 c))
             (whole (butlast (emacs-syntax-table-parse-partial-sexp
                              1 (1+ (length text)) (funcall mk text))))
             (b2 (funcall mk text))
             (s1 (emacs-syntax-table-parse-partial-sexp 1 split b2))
             (chunked (butlast (emacs-syntax-table-parse-partial-sexp
                                split (1+ (length text)) b2 nil s1))))
        (should (equal whole chunked))))))

(ert-deftest emacs-syntax-table-test/parse-partial-sexp-targetdepth-stopbefore ()
  "parse-partial-sexp TARGETDEPTH / STOPBEFORE and point match host."
  (let ((mine (lambda (text td sb)
                (let ((b (nelisp-ec-generate-new-buffer "*p*")))
                  (let ((nelisp-ec--current-buffer b))
                    (nelisp-ec-insert text)
                    (list (butlast (emacs-syntax-table-parse-partial-sexp
                                    1 (1+ (length text)) b nil nil td sb))
                          (nelisp-ec-point)))))))
    (dolist (c (list (list "(a) b" 0 nil)
                     (list "  (a)" nil t)
                     (list "(a (b) c)" 1 nil)
                     (list "(a (b) c)" nil nil)
                     (list "x (y)" nil t)))
      (let* ((text (nth 0 c)) (td (nth 1 c)) (sb (nth 2 c))
             (host (with-temp-buffer
                     (insert text)
                     (let ((st (parse-partial-sexp 1 (1+ (length text)) td sb)))
                       (list (butlast st) (point))))))
        (should (equal host (funcall mine text td sb)))))))

(ert-deftest emacs-syntax-table-test/buffer-local-syntax-tables ()
  "set-syntax-table is buffer-local; with-syntax-table dynamically overrides."
  (let ((a (nelisp-ec-generate-new-buffer "*a*"))
        (b (nelisp-ec-generate-new-buffer "*b*"))
        (tbl (emacs-syntax-table-make)))
    (emacs-syntax-table-modify-entry ?- "w" tbl)
    (let ((nelisp-ec--current-buffer a))
      (emacs-syntax-table-set-current tbl)
      (should (= ?w (emacs-syntax-table-char-syntax ?-))))
    ;; a different buffer keeps the standard classification
    (let ((nelisp-ec--current-buffer b))
      (should (= ?_ (emacs-syntax-table-char-syntax ?-))))
    ;; the buffer-local table persists in A
    (let ((nelisp-ec--current-buffer a))
      (should (= ?w (emacs-syntax-table-char-syntax ?-)))
      ;; with-syntax-table dynamically overrides the buffer-local table
      (should (= ?_ (let ((emacs-syntax-table--current
                           (emacs-syntax-table-standard)))
                      (emacs-syntax-table-char-syntax ?-)))))))

;;;; --- Doc 51 T53 regression: install-gate must beat earlier bootstrap stubs

;; Bug pinned down by the T53 package-load-matrix sweep: `emacs-stub.el'
;; installs early-bootstrap-order fallbacks for `make-syntax-table' /
;; `standard-syntax-table' / `syntax-table' / `set-syntax-table' /
;; `modify-syntax-entry', and `emacs-stub-bulk.el' installs a blanket
;; nil-returning stub for `char-syntax' (its "trivial stubs" dolist) --
;; both load before this file in the standalone bootstrap order (see
;; `nelisp-bootstrap--enforce-bootstrap-order' in
;; scripts/build-nelisp-bootstrap.el).  `emacs-syntax-table--install-function-p'
;; used to test standalone-ness with a bare
;; `(not (boundp \='emacs-version))', which misfires under standalone NeLisp
;; (the NeLisp reader also binds `emacs-version'), so it took the host
;; branch `(not (fboundp symbol))' -- which is false for these
;; already-stubbed names, so the real char-table-backed implementations here
;; never installed.  Vendor code (org-download, org-caldav, org-draw, elfeed,
;; elfeed-summarize in the T53 sweep) that called the bare, un-overridden
;; `(make-syntax-table)' got back the stub's `(syntax-table nil)' list, and
;; any later `aref' on it signalled `(wrong-type-argument arrayp (syntax-table
;; nil))'.  The fix mirrors `emacs-char-table--standalone-p' in
;; `emacs-char-table.el': detect standalone via the NeLisp-only
;; `nl-write-file' primitive first.

(ert-deftest emacs-syntax-table-test/standalone-p-false-under-host ()
  "Sanity: without a NeLisp-only primitive present, this is host Emacs."
  (should-not (fboundp 'nl-write-file))
  (should-not (emacs-syntax-table--standalone-p)))

(ert-deftest emacs-syntax-table-test/install-gate-overrides-preexisting-stub-when-standalone ()
  "Regression: simulated-standalone install-gate must win even when the
unprefixed name is already `fboundp' from an earlier bootstrap-order stub."
  (let ((probe (make-symbol "emacs-syntax-table-test--stub-probe"))
        (had-nl-write-file (fboundp 'nl-write-file)))
    ;; Mimic the emacs-stub.el shape: a pre-existing, already-`fboundp'
    ;; broken stand-in for e.g. `make-syntax-table'.
    (fset probe (lambda (&rest _) (list 'syntax-table nil)))
    (unwind-protect
        (progn
          (should (fboundp probe))
          ;; Off the standalone marker: host behavior is preserved, an
          ;; already-`fboundp' name is left alone.
          (should-not (emacs-syntax-table--install-function-p probe))
          ;; Flip on the standalone marker (`nl-write-file' fboundp) without
          ;; a real standalone runtime, and the gate must now say "override"
          ;; despite PROBE already being `fboundp' -- this is the exact
          ;; condition the defect got wrong.
          (fset 'nl-write-file (lambda (&rest _) nil))
          (should (emacs-syntax-table--standalone-p))
          (should (emacs-syntax-table--install-function-p probe)))
      (fmakunbound probe)
      (unless had-nl-write-file (fmakunbound 'nl-write-file)))))

;;;; --- Doc 51 T53: char-table-p / syntax-table-p / char-table-range contract

(ert-deftest emacs-syntax-table-test/tables-are-real-char-tables ()
  "`make-syntax-table', `standard-syntax-table' and a copy must all satisfy
`emacs-char-table-p' with subtype `syntax-table' -- not the broken
`(syntax-table . PARENT)' list / bare 256-vector legacy stand-ins."
  (dolist (tbl (list (emacs-syntax-table-make)
                     (emacs-syntax-table-standard)
                     (emacs-char-table-copy (emacs-syntax-table-standard))))
    (should (emacs-char-table-p tbl))
    (should (eq 'syntax-table (emacs-char-table-subtype tbl)))))

(ert-deftest emacs-syntax-table-test/char-table-range-nil-char-cons ()
  "The prefixed `emacs-char-table-range' on a syntax char-table for nil / a
char / a cons matches host `char-table-range' semantics for the
corresponding table.  Host Emacs' own unprefixed `char-table-range' is a
real C primitive that requires a real host char-table, so the standalone
representation is exercised through the prefixed API here, matching the
rest of this file's upstream-compatible-layer tests."
  (let ((mine (emacs-syntax-table-make))
        (host-default (char-table-range (standard-syntax-table) nil)))
    ;; nil = the table's default value (host default for the standard
    ;; syntax table happens to be the punctuation descriptor).
    (should (equal host-default (char-table-range (standard-syntax-table) nil)))
    (should (equal nil (emacs-char-table-range mine nil)))
    ;; a plain character samples that slot.
    (emacs-char-table-set mine ?a '(2 . nil))
    (should (equal '(2 . nil) (emacs-char-table-range mine ?a)))
    ;; a (FROM . TO) cons samples its CAR, mirroring host `char-table-range'
    ;; called with a range cons.
    (should (equal '(2 . nil) (emacs-char-table-range mine (cons ?a ?a))))))

(ert-deftest emacs-syntax-table-test/modify-syntax-entry-cons-range-matches-host ()
  "modify-syntax-entry with a (FROM . TO) cons range matches host for every
character the range covers."
  (let ((host (make-syntax-table))
        (mine (emacs-syntax-table-make)))
    (modify-syntax-entry (cons ?0 ?9) "_" host)
    (emacs-syntax-table-modify-entry (cons ?0 ?9) "_" mine)
    (dolist (c (number-sequence ?0 ?9))
      (should (= (with-syntax-table host (char-syntax c))
                 (emacs-syntax-table-char-syntax c mine))))
    ;; outside the range, both keep the standard word class.
    (should (= (with-syntax-table host (char-syntax ?a))
               (emacs-syntax-table-char-syntax ?a mine)))))

(ert-deftest emacs-syntax-table-test/copy-syntax-table-independent-of-source ()
  "A copy is `equal'-shaped to its source at copy time but independent of
later mutation, matching host `copy-syntax-table' behavior."
  (let* ((mine (emacs-syntax-table-make))
         (copy (emacs-char-table-copy mine)))
    (should (= (emacs-syntax-table-char-syntax ?a mine)
               (emacs-syntax-table-char-syntax ?a copy)))
    (emacs-syntax-table-modify-entry ?a "_" mine)
    (should (= ?_ (emacs-syntax-table-char-syntax ?a mine)))
    (should (= ?w (emacs-syntax-table-char-syntax ?a copy)))))
