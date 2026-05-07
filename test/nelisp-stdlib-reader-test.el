;;; nelisp-stdlib-reader-test.el --- ERT tests for Stage 7.2.a tokenizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Phase 7 Stage 7.2.a (Doc 66) — tokenizer tests for
;; `lisp/nelisp-stdlib-reader.el'.  Stage 7.2.a is parallel-impl: the
;; Rust reader still drives every `read-from-string', the elisp
;; tokenizer is exercised through these ERTs only.
;;
;; Subprocess pattern (= same shape as `test/nelisp-stdlib-prn-test.el'):
;; each test runs `target/release/nelisp eval EXPR' where EXPR ends in
;; `(princ ...)' that prints either token types or values.  This
;; bypasses the host-Emacs ERT runner whose own `nelisp--read-tokenize'
;; binding does not exist, and exercises the bundled NeLisp stdlib for
;; real.

;;; Code:

(require 'ert)

(defconst nelisp-stdlib-reader-test--repo-root
  (let* ((this (or load-file-name buffer-file-name))
         (test-dir (and this (file-name-directory this))))
    (and test-dir (expand-file-name ".." test-dir)))
  "Absolute path to the dev/nelisp worktree, or nil when undeterminable.")

(defconst nelisp-stdlib-reader-test--bin
  (and nelisp-stdlib-reader-test--repo-root
       (expand-file-name "target/release/nelisp"
                         nelisp-stdlib-reader-test--repo-root))
  "Path to the build-tool `nelisp' binary.")

(defun nelisp-stdlib-reader-test--skip-unless-built ()
  (unless (and nelisp-stdlib-reader-test--bin
               (file-executable-p nelisp-stdlib-reader-test--bin))
    (ert-skip
     (format "build-tool nelisp binary missing — run `cargo build --release --bin nelisp' (looked at %s)"
             nelisp-stdlib-reader-test--bin))))

(defun nelisp-stdlib-reader-test--eval (expr-string)
  "Run `nelisp eval EXPR-STRING' and return STDOUT (string)."
  (with-temp-buffer
    (let ((code (call-process nelisp-stdlib-reader-test--bin nil t nil
                              "eval" expr-string)))
      (unless (zerop code)
        (error "nelisp eval failed (exit %d): %s"
               code (buffer-substring-no-properties (point-min) (point-max))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun nelisp-stdlib-reader-test--strip-trailing (out _tail)
  "Strip trailing newline + progn auto-printed `nil' from OUT.
The second argument is kept for back-compatibility but ignored — the
binary always ends with a newline followed by `nil' (= the auto-print
of the `progn' return value)."
  (replace-regexp-in-string "nil\n?\\'" "" out))

(defun nelisp-stdlib-reader-test--types (input-str)
  "Tokenize INPUT-STR via subprocess, return a list of `:type' symbol names."
  (nelisp-stdlib-reader-test--skip-unless-built)
  (let* ((quoted (replace-regexp-in-string "\\\\" "\\\\" input-str t t))
         (quoted (replace-regexp-in-string "\"" "\\\"" quoted t t))
         (expr (format "(progn (princ (prin1-to-string (mapcar (lambda (tk) (plist-get tk :type)) (nelisp--read-tokenize \"%s\")))) nil)"
                       quoted))
         (out (nelisp-stdlib-reader-test--eval expr))
         (out (nelisp-stdlib-reader-test--strip-trailing out "nil")))
    out))

(defun nelisp-stdlib-reader-test--values (input-str)
  "Tokenize INPUT-STR via subprocess, return prin1-of-`:value'-list as string."
  (nelisp-stdlib-reader-test--skip-unless-built)
  (let* ((quoted (replace-regexp-in-string "\\\\" "\\\\" input-str t t))
         (quoted (replace-regexp-in-string "\"" "\\\"" quoted t t))
         (expr (format "(progn (princ (prin1-to-string (mapcar (lambda (tk) (plist-get tk :value)) (nelisp--read-tokenize \"%s\")))) nil)"
                       quoted))
         (out (nelisp-stdlib-reader-test--eval expr))
         (out (nelisp-stdlib-reader-test--strip-trailing out "nil")))
    out))

;; ---------------------------------------------------------------------------
;; Atoms (= int / float / symbol / dot / nil / t shape).
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-int-decimal ()
  (should (equal (nelisp-stdlib-reader-test--types "42") "(int)"))
  (should (equal (nelisp-stdlib-reader-test--values "42") "(42)"))
  (should (equal (nelisp-stdlib-reader-test--values "-7") "(-7)"))
  (should (equal (nelisp-stdlib-reader-test--values "+15") "(15)")))

(ert-deftest nelisp-stdlib-reader-tok-int-radix ()
  (should (equal (nelisp-stdlib-reader-test--values "#xff") "(255)"))
  (should (equal (nelisp-stdlib-reader-test--values "#xFF") "(255)"))
  (should (equal (nelisp-stdlib-reader-test--values "#o17") "(15)"))
  (should (equal (nelisp-stdlib-reader-test--values "#b1010") "(10)"))
  (should (equal (nelisp-stdlib-reader-test--values "#x-10") "(-16)")))

(ert-deftest nelisp-stdlib-reader-tok-float ()
  (should (equal (nelisp-stdlib-reader-test--types "3.14") "(float)"))
  (should (equal (nelisp-stdlib-reader-test--values "3.14") "(3.14)"))
  (should (equal (nelisp-stdlib-reader-test--values "1.5") "(1.5)")))

(ert-deftest nelisp-stdlib-reader-tok-symbol ()
  (should (equal (nelisp-stdlib-reader-test--types "foo") "(symbol)"))
  (should (equal (nelisp-stdlib-reader-test--values "foo") "(\"foo\")"))
  (should (equal (nelisp-stdlib-reader-test--values "let*") "(\"let*\")"))
  ;; "nil"/"t" tokenize as `symbol' here; the parser (Stage 7.2.b) is
  ;; the single source of truth for nil/t literal recognition.
  (should (equal (nelisp-stdlib-reader-test--values "nil") "(\"nil\")"))
  (should (equal (nelisp-stdlib-reader-test--values "t") "(\"t\")")))

(ert-deftest nelisp-stdlib-reader-tok-dot ()
  (should (equal (nelisp-stdlib-reader-test--types "(a . b)")
                 "(lparen symbol dot symbol rparen)")))

;; ---------------------------------------------------------------------------
;; Strings + escapes.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-string-simple ()
  (should (equal (nelisp-stdlib-reader-test--values "\"hello\"")
                 "(\"hello\")")))

(ert-deftest nelisp-stdlib-reader-tok-string-escape-newline ()
  (should (equal (nelisp-stdlib-reader-test--values "\"a\\nb\"")
                 "(\"a\\nb\")")))

;; ---------------------------------------------------------------------------
;; Lists / vectors.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-list-empty ()
  (should (equal (nelisp-stdlib-reader-test--types "()")
                 "(lparen rparen)")))

(ert-deftest nelisp-stdlib-reader-tok-list-mixed ()
  (should (equal (nelisp-stdlib-reader-test--types "(foo 42 \"hi\" 'q)")
                 "(lparen symbol int str quote symbol rparen)")))

(ert-deftest nelisp-stdlib-reader-tok-vector ()
  (should (equal (nelisp-stdlib-reader-test--types "[1 2 3]")
                 "(lbracket int int int rbracket)")))

;; ---------------------------------------------------------------------------
;; Quote-family prefixes.
;; ---------------------------------------------------------------------------

;; Note: `prin1-to-string' (= nelisp-stdlib-prn.el) abbreviates
;; `(TAG ARG)' for TAG ∈ {quote, backquote, comma, comma-at, function}
;; into `'/`/,/,@/#'-prefixed shape (= reader-macro abbreviation), so a
;; 2-element type list whose head matches one of those gets printed as
;; the abbreviated form.  We compare against that abbreviated string
;; (= what `prin1' actually returns) rather than the parenthesized
;; layout.

(ert-deftest nelisp-stdlib-reader-tok-quote ()
  (should (equal (nelisp-stdlib-reader-test--types "'x")
                 "'symbol")))

(ert-deftest nelisp-stdlib-reader-tok-backquote ()
  (should (equal (nelisp-stdlib-reader-test--types "`x")
                 "`symbol")))

(ert-deftest nelisp-stdlib-reader-tok-comma ()
  (should (equal (nelisp-stdlib-reader-test--types ",x")
                 ",symbol")))

(ert-deftest nelisp-stdlib-reader-tok-comma-at ()
  (should (equal (nelisp-stdlib-reader-test--types ",@x")
                 ",@symbol")))

(ert-deftest nelisp-stdlib-reader-tok-function-quote ()
  (should (equal (nelisp-stdlib-reader-test--types "#'x")
                 "(function-quote symbol)")))

;; ---------------------------------------------------------------------------
;; Comments + line tracking.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-comment-skips-content ()
  (should (equal (nelisp-stdlib-reader-test--types "; one line comment\nfoo")
                 "(symbol)")))

;; ---------------------------------------------------------------------------
;; Char literals.  Tokenized as `int' value.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-char-plain ()
  (should (equal (nelisp-stdlib-reader-test--values "?A") "(65)"))
  (should (equal (nelisp-stdlib-reader-test--values "?a") "(97)")))

(ert-deftest nelisp-stdlib-reader-tok-char-newline-escape ()
  (should (equal (nelisp-stdlib-reader-test--values "?\\n") "(10)")))

(ert-deftest nelisp-stdlib-reader-tok-char-tab-escape ()
  (should (equal (nelisp-stdlib-reader-test--values "?\\t") "(9)")))

(ert-deftest nelisp-stdlib-reader-tok-char-control ()
  ;; ?\C-a = 1
  (should (equal (nelisp-stdlib-reader-test--values "?\\C-a") "(1)")))

(ert-deftest nelisp-stdlib-reader-tok-char-meta ()
  ;; ?\M-a = 97 | 0x8000000 = 134217825
  (should (equal (nelisp-stdlib-reader-test--values "?\\M-a")
                 "(134217825)")))

;; ---------------------------------------------------------------------------
;; Sharpsign forms.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-sharpsign-record ()
  ;; #s( ... ) emits a single sharps-paren token followed by body.
  (should (equal (nelisp-stdlib-reader-test--types "#s(point 1 2)")
                 "(sharps-paren symbol int int rparen)")))

;; ---------------------------------------------------------------------------
;; Nested structures.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-reader-tok-nested ()
  (should (equal (nelisp-stdlib-reader-test--types
                  "(let ((x 1)) (+ x 2))")
                 "(lparen symbol lparen lparen symbol int rparen rparen lparen symbol symbol int rparen rparen)")))

;; ---------------------------------------------------------------------------
;; Stage 7.2.b parser tests.  Exercise `read-from-string' (= the
;; takeover entry installed by `lisp/nelisp-stdlib-reader.el') through
;; the same subprocess pattern as the tokenizer tests.  Each test prints
;; `(prin1-to-string (read-from-string "..."))' and compares against
;; the expected `(FORM . CONSUMED-END)' string.
;; ---------------------------------------------------------------------------

(defun nelisp-stdlib-reader-test--read (input-str)
  "Run `(read-from-string INPUT-STR)' via subprocess, return prin1 of result."
  (nelisp-stdlib-reader-test--skip-unless-built)
  (let* ((quoted (replace-regexp-in-string "\\\\" "\\\\" input-str t t))
         (quoted (replace-regexp-in-string "\"" "\\\"" quoted t t))
         (expr (format "(progn (princ (prin1-to-string (read-from-string \"%s\"))) nil)"
                       quoted))
         (out (nelisp-stdlib-reader-test--eval expr))
         (out (nelisp-stdlib-reader-test--strip-trailing out "nil")))
    out))

(ert-deftest nelisp-stdlib-reader-parse-int ()
  (should (equal (nelisp-stdlib-reader-test--read "42")
                 "(42 . 2)")))

(ert-deftest nelisp-stdlib-reader-parse-float ()
  (should (equal (nelisp-stdlib-reader-test--read "3.14")
                 "(3.14 . 4)")))

(ert-deftest nelisp-stdlib-reader-parse-string ()
  (should (equal (nelisp-stdlib-reader-test--read "\"hello\"")
                 "(\"hello\" . 7)")))

(ert-deftest nelisp-stdlib-reader-parse-symbol ()
  (should (equal (nelisp-stdlib-reader-test--read "foo")
                 "(foo . 3)")))

(ert-deftest nelisp-stdlib-reader-parse-nil-t ()
  (should (equal (nelisp-stdlib-reader-test--read "nil")
                 "(nil . 3)"))
  (should (equal (nelisp-stdlib-reader-test--read "t")
                 "(t . 1)")))

(ert-deftest nelisp-stdlib-reader-parse-list-empty ()
  (should (equal (nelisp-stdlib-reader-test--read "()")
                 "(nil . 2)")))

(ert-deftest nelisp-stdlib-reader-parse-list-three ()
  (should (equal (nelisp-stdlib-reader-test--read "(a b c)")
                 "((a b c) . 7)")))

(ert-deftest nelisp-stdlib-reader-parse-list-mixed ()
  (should (equal (nelisp-stdlib-reader-test--read "(foo 42 \"hi\")")
                 "((foo 42 \"hi\") . 13)")))

(ert-deftest nelisp-stdlib-reader-parse-dotted ()
  (should (equal (nelisp-stdlib-reader-test--read "(a . b)")
                 "((a . b) . 7)")))

(ert-deftest nelisp-stdlib-reader-parse-vector ()
  (should (equal (nelisp-stdlib-reader-test--read "[1 2 3]")
                 "([1 2 3] . 7)")))

(ert-deftest nelisp-stdlib-reader-parse-quote ()
  ;; (read-from-string "'x") => ('x . 2) ⟶ ((quote x) . 2) printed
  ;; abbreviated as `('x . 2)'.
  (should (equal (nelisp-stdlib-reader-test--read "'x")
                 "('x . 2)")))

(ert-deftest nelisp-stdlib-reader-parse-backquote-comma ()
  (should (equal (nelisp-stdlib-reader-test--read "`(a ,b)")
                 "(`(a ,b) . 7)")))

(ert-deftest nelisp-stdlib-reader-parse-function-quote ()
  (should (equal (nelisp-stdlib-reader-test--read "#'foo")
                 "(#'foo . 5)")))

(ert-deftest nelisp-stdlib-reader-parse-record ()
  (should (equal (nelisp-stdlib-reader-test--read "#s(point 1 2)")
                 "(#s(point 1 2) . 13)")))

(ert-deftest nelisp-stdlib-reader-parse-nested ()
  (should (equal (nelisp-stdlib-reader-test--read "(let ((x 1)) (+ x 2))")
                 "((let ((x 1)) (+ x 2)) . 21)")))

;; ---------------------------------------------------------------------------
;; round-trip property: `(read-from-string (prin1-to-string OBJ))' returns
;; OBJ unchanged.  This is the foundation of Stage 7.2.c gate but adding a
;; few representative cases here gives early signal.
;; ---------------------------------------------------------------------------

(defun nelisp-stdlib-reader-test--round-trip (obj-form)
  "Eval `(equal OBJ (car (read-from-string (prin1-to-string OBJ))))'
where OBJ is OBJ-FORM evaluated; return stdout (= `t' or `nil')."
  (nelisp-stdlib-reader-test--skip-unless-built)
  (let* ((expr (format
                "(progn (princ (prin1-to-string (let* ((obj %s) (s (prin1-to-string obj)) (rt (car (read-from-string s)))) (equal obj rt)))) nil)"
                obj-form))
         (out (nelisp-stdlib-reader-test--eval expr))
         (out (nelisp-stdlib-reader-test--strip-trailing out "nil")))
    out))

(ert-deftest nelisp-stdlib-reader-roundtrip-int ()
  (should (equal (nelisp-stdlib-reader-test--round-trip "42") "t")))

(ert-deftest nelisp-stdlib-reader-roundtrip-list ()
  (should (equal (nelisp-stdlib-reader-test--round-trip "'(a b c)") "t")))

(ert-deftest nelisp-stdlib-reader-roundtrip-string ()
  (should (equal (nelisp-stdlib-reader-test--round-trip "\"hello\"") "t")))

(ert-deftest nelisp-stdlib-reader-roundtrip-vector ()
  (should (equal (nelisp-stdlib-reader-test--round-trip "[1 2 3]") "t")))

(ert-deftest nelisp-stdlib-reader-roundtrip-nested ()
  (should (equal (nelisp-stdlib-reader-test--round-trip
                  "'(let ((x 1)) (+ x 2))")
                 "t")))

(provide 'nelisp-stdlib-reader-test)

;;; nelisp-stdlib-reader-test.el ends here
