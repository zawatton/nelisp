;;; nelisp-reader-lexer-hoist-test.el --- structural test for lexer pointer hoisting  -*- lexical-binding: t; -*-

;; The hot reader loops must derive the string data pointer once per scan
;; (`str-data-ptr') and read bytes with `ptr-byte-at'; loops that re-enter the
;; non-hoisted escape decoder must re-derive it through the wrapper.
;; NELISP_LEXER_ROOT may point at a directory holding the three files.

(require 'ert)
(require 'cl-lib)

(defconst nelisp-reader-lexer-hoist-test--root
  (or (getenv "NELISP_LEXER_ROOT")
      (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-reader-lexer-hoist-test--file (name)
  (let ((r nelisp-reader-lexer-hoist-test--root))
    (expand-file-name name (if (file-exists-p (expand-file-name name r)) r
                             (expand-file-name "lisp" r)))))

(defun nelisp-reader-lexer-hoist-test--defun (file name)
  "Return the text of defun NAME in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents (nelisp-reader-lexer-hoist-test--file file))
    (when (re-search-forward (format "^ *(defun %s " (regexp-quote name)) nil t)
      (goto-char (match-beginning 0))
      (buffer-substring (point) (progn (forward-sexp) (point))))))

(defconst nelisp-reader-lexer-hoist-test--loops
  '("nelisp_reader_skip_comment_p" "nelisp_reader_skip_ws_p"
    "nelisp_reader_scan_atom_p" "nelisp_reader_scan_atom_escape_p"
    "nelisp_reader_classify_step_p" "nelisp_reader_string_body_p"
    "nelisp_reader_string_plain_p"))

(ert-deftest nelisp-reader-lexer-hoist-loops-use-pointer ()
  (dolist (n nelisp-reader-lexer-hoist-test--loops)
    (let ((txt (nelisp-reader-lexer-hoist-test--defun "nelisp-cc-reader-lexer.el" n)))
      (should txt)
      (should (string-match-p "ptr-byte-at dp" txt))
      (should-not (string-match-p "str-byte-at" txt))
      (should-not (string-match-p "str-data-ptr" txt)))))

(ert-deftest nelisp-reader-lexer-hoist-wrappers-derive-once ()
  (dolist (n '("nelisp_reader_skip_ws" "nelisp_reader_scan_atom"
               "nelisp_reader_classify_step" "nelisp_reader_string_body"))
    (let ((txt (nelisp-reader-lexer-hoist-test--defun "nelisp-cc-reader-lexer.el" n)))
      (should txt)
      (should (string-match-p "(str-data-ptr str-ptr)" txt))
      (should-not (string-match-p "str-byte-at" txt)))))

(ert-deftest nelisp-reader-lexer-hoist-rederive-after-escape ()
  ;; The escape decoder allocates and is not hoisted: it receives STR-PTR
  ;; only, and its re-entry into the body goes through the wrapper.
  (let ((txt (nelisp-reader-lexer-hoist-test--defun
              "nelisp-cc-reader-lexer.el" "nelisp_reader_string_body_p")))
    (should (string-match-p "(nelisp_reader_string_escape str-ptr " txt))
    (should-not (string-match-p "(nelisp_reader_string_escape[^)]*dp" txt)))
  (let ((txt (nelisp-reader-lexer-hoist-test--defun
              "nelisp-cc-reader-lexer.el" "nelisp_reader_string_escape")))
    (should (string-match-p "(nelisp_reader_string_body str-ptr" txt))
    (should-not (string-match-p "string_body_p" txt))))

(ert-deftest nelisp-reader-lexer-hoist-accessor-ops ()
  (with-temp-buffer
    (insert-file-contents (nelisp-reader-lexer-hoist-test--file "nelisp-heap-accessors.el"))
    (should (search-forward "(eq (car source) 'str-data-ptr))" nil t))
    (should (search-forward "((eq (car source) 'ptr-byte-at)" nil t))))

(provide 'nelisp-reader-lexer-hoist-test)
