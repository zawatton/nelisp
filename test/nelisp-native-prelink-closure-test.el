;;; nelisp-native-prelink-closure-test.el --- ERT tests for the prelink closure port  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs --batch -Q -L lisp -L scripts -l test/nelisp-native-prelink-closure-test.el \
;;     -f ert-run-tests-batch-and-exit
;; The expected strings are the output of Python's `json.dumps(..., indent=2)'.
;; The end-to-end proof against the Python scripts lives in
;; tools/ai/prelink-closure-diff/.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-x86-64-decode)
(load (expand-file-name
       "../scripts/nelisp-native-rooted-prelink-closure.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(defun nelisp-native-prelink-closure-test--enc (value)
  "Encode VALUE with the port's emitter."
  (nelisp-native-rooted-prelink-closure-encode value))

(ert-deftest nelisp-native-prelink-closure-empty-containers ()
  (should (equal (nelisp-native-prelink-closure-test--enc []) "[]\n"))
  (should (equal (nelisp-native-prelink-closure-test--enc '(:object)) "{}\n")))

(ert-deftest nelisp-native-prelink-closure-nested-layout ()
  (should (equal (nelisp-native-prelink-closure-test--enc
                  (list :object
                        (cons "a" [])
                        (cons "b" '(:object))
                        (cons "c" (vector '(:object) []
                                          (list :object (cons "x" -5))))
                        (cons "k" (vector 1 (vector 2 [])))))
                 (concat "{\n  \"a\": [],\n  \"b\": {},\n  \"c\": [\n    {},\n    [],\n"
                         "    {\n      \"x\": -5\n    }\n  ],\n  \"k\": [\n    1,\n"
                         "    [\n      2,\n      []\n    ]\n  ]\n}\n"))))

(ert-deftest nelisp-native-prelink-closure-scalars ()
  (should (equal (nelisp-native-prelink-closure-test--enc
                  (list :object (cons "n" :null) (cons "t" t) (cons "f" :false)
                        (cons "big" 12345678901234567890) (cons "z" 0)
                        (cons "neg" -123)))
                 (concat "{\n  \"n\": null,\n  \"t\": true,\n  \"f\": false,\n"
                         "  \"big\": 12345678901234567890,\n  \"z\": 0,\n"
                         "  \"neg\": -123\n}\n"))))

(ert-deftest nelisp-native-prelink-closure-string-escaping ()
  ;; e-acute, U+1F600, DEL, newline, tab, quote, backslash, SOH, BS, FF, slash.
  (should (equal (nelisp-native-prelink-closure-test--enc
                  (string #xe9 #x1f600 #x7f ?\n ?\t ?\" ?\\ 1 8 12 ?/))
                 "\"\\u00e9\\ud83d\\ude00\\u007f\\n\\t\\\"\\\\\\u0001\\b\\f/\"\n"))
  ;; A raw byte (Python surrogateescape) and plain ASCII.
  (should (equal (nelisp-native-prelink-closure-test--enc (string #x3fff80))
                 "\"\\udc80\"\n"))
  (should (equal (nelisp-native-prelink-closure-test--enc "nl_alloc_symbol")
                 "\"nl_alloc_symbol\"\n")))

(ert-deftest nelisp-native-prelink-closure-unsupported-value ()
  (should-error (nelisp-native-prelink-closure-test--enc 1.5))
  (should-error (nelisp-native-prelink-closure-test--enc nil)))

(ert-deftest nelisp-native-prelink-closure-object-set ()
  (let ((object (list :object (cons "domain" "a") (cons "status" "b"))))
    (nelisp-native-rooted-prelink-closure-object-set object "domain" "c")
    (nelisp-native-rooted-prelink-closure-object-set object "extra" 1)
    (should (equal object '(:object ("domain" . "c") ("status" . "b") ("extra" . 1))))))

(ert-deftest nelisp-native-prelink-closure-parse-args ()
  (let ((parsed (nelisp-native-rooted-prelink-closure-parse-args
                 '("--metadata" "m" "--root" "a" "--output=o" "--root" "b"
                   "--metadata" "m2")
                 '("metadata" "root" "output"))))
    (should (equal (cdr (assoc "metadata" parsed)) "m2"))
    (should (equal (cdr (assoc "output" parsed)) "o"))
    (should (equal (cdr (assoc "root" parsed)) '("a" "b"))))
  (should-error (nelisp-native-rooted-prelink-closure-parse-args
                 '("--bogus" "x") '("metadata")))
  (should-error (nelisp-native-rooted-prelink-closure-parse-args
                 '("positional") '("metadata")))
  (should-error (nelisp-native-rooted-prelink-closure-parse-args
                 '("--metadata") '("metadata")))
  (should-error (nelisp-native-rooted-prelink-closure-require-options
                 '(("metadata" . "m")) '("metadata" "output"))))

(ert-deftest nelisp-native-prelink-closure-main-refuses-incomplete-cli ()
  ;; --manifest without --source-root is refused like argparse's parser.error.
  (should-error (nelisp-native-rooted-prelink-closure-main
                 '("--metadata" "m" "--unit-directory" "d" "--generated-data" "g"
                   "--root" "r" "--output" "o" "--manifest" "x"))
                :type 'error)
  (should-error (nelisp-native-rooted-prelink-closure-main '("--root" "r"))
                :type 'error))

(ert-deftest nelisp-native-prelink-closure-hex-slice ()
  (let ((raw "xx00ff7f80ab"))
    (should (equal (nelisp-native-rooted-prelink-closure--hex-slice raw 2 1 3)
                   (unibyte-string #xff #x7f)))
    (should (equal (nelisp-native-rooted-prelink-closure--hex-slice raw 2 0 4)
                   (unibyte-string 0 #xff #x7f #x80)))))

(ert-deftest nelisp-native-prelink-closure-py-path-name ()
  (dolist (case '(("a/b" . "b") ("a/" . "a") ("" . "") ("." . "") ("a/." . "a")
                  ("a/./b" . "b") (".." . "..") ("/x/y.unit" . "y.unit")))
    (should (equal (nelisp-native-rooted-prelink-closure--py-name (car case))
                   (cdr case)))))

(provide 'nelisp-native-prelink-closure-test)
;;; nelisp-native-prelink-closure-test.el ends here
