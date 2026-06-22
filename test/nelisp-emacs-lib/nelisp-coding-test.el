;;; nelisp-coding-test.el --- tests for nelisp-coding public API  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-coding)

(ert-deftest nelisp-coding-test/default-policy-constants ()
  (should-not nelisp-coding-utf8-bom-emit-on-write)
  (should (eq 'replace nelisp-coding-error-strategy))
  (should (= ?\? nelisp-coding-latin1-replacement-codepoint))
  (should (equal '(#xEF #xBB #xBF) nelisp-coding-utf8-bom))
  (should (= #xFFFD nelisp-coding-utf8-replacement-char))
  (should (= #x10FFFF nelisp-coding-utf8-max-codepoint))
  (should (= #xD800 nelisp-coding-utf8-surrogate-min))
  (should (= #xDFFF nelisp-coding-utf8-surrogate-max))
  (should (= #xFF nelisp-coding-latin1-max-codepoint))
  (should (integerp nelisp-coding-stream-default-chunk-size)))

(ert-deftest nelisp-coding-test/utf8-round-trip-and-bom ()
  (let* ((text (concat "A" (string #xE9) (string #x1F600)))
         (bytes (nelisp-coding-utf8-encode text)))
    (should (equal text
                   (plist-get (nelisp-coding-utf8-decode bytes) :string)))
    (should (equal (apply #'unibyte-string bytes)
                   (nelisp-coding-utf8-encode-string text)))
    (should (equal nelisp-coding-utf8-bom
                   (seq-take (nelisp-coding-utf8-encode text t) 3)))
    (should (equal text
                   (plist-get
                    (nelisp-coding-utf8-decode
                     (append nelisp-coding-utf8-bom bytes))
                    :string)))))

(ert-deftest nelisp-coding-test/latin1-round-trip-and-replacement ()
  (should (equal "ABC"
                 (plist-get
                  (nelisp-coding-latin1-decode '(#x41 #x42 #x43))
                  :string)))
  (should (equal '(#x41 #xE9)
                 (plist-get
                  (nelisp-coding-latin1-encode
                   (concat "A" (string #xE9)))
                  :bytes)))
  (let ((result (nelisp-coding-latin1-encode
                 (concat "A" (string #x100))
                 'replace)))
    (should (equal '(#x41 #x3F) (plist-get result :bytes)))
    (should (equal '(1) (plist-get result :invalid-positions)))
    (should (= 1 (plist-get result :replacements))))
  (should (equal (unibyte-string #x41 #x3F)
                 (nelisp-coding-latin1-encode-string
                  (concat "A" (string #x100))))))

(ert-deftest nelisp-coding-test/japanese-ascii-and-halfwidth-smokes ()
  (should (nelisp-coding-jis-tables-rebuild))
  (should (equal (concat "A" (string #xFF66))
                 (plist-get
                  (nelisp-coding-shift-jis-decode '(#x41 #xA6))
                  :string)))
  (should (equal '(#x41 #xA6)
                 (plist-get
                  (nelisp-coding-shift-jis-encode
                   (concat "A" (string #xFF66)))
                  :bytes)))
  (should (equal (unibyte-string #x41 #xA6)
                 (nelisp-coding-shift-jis-encode-string
                  (concat "A" (string #xFF66)))))
  (should (equal (concat "A" (string #xFF66))
                 (plist-get
                  (nelisp-coding-euc-jp-decode '(#x41 #x8E #xA6))
                  :string)))
  (should (equal '(#x41 #x8E #xA6)
                 (plist-get
                  (nelisp-coding-euc-jp-encode
                   (concat "A" (string #xFF66)))
                  :bytes)))
  (should (equal (unibyte-string #x41 #x8E #xA6)
                 (nelisp-coding-euc-jp-encode-string
                  (concat "A" (string #xFF66))))))

(ert-deftest nelisp-coding-test/stream-decode-and-encode ()
  (let ((state (nelisp-coding-stream-state-create 'utf-8 'decode)))
    (nelisp-coding-stream-decode-chunk state '(#x41 #xC3))
    (nelisp-coding-stream-decode-chunk state '(#xA9))
    (should (equal (concat "A" (string #xE9))
                   (plist-get
                    (nelisp-coding-stream-decode-finalize state)
                    :string))))
  (let ((state (nelisp-coding-stream-state-create 'latin-1 'encode)))
    (nelisp-coding-stream-encode-chunk state
                                       (concat "A" (string #x100)))
    (let ((result (nelisp-coding-stream-encode-finalize state)))
      (should (equal '(#x41 #x3F) (plist-get result :bytes)))
      (should (equal '(1) (plist-get result :invalid-positions))))))

(ert-deftest nelisp-coding-test/file-io-wrappers ()
  (let ((file (make-temp-file "nelisp-coding-test-")))
    (unwind-protect
        (let ((text (concat "A" (string #xE9))))
          (let ((write-result
                 (nelisp-coding-write-file-with-encoding
                  file text 'utf-8 nil 1)))
            (should (equal file (plist-get write-result :path))))
          (let ((read-result
                 (nelisp-coding-read-file-with-encoding
                  file 'utf-8 nil 1)))
            (should (equal file (plist-get read-result :path)))
            (should (equal text (plist-get read-result :string)))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest nelisp-coding-test/lazy-jis-table-bindings ()
  (require 'nelisp-coding-jis-tables)
  (should (boundp 'nelisp-coding-shift-jis-x0208-decode-table))
  (should (boundp 'nelisp-coding-cp932-extension-decode-table))
  (should (boundp 'nelisp-coding-euc-jp-x0208-decode-table))
  (should (boundp 'nelisp-coding-euc-jp-x0212-decode-table))
  (should (boundp 'nelisp-coding-jis-tables-sha256))
  (should (fboundp 'nelisp-coding-jis-tables-verify-hash)))

(provide 'nelisp-coding-test)

;;; nelisp-coding-test.el ends here
