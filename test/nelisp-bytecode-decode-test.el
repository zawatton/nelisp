;;; nelisp-bytecode-decode-test.el --- Isolated decoder test wrapper -*- lexical-binding: t; -*-

(require 'ert)

(defconst nelisp-bytecode-decode-test-selftest-file
  (expand-file-name "../tools/nelisp-bytecode-decode-selftest.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Self-test script run in a fresh Emacs process.")

(ert-deftest nelisp-bytecode-decode-selftest ()
  ;; Keep corpus library definitions and tool loading out of the shared suite.
  (with-temp-buffer
    (let* ((status (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil (current-buffer) nil
                    "-Q" "--batch" "-l"
                    nelisp-bytecode-decode-test-selftest-file))
           (output (buffer-string)))
      (ert-info ((format "Decoder self-test exit: %S\n%s" status output))
        (should (equal status 0))
        (should (string-match-p
                 "^Parity: [0-9]+ source functions," output))))))

;;; nelisp-bytecode-decode-test.el ends here
