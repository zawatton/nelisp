;;; nelisp-ai-host-json-test.el --- Host runner must retain GNU JSON -*- lexical-binding: t; -*-
(require 'ert)
(require 'json)

(ert-deftest nelisp-ai-host-json-reads-a-file ()
  "A standalone compatibility package must not replace the host tooling API."
  (let ((file (make-temp-file "nelisp-host-json-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "{\"answer\":42}"))
          (should (fboundp 'json-read-file))
          (should (equal (let ((json-object-type 'alist)
                               (json-key-type 'symbol))
                           (json-read-file file))
                         '((answer . 42)))))
      (delete-file file))))
