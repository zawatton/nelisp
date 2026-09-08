;;; nelisp-u8-cidx-loop-test.el --- bounded UTF-8 index walk -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defconst nelisp-u8-cidx-loop-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory
                         (or load-file-name buffer-file-name)))))

(ert-deftest nelisp-u8-cidx-loop/generated-helper-is-iterative ()
  "The shared character-index helper has no recursive call site."
  (let ((form (cl-find 'nl_u8_cidx_byte
                       nelisp-standalone--applyfn-m5-helpers
                       :key (lambda (item) (and (consp item) (cadr item))))))
    (should form)
    (should (memq 'while (flatten-tree form)))
    ;; The sole occurrence is the defun name itself.
    (should (= 1 (cl-count 'nl_u8_cidx_byte (flatten-tree form))))))

(ert-deftest nelisp-u8-cidx-loop/long-string-probes ()
  "Check mutation and exceed the old recursive walk's 1 GiB stack."
  (let* ((binary (cl-find-if
                  #'file-executable-p
                  (mapcar (lambda (name)
                            (expand-file-name name nelisp-u8-cidx-loop-test--root))
                          (if (eq system-type 'windows-nt)
                              '("target/nelisp.exe" "target/nelisp")
                            '("target/nelisp" "target/nelisp.exe")))))
         (timeout (or (executable-find "gtimeout")
                      (and (not (eq system-type 'windows-nt))
                           (executable-find "timeout")))))
    (unless binary (ert-skip "no executable target/nelisp[.exe]"))
    (unless timeout (ert-skip "GNU timeout/gtimeout is required"))
    (dolist (case '(("(let ((s (make-string 300000 97))) (aset s 299999 98) (list (aref s 299999) (substring s 299997 300000)))"
                    . "(98 \"aab\")")
                   ;; The negative start and omitted end exercise the slow
                   ;; native substring path, not T95's cached positive path.
                   ("(let* ((s (make-string 5000000 12354)) (tail (substring s -3))) (list (length tail) (aref tail 0) (aref tail 2)))"
                    . "(3 12354 12354)")))
      (with-temp-buffer
        (let ((rc (call-process timeout nil t nil "30s" binary "--eval"
                                (car case))))
          (should (= rc 0))
          (should (equal (string-trim (buffer-string)) (cdr case))))))))

(ert-deftest nelisp-u8-cidx-loop/long-multibyte-substring ()
  "Walk three- and four-byte characters with a bounded native stack."
  (let* ((binary (expand-file-name
                  (if (eq system-type 'windows-nt)
                      "target/nelisp.exe" "target/nelisp")
                  nelisp-u8-cidx-loop-test--root))
         (timeout (or (executable-find "gtimeout")
                      (and (not (eq system-type 'windows-nt))
                           (executable-find "timeout")))))
    (unless (file-executable-p binary) (ert-skip "no standalone binary"))
    (unless timeout (ert-skip "GNU timeout/gtimeout is required"))
    (with-temp-buffer
      (let ((rc (call-process
                 timeout nil t nil "30s" binary "--eval"
                 "(let* ((s (concat (make-string 100000 12354) (make-string 100000 128512) \"z\")) (tail (substring s 199998)) (cross (substring s 99999 100001))) (and (= (length s) 200001) (= (aref s 99999) 12354) (= (aref s 100000) 128512) (= (length tail) 3) (= (aref tail 0) 128512) (= (aref tail 1) 128512) (= (aref tail 2) 122) (= (length cross) 2) (= (aref cross 0) 12354) (= (aref cross 1) 128512) (equal (substring s -3) tail) (equal (substring s 200001) \"\")))")))
        (should (= rc 0))
        (should (equal (string-trim (buffer-string)) "t"))))))

(provide 'nelisp-u8-cidx-loop-test)
;;; nelisp-u8-cidx-loop-test.el ends here
