;;; emacs-string-properties-test.el --- string property substrate checks -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-string-properties-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(dolist (dir '("packages/nelisp-emacs-foundation/src"
               "packages/nelisp-emacs-buffer-core/src"
               "packages/nelisp-emacs-text-core/src"))
  (add-to-list 'load-path (expand-file-name dir emacs-string-properties-test--root)))

(load "nelisp-emacs-compat" nil t)
(load "emacs-buffer" nil t)
(load "emacs-buffer-builtins" nil t)
(load "emacs-string" nil t)

(ert-deftest emacs-string-properties-test/string-ranges-copy-and-mutate ()
  "String sidecars use zero-based ranges and copy independently."
  (let* ((source (copy-sequence "abcd"))
         (copy (copy-sequence source)))
    (emacs-buffer-string-text-property 'put source 1 3 'help-echo "source")
    (emacs-buffer-string-text-property 'copy source copy)
    (emacs-buffer-string-text-property 'add copy 0 4 '(face bold))
    (should-not (emacs-buffer-string-text-property 'get source 0 'help-echo))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get source 1 'help-echo)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 0 'face)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 3 'face)))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get copy 1 'help-echo)))
    (emacs-buffer-string-text-property 'set copy 2 3 '(face italic))
    (should (eq 'italic (emacs-buffer-string-text-property 'get copy 2 'face)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 1 'face)))
    (emacs-buffer-string-text-property 'remove copy 1 2 '(help-echo))
    (should-not (emacs-buffer-string-text-property 'get copy 1 'help-echo))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get source 1 'help-echo)))))

(ert-deftest emacs-string-properties-test/end-boundary-is-readable ()
  "String property reads at length and at empty-string position zero are nil."
  (should-not (get-text-property 4 'face "abcd"))
  (should-not (text-properties-at 4 "abcd"))
  (should-not (get-text-property 0 'face ""))
  (should-not (text-properties-at 0 "")))

(ert-deftest emacs-string-properties-test/insert-copies-runs-at-offsets ()
  "Insertion transfers string ranges and preserves shifted buffer ranges."
  (let* ((source (copy-sequence "abcd"))
         (copy (copy-sequence source))
         (buffer (nelisp-ec-generate-new-buffer "*string-properties-test*"))
         (old-buffer (nelisp-ec-current-buffer)))
    (emacs-buffer-string-text-property 'put source 1 3 'face 'bold)
    (emacs-buffer-string-text-property 'copy source copy)
    (unwind-protect
        (progn
          (nelisp-ec-set-buffer buffer)
          (nelisp-ec-insert "tail")
          (emacs-buffer-put-text-property 2 4 'rear-nonsticky t buffer)
          (nelisp-ec--set-buffer-point buffer 1)
          (nelisp-ec-insert "A" copy "B")
          (should (equal "AabcdBtail" (nelisp-ec-buffer-string)))
          (should-not (emacs-buffer-get-text-property 2 'face buffer))
          (should (eq 'bold (emacs-buffer-get-text-property 3 'face buffer)))
          (should (eq 'bold (emacs-buffer-get-text-property 4 'face buffer)))
          (should-not (emacs-buffer-get-text-property 5 'face buffer))
          (should-not (emacs-buffer-get-text-property 7 'rear-nonsticky buffer))
          (should (eq t (emacs-buffer-get-text-property 8 'rear-nonsticky buffer)))
          (should (eq t (emacs-buffer-get-text-property 9 'rear-nonsticky buffer)))
          (should-not (emacs-buffer-get-text-property 10 'rear-nonsticky buffer)))
      (when old-buffer (nelisp-ec-set-buffer old-buffer)))))

(ert-deftest emacs-string-properties-test/public-insert-preserves-sidecar-runs ()
  "The public native INSERT path exposes ranges through the property API."
  (let* ((source (copy-sequence "xy"))
         (styled (propertize source 'face 'bold)))
    (with-temp-buffer
      (insert "L" styled "R")
      (should (equal "LxyR" (buffer-string)))
      (should-not (get-text-property 1 'face))
      (should (eq 'bold (get-text-property 2 'face)))
      (should (eq 'bold (get-text-property 3 'face)))
      (should-not (get-text-property 4 'face))
      (should-not (emacs-buffer-string-text-property 'get source 0 'face)))))

(ert-deftest emacs-string-properties-test/buffer-ranges-copy-to-strings ()
  "Buffer sidecar ranges become zero-based string runs without changing text."
  (with-temp-buffer
    (insert "abcd")
    (let* ((buffer (current-buffer))
           (ext (emacs-buffer--ensure-text-property-ext buffer))
           (text (buffer-substring-no-properties 1 5)))
      (setf (emacs-buffer--ext-text-props ext)
            (emacs-buffer--tp-add nil 2 4 '(face bold)))
      (emacs-buffer--copy-buffer-properties-to-string buffer 1 5 text)
      (should (equal "abcd" text))
      (should-not (emacs-buffer-string-text-property 'get text 0 'face))
      (should (eq 'bold (emacs-buffer-string-text-property 'get text 1 'face)))
      (should (eq 'bold (emacs-buffer-string-text-property 'get text 2 'face)))
      (should-not (emacs-buffer-string-text-property 'get text 3 'face)))))

(ert-deftest emacs-string-properties-test/gnu-propertize-binding-is-preserved ()
  "Loading the fallback source does not replace an existing GNU propertize."
  (let ((before (symbol-function 'propertize)))
    (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-string.el"
                            emacs-string-properties-test--root)
          nil t)
    (should (eq before (symbol-function 'propertize)))))

(provide 'emacs-string-properties-test)
;;; emacs-string-properties-test.el ends here
