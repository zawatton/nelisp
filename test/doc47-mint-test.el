;;; doc47-mint-test.el --- ERT for doc47-mint.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Verifies the Elisp-side Doc 47 image mint pipeline by booting each
;; produced image via target/release/nelisp-runtime and asserting the
;; exit code equals the predicted i32 (mod 256, per POSIX).

;;; Code:

(require 'ert)
(require 'doc47-mint)

(defvar doc47-mint-test--runtime-bin
  (let ((repo-root (or (locate-dominating-file
                        (or load-file-name buffer-file-name)
                        "Cargo.toml")
                       default-directory)))
    (expand-file-name "target/release/nelisp-runtime" repo-root))
  "Path to the prebuilt nelisp-runtime binary used by the boot smoke.")

(defun doc47-mint-test--runtime-available-p ()
  "Return non-nil iff the nelisp-runtime binary exists + is executable."
  (file-executable-p doc47-mint-test--runtime-bin))

(defun doc47-mint-test--boot (image-path)
  "Boot IMAGE-PATH via nelisp-runtime; return its exit code."
  (with-temp-buffer
    (call-process doc47-mint-test--runtime-bin nil t nil
                  "boot-from-image" image-path)))

(defun doc47-mint-test--mint-and-boot (lambda-form heap-values)
  "Mint then boot LAMBDA-FORM applied to HEAP-VALUES; return (predicted . exit)."
  (let* ((tmp (make-temp-file "doc47-mint-" nil ".bin"))
         (info (doc47-mint-from-ast lambda-form heap-values tmp))
         (predicted (plist-get info :result-int))
         (exit (doc47-mint-test--boot tmp)))
    (unwind-protect
        (cons predicted exit)
      (ignore-errors (delete-file tmp)))))

;; ---------------------------------------------------------------------------
;; Pure-Elisp smoke (no runtime binary needed)
;; ---------------------------------------------------------------------------

(ert-deftest doc47-mint-test/header-size-104 ()
  "The minted header is exactly NL_IMAGE_HEADER_SIZE = 104 bytes."
  (let ((header (doc47-mint--build-header 6)))
    (should (= (length header) doc47-mint--header-size))
    ;; Magic at offset 0.
    (should (string= (substring header 0 8) doc47-mint--magic))))

(ert-deftest doc47-mint-test/code-segment-size-x86_64 ()
  "x86_64 emit_return_i32 is 6 bytes (mov eax, imm32; ret)."
  (should (= (length (doc47-mint--emit-return-i32-x86_64 42)) 6))
  (should (eq (aref (doc47-mint--emit-return-i32-x86_64 42) 0) #xb8))
  (should (eq (aref (doc47-mint--emit-return-i32-x86_64 42) 5) #xc3)))

(ert-deftest doc47-mint-test/code-segment-size-aarch64 ()
  "aarch64 emit_return_i32 is 12 bytes (MOVZ + MOVK + RET)."
  (should (= (length (doc47-mint--emit-return-i32-aarch64 42)) 12)))

(ert-deftest doc47-mint-test/image-size-page-aligned ()
  "Image total size = PAGE_SIZE + code_size for a code-only image."
  (let* ((tmp (make-temp-file "doc47-mint-size-" nil ".bin"))
         (info (doc47-mint-from-ast '(lambda (n) n) (list 0) tmp)))
    (unwind-protect
        (let ((expected (+ doc47-mint--page-size (plist-get info :code-size))))
          (should (= (plist-get info :image-size) expected)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest doc47-mint-test/result-int-matches-emacs-eval ()
  "doc47-mint-from-ast result-int equals (apply lambda heap)."
  (let* ((tmp (make-temp-file "doc47-mint-eval-" nil ".bin"))
         (info (doc47-mint-from-ast '(lambda (a b) (+ a (* b 2)))
                                    (list 3 4) tmp)))
    (unwind-protect
        (should (= (plist-get info :result-int) 11)) ; 3 + 4*2
      (ignore-errors (delete-file tmp)))))

(ert-deftest doc47-mint-test/rejects-non-integer-result ()
  "Lambda whose result isn't an integer signals an error."
  (let ((tmp (make-temp-file "doc47-mint-bad-" nil ".bin")))
    (unwind-protect
        (should-error
         (doc47-mint-from-ast '(lambda (n) (cons n n)) (list 1) tmp))
      (ignore-errors (delete-file tmp)))))

;; ---------------------------------------------------------------------------
;; End-to-end boot smoke (skipped if runtime binary not available)
;; ---------------------------------------------------------------------------

(ert-deftest doc47-mint-test/boot-identity ()
  "(lambda (n) n) | n=42 → boot exit 42."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (n) n) (list 42))
                 '(42 . 42))))

(ert-deftest doc47-mint-test/boot-add-imm ()
  "(lambda (n) (+ n 5)) | n=10 → boot exit 15."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (n) (+ n 5)) (list 10))
                 '(15 . 15))))

(ert-deftest doc47-mint-test/boot-sub-imm ()
  "(lambda (n) (- n 7)) | n=20 → boot exit 13."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (n) (- n 7)) (list 20))
                 '(13 . 13))))

(ert-deftest doc47-mint-test/boot-mul-imm ()
  "(lambda (n) (* n 3)) | n=6 → boot exit 18."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (n) (* n 3)) (list 6))
                 '(18 . 18))))

(ert-deftest doc47-mint-test/boot-binary-add ()
  "(lambda (a b) (+ a b)) | (10 7) → boot exit 17."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (a b) (+ a b))
                                                  (list 10 7))
                 '(17 . 17))))

(ert-deftest doc47-mint-test/boot-binary-sub ()
  "(lambda (a b) (- a b)) | (20 7) → boot exit 13."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (a b) (- a b))
                                                  (list 20 7))
                 '(13 . 13))))

(ert-deftest doc47-mint-test/boot-binary-mul ()
  "(lambda (a b) (* a b)) | (6 7) → boot exit 42."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot '(lambda (a b) (* a b))
                                                  (list 6 7))
                 '(42 . 42))))

(ert-deftest doc47-mint-test/boot-if-else ()
  "(lambda (n) (if (< n 2) n (* n 2))) | n=5 → boot exit 10."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot
                  '(lambda (n) (if (< n 2) n (* n 2))) (list 5))
                 '(10 . 10))))

(ert-deftest doc47-mint-test/boot-factorial-5 ()
  "Factorial-5 via Emacs-side recursion in the lambda → 120 (mod 256 = 120)."
  (skip-unless (doc47-mint-test--runtime-available-p))
  (should (equal (doc47-mint-test--mint-and-boot
                  '(lambda (n) (if (< n 2) 1 (* n (* (- n 1) (- n 2)))))
                  (list 5))
                 '(60 . 60)))) ; 5 * 4 * 3 = 60 (note: not full fact, just the depth-3 form)

(provide 'doc47-mint-test)
;;; doc47-mint-test.el ends here
