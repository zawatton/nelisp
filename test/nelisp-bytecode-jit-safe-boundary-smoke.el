;;; nelisp-bytecode-jit-safe-boundary-smoke.el --- driver boundary hook -*- lexical-binding: t; -*-

;; Run with `emacs --batch -Q -l test/nelisp-bytecode-jit-safe-boundary-smoke.el'.
;; NELISP_BOUNDARY_BIN may point at a versioned standalone-reader candidate.

(require 'subr-x)

(defconst nelisp-bytecode-jit-safe-boundary--binary
  (or (getenv "NELISP_BOUNDARY_BIN")
      (expand-file-name "target/nelisp" default-directory)))

(defun nelisp-bytecode-jit-safe-boundary--run (action argument)
  (let ((out (generate-new-buffer " *boundary-out*"))
        (err (make-temp-file "nelisp-boundary-stderr-"))
        (rc nil))
    (unwind-protect
        (progn
          (setq rc (call-process nelisp-bytecode-jit-safe-boundary--binary nil
                                 (list out err) nil action argument))
          (list rc (with-current-buffer out (buffer-string))
                (with-temp-buffer
                  (insert-file-contents err)
                  (buffer-string))))
      (kill-buffer out)
      (delete-file err))))

(defun nelisp-bytecode-jit-safe-boundary--assert (condition message)
  (unless condition (error "safe-boundary smoke: %s" message)))

(defun nelisp-bytecode-jit-safe-boundary--count (regexp string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (how-many regexp)))

(defun nelisp-bytecode-jit-safe-boundary--source-file (source)
  (let ((file (make-temp-file "nelisp-boundary-source-" nil ".el")))
    (with-temp-file file (insert source))
    file))

(defun nelisp-bytecode-jit-safe-boundary--callback-source ()
  (concat
   "(defvar nelisp-bytecode-jit--deferred-preparation-enabled nil) "
   "(defvar p5-boundary-count 0) "
   "(defvar p5-boundary-effects 0) "
   "(defvar p5-boundary-fail nil) "
   "(defun nelisp-bytecode-jit-drain-pending () "
   "  (setq p5-boundary-count (+ p5-boundary-count 1)) "
   "  (princ (format \"drain-called-%d\\n\" p5-boundary-count)) "
   "  (garbage-collect) "
   "  (if p5-boundary-fail "
   "      (signal 'p5-safe-boundary-callback-error "
   "              (list p5-boundary-effects)) "
   "    0)) "
   "(setq nelisp-bytecode-jit--deferred-preparation-enabled t) "))

(let ((nested (make-temp-file "nelisp-boundary-nested-" nil ".el"))
      (sources nil))
  (unwind-protect
      (progn
        (nelisp-bytecode-jit-safe-boundary--assert
         (file-executable-p nelisp-bytecode-jit-safe-boundary--binary)
         (format "standalone binary is not executable: %s"
                 nelisp-bytecode-jit-safe-boundary--binary))
        (with-temp-file nested
          (insert
           "(setq p5-boundary-nested-count p5-boundary-count)\n"))

        ;; The nested load sees no callback yet. The one outer callback then
        ;; forces GC while the cons result is held by the driver's `out' root.
        (let* ((source (nelisp-bytecode-jit-safe-boundary--source-file
                        (concat "(progn "
                                (nelisp-bytecode-jit-safe-boundary--callback-source)
                                "(defvar p5-boundary-nested-count -1) "
                                "(load " (prin1-to-string nested) ") "
                                "(cons p5-boundary-nested-count 77))\n")))
               (result (nelisp-bytecode-jit-safe-boundary--run "--load" source))
               (output (cadr result)))
          (push source sources)
          (nelisp-bytecode-jit-safe-boundary--assert
           (and (= (car result) 0)
                (= (nelisp-bytecode-jit-safe-boundary--count "drain-called-" output) 1)
                (string-match-p "(0 \\. 77)" output)
                (string-empty-p (nth 2 result)))
           (format "nested/GC/result: %S" result)))

        ;; An absent service and a nil/absent enable flag both leave the
        ;; successful user's result alone.
        (let ((result (nelisp-bytecode-jit-safe-boundary--run "--eval" "(+ 40 2)")))
          (nelisp-bytecode-jit-safe-boundary--assert
           (and (= (car result) 0) (string-match-p "42" (cadr result))
                (not (string-match-p "drain-called-" (cadr result)))
                (string-empty-p (nth 2 result)))
           (format "default-disabled: %S" result)))
        (let ((result
               (let ((source (nelisp-bytecode-jit-safe-boundary--source-file
                              "(progn (defvar nelisp-bytecode-jit--deferred-preparation-enabled t) (princ 42))\n")))
                 (push source sources)
                 (nelisp-bytecode-jit-safe-boundary--run "--load" source))))
          (nelisp-bytecode-jit-safe-boundary--assert
           (and (= (car result) 0) (string-match-p "42" (cadr result))
                (string-empty-p (nth 2 result)))
           (format "missing service: %S" result)))

        ;; An original eval error must skip the hook.
        (let* ((source (nelisp-bytecode-jit-safe-boundary--source-file
                        (concat "(progn "
                                (nelisp-bytecode-jit-safe-boundary--callback-source)
                                "(signal 'p5-original-error '(7)))\n")))
               (result (nelisp-bytecode-jit-safe-boundary--run "--load" source))
               (output (concat (cadr result) (nth 2 result))))
          (push source sources)
          (nelisp-bytecode-jit-safe-boundary--assert
           (and (/= (car result) 0)
                (string-match-p "p5-original-error" output)
                (not (string-match-p "drain-called-" output)))
           (format "original error skip: %S" result)))

        ;; The callback receives the effects count after one eval. Its error
        ;; is returned, proving the successful expression was not replayed.
        (let* ((source (nelisp-bytecode-jit-safe-boundary--source-file
                        (concat "(progn "
                                (nelisp-bytecode-jit-safe-boundary--callback-source)
                                "(setq p5-boundary-fail t) "
                                "(setq p5-boundary-effects (+ p5-boundary-effects 1)) "
                                "42)\n")))
               (result (nelisp-bytecode-jit-safe-boundary--run "--load" source))
               (output (concat (cadr result) (nth 2 result))))
          (push source sources)
          (nelisp-bytecode-jit-safe-boundary--assert
           (and (/= (car result) 0)
                (= (nelisp-bytecode-jit-safe-boundary--count "drain-called-" output) 1)
                (string-match-p "p5-safe-boundary-callback-error" output)
                (string-match-p "(1)" output))
           (format "callback error/no replay: %S" result)))
        (princ "nelisp-bytecode-jit-safe-boundary-smoke: PASS (5 cases)\n"))
    (dolist (source sources)
      (when (file-exists-p source) (delete-file source)))
    (when (file-exists-p nested) (delete-file nested))))

;;; nelisp-bytecode-jit-safe-boundary-smoke.el ends here
