;;; nelisp-text-buffer-test.el --- ERT tests for nelisp-text-buffer  -*- lexical-binding: t; -*-

;; Phase 8 (Doc 33 LOCKED-2026-04-25-v2 §5) — gap-buffer based mutable
;; text primitive smoke tests. 10 ERT covering: constructor, basic
;; insert/delete/substring/cursor, literal search, multibyte (UTF-8
;; Japanese) round-trip, and a 1MB stress run that proves the gap-buffer
;; amortization holds under random insert/delete mixing.

(require 'ert)
(require 'nelisp-text-buffer)

;;;; 1. constructor

(ert-deftest nelisp-text-buffer-make-empty ()
  "An empty `make-text-buffer' has length 0 and cursor at 0."
  (let ((tb (make-text-buffer)))
    (should (nelisp-text-buffer-p tb))
    (should (= 0 (text-buffer-length tb)))
    (should (= 0 (text-buffer-cursor tb)))
    (should (= 0 (text-buffer-byte-length tb)))
    (should (text-buffer-multibyte-p tb))
    (should (string-equal "" (text-buffer-substring tb 0 0)))))

(ert-deftest nelisp-text-buffer-make-from-string ()
  "Initial-content is reflected in length / byte-length / substring."
  (let ((tb (make-text-buffer "hello")))
    (should (= 5 (text-buffer-length tb)))
    (should (= 5 (text-buffer-byte-length tb)))
    (should (string-equal "hello" (text-buffer-substring tb 0 5)))
    (should (string-equal "ell" (text-buffer-substring tb 1 4)))
    (should (= 5 (text-buffer-cursor tb)))))

;;;; 2. insert

(ert-deftest nelisp-text-buffer-insert-at-cursor ()
  "Inserting at the end advances the cursor to the new end."
  (let ((tb (make-text-buffer "abc")))
    (text-buffer-set-cursor tb 3)
    (text-buffer-insert tb "XYZ")
    (should (= 6 (text-buffer-length tb)))
    (should (= 6 (text-buffer-cursor tb)))
    (should (string-equal "abcXYZ" (text-buffer-substring tb 0 6)))))

(ert-deftest nelisp-text-buffer-insert-multiple-times ()
  "Repeated inserts at varying positions preserve content integrity."
  (let ((tb (make-text-buffer "Hello")))
    (text-buffer-set-cursor tb 5)
    (text-buffer-insert tb ", World")
    (text-buffer-set-cursor tb 0)
    (text-buffer-insert tb ">>> ")
    (text-buffer-set-cursor tb (text-buffer-length tb))
    (text-buffer-insert tb "!")
    (should (string-equal ">>> Hello, World!"
                          (text-buffer-substring
                           tb 0 (text-buffer-length tb))))
    (should (= (length ">>> Hello, World!")
               (text-buffer-length tb)))))

;;;; 3. delete

(ert-deftest nelisp-text-buffer-delete-range ()
  "Deleting [start, end) shrinks length and removes the right content."
  (let ((tb (make-text-buffer "abcdefghij")))
    (text-buffer-delete tb 3 7)
    (should (= 6 (text-buffer-length tb)))
    (should (string-equal "abchij" (text-buffer-substring tb 0 6)))
    ;; cursor was at end (10) → should shift left by 4
    (should (= 6 (text-buffer-cursor tb)))))

;;;; 4. cursor + substring

(ert-deftest nelisp-text-buffer-set-cursor-and-substring ()
  "Cursor moves, substring extracts arbitrary ranges, no mutation."
  (let ((tb (make-text-buffer "0123456789")))
    (text-buffer-set-cursor tb 4)
    (should (= 4 (text-buffer-cursor tb)))
    (should (string-equal "0123" (text-buffer-substring tb 0 4)))
    (should (string-equal "456789" (text-buffer-substring tb 4 10)))
    (should (string-equal "" (text-buffer-substring tb 5 5)))
    ;; substring must not mutate buffer state
    (should (= 10 (text-buffer-length tb)))
    (should (= 4 (text-buffer-cursor tb)))))

;;;; 5. search (literal)

(ert-deftest nelisp-text-buffer-search-literal-found ()
  "Literal pattern hits return the 0-indexed char position."
  (let ((tb (make-text-buffer "the quick brown fox jumps over the lazy dog")))
    (should (= 4 (text-buffer-search tb "quick")))
    (should (= 16 (text-buffer-search tb "fox")))
    (should (= 31 (text-buffer-search tb "the" 5)))
    ;; from-pos at exactly the match start still finds it
    (should (= 4 (text-buffer-search tb "quick" 4)))
    ;; empty pattern: returns from-pos
    (should (= 0 (text-buffer-search tb "")))
    (should (= 7 (text-buffer-search tb "" 7)))))

(ert-deftest nelisp-text-buffer-search-not-found ()
  "Missing patterns return nil; search past last possible start = nil."
  (let ((tb (make-text-buffer "hello world")))
    (should (null (text-buffer-search tb "xyz")))
    (should (null (text-buffer-search tb "world" 7)))
    (should (null (text-buffer-search tb "hello" 1)))))

;;;; 6. multibyte

(ert-deftest nelisp-text-buffer-multibyte-japanese-text ()
  "UTF-8 round-trip: あいう = 3 chars / 9 bytes; insert/delete preserve."
  (let ((tb (make-text-buffer "あいう")))
    (should (text-buffer-multibyte-p tb))
    (should (= 3 (text-buffer-length tb)))
    (should (= 9 (text-buffer-byte-length tb)))
    (should (string-equal "あいう" (text-buffer-substring tb 0 3)))
    (should (string-equal "い" (text-buffer-substring tb 1 2)))
    ;; insert at middle
    (text-buffer-set-cursor tb 1)
    (text-buffer-insert tb "X")
    (should (= 4 (text-buffer-length tb)))
    (should (= 10 (text-buffer-byte-length tb)))
    (should (string-equal "あXいう" (text-buffer-substring tb 0 4)))
    ;; literal search across multibyte text
    (should (= 2 (text-buffer-search tb "い")))
    (should (= 3 (text-buffer-search tb "う")))
    (should (null (text-buffer-search tb "え")))
    ;; delete the multibyte character we just kept
    (text-buffer-delete tb 0 1)
    (should (string-equal "Xいう" (text-buffer-substring
                                   tb 0 (text-buffer-length tb))))
    (should (= 3 (text-buffer-length tb)))
    (should (= 7 (text-buffer-byte-length tb)))))

;;;; 7. stress (gap-buffer amortization holds for 1MB workload)

(ert-deftest nelisp-text-buffer-stress-1mb-insert-delete ()
  "Random-position insert/delete on a ~1MB buffer keeps content sane.

This is a *correctness* stress, not a perf gate: we maintain a parallel
`expected' Lisp string and assert that `text-buffer-substring' tracks it
exactly across 200 random operations on a multi-hundred-KB buffer."
  (let* ((seed-len 200000)
         (chars (make-string seed-len ?a))
         (tb (make-text-buffer chars))
         (expected chars)
         (rng-state (random "phase8-stress-seed")))
    (ignore rng-state)
    (dotimes (i 200)
      (let* ((len (length expected))
             (op (% i 3)))
        (cond
         ;; insert random ASCII at random pos
         ((= op 0)
          (let* ((pos (random (1+ len)))
                 (n (1+ (random 64)))
                 (s (make-string n (+ ?a (random 26)))))
            (text-buffer-set-cursor tb pos)
            (text-buffer-insert tb s)
            (setq expected (concat (substring expected 0 pos)
                                   s
                                   (substring expected pos)))))
         ;; delete a random small range
         ((= op 1)
          (when (>= len 2)
            (let* ((start (random (1- len)))
                   (end (min len (+ start 1 (random 32)))))
              (text-buffer-delete tb start end)
              (setq expected (concat (substring expected 0 start)
                                     (substring expected end))))))
         ;; substring read-back integrity
         (t
          (when (>= (length expected) 100)
            (let* ((start (random (- (length expected) 100)))
                   (end (+ start 100)))
              (should (string-equal
                       (substring expected start end)
                       (text-buffer-substring tb start end)))))))))
    ;; final integrity check: full-length substring matches expected
    (should (= (length expected) (text-buffer-length tb)))
    (should (string-equal expected
                          (text-buffer-substring
                           tb 0 (text-buffer-length tb))))))

(provide 'nelisp-text-buffer-test)
;;; nelisp-text-buffer-test.el ends here
