;;; nelisp-ht-fastpath3-test.el --- hash3 structural pins and hash identity  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pins the hash3 changes: physical header reads in the string hash/compare
;; path, a word-extracted tail fold, and the direct gethash dispatch in
;; `nl_directcall_invoke'.  A host-side model checks that the new tail fold
;; equals the old per-byte fold for every length 0..300.  NELISP_HT3_BUILD
;; overrides the build script path (negative control against the seed).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-ht-fastpath3-test--build
  (or (getenv "NELISP_HT3_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name default-directory)))))

(defun nelisp-ht-fastpath3-test--defun (name)
  "Return the text of the first DSL defun NAME in the build script."
  (with-temp-buffer
    (insert-file-contents nelisp-ht-fastpath3-test--build)
    (let* ((text (buffer-string))
           (start (string-match (format "^ +(defun %s " (regexp-quote name)) text))
           (end (and start (string-match "^ +(defun " text (1+ start)))))
      (should start)
      (substring text start end))))

(ert-deftest nelisp-ht-fastpath3-physical-header-reads ()
  (dolist (n '("nl_ht_str_hash" "nl_ht_dstr_eq" "nl_ht_dbytes_eq"))
    (should-not (string-match-p "heap-read-u64 \\(hd\\|hk\\|ho\\) [0-9]"
                                (nelisp-ht-fastpath3-test--defun n))))
  (should (string-match-p "ptr-read-u64 hd 24" (nelisp-ht-fastpath3-test--defun "nl_ht_str_hash"))))

(ert-deftest nelisp-ht-fastpath3-word-tail ()
  (let ((w (nelisp-ht-fastpath3-test--defun "nl_ht_hash_words")))
    (should (string-match-p "nl_ht_fold_bytes" w))
    (should (string-match-p "4095" w)))
  (should (string-match-p "defun nl_ht_fold_bytes"
                          (nelisp-ht-fastpath3-test--defun "nl_ht_fold_bytes")))
  (should (string-match-p "4095" (nelisp-ht-fastpath3-test--defun "nl_ht_dbytes_eq"))))

(ert-deftest nelisp-ht-fastpath3-direct-hash-dispatch ()
  (let ((b (nelisp-ht-fastpath3-test--defun "nl_directcall_invoke")))
    (should (= 3 (length (split-string b "(nl_directcall_hash_run id base argc out env)"))))))

;; Host model of the hash: OLD = per-byte masked tail, NEW = word-extracted.
(defun nelisp-ht-fastpath3-test--old (bytes h)
  (dolist (b bytes h) (setq h (logand (+ (* h 33) b) #x7fffffff))))

(defun nelisp-ht-fastpath3-test--fold (h w k)
  (let ((a (+ (* h 33) (logand w 255))))
    (when (> k 1) (setq a (+ (* a 33) (logand (ash w -8) 255))))
    (when (> k 2) (setq a (+ (* a 33) (logand (ash w -16) 255))))
    (when (> k 3) (setq a (+ (* a 33) (logand (ash w -24) 255))))
    (logand a #x7fffffff)))

(defun nelisp-ht-fastpath3-test--new (bytes h)
  "BYTES is a 1-7 byte tail, packed little endian with arbitrary junk above."
  (let* ((k (length bytes)) (w 0) (j 0))
    (dolist (b bytes) (setq w (logior w (ash b (* 8 j))) j (1+ j)))
    (setq w (logior w (ash (random #xffffff) (* 8 k))))
    (setq h (nelisp-ht-fastpath3-test--fold h w (min k 4)))
    (if (> k 4) (nelisp-ht-fastpath3-test--fold h (ash w -32) (- k 4)) h)))

(ert-deftest nelisp-ht-fastpath3-tail-fold-identity ()
  (dotimes (n 8)
    (dotimes (_ 200)
      (let ((bytes (cl-loop repeat n collect (random 256)))
            (h (random #x80000000)))
        (when (> n 0)
          (should (= (nelisp-ht-fastpath3-test--old bytes h)
                     (nelisp-ht-fastpath3-test--new bytes h))))))))

(provide 'nelisp-ht-fastpath3-test)
;;; nelisp-ht-fastpath3-test.el ends here
