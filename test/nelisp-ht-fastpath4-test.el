;;; nelisp-ht-fastpath4-test.el --- hash4 structural pins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Pins the hash4 changes: one-pass tail fold, gethash-only leaf, and the
;; inline gethash entry in `nc_dsubr_call'.  NELISP_HT4_BUILD overrides the
;; build script path (negative control against the seed).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-ht-fastpath4-test--root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name default-directory))))

(defconst nelisp-ht-fastpath4-test--build
  (or (getenv "NELISP_HT4_BUILD")
      (expand-file-name "scripts/nelisp-standalone-build.el"
                        nelisp-ht-fastpath4-test--root)))

(defconst nelisp-ht-fastpath4-test--native
  (expand-file-name "lisp/nelisp-cc-fxir-native.el" nelisp-ht-fastpath4-test--root))

(defun nelisp-ht-fastpath4-test--defun (file name)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((text (buffer-string))
           (start (string-match (format "^ +(defun %s " (regexp-quote name)) text))
           (end (and start (string-match "^ +(defun " text (1+ start)))))
      (should start)
      (substring text start end))))

(ert-deftest nelisp-ht-fastpath4-one-pass-tail ()
  (let ((w (nelisp-ht-fastpath4-test--defun nelisp-ht-fastpath4-test--build "nl_ht_hash_words")))
    (should (string-match-p "(nl_ht_fold_tail h w t0)" w))
    (should-not (string-match-p "(nl_ht_fold_bytes h" w)))
  (should (string-match-p "(shr w 48)"
                          (nelisp-ht-fastpath4-test--defun
                           nelisp-ht-fastpath4-test--build "nl_ht_fold_tail"))))

(ert-deftest nelisp-ht-fastpath4-gethash-leaf ()
  (let ((g (nelisp-ht-fastpath4-test--defun nelisp-ht-fastpath4-test--build "nl_directcall_hash_get")))
    (should (string-match-p "nl_ht_fast_hdr" g))
    (should (string-match-p "nl_ht_fast_find" g))
    (should-not (string-match-p "(= id " g))))

(ert-deftest nelisp-ht-fastpath4-dsubr-inline-entry ()
  (let ((d (nelisp-ht-fastpath4-test--defun nelisp-ht-fastpath4-test--native "nc_dsubr_call")))
    ;; Guard semantics kept: header kind 20, id byte 118, arity, policy, depth.
    (dolist (re '("(logand hdr 63) 20" "(logand desc 255) 118" "3145728"
                  "32768" "nl_directcall_hash_get" "nl_directcall_invoke"))
      (should (string-match-p (regexp-quote re) d)))))

;; Host model: a one-pass unmasked fold equals the per-byte masked fold, with
;; 64-bit wraparound of the intermediate.
(ert-deftest nelisp-ht-fastpath4-fold-identity ()
  (dotimes (n 8)
    (dotimes (_ 300)
      (let ((bytes (cl-loop repeat n collect (random 256))) (h (random #x80000000)) (a nil))
        (when (> n 0)
          (setq a h)
          (dolist (b bytes) (setq a (logand (+ (* a 33) b) #xffffffffffffffff)))
          (let ((old h))
            (dolist (b bytes) (setq old (logand (+ (* old 33) b) #x7fffffff)))
            (should (= old (logand a #x7fffffff)))))))))

(provide 'nelisp-ht-fastpath4-test)
;;; nelisp-ht-fastpath4-test.el ends here
