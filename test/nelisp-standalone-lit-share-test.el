;;; nelisp-standalone-lit-share-test.el --- Shared driver literal segments -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Guard: the reader driver's large command sources (substrate + a command
;; tail each) must be embedded once, not once per command.

(require 'ert)
(require 'cl-lib)

(let ((load-path (cons (expand-file-name "../scripts"
                                         (file-name-directory
                                          (or load-file-name buffer-file-name)))
                       (cons (expand-file-name "../lisp"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name)))
                             load-path))))
  (require 'nelisp-standalone-build))

(defun nelisp-lit-share-test--forms (tag n)
  "Return N distinct top-level forms made from TAG, about 300 bytes each."
  (mapconcat (lambda (i) (format "(defun %s-%d () \"%s\")\n" tag i (make-string 280 ?x)))
             (number-sequence 1 n) ""))

(defun nelisp-lit-share-test--hoisted ()
  "Return a hoisted (SOURCE . BLOBS) with three commands over one substrate."
  (let* ((substrate (nelisp-lit-share-test--forms "sub" 4000))
         (lits (list (concat substrate (nelisp-lit-share-test--forms "a" 300))
                     (concat substrate (nelisp-lit-share-test--forms "b" 300))
                     (concat substrate (nelisp-lit-share-test--forms "c" 300))))
         (i -1)
         (nelisp-standalone--target 'linux-x86_64))
    (nelisp-standalone--hoist-large-str-lits
     `(seq ,@(mapcar (lambda (l)
                       (setq i (1+ i))
                       `(defun ,(intern (format "cmd%d" i)) ()
                          (sexp-write-str-lit 'slot ,l)))
                     lits))
     "nl_driver_lit")))

(defun nelisp-lit-share-test--blob-bytes (blobs)
  (apply #'+ (mapcar (lambda (b) (length (cdr b))) blobs)))

(ert-deftest nelisp-lit-share-substrate-embedded-once ()
  (let* ((hoisted (nelisp-lit-share-test--hoisted))
         (nelisp-standalone--target 'linux-x86_64)
         (shared (nelisp-standalone--share-lit-segments hoisted "nl_driver_lit"))
         (marker (format "(defun sub-%d () " 2000)))
    ;; Unshared: the substrate sits in every blob, so it is reported as repeated.
    (should (> (nelisp-standalone--lit-duplicate-bytes (cdr hoisted)) 1000000))
    ;; Shared: nothing repeated, and the marker form occurs in exactly one blob.
    (should (= 0 (nelisp-standalone--lit-duplicate-bytes (cdr shared))))
    (should (= 1 (cl-count-if (lambda (b) (string-search marker (cdr b))) (cdr shared))))
    (should (= 1 (cl-loop for b in (cdr shared)
                          sum (cl-loop with pos = 0
                                       for hit = (string-search marker (cdr b) pos)
                                       while hit count t do (setq pos (1+ hit))))))
    (should (< (* 2 (nelisp-lit-share-test--blob-bytes (cdr shared)))
               (nelisp-lit-share-test--blob-bytes (cdr hoisted))))
    ;; Each use calls the assembler on the same blob names; no raw use is left.
    (should (cl-search "nl_driver_lit_assemble" (format "%S" (car shared))))
    (should-not (cl-search "(nl_alloc_str (+ (data-addr" (format "%S" (car shared))))))

(ert-deftest nelisp-lit-share-negative-control-disabled-embeds-twice ()
  "With sharing off the same checker flags the duplicate embedding."
  (let* ((hoisted (nelisp-lit-share-test--hoisted))
         (nelisp-standalone--target 'linux-x86_64)
         (off (let ((nelisp-standalone--share-lit-segments-enabled nil))
                (nelisp-standalone--share-lit-segments hoisted "nl_driver_lit"))))
    (should (> (nelisp-standalone--lit-duplicate-bytes (cdr off)) 1000000))))

(ert-deftest nelisp-lit-share-recipes-rebuild-literals ()
  "Assembling each recipe from the pool gives back the hoisted bytes."
  (let* ((hoisted (nelisp-lit-share-test--hoisted))
         (nelisp-standalone--target 'linux-x86_64)
         (shared (nelisp-standalone--share-lit-segments hoisted "nl_driver_lit"))
         (pool (cdr (assq 'nl_driver_lit_pool (cdr shared)))))
    (should pool)
    (dolist (orig (cdr hoisted))
      (let* ((recipe (cdr (assq (car orig) (cdr shared))))
             (u64 (lambda (off) (let ((v 0))
                                  (dotimes (i 8) (setq v (+ v (ash (aref recipe (+ off i)) (* 8 i)))))
                                  v)))
             (nseg (funcall u64 8))
             (out (apply #'concat
                         (cl-loop for k below nseg
                                  collect (let ((o (funcall u64 (+ 16 (* 16 k))))
                                                (l (funcall u64 (+ 24 (* 16 k)))))
                                            (substring pool o (+ o l)))))))
        (should (= (funcall u64 0) (length out)))
        (should (equal out (substring (cdr orig) 8)))))))

(provide 'nelisp-standalone-lit-share-test)
;;; nelisp-standalone-lit-share-test.el ends here
