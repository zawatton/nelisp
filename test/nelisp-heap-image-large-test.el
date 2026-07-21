;;; nelisp-heap-image-large-test.el --- Stress ERT for heap image codec -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-heap-image)

(defun nelisp-heap-image-large-test--roots ()
  "Return a large shared graph close to the full-init shape."
  (let* ((shared-tail (list 'tail 99))
         (shared-record (nelisp--make-record 'blob "name" shared-tail))
         (shared-vector (make-vector 1024 nil))
         (roots nil)
         (i 0))
    (while (< i 1024)
      (aset shared-vector i (if (= (% i 7) 0) shared-tail i))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 553)
      (setq roots
            (cons (cons (format "root-%d" i)
                        (vector i shared-tail shared-record shared-vector))
                  roots))
      (setq i (1+ i)))
    (cons (cons "shared-tail" shared-tail)
          (cons (cons "shared-record" shared-record)
                (cons (cons "shared-vector" shared-vector)
                      (nreverse roots))))))

(ert-deftest nelisp-heap-image-large-test-private-eq-table-resizes ()
  (let ((table (nelisp-heap-image--table-make
                'eq
                'nelisp-heap-image--object-key-hash
                4))
        (objects nil))
    (dotimes (i 300)
      (let ((obj (vector i (list i))))
        (push obj objects)
        (nelisp-heap-image--table-put table obj i)))
    (dotimes (_ 400)
      (make-list 8 'noise))
    (dolist (obj objects)
      (should (integerp (nelisp-heap-image--table-get table obj nil))))
    (should (> (length (nelisp-heap-image--table-buckets table)) 4))))

(ert-deftest nelisp-heap-image-large-test-private-eql-id-table ()
  (let ((table (nelisp-heap-image--table-make
                'eql
                (lambda (id) (logand id nelisp-heap-image--table-mask))
                4)))
    (dotimes (i 500)
      (nelisp-heap-image--table-put table i (list 'id i)))
    (dotimes (i 500)
      (should (equal (nelisp-heap-image--table-get table i nil)
                     (list 'id i))))))

(ert-deftest nelisp-heap-image-large-test-large-roundtrip-preserves-sharing ()
  (let* ((decoded (nelisp-heap-image-read-string
                   (nelisp-heap-image-dump-string
                    (nelisp-heap-image-large-test--roots))))
         (shared-tail (cdr (assoc "shared-tail" decoded)))
         (shared-record (cdr (assoc "shared-record" decoded)))
         (shared-vector (cdr (assoc "shared-vector" decoded)))
         (root-0 (cdr (assoc "root-0" decoded)))
         (root-552 (cdr (assoc "root-552" decoded))))
    (should (eq (aref root-0 1) shared-tail))
    (should (eq (aref root-0 2) shared-record))
    (should (eq (aref root-0 3) shared-vector))
    (should (eq (aref root-552 1) shared-tail))
    (should (eq (aref root-552 2) shared-record))
    (should (eq (aref root-552 3) shared-vector))
    (should (eq (nelisp--record-ref shared-record 1) shared-tail))
    (should (eq (aref shared-vector 0) shared-tail))
    (should (= (length shared-vector) 1024))))

(provide 'nelisp-heap-image-large-test)

;;; nelisp-heap-image-large-test.el ends here
