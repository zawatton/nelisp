;;; standalone-literal-file-bytes.el --- Arbitrary-byte read regression -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(let* ((file (getenv "NELISP_LITERAL_FIXTURE"))
       (all (apply #'unibyte-string (number-sequence 0 255)))
       (expected (concat (make-string 4095 ?x) (unibyte-string 192 193)
                         all all (unibyte-string 13 10 0 192 128 193 191
                                                   194 128 255 192)))
       (cases 0))
  (dolist (reader '(insert-file-contents-literally insert-file-contents))
    (dolist (window (list (list nil nil) (list 4095 4097) (list 4094 4100)
                         (list 4097 4353) (list 4353 4609)
                         (list 4609 4620) (list 4619 9000) (list 9000 9001)))
      (let* ((beg (car window)) (end (cadr window))
             (from (min (or beg 0) (length expected)))
             (to (max from (min (or end (length expected)) (length expected))))
             (want (substring expected from to))
             (coding-system-for-read 'no-conversion))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((result (funcall reader file nil beg end)))
            (unless (and (equal (buffer-string) want)
                         (= (cadr result) (length want)) (= (point) 1))
              (error "Literal byte mismatch reader=%S window=%S wanted=%d got=%d"
                     reader window (length want) (length (buffer-string))))))
        (setq cases (1+ cases)))))
  (princ (format "LITERAL-BYTES-PASS cases=%d bytes=%d\n" cases (length expected))))
