;;; native-stackset-u5-fixtures.el --- Stack transfer parity corpus -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar u5-remaining 0)
(defvar u5-history nil)
(defun u5-step ()
  (when (> u5-remaining 0)
    (push u5-remaining u5-history)
    (setq u5-remaining (1- u5-remaining)) t))
(defun native-stackset-u5-fixtures ()
  "Return (NAME FUNCTION CASES) rows; cases retain distinct boxed identities."
  (let ((pairs '((left right) (nil t) ("left" "right") ([left] [right])
                 ((left . tail) (right . tail)) (0 1073741825))))
    (list
     (list 'set0 (make-byte-code 514 (unibyte-string 178 0 135) [] 2) pairs)
     (list 'set1 (make-byte-code 514 (unibyte-string 178 1 135) [] 2) pairs)
     (list 'set2zero (make-byte-code 514 (unibyte-string 179 0 0 135) [] 2) pairs)
     (list 'set2one (make-byte-code 514 (unibyte-string 179 1 0 135) [] 2) pairs)
     (list 'set255 (make-byte-code 514
                                  (concat (apply #'unibyte-string (make-list 254 137))
                                          (unibyte-string 178 255 182 127 182 127 135)) [] 256) pairs)
     (list 'set256 (make-byte-code 514
                                  (concat (apply #'unibyte-string (make-list 255 137))
                                          (unibyte-string 179 0 1 182 127 182 127 136 135)) [] 257) pairs)
     (list 'drop0 (make-byte-code 514 (unibyte-string 182 0 135) [] 2) pairs)
     (list 'drop1 (make-byte-code 514 (unibyte-string 182 1 135) [] 2) pairs)
     (list 'keep0 (make-byte-code 514 (unibyte-string 182 128 135) [] 2) pairs)
     (list 'keep1 (make-byte-code 514 (unibyte-string 182 129 135) [] 2) pairs)
     (list 'keep127 (make-byte-code 514
                                   (concat (apply #'unibyte-string (make-list 126 137))
                                           (unibyte-string 182 255 135)) [] 128) pairs)
     ;; The loop's two carried argument cells actually swap on every backedge.
     ;; Copies at 5, 7, 9 save old Y, replace Y with X, then replace X with Y.
     (list 'swap (make-byte-code 514
                                (unibyte-string 192 32 131 14 0
                                                137 2 178 2 178 2 130 0 0
                                                66 135)
                                [u5-step] 4) pairs)
     (list 'swap2 (make-byte-code 514
                                 (unibyte-string 192 32 131 16 0
                                                 137 2 179 2 0 179 2 0 130 0 0
                                                 66 135)
                                 [u5-step] 4) pairs))))
(defun native-stackset-u5-reference (function arguments)
  "Lisp reference interpreter for the exact corpus, independent of frame IR.
The list stack has its TOS first; SET offsets and preserve counts address the
old stack before dropping values. GNU Emacs execution qualifies this oracle."
  (let ((code (aref function 1)) (constants (aref function 2))
        (stack (reverse arguments)) (pc 0) (done nil) result)
    (while (not done)
      (let ((op (aref code pc)))
        (setq pc (1+ pc))
        (cond
         ((<= 192 op 255) (push (aref constants (- op 192)) stack))
         ((<= 1 op 5) (push (nth op stack) stack))
         ((= op 137) (push (car stack) stack))
         ((= op 136) (pop stack))
         ((memq op '(178 179))
          (let ((offset (aref code pc)))
            (setq pc (1+ pc))
            (when (= op 179)
              (setq offset (+ offset (* 256 (aref code pc))) pc (1+ pc)))
            (setcar (nthcdr offset stack) (car stack))
            (pop stack)))
         ((= op 182)
          (let* ((raw (aref code pc)) (count (logand raw 127)) (top (car stack)))
            (setq pc (1+ pc))
            (when (/= (logand raw 128) 0) (setcar (nthcdr count stack) top))
            (setq stack (nthcdr count stack))))
         ((= op 32) (push (funcall (pop stack)) stack))
         ((memq op '(130 131))
          (let ((target (+ (aref code pc) (* 256 (aref code (1+ pc))))))
            (setq pc (+ pc 2))
            (when (or (= op 130) (null (pop stack))) (setq pc target))))
         ((= op 66) (let ((right (pop stack)) (left (pop stack))) (push (cons left right) stack)))
         ((= op 135) (setq done t result (car stack)))
         (t (error "U5 reference opcode %s" op)))))
    result))
(provide 'native-stackset-u5-fixtures)
