;;; nelisp-bytecode-native-rooted-cfg-postdom-test.el --- postdominator analysis -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg)
(require 'nelisp-bytecode-native-rooted-cfg-postdom)

(defun nelisp-bytecode-native-rooted-cfg-postdom-test--two-diamonds ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (condition-a left-a right-a condition-b left-b right-b)
      (cons (car (if condition-a left-a right-a))
            (cdr (if condition-b left-b right-b)))))))

(defun nelisp-bytecode-native-rooted-cfg-postdom-test--carried-phi ()
  (nelisp-bytecode-compiler-input-build
   (byte-compile
    (lambda (a x y b z)
      (cons (if a x y) (if b z (if a x y)))))))

(defun nelisp-bytecode-native-rooted-cfg-postdom-test--renumber (frame offset)
  (let ((copy (copy-tree frame t)))
    (dolist (block (append (plist-get copy :blocks) nil))
      (setf (plist-get block :start) (+ offset (plist-get block :start)))
      (dolist (edge (append (plist-get block :successors) nil))
        (setf (plist-get edge :target) (+ offset (plist-get edge :target)))
        (dotimes (i (length (plist-get edge :target-slots)))
          (let ((token (aref (plist-get edge :target-slots) i)))
            (setcar (cdr token) (+ offset (cadr token)))))))
    copy))

(ert-deftest nelisp-bytecode-native-rooted-cfg-postdom/two-independent-diamonds-share-exit ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-postdom-test--two-diamonds))
         (frame (plist-get input :frame-result))
         (topology (nelisp-bytecode-native-rooted-cfg-topology-check frame))
         (analysis (nelisp-bytecode-native-rooted-cfg-postdom-analyze input))
         (joins (plist-get analysis :nearest-joins)))
    (should (eq (plist-get input :status) 'complete))
    (should (eq (plist-get analysis :status) 'complete))
    (should (= (length joins) 2))
    (should (= (cdr (car joins)) (nth 3 (plist-get topology :block-order))))
    (should (= (cdr (cadr joins)) (car (last (plist-get topology :block-order)))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-postdom/carried-phi-uses-verified-cfg ()
  (skip-unless (equal emacs-version "31.1"))
  (unless (equal emacs-version "31.1") (ert-skip "Requires GNU Emacs 31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-postdom-test--carried-phi))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (analysis (nelisp-bytecode-native-rooted-cfg-postdom-analyze input)))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get analysis :status) 'complete))
    (should (>= (length (plist-get analysis :nearest-joins)) 2))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-postdom/is-stable-under-edge-order-and-id-renaming ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((input (nelisp-bytecode-native-rooted-cfg-postdom-test--two-diamonds))
         (frame (plist-get input :frame-result))
         (blocks (append (plist-get frame :blocks) nil))
         (topology (nelisp-bytecode-native-rooted-cfg-topology-check frame))
         (order (plist-get topology :block-order))
         (baseline (nelisp-bytecode-native-rooted-cfg-postdom--compute blocks order))
         (swapped (copy-tree frame t)))
    (dolist (block (append (plist-get swapped :blocks) nil))
      (let ((edges (plist-get block :successors)))
        (when (= (length edges) 2)
          (setf (plist-get block :successors) (vector (aref edges 1) (aref edges 0))))))
    (let* ((swapped-topology (nelisp-bytecode-native-rooted-cfg-topology-check swapped))
           (swapped-result
            (nelisp-bytecode-native-rooted-cfg-postdom--compute
             (append (plist-get swapped :blocks) nil)
             (plist-get swapped-topology :block-order)))
           (renumbered (nelisp-bytecode-native-rooted-cfg-postdom-test--renumber frame 1000))
           (renumbered-topology (nelisp-bytecode-native-rooted-cfg-topology-check renumbered))
           (renumbered-result
            (nelisp-bytecode-native-rooted-cfg-postdom--compute
             (append (plist-get renumbered :blocks) nil)
             (plist-get renumbered-topology :block-order))))
      (should (eq (plist-get swapped-topology :status) 'complete))
      (should (eq (plist-get renumbered-topology :status) 'complete))
      (should (equal (mapcar #'cdr (plist-get baseline :nearest-joins))
                     (mapcar #'cdr (plist-get swapped-result :nearest-joins))))
      (should (equal (mapcar (lambda (entry) (+ 1000 (cdr entry)))
                             (plist-get baseline :nearest-joins))
                     (mapcar #'cdr (plist-get renumbered-result :nearest-joins)))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-postdom/supports-multiple-exits-and-refuses-bad-canonical-input ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((multi-exit-frame
          '(:status complete
            :blocks [(:start 0 :instructions [(:opcode 131)]
                      :successors [(:target 1 :slots [] :target-slots [])
                                   (:target 2 :slots [] :target-slots [])])
                     (:start 1 :instructions [(:opcode 135)] :successors [])
                     (:start 2 :instructions [(:opcode 135)] :successors [])]))
         (topology (nelisp-bytecode-native-rooted-cfg-topology-check multi-exit-frame))
         (multi-exit (nelisp-bytecode-native-rooted-cfg-postdom--compute
                      (append (plist-get multi-exit-frame :blocks) nil)
                      (plist-get topology :block-order)))
         (input (nelisp-bytecode-native-rooted-cfg-postdom-test--two-diamonds))
         (cycle (copy-tree input t))
         (missing (copy-tree input t))
         (cycle-blocks (plist-get (plist-get cycle :frame-result) :blocks))
         (missing-blocks (plist-get (plist-get missing :frame-result) :blocks)))
    (should (eq (plist-get topology :status) 'complete))
    (should (eq (plist-get multi-exit :status) 'complete))
    (should (equal (plist-get multi-exit :nearest-joins) '((0 . nil))))
    (should (equal (cdr (assq 1 (plist-get multi-exit :postdominators))) '(1)))
    (should (equal (cdr (assq 2 (plist-get multi-exit :postdominators))) '(2)))
    (setf (plist-get (aref (plist-get (aref cycle-blocks 1) :successors) 0) :target) 0)
    (setf (plist-get (aref missing-blocks 0) :successors) [])
    (dolist (invalid (list cycle missing))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-postdom-analyze invalid)
                             :status)
                  'unsupported)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-postdom/disabled-analysis-does-not-pass-real-input ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((input (nelisp-bytecode-native-rooted-cfg-postdom-test--two-diamonds)))
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-postdom--compute)
               (lambda (_blocks _order) (list :status 'unsupported :reason "disabled"))))
      (should-not (eq (plist-get (nelisp-bytecode-native-rooted-cfg-postdom-analyze input)
                                 :status)
                      'complete)))))

(provide 'nelisp-bytecode-native-rooted-cfg-postdom-test)
;;; nelisp-bytecode-native-rooted-cfg-postdom-test.el ends here
