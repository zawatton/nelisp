;;; wasm-rootstack-test.el --- Focused static ownership checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'wasm-rootstack-compose)

(ert-deftest wasm-rootstack/genuine-source-selection ()
  (let ((selected (wasm-rootstack-definitions)))
    (should (= (length selected) 4))
    (should (equal (mapcar #'cadr selected) wasm-rootstack--names))
    (dolist (form selected) (should (memq form (cdr nelisp-cc-rootstack--source))))))

(ert-deftest wasm-rootstack/authority-extents-and-generators ()
  (let ((owners (plist-get (wasm-rootstack-authority) :owners)))
    (should (equal (mapcar (lambda (row) (plist-get row :size)) (cl-subseq owners 0 4))
                   '(8 4194304 64 1040)))
    (should (= (/ (plist-get (cadr owners) :size) 32) 131072))
    (should (cl-every (lambda (row) (integerp (plist-get row :size))) (nthcdr 4 owners)))))

(ert-deftest wasm-rootstack/reject-duplicate-owner ()
  (let ((owners (cl-subseq (plist-get (wasm-rootstack-authority) :owners) 0 2)))
    (should-error (wasm-rootstack-compose :owners (append owners (list (car owners)))))))

(ert-deftest wasm-rootstack/reject-undersized-region ()
  (let ((owners (copy-tree (cl-subseq (plist-get (wasm-rootstack-authority) :owners) 0 2))))
    (setf (plist-get (cadr owners) :size) (- 4194304 32))
    (should-error (wasm-rootstack-compose :owners owners))))

(ert-deftest wasm-rootstack/reject-unaligned-blob ()
  (let ((owner (copy-tree (car (plist-get (wasm-rootstack-authority) :owners)))))
    (setf (plist-get owner :size) 7)
    (should-error (wasm-rootstack-compose :owners (list owner)))))

(ert-deftest wasm-rootstack/reject-other-owner ()
  (let ((owner (copy-tree (car (plist-get (wasm-rootstack-authority) :owners)))))
    (setf (plist-get owner :owner) 'external-module)
    (should-error (wasm-rootstack-compose :owners (list owner)))))

(ert-deftest wasm-rootstack/missing-each-owner-is-compiler-red ()
  (let ((owners (cl-subseq (plist-get (wasm-rootstack-authority) :owners) 0 2)))
    (dolist (owner owners)
      (let* ((condition (should-error
                         (nelisp-aot-compile-to-link-unit
                          (wasm-rootstack-compose :owners (list owner)) :arch 'wasm32 :format 'wasm)
                         :type 'nelisp-aot-compiler-error)))
        (should (eq (cadr condition) :wasm-p3-external-data-addr))
        (should (equal (caddr condition)
                       (if (eq (plist-get owner :name) 'nl_rootstack_top)
                           "nl_rootstack_region" "nl_rootstack_top")))))))

(ert-deftest wasm-rootstack/static-layout-and-cursor ()
  (let* ((layout (wasm-rootstack-layout (wasm-rootstack-compose)))
         (top (cdr (assoc "nl_rootstack_top" (plist-get layout :symbol-addrs))))
         (region (cdr (assoc "nl_rootstack_region" (plist-get layout :symbol-addrs)))))
    (should (> top 0)) (should (= (% top 8) 0))
    (should (= region (+ top 8)))
    (should (= (plist-get layout :stack-top) (+ region 4194304)))))

(provide 'wasm-rootstack-test)
