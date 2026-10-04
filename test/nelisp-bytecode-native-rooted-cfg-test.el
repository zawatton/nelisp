;;; nelisp-bytecode-native-rooted-cfg-test.el --- Topology tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)
(require 'nelisp-bytecode-native-rooted-cfg)

(defun nelisp-bytecode-native-rooted-cfg-test--frame (edges)
  (list :status 'complete :blocks
        (vconcat
         (cl-loop for (id . successors) in edges collect
                  (list :start id :instructions [] :successors (vconcat successors))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-genuine-branch-topology ()
  (let* ((fn (byte-compile '(lambda (x) (if x (car x) (cdr x)))))
         (frame (nelisp-bytecode-frame-ir-build (aref fn 1) (aref fn 2) 1))
         (result (nelisp-bytecode-native-rooted-cfg-topology-check frame)))
    (should (eq (plist-get result :status) 'complete))
    (should (eq (plist-get result :scope) 'topology-only))
    (should (equal (plist-get result :block-order) '(0 4 6)))
    (should (= (plist-get result :path-count) 2))
    (should (equal (plist-get (car (plist-get result :branches)) :taken) 'nilp))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-refuses-cycle-and-bad-edges ()
  (let ((cycle (nelisp-bytecode-native-rooted-cfg-test--frame
                '((0 . ((:target 1 :slots [] :target-slots [])))
                  (1 . ((:target 0 :slots [] :target-slots []))))))
        (unknown (nelisp-bytecode-native-rooted-cfg-test--frame
                  '((0 . ((:target 9 :slots [] :target-slots []))))))
        (mismatch (nelisp-bytecode-native-rooted-cfg-test--frame
                   '((0 . ((:target 1 :slots [a] :target-slots [])))
                     (1 . nil)))))
    (dolist (frame (list cycle unknown mismatch))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check frame)
                             :status)
                  'unsupported)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-refuses-malformed-shape-and-total-path-overflow ()
  (let* ((fn (byte-compile '(lambda (x) (if x (car x) (cdr x)))))
         (frame (nelisp-bytecode-frame-ir-build (aref fn 1) (aref fn 2) 1))
         (malformed (copy-sequence frame)))
    (setf (plist-get malformed :blocks) 'not-a-vector)
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check malformed)
                           :status)
                'unsupported))
    (let ((nelisp-bytecode-native-rooted-cfg-max-paths 1))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check frame)
                             :status)
                  'unsupported)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-refuses-scalar-records ()
  (let ((bad-frame 7)
        (bad-block '(:status complete :blocks [7]))
        (bad-edge '(:status complete :blocks [(:start 0 :instructions [] :successors [7])]))
        (bad-ins '(:status complete :blocks [(:start 0 :instructions [7] :successors [])])))
    (dolist (frame (list bad-frame bad-block bad-edge bad-ins))
      (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-topology-check frame)
                             :status)
                  'unsupported)))))

(provide 'nelisp-bytecode-native-rooted-cfg-test)
;;; nelisp-bytecode-native-rooted-cfg-test.el ends here
