;;; nelisp-bytecode-native-joined-add-test.el --- Joined operand controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)

(defun nelisp-bytecode-native-joined-add-test--fixture ()
  (byte-compile '(lambda (flag left right increment)
                   (+ (if flag left right) increment))))

(defun nelisp-bytecode-native-joined-add-test--rewrite (form)
  (if (consp form)
      (if (eq (car form) 'extern-call)
          (cons 'nelisp-bytecode-native-joined-add-test--gateway
                (cons (list 'quote (nth 1 form))
                      (mapcar #'nelisp-bytecode-native-joined-add-test--rewrite
                              (cddr form))))
        (mapcar #'nelisp-bytecode-native-joined-add-test--rewrite form))
    form))

(defvar nelisp-bytecode-native-joined-add-test--roots)
(defvar nelisp-bytecode-native-joined-add-test--calls)
(defvar nelisp-bytecode-native-joined-add-test--missing-slot)
(defun nelisp-bytecode-native-joined-add-test--gateway (name &rest arguments)
  (pcase name
    ('nl_root_pin_slot_v2
     (let ((root (nth 2 arguments)))
       (if (equal root nelisp-bytecode-native-joined-add-test--missing-slot) 0
         (1+ root))))
    ('nl_native_add_v2
     (let* ((left (nth 2 arguments)) (right (nth 3 arguments))
            (output (nth 4 arguments)))
       (push (list left right output) nelisp-bytecode-native-joined-add-test--calls)
       (garbage-collect)
       (condition-case condition
           (progn
             (aset nelisp-bytecode-native-joined-add-test--roots output
                   (vector 2 (+ (aref (aref nelisp-bytecode-native-joined-add-test--roots left) 1)
                                (aref (aref nelisp-bytecode-native-joined-add-test--roots right) 1))
                           0 0))
             0)
         (error
          (let ((exit-base (nth 5 arguments)))
            (aset nelisp-bytecode-native-joined-add-test--roots exit-base (vector 2 1 0 0))
            (aset nelisp-bytecode-native-joined-add-test--roots (1+ exit-base)
                  (vector 4 (car condition) 0 0))
            (aset nelisp-bytecode-native-joined-add-test--roots (+ exit-base 2)
                  (vector 4 (cdr condition) 0 0)))
          1))))
    (_ (error "Unexpected model import: %S" name))))

(defun nelisp-bytecode-native-joined-add-test--run (form arguments root-count &optional missing)
  (let ((nelisp-bytecode-native-joined-add-test--roots (make-vector root-count nil))
        (nelisp-bytecode-native-joined-add-test--calls nil)
        (nelisp-bytecode-native-joined-add-test--missing-slot missing))
    (dotimes (index root-count)
      (aset nelisp-bytecode-native-joined-add-test--roots index (vector 0 nil 91 92)))
    (cl-loop for value in arguments for root from 1 do
             (aset nelisp-bytecode-native-joined-add-test--roots root
                   (vector (if value 2 0) value (+ 100 root) (+ 200 root))))
    (cl-letf (((symbol-function 'ptr-read-u64)
               (lambda (pointer offset)
                 (aref (aref nelisp-bytecode-native-joined-add-test--roots (1- pointer))
                       (/ offset 8))))
              ((symbol-function 'ptr-write-u64)
               (lambda (pointer offset value)
                 (aset (aref nelisp-bytecode-native-joined-add-test--roots (1- pointer))
                       (/ offset 8) value))))
      (let* ((function (eval (cons 'lambda (cddr form)) t))
             (status (funcall function 123 456 (length arguments) root-count)))
        (list status nelisp-bytecode-native-joined-add-test--roots
              nelisp-bytecode-native-joined-add-test--calls)))))

(ert-deftest nelisp-bytecode-native-joined-add-genuine-oracle-and-materialization ()
  (let* ((fixture (nelisp-bytecode-native-joined-add-test--fixture))
         (input (nelisp-bytecode-compiler-input-build fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "joined_add"))
         (form (nelisp-bytecode-native-joined-add-test--rewrite (plist-get emitted :form)))
         (root-count (plist-get plan :required-root-count)))
    (should (eq (plist-get emitted :status) 'complete))
    (dolist (arguments '((t 10 20 3) (nil 10 20 3) (t 1.5 9.5 2.25)))
      (let* ((result (nelisp-bytecode-native-joined-add-test--run form arguments root-count))
             (roots (nth 1 result)) (calls (nth 2 result)))
        (should (= (car result) (+ 512 5)))
        (should (equal calls '((6 7 5))))
        (should (= (aref (aref roots 5) 1) (apply fixture arguments)))
        ;; All four words survive the selected live-root copy.
        (should (equal (aref roots 6) (aref roots (if (car arguments) 2 3))))
        (should (equal (aref roots 7) (aref roots 4)))))
    (let ((result (nelisp-bytecode-native-joined-add-test--run
                   form '(t 10 20 3) root-count 7)))
      (should (= (car result) 2))
      (should-not (nth 2 result)))))

(ert-deftest nelisp-bytecode-native-joined-add-forged-slot-refused ()
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (nelisp-bytecode-native-joined-add-test--fixture)))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (forged (copy-tree plan)))
    (dolist (block (plist-get forged :blocks))
      (dolist (operation (plist-get block :operations))
        (when (eq (plist-get operation :opcode) 'add)
          (plist-put operation :materialized-input-roots
                     (list (plist-get forged :exit-root-base) 7)))))
    (should (eq (plist-get (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                           forged "joined_add") :status) 'unsupported))))

(ert-deftest nelisp-bytecode-native-joined-add-signal-retains-object ()
  (let* ((fixture (nelisp-bytecode-native-joined-add-test--fixture))
         (input (nelisp-bytecode-compiler-input-build fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "joined_add"))
         (bad (make-symbol "bad-number"))
         (result (nelisp-bytecode-native-joined-add-test--run
                  (nelisp-bytecode-native-joined-add-test--rewrite (plist-get emitted :form))
                  (list t bad 20 3) (plist-get plan :required-root-count)))
         (exit-base (plist-get plan :exit-root-base))
         (roots (nth 1 result))
         (oracle (condition-case condition (funcall fixture t bad 20 3)
                   (error condition))))
    (should (= (car result) (+ 1024 exit-base)))
    (should (= (length (nth 2 result)) 1))
    (should (eq (aref (aref roots (1+ exit-base)) 1) (car oracle)))
    (should (equal (aref (aref roots (+ exit-base 2)) 1) (cdr oracle)))
    (should (eq (nth 1 (aref (aref roots (+ exit-base 2)) 1)) bad))))

(ert-deftest nelisp-bytecode-native-joined-add-wrong-edge-oracle-detects ()
  (let* ((fixture (nelisp-bytecode-native-joined-add-test--fixture))
         (input (nelisp-bytecode-compiler-input-build fixture))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments))
         emitted)
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments)
               (lambda (context from to)
                 (let ((form (funcall owner context from to)))
                   ;; Force the wrong predecessor's live root on the nil branch.
                   (cl-labels ((mutate (value)
                                 (cond ((equal value '(setq rooted_cfg_phi_0 3))
                                        '(setq rooted_cfg_phi_0 2))
                                       ((consp value) (mapcar #'mutate value))
                                       (t value))))
                     (mutate form))))))
      (setq emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "joined_add")))
    (should (eq (plist-get emitted :status) 'complete))
    (let* ((result (nelisp-bytecode-native-joined-add-test--run
                    (nelisp-bytecode-native-joined-add-test--rewrite (plist-get emitted :form))
                    '(nil 10 20 3) (plist-get plan :required-root-count)))
           (value (aref (aref (nth 1 result) 5) 1)))
      (should (= (car result) (+ 512 5)))
      (should-not (= value (funcall fixture nil 10 20 3))))))
