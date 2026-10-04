;;; nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test.el --- safe status semantics -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)

(defun nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
    (form bindings slots roots gateway-mode write-p)
  "Interpret only the emitted raw-v2 forms needed by the safe-nil tests."
  (cond
   ((symbolp form) (if (assq form bindings) (cdr (assq form bindings)) form))
   ((not (consp form)) form)
   ((eq (car form) 'if)
    (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
     (if (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
          (nth 1 form) bindings slots roots gateway-mode write-p)
         (nth 2 form) (nth 3 form))
     bindings slots roots gateway-mode write-p))
   ((eq (car form) 'let)
    (let ((new-bindings (copy-sequence bindings)))
      (dolist (binding (nth 1 form))
        (push (cons (car binding)
                    (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
                     (cadr binding) bindings slots roots gateway-mode write-p))
              new-bindings))
      (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
       (nth 2 form) new-bindings slots roots gateway-mode write-p)))
   ((eq (car form) 'progn)
    (let (value)
      (dolist (item (cdr form))
        (setq value
              (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
               item bindings slots roots gateway-mode write-p)))
      value))
   ((memq (car form) '(= /= +))
    (let ((args (mapcar
                 (lambda (item)
                   (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
                    item bindings slots roots gateway-mode write-p))
                 (cdr form))))
      (pcase (car form)
        ('= (apply #'= args)) ('/= (apply #'/= args)) ('+ (apply #'+ args)))))
   ((eq (car form) 'extern-call)
    (let* ((name (cadr form))
           (args (mapcar
                  (lambda (item)
                    (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
                     item bindings slots roots gateway-mode write-p))
                  (cddr form))))
      (pcase name
        ((or 'nl_native_car_v2 'nl_native_cdr_v2)
         (if (integerp gateway-mode)
             gateway-mode
           (let* ((input-root (nth 2 args))
                  (output-root (nth 3 args))
                  (value (aref roots input-root)))
             (if (consp value)
                 (progn
                   (aset roots output-root
                         (if (eq name 'nl_native_car_v2) (car value) (cdr value)))
                   0)
               1))))
        ('nl_root_pin_slot_v2 (nth 2 args))
        (_ (error "Unexpected extern call in safe probe: %S" name)))))
   ((eq (car form) 'ptr-write-u64)
    (let* ((pointer (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
                     (nth 1 form) bindings slots roots gateway-mode write-p))
           (offset (nth 2 form))
           (value (nth 3 form)))
      (when write-p
        (aset (aref slots pointer) (/ offset 8) value)
        (when (equal (aref slots pointer) [0 0 0 0])
          (aset roots pointer nil)))
      value))
   (t (error "Unexpected raw-v2 form in safe probe: %S" form))))

(defun nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--run
    (operator argument gateway-mode write-p)
  (let* ((input (nelisp-bytecode-compiler-input-build
                 (byte-compile (list 'lambda '(value) (list operator 'value)))))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input 'safe-primitives-v3))
         (emitted (nelisp-bytecode-native-rooted-cfg-emit
                   plan "nl_native_rooted_cfg_probe_v1"))
         (root-count (plist-get plan :required-root-count))
         (slots (make-vector root-count nil))
         (roots (make-vector root-count :poison)))
    (dotimes (index root-count)
      (aset slots index (vector #x55 #x55 #x55 #x55)))
    (aset roots 1 argument)
    (let ((return-value
           (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--eval
            (nth 3 (plist-get emitted :form))
            `((env . test-env) (ticket . test-ticket)
              (argument-count . 1) (root-count . ,root-count))
            slots roots gateway-mode write-p)))
      (list :plan plan :emitted emitted :slots slots :roots roots :return return-value
            :argument argument))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics/status-one-materializes-nil ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (operator '(car-safe cdr-safe))
    (dolist (value '(nil t 0 7 "text" ""))
      (let* ((result (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--run
                      operator value 'auto t))
             (root (plist-get (plist-get result :plan) :final-root)))
        (should (null (funcall operator value)))
        (should (null (aref (plist-get result :roots) root)))
        (should (equal (aref (plist-get result :slots) root) [0 0 0 0]))))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics/cons-results-match-host ()
  (skip-unless (equal emacs-version "31.1"))
  (dolist (operator '(car-safe cdr-safe))
    (let* ((value '(left . right))
           (result (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--run
                    operator value 'auto t))
           (root (plist-get (plist-get result :plan) :final-root)))
      (should (equal (aref (plist-get result :roots) root) (funcall operator value)))
      (should (eq (aref (plist-get result :roots) 1) value)))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics/status-two-propagates ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((result (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--run
                  'car-safe 17 2 t))
         (root (plist-get (plist-get result :plan) :final-root)))
    (should (= (plist-get result :return) 2))
    (should (equal (aref (plist-get result :slots) root) [#x55 #x55 #x55 #x55]))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics/nil-assertion-detects-disabled-stores ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((result (nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test--run
                  'car-safe 17 'auto nil))
         (root (plist-get (plist-get result :plan) :final-root)))
    (should-error
     (unless (null (aref (plist-get result :roots) root))
       (error "GNU nil result was not materialized")))))

(ert-run-tests-batch-and-exit)

;;; nelisp-bytecode-native-rooted-cfg-safe-primitives-semantics-test.el ends here
