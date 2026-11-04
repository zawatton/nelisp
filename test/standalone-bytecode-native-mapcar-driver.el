;;; standalone-bytecode-native-mapcar-driver.el --- mapcar arity regression -*- lexical-binding: t; -*-

(defun nelisp-mapcar-regression--capture (form)
  (condition-case err (eval form) (error err)))

(let ((actual
       (list (nelisp-mapcar-regression--capture '(mapcar))
             (nelisp-mapcar-regression--capture '(mapcar #'identity))
             (nelisp-mapcar-regression--capture '(mapcar #'identity nil nil))
             (mapcar nil nil)
             (mapcar 'missing-mapcar-function nil)
             (mapcar nil [])
             (mapcar #'identity '(nil 1 nil))
             (mapcar #'identity [nil 2])))
      (expected
       '((wrong-number-of-arguments mapcar 0)
         (wrong-number-of-arguments mapcar 1)
         (wrong-number-of-arguments mapcar 3)
         nil nil nil (nil 1 nil) (nil 2))))
  (unless (equal actual expected)
    (error "mapcar regression mismatch: actual=%S expected=%S" actual expected)))

(princ "mapcar-regression: PASS\n")
