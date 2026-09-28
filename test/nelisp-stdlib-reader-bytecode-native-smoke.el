;;; nelisp-stdlib-reader-bytecode-native-smoke.el --- runtime reader checks -*- lexical-binding: t; -*-

(defun nelisp-stdlib-reader-bytecode-native-smoke--assert (condition message)
  (unless condition (error "byte-code reader smoke: %s" message)))

(defun nelisp-stdlib-reader-bytecode-native-smoke--condition (reader source)
  (condition-case data
      (progn (nelisp-stdlib-reader-bytecode-native-smoke--invoke reader source)
             nil)
    (error (car data))))

(defun nelisp-stdlib-reader-bytecode-native-smoke--invoke (reader source)
  (cond
   ((eq reader 'read) (read source))
   ((eq reader 'read-from-string) (car (read-from-string source)))
   ((eq reader 'rd-one) (car (nelisp--rd-one source 0 (length source))))
   ((eq reader 'rd-read-one)
    (car (nelisp--rd-read-one source 0 (length source))))
   (t (error "unknown reader route: %s" reader))))

(let ((valid '("#[nil \"x\" [] 1]"
               "#[-1 \"x\" [] 1]"
               "#[(a . b) \"x\" [] 1]"
               "#[nil \"x\" [] 1 nil nil]"))
      (callable "#[nil \"\\300\\207\" [42] 1]")
      (invalid '("#[t \"x\" [] 1]"
                 "#[nil \"x\" [] 1 nil nil nil]"
                 "#[nil \"x\" [] -1]"
                 "#[nil \"x\" [] 1.0]"
                 "#[nil \"x\" nil 1]"
                 "#[nil \"x\" [] 1")))
  (dolist (source valid)
    (dolist (reader '(read read-from-string rd-one rd-read-one))
      (let ((object
             (nelisp-stdlib-reader-bytecode-native-smoke--invoke reader source)))
        (nelisp-stdlib-reader-bytecode-native-smoke--assert
         (byte-code-function-p object)
         (format "%s did not return bytecode: %s" reader source)))))
  (dolist (reader '(read read-from-string rd-one rd-read-one))
    (let ((object
           (nelisp-stdlib-reader-bytecode-native-smoke--invoke reader callable)))
      (nelisp-stdlib-reader-bytecode-native-smoke--assert
       (and (byte-code-function-p object) (= (funcall object) 42))
       (format "%s failed to execute zero-argument bytecode" reader))))
  (dolist (source invalid)
    (dolist (reader '(read read-from-string rd-one rd-read-one))
      (nelisp-stdlib-reader-bytecode-native-smoke--assert
       (eq (nelisp-stdlib-reader-bytecode-native-smoke--condition reader source)
           'invalid-read-syntax)
       (format "%s did not signal invalid-read-syntax for: %s" reader source))))
  (dolist (reader '(read read-from-string rd-one rd-read-one))
    (nelisp-stdlib-reader-bytecode-native-smoke--assert
     (eq (nelisp-stdlib-reader-bytecode-native-smoke--condition
          reader "#[nil (abc . 1) nil]")
         'unsupported-feature)
     (format "%s did not signal unsupported-feature for interpreted form"
             reader)))
  (princ "NELISP-BYTECODE-READER-PASS\n"))
