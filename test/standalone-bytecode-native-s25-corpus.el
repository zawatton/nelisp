;;; standalone-bytecode-native-s25-corpus.el --- S2.5 native CFG corpus -*- lexical-binding: t; -*-

(defun nelisp-test-native-s25-corpus ()
  "Run source-free boxed and raw byte-code parity in one standalone process."
  (let* ((root (getenv "NELISP_REPO_ROOT"))
         (dir (getenv "NELISP_S25_ARTIFACT_DIR"))
         (branch-code (unibyte-string 137 134 6 0 192 135 135))
         (join-code (unibyte-string 137 131 8 0 192 130 12 0 193 130 12 0 135))
         (diamond-code (unibyte-string 192 193 194 131 10 0 195 130 11 0 196 135))
         (switch-code (unibyte-string 137 192 183 130 10 0 193 135 194 135 195 135))
         (loop-code (unibyte-string 192 193 194 131 15 0 194 131 12 0 136 135
                                    130 6 0 130 2 0))
         (left (cons 'left nil)) (right (cons 'right nil))
         (boxed-branch (make-byte-code 257 branch-code (vector left) 2))
         (boxed-join (make-byte-code 257 join-code (vector left right) 2))
         (hidden (cons 'hidden nil))
         (boxed-two (make-byte-code 514 branch-code [nil] 3))
         (arg-one (cons 'first-argument nil))
         (arg-two (cons 'mutable-second nil))
         (raw-diamond nil) (raw-diamond-t nil)
         (switch-table (make-hash-table :test 'eq))
         (raw-switch nil) (raw-loop nil)
         (unsupported-call (make-byte-code 257
                            (unibyte-string 137 131 7 0 32 135 192 135) [nil] 2))
         (unsafe-return (make-byte-code nil (unibyte-string 192 135) [left] 1))
         (branch-path (expand-file-name "boxed-branch.neln" dir))
         (join-path (expand-file-name "boxed-join.neln" dir))
         (two-path (expand-file-name "boxed-two-arg.neln" dir))
         (call-path (expand-file-name "unsupported-call.neln" dir))
         (unsafe-path (expand-file-name "unsafe-return.nelr" dir))
         (raw-paths (mapcar (lambda (name) (expand-file-name name dir))
                            '("diamond-nil.nelr" "diamond-t.nelr"
                              "switch.nelr" "nested-loop.nelr")))
         (boxed-units nil) (raw-units nil) (ok nil))
    (unless (and root dir (file-directory-p dir))
      (error "S2.5 corpus paths are unset"))
    (puthash 1 6 switch-table)
    (puthash 2 8 switch-table)
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (require 'nelisp-bytecode-frame-ir)
    (require 'nelisp-bytecode-native-compiler)
    (require 'nelisp-bytecode-native-compiler-raw)
    (require 'nelisp-native-boxed-unit)
    (require 'nelisp-native-unit)
    (aset (aref boxed-two 2) 0 hidden)
    (setq raw-diamond
          (let* ((constants [5 10 nil 20 30])
                 (frame (nelisp-bytecode-frame-ir-build diamond-code constants)))
            (make-byte-code nil diamond-code constants
                            (plist-get frame :max-stack-depth)))
          raw-diamond-t
          (let* ((constants [5 10 t 20 30])
                 (frame (nelisp-bytecode-frame-ir-build diamond-code constants)))
            (make-byte-code nil diamond-code constants
                            (plist-get frame :max-stack-depth)))
          raw-switch
          (let* ((constants (vector switch-table 10 20 30))
                 (frame (nelisp-bytecode-frame-ir-build switch-code constants 1)))
            (make-byte-code 257 switch-code constants
                            (plist-get frame :max-stack-depth)))
          raw-loop
          (let* ((constants [42 7 t])
                 (frame (nelisp-bytecode-frame-ir-build loop-code constants)))
            (make-byte-code nil loop-code constants
                            (plist-get frame :max-stack-depth))))
    (let ((b (nelisp-bytecode-native-compiler-build
              boxed-branch branch-path "nl_s25_boxed_branch"))
          (j (nelisp-bytecode-native-compiler-build
              boxed-join join-path "nl_s25_boxed_join"))
          (two (nelisp-bytecode-native-compiler-build
                boxed-two two-path "nl_s25_boxed_two_arg"))
          (bad-call (nelisp-bytecode-native-compiler-build
                     unsupported-call call-path "nl_s25_unsupported_call"))
          (bad-return (nelisp-bytecode-native-compiler-raw-build
                       unsafe-return unsafe-path "nl_s25_unsafe_return" nil)))
      (unless (and (eq (plist-get b :status) 'complete)
                   (eq (plist-get j :status) 'complete)
                   (eq (plist-get two :status) 'complete)
                   (memq (plist-get bad-call :status) '(unsupported malformed))
                   (eq (plist-get bad-return :status) 'unsupported)
                   (file-readable-p branch-path) (file-readable-p join-path)
                   (file-readable-p two-path)
                   (not (file-exists-p call-path)) (not (file-exists-p unsafe-path)))
        (error "S2.5 public routing/refusal failed: %S %S %S %S"
               b j bad-call bad-return)))
      (let ((cases `((,raw-diamond "nl_s25_diamond_nil" ,(nth 0 raw-paths) nil)
                   (,raw-diamond-t "nl_s25_diamond_t" ,(nth 1 raw-paths) t)
                   (,raw-switch "nl_s25_switch" ,(nth 2 raw-paths) switch)
                   (,raw-loop "nl_s25_nested_loop" ,(nth 3 raw-paths) loop))))
      (dolist (case cases)
        (let* ((function (nth 0 case)) (name (nth 1 case))
               (path (nth 2 case)) (kind (nth 3 case))
               (contract (if (eq kind 'switch) '(raw-i64) nil))
               (compiled (nelisp-bytecode-native-compiler-raw-build
                          function path name contract)))
          (unless (and (eq (plist-get compiled :status) 'complete)
                       (file-readable-p path))
            (error "S2.5 raw compile failed (%s): %S" name compiled))
          (when (eq kind 'nil)
            (unless (= (funcall function) 30) (error "GNU VM nil diamond golden failed")))
          (when (eq kind t)
            (unless (= (funcall function) 20) (error "GNU VM t diamond golden failed")))
          (when (eq kind 'switch)
            (dolist (pair '((1 . 10) (2 . 20) (7 . 30)))
              (unless (= (funcall function (car pair)) (cdr pair))
                (error "GNU VM switch golden mismatch: %S" pair))))
          (when (eq kind 'loop)
            (unless (= (funcall function) 42) (error "GNU VM nested-loop golden failed"))))))
    ;; Stage and publish the raw corpus through the public native unit API.
    (cl-loop for name in '("nl_s25_diamond_nil" "nl_s25_diamond_t"
                           "nl_s25_switch" "nl_s25_nested_loop")
             for path in raw-paths
             for staged = (nelisp-native-unit-stage path nil (list name))
             do (unless (eq (plist-get staged :status) 'staged)
                  (error "S2.5 stage failed: %S" staged))
             do (let* ((unit-id (plist-get staged :unit-id))
                       (published (nelisp-native-unit-publish
                                   (plist-get staged :candidate-id))))
                  (unless (eq (plist-get published :status) 'published)
                    (error "S2.5 publish failed: %S" published))
                  (push (cons name unit-id) raw-units)))
    ;; Boxed load retains the constant roots; compare pointer identity after GC.
    (push (nelisp-native-boxed-unit-open-with-constants
           branch-path "nl_s25_boxed_branch" (aref boxed-branch 2) 1)
          boxed-units)
    (push (nelisp-native-boxed-unit-open-with-constants
           join-path "nl_s25_boxed_join" (aref boxed-join 2) 1)
          boxed-units)
    (push (nelisp-native-boxed-unit-open-with-constants
           two-path "nl_s25_boxed_two_arg" (aref boxed-two 2) 2)
          boxed-units)
    (unwind-protect
        (progn
          (dolist (truthy (list nil t (cons 'mutable-root nil)))
            (let* ((vm-b (funcall boxed-branch truthy))
                   (_gc (garbage-collect))
                   (native-b (nelisp-native-boxed-unit-call
                              (nth 2 boxed-units) (list truthy)))
                   (vm-j (funcall boxed-join truthy))
                   (_gc2 (garbage-collect))
                   (native-j (nelisp-native-boxed-unit-call
                              (nth 1 boxed-units) (list truthy))))
              (unless (and (eq vm-b native-b) (eq vm-j native-j)
                           (if truthy (eq vm-j left) (eq vm-j right)))
                (error "S2.5 boxed VM/native identity mismatch"))))
          (let ((vm-false (funcall boxed-two arg-one nil))
                (native-false (nelisp-native-boxed-unit-call
                               (car boxed-units) (list arg-one nil)))
                (vm-true (funcall boxed-two arg-one arg-two))
                (native-true (nelisp-native-boxed-unit-call
                              (car boxed-units) (list arg-one arg-two))))
            (unless (and (eq vm-false hidden) (eq native-false hidden)
                         (eq vm-true arg-two) (eq native-true arg-two))
              (error "S2.5 packed514 VM/native identity mismatch"))
            (garbage-collect)
            (setcar arg-two 'mutated-second)
            (setcdr arg-two (list 'tail))
            (garbage-collect)
            (unless (and (eq (funcall boxed-two arg-one arg-two) arg-two)
                         (eq (nelisp-native-boxed-unit-call
                              (car boxed-units) (list arg-one arg-two)) arg-two)
                         (eq (car arg-two) 'mutated-second)
                         (equal (cdr arg-two) '(tail)))
              (error "S2.5 packed514 mutable GC identity mismatch")))
          (let ((value (cons 'mutable nil)))
            (garbage-collect)
            (let ((native (nelisp-native-boxed-unit-call
                           (nth 2 boxed-units) (list value))))
              (setcdr value 'changed)
              (garbage-collect)
              (unless (and (eq native value) (eq (cdr native) 'changed))
                (error "S2.5 boxed mutable cons identity/GC mismatch"))))
          (dolist (pair raw-units)
            (let ((name (car pair)) (unit-id (cdr pair)))
              (cond
               ((equal name "nl_s25_diamond_nil")
                (unless (= (nelisp-native-unit-call unit-id name nil) 30)
                  (error "native nil diamond mismatch")))
               ((equal name "nl_s25_diamond_t")
                (unless (= (nelisp-native-unit-call unit-id name nil) 20)
                  (error "native t diamond mismatch")))
               ((equal name "nl_s25_switch")
                (dolist (pair '((1 . 10) (2 . 20) (7 . 30)))
                  (unless (= (nelisp-native-unit-call unit-id name
                                                      (list (car pair)))
                             (cdr pair))
                    (error "native switch mismatch: %S" pair))))
               ((equal name "nl_s25_nested_loop")
                (unless (= (nelisp-native-unit-call unit-id name nil) 42)
                  (error "native nested-loop mismatch"))))))
          (setq ok t))
      (dolist (unit boxed-units) (nelisp-native-boxed-unit-close unit)))
    (unless ok (error "S2.5 corpus did not finish"))
    (princ "S2.5-CONTROL-FLOW-CORPUS: PASS\n")
    t))

(provide 'standalone-bytecode-native-s25-corpus)
;;; standalone-bytecode-native-s25-corpus.el ends here
