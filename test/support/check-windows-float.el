;;; check-windows-float.el --- Rebuild-free Win64 float-step probe -*- lexical-binding: t -*-
;; Compile: emacs -Q --batch -L lisp -L src -L scripts -l test/support/check-windows-float.el
;; Run on Windows: target/windows-float-check.exe
;; Run on Linux: WINEDEBUG=-all wine target/windows-float-check.exe
;; Exit 42 means a production 1+/1- float arm wrote an incorrect value.
(require 'cl-lib)
(load (if-let* ((source (getenv "WINDOWS_FLOAT_BUILD_SOURCE")))
          (expand-file-name source)
        "nelisp-standalone-build") nil t)
(let* ((nelisp-standalone--target 'windows-x86_64)
       (branches nil)
       (forms nil)
       (checks nil)
       (index 0)
       (slot (nelisp-link-unit-make
              "float-slot.obj" (list (cons 'bss 64))
              (list (nelisp-link-symbol "float_slot" 0 :section 'bss :bind 'global)) nil)))
  ;; Extract both marker-normalized and ordinary arms from the real table.
  ;; Do not recreate the implementation: this must fail against the old source.
  (cl-labels ((walk (form)
                (when (consp form)
                  (if (and (eq (car form) 'if)
                           (equal (cadr form) '(= (ptr-read-u64 p 0) 3)))
                      (push (nth 2 form) branches)
                    (mapc #'walk form)))))
    (dolist (name '("1+" "1-"))
      (setq branches nil)
      (walk (cdr (assoc (list :u8 name) nelisp-standalone--applyfn-dispatch-table)))
      (unless (= (length branches) 2) (error "Expected two float arms for %s" name))
      (dolist (branch branches)
        (let ((function (intern (format "float_step_%d" index)))
              (expected (if (equal name "1+") 4615063718147915776 4609434218613702656)))
          (push `(defun ,function (out p) ,branch) forms)
          (push `(seq
                  (ptr-write-u64 out 0 99)
                  (ptr-write-u64 out 16 99)
                  (ptr-write-u64 out 24 99)
                  ;; Leave RDI pointing at P, not OUT: generic calls must
                  ;; not pass by accidentally reusing a previous store base.
                  (ptr-write-u64 p 8 4612811918334230528)
                  (,function out p)
                  (if (and (= (ptr-read-u64 out 0) 3)
                           (= (ptr-read-u64 out 8) ,expected)
                           (= (ptr-read-u64 out 16) 0)
                           (= (ptr-read-u64 out 24) 0))
                      0 (setq failures (+ failures 1)))) checks)
          (setq index (1+ index))))))
  (let* ((main `(defun _start ()
                 (let* ((out (data-addr float_slot))
                        (p (+ out 32)) (failures 0))
                   (seq ,@(nreverse checks)
                        (extern-call ExitProcess (if (= failures 0) 0 42))))))
         (unit (nelisp-standalone--compile-to-unit
                "float-probe.obj" `(seq ,@(nreverse forms) ,main))))
    (make-directory "target" t)
    (nelisp-link-units-pe32 "target/windows-float-check.exe"
                           (list unit slot (nelisp-standalone--reader-float-unit))
                           "_start" '("ExitProcess"))
    (princ (format "WINDOWS-FLOAT compiled %d production float arms\n" index))))
