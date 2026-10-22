;;; funcall-driver.el --- the codegen lane's S5.13 funcall workload plus the collection count  -*- lexical-binding: t; -*-
;; Environment: CODEGEN_RUNTIME_BYTECODE (funcall.eld), CODEGEN_RUNTIME_WORKLOAD=funcall,
;; CODEGEN_RUNTIME_N.  Loads the frozen workload (it prints "S5-WORK funcall N ANSWER SECONDS"),
;; then prints the number of mid-form collections the allocator ran (stats element 7 of
;; debug switch 2, present in every build of this line).
(load (expand-file-name "funcall-workload.el" (file-name-directory load-file-name)) nil t)
(princ (format "VM-POLL-COLLECTIONS %S\n"
               (let ((stats (nelisp--debug-switch 2))) (and (consp stats) (nth 7 stats)))))
