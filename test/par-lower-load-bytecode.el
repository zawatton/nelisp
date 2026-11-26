;;; par-lower-load-bytecode.el --- Top-level bytecode with live call and unwind -*- lexical-binding: t; -*-

(defvar par-lower-bytecode--events nil)

(defun par-lower-bytecode--step (value)
  (push value par-lower-bytecode--events))

(defun par-lower-bytecode--fail ()
  (par-lower-bytecode--step
   (list 'frame (and (backtrace-frame 0 'par-lower-bytecode--fail) t)))
  (signal 'wrong-type-argument '(integerp broken)))

(let ((file (make-temp-file "par-lower-bytecode-" nil ".elc"))
      (coding-system-for-write 'no-conversion)
      (par-lower-bytecode--events nil))
  (unwind-protect
      (progn
        ;; Constant; call with no arguments; return. The two functions are
        ;; resolved at execution time, across a live fset and a collection.
        (write-region
         (concat ";ELC\037\0\0\0\n;;; in Emacs version 31.1\n"
                 "(defalias 'par-lower-bytecode--callee #[0 \"\\300\\301\\041\\207\" [par-lower-bytecode--step original] 2])\n"
                 "(byte-code \"\\300\\040\\207\" [par-lower-bytecode--callee] 1)\n"
                 "(defalias 'par-lower-bytecode--callee #[0 \"\\300\\301\\041\\207\" [par-lower-bytecode--step replacement] 2])\n"
                 "(garbage-collect)\n"
                 "(byte-code \"\\300\\040\\207\" [par-lower-bytecode--callee] 1)\n"
                 "(byte-code \"\\300\\040\\207\" [par-lower-bytecode--fail] 1)\n"
                 "(par-lower-bytecode--step 'unreachable)\n")
         nil file nil 'silent)
        (let ((current-load-list '(caller))
              (debug-on-error t))
          (prin1 (list 'bytecode-exit
                       (condition-case caught (load file nil t t)
                         (wrong-type-argument caught))
                       current-load-list))
          (terpri))
        (prin1 (list 'bytecode-order (reverse par-lower-bytecode--events)
                     (assoc file load-history)))
        (terpri))
    (delete-file file)))
