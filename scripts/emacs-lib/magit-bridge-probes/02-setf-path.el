(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "PRE-OK\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S1 acc-name=%S\n" (symbol-function 'eieio--class-name)))
    (error (nelisp--write-stdout-bytes (format "S1 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S2 acc-parents=%S\n" (symbol-function 'eieio--class-parents)))
    (error (nelisp--write-stdout-bytes (format "S2 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S3 setf-expand=%S\n"
               (macroexpand '(setf (eieio--class-parents zzobj) zzval))))
    (error (nelisp--write-stdout-bytes (format "S3 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S4 slotdesc=%S copy=%S clclassp=%S\n"
               (and (fboundp 'cl--make-slot-descriptor)
                    (symbol-function 'cl--make-slot-descriptor))
               (fboundp 'cl--copy-slot-descriptor)
               (fboundp 'cl--class-p)))
    (error (nelisp--write-stdout-bytes (format "S4 ERR %S\n" e))))

  (nelisp--write-stdout-bytes "PART1-BEGIN\n")
  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "PART1 ERR %S\n" e))))
  (nelisp--write-stdout-bytes "PART1-END\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S5 acc-name=%S\n" (symbol-function 'eieio--class-name)))
    (error (nelisp--write-stdout-bytes (format "S5 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "S6 setf-expand=%S\n"
               (macroexpand '(setf (eieio--class-parents zzobj) zzval))))
    (error (nelisp--write-stdout-bytes (format "S6 ERR %S\n" e))))

  (setq zz-probe-class nil)
  (nelisp--write-stdout-bytes "STEP-A\n")
  (condition-case e
      (setq zz-probe-class (eieio--class-make 'zz-probe-name))
    (error (nelisp--write-stdout-bytes (format "STEP-A ERR %S\n" e))))
  (nelisp--write-stdout-bytes
   (format "STEP-A-OUT obj=%S\n" zz-probe-class))

  (nelisp--write-stdout-bytes "STEP-B\n")
  (condition-case e
      (eval '(setf (cl--class-parents zz-probe-class) (list 'p1)) t)
    (error (nelisp--write-stdout-bytes (format "STEP-B ERR %S\n" e))))
  (nelisp--write-stdout-bytes
   (format "STEP-B-OUT obj=%S read=%S\n"
           zz-probe-class
           (condition-case e2 (cl--class-parents zz-probe-class)
             (error (list 'ERR e2)))))

  (nelisp--write-stdout-bytes "STEP-C\n")
  (condition-case e
      (eval '(setf (eieio--class-slots zz-probe-class) (vector 's1 's2)) t)
    (error (nelisp--write-stdout-bytes (format "STEP-C ERR %S\n" e))))
  (nelisp--write-stdout-bytes
   (format "STEP-C-OUT read=%S\n"
           (condition-case e2 (eieio--class-slots zz-probe-class)
             (error (list 'ERR e2)))))

  (nelisp--write-stdout-bytes "STEP-D\n")
  (condition-case e
      (eval '(setf (cl--find-class 'zz-probe-name) zz-probe-class) t)
    (error (nelisp--write-stdout-bytes (format "STEP-D ERR %S\n" e))))
  (nelisp--write-stdout-bytes
   (format "STEP-D-OUT found=%S\n"
           (condition-case e2 (and (find-class 'zz-probe-name nil) t)
             (error (list 'ERR e2)))))

  (nelisp--write-stdout-bytes "STEP-E\n")
  (condition-case e
      (eval '(cl-pushnew 'kid (eieio--class-children zz-probe-class)) t)
    (error (nelisp--write-stdout-bytes (format "STEP-E ERR %S\n" e))))
  (nelisp--write-stdout-bytes
   (format "STEP-E-OUT read=%S\n"
           (condition-case e2 (eieio--class-children zz-probe-class)
             (error (list 'ERR e2)))))

  (nelisp--write-stdout-bytes "STEP-F\n")
  (condition-case e
      (nelisp--write-stdout-bytes
       (format "STEP-F-OUT desc=%S\n"
               (cl--make-slot-descriptor 'aa nil t nil)))
    (error (nelisp--write-stdout-bytes (format "STEP-F ERR %S\n" e))))

  (nelisp--write-stdout-bytes "PROBE-DONE\n")
  0)
