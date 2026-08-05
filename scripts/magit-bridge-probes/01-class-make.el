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
       (format "PROBE1 ctor-fboundp=%S fn=%S\n"
               (fboundp 'eieio--class-make)
               (and (fboundp 'eieio--class-make)
                    (symbol-function 'eieio--class-make))))
    (error (nelisp--write-stdout-bytes (format "PROBE1 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "PROBE2 struct-slots=%S\n"
               (and (fboundp 'nelisp-cl-macros--struct-lookup-slots)
                    (nelisp-cl-macros--struct-lookup-slots 'eieio--class))))
    (error (nelisp--write-stdout-bytes (format "PROBE2 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "PROBE3 acc name=%S parents=%S slots=%S idx=%S opts=%S\n"
               (assq 'eieio--class-name nelisp-cl-macros--accessor-info)
               (assq 'eieio--class-parents nelisp-cl-macros--accessor-info)
               (assq 'eieio--class-slots nelisp-cl-macros--accessor-info)
               (assq 'eieio--class-index-table nelisp-cl-macros--accessor-info)
               (assq 'eieio--class-options nelisp-cl-macros--accessor-info)))
    (error (nelisp--write-stdout-bytes (format "PROBE3 ERR %S\n" e))))

  (condition-case e
      (let ((c (eieio--class-make 'nelisp-probe-raw)))
        (nelisp--write-stdout-bytes
         (format "PROBE4 recp=%S type=%S ref0=%S name=%S\n"
                 (recordp c)
                 (and (recordp c) (nelisp--record-type c))
                 (and (recordp c) (nelisp--record-ref c 0))
                 (eieio--class-name c))))
    (error (nelisp--write-stdout-bytes (format "PROBE4 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "PROBE5 defclass=%S dci=%S find-class=%S setter=%S\n"
               (fboundp 'defclass)
               (fboundp 'eieio-defclass-internal)
               (fboundp 'find-class)
               (get 'cl--find-class 'cl-simple-setter)))
    (error (nelisp--write-stdout-bytes (format "PROBE5 ERR %S\n" e))))

  (condition-case e
      (let ((root (ignore-errors (find-class 'eieio-default-superclass nil))))
        (nelisp--write-stdout-bytes
         (format "PROBE6 root=%S name=%S nslots=%S\n"
                 (and root t)
                 (and root (ignore-errors (eieio--class-name root)))
                 (and root (ignore-errors
                             (length (append (eieio--class-slots root) nil)))))))
    (error (nelisp--write-stdout-bytes (format "PROBE6 ERR %S\n" e))))

  (nelisp--write-stdout-bytes "PART1-BEGIN\n")
  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "PART1 ERR %S\n" e))))
  (nelisp--write-stdout-bytes "PART1-END\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "PROBE7 ctor-fboundp=%S dci=%S struct-slots=%S\n"
               (fboundp 'eieio--class-make)
               (fboundp 'eieio-defclass-internal)
               (and (fboundp 'nelisp-cl-macros--struct-lookup-slots)
                    (nelisp-cl-macros--struct-lookup-slots 'eieio--class))))
    (error (nelisp--write-stdout-bytes (format "PROBE7 ERR %S\n" e))))

  (condition-case e
      (let ((c (eieio--class-make 'nelisp-probe-raw2)))
        (nelisp--write-stdout-bytes
         (format "PROBE8 recp=%S ref0=%S name=%S\n"
                 (recordp c)
                 (and (recordp c) (nelisp--record-ref c 0))
                 (eieio--class-name c))))
    (error (nelisp--write-stdout-bytes (format "PROBE8 ERR %S\n" e))))

  (condition-case e
      (progn
        (eval '(defclass nelisp-probe-cls nil
                 ((aa :initarg :aa :initform nil)
                  (bb :initarg :bb :initform nil)))
              t)
        (nelisp--write-stdout-bytes "PROBE9 defclass-eval-ok\n"))
    (error (nelisp--write-stdout-bytes (format "PROBE9 ERR %S\n" e))))

  (condition-case e
      (let ((c (ignore-errors (find-class 'nelisp-probe-cls nil))))
        (nelisp--write-stdout-bytes
         (format "PROBE10 reg=%S name=%S nslots=%S nparents=%S\n"
                 (and c t)
                 (and c (ignore-errors (eieio--class-name c)))
                 (and c (ignore-errors
                          (length (append (eieio--class-slots c) nil))))
                 (and c (ignore-errors
                          (length (append (eieio--class-parents c) nil)))))))
    (error (nelisp--write-stdout-bytes (format "PROBE10 ERR %S\n" e))))

  (nelisp--write-stdout-bytes "PROBE-DONE\n")
  0)
