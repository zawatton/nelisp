;;; part2-walk.el --- locate the silently aborting form in bundle part2  -*- lexical-binding: t; -*-
;;
;; The abort prints nothing and is not catchable by `condition-case', so the
;; only usable signal is "which marker was the last one written".  Print the
;; index BEFORE evaluating each top-level form; the highest `F N' with no
;; matching `f N' is the offender.
(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "WALK-PRE-OK\n")

  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "WALK-PART1-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "WALK-PART1-OK\n")

  (setq zz-src
        (with-temp-buffer
          (insert-file-contents
           "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part2.el")
          (buffer-string)))
  (setq zz-n (length zz-src))
  (setq zz-pos 0)
  (setq zz-idx 0)
  (setq zz-stop nil)
  (nelisp--write-stdout-bytes (format "WALK-BEGIN chars=%d\n" zz-n))

  (while (and (< zz-pos zz-n) (not zz-stop))
    (setq zz-r (condition-case e
                   (read-from-string zz-src zz-pos)
                 (error
                  (nelisp--write-stdout-bytes
                   (format "WALK-READ-ERR idx=%d pos=%d err=%S\n"
                           zz-idx zz-pos (car e)))
                  (setq zz-stop t)
                  nil)))
    (if (null zz-r)
        (setq zz-stop t)
      (setq zz-idx (1+ zz-idx))
      (setq zz-pos (cdr zz-r))
      (nelisp--write-stdout-bytes (format "F %d\n" zz-idx))
      (condition-case e
          (eval (car zz-r) t)
        (error
         (nelisp--write-stdout-bytes
          (format "E %d err=%S\n" zz-idx (car e)))))
      (nelisp--write-stdout-bytes (format "f %d\n" zz-idx))))

  (nelisp--write-stdout-bytes (format "WALK-END forms=%d\n" zz-idx))
  (condition-case e
      (nelisp--write-stdout-bytes
       (format "WALK-CLASSINV tdt=%S suffix=%S prefix=%S\n"
               (and (ignore-errors (find-class 'transient-describe-target nil)) t)
               (and (ignore-errors (find-class 'transient-suffix nil)) t)
               (and (ignore-errors (find-class 'transient-prefix nil)) t)))
    (error (nelisp--write-stdout-bytes (format "WALK-CLASSINV ERR %S\n" e))))
  (nelisp--write-stdout-bytes "WALK-DONE\n")
  0)
