;;; part2-walk2.el --- locate the `void-function c' form in part2  -*- lexical-binding: t; -*-
;;
;; Same walk as part2-walk.el, but with the loader's pre-bundle calls made
;; first (magit-bridge.el:4733-4735).  That one delta turned a clean
;; `load' of part2 into `(void-function c)'.  Print the offending form.
(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "W2-PRE-OK\n")

  (nelisp-emacs-magit-bridge--wrap-oset-default)
  (nelisp-emacs-magit-bridge--wrap-make-instance)
  (nelisp-emacs-magit-bridge--symverify)
  (nelisp--write-stdout-bytes "W2-ARM-OK\n")

  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "W2-PART1-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "W2-PART1-OK\n")

  (setq zz-src
        (with-temp-buffer
          (insert-file-contents
           "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part2.el")
          (buffer-string)))
  (setq zz-n (length zz-src))
  (setq zz-pos 0)
  (setq zz-idx 0)
  (setq zz-stop nil)
  (nelisp--write-stdout-bytes "W2-WALK-BEGIN\n")

  (while (and (< zz-pos zz-n) (not zz-stop))
    (setq zz-start zz-pos)
    (setq zz-r (condition-case e
                   (read-from-string zz-src zz-pos)
                 (error
                  (nelisp--write-stdout-bytes
                   (format "W2-READ-ERR idx=%d err=%S\n" zz-idx (car e)))
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
          (format "E %d err=%S src=%s\n"
                  zz-idx e
                  (substring zz-src zz-start
                             (min zz-n (+ zz-start 200)))))))
      (nelisp--write-stdout-bytes (format "f %d\n" zz-idx))))

  (nelisp--write-stdout-bytes (format "W2-WALK-END forms=%d\n" zz-idx))
  (condition-case e
      (nelisp--write-stdout-bytes
       (format "W2-CLASSINV tdt=%S suffix=%S prefix=%S\n"
               (and (ignore-errors (find-class 'transient-describe-target nil)) t)
               (and (ignore-errors (find-class 'transient-suffix nil)) t)
               (and (ignore-errors (find-class 'transient-prefix nil)) t)))
    (error (nelisp--write-stdout-bytes (format "W2-CLASSINV ERR %S\n" e))))
  (nelisp--write-stdout-bytes "W2-DONE\n")
  0)
