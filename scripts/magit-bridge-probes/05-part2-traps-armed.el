;;; part2-wrapped.el --- one-variable delta: arm the diagnostic traps  -*- lexical-binding: t; -*-
;;
;; `load' of part1+part2 on the base image succeeds (LOAD-PART2-OK).  The
;; smoke path differs only in that `nelisp-emacs-magit-bridge-load' arms
;; `--wrap-oset-default' / `--wrap-make-instance' / `--symverify' before the
;; bundle runs (magit-bridge.el:4733-4735).  Arm them here and change nothing
;; else.
(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "WRAP-PRE-OK\n")

  (nelisp--write-stdout-bytes "WRAP-ARM-1\n")
  (nelisp-emacs-magit-bridge--wrap-oset-default)
  (nelisp--write-stdout-bytes "WRAP-ARM-2\n")
  (nelisp-emacs-magit-bridge--wrap-make-instance)
  (nelisp--write-stdout-bytes "WRAP-ARM-3\n")
  (nelisp-emacs-magit-bridge--symverify)
  (nelisp--write-stdout-bytes "WRAP-ARM-OK\n")

  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "WRAP-PART1-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "WRAP-PART1-OK\n")

  (nelisp--write-stdout-bytes "WRAP-PART2-BEGIN\n")
  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part2.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "WRAP-PART2-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "WRAP-PART2-OK\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "WRAP-CLASSINV tdt=%S suffix=%S prefix=%S\n"
               (and (ignore-errors (find-class 'transient-describe-target nil)) t)
               (and (ignore-errors (find-class 'transient-suffix nil)) t)
               (and (ignore-errors (find-class 'transient-prefix nil)) t)))
    (error (nelisp--write-stdout-bytes (format "WRAP-CLASSINV ERR %S\n" e))))
  (nelisp--write-stdout-bytes "WRAP-DONE\n")
  0)
