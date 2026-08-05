;;; part2-load.el --- does `load' of the whole part2 file abort?  -*- lexical-binding: t; -*-
;;
;; The form-by-form walk (`read-from-string' + `eval') completed all 401
;; top-level forms of part2 and registered the transient classes.  The smoke
;; loads the same file with `load'.  This isolates loader-path vs semantics.
(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (defvar nelisp-emacs-magit-bridge-bundle--loaded-features nil)
  (nelisp--write-stdout-bytes "LOAD-PRE-OK\n")

  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part1.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "LOAD-PART1-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "LOAD-PART1-OK\n")

  (nelisp--write-stdout-bytes "LOAD-PART2-BEGIN\n")
  (condition-case e
      (load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/build/nelisp-emacs-magit-bridge-bundle-part2.el"
            nil 'no-message t t)
    (error (nelisp--write-stdout-bytes (format "LOAD-PART2-ERR %S\n" e))))
  (nelisp--write-stdout-bytes "LOAD-PART2-OK\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "LOAD-CLASSINV tdt=%S suffix=%S prefix=%S\n"
               (and (ignore-errors (find-class 'transient-describe-target nil)) t)
               (and (ignore-errors (find-class 'transient-suffix nil)) t)
               (and (ignore-errors (find-class 'transient-prefix nil)) t)))
    (error (nelisp--write-stdout-bytes (format "LOAD-CLASSINV ERR %S\n" e))))
  (nelisp--write-stdout-bytes "LOAD-DONE\n")
  0)
