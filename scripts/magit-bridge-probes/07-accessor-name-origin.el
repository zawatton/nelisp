;;; accessor-name-probe.el --- is the mangled accessor name created at DEFINITION time?  -*- lexical-binding: t; -*-
;;
;; The smoke now dies with `void-function: (eieio--class-class-slotseie)'.
;; The real name is `eieio--class-class-slots'; the corrupt one has "eie"
;; appended.  The shim builds accessor names with
;; `(intern (concat CONC-NAME (symbol-name SLOT)))'.  If the corrupt symbol
;; is fbound, the mangling happened when the accessor was DEFINED; if only
;; the correct one is fbound, the mangling happens at the CALL site.
(progn
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "src/nelisp-emacs-magit-bridge.el"
                     "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib"))
  (setq nelisp-emacs-magit-bridge-repo-root
        "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (nelisp--write-stdout-bytes "AN-PRE-OK\n")

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "AN1 good=%S bad=%S\n"
               (fboundp 'eieio--class-class-slots)
               (fboundp 'eieio--class-class-slotseie)))
    (error (nelisp--write-stdout-bytes (format "AN1 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "AN2 concat=%S intern-eq=%S\n"
               (concat "eieio--class-" (symbol-name 'class-slots))
               (eq (intern (concat "eieio--class-" (symbol-name 'class-slots)))
                   'eieio--class-class-slots)))
    (error (nelisp--write-stdout-bytes (format "AN2 ERR %S\n" e))))

  (condition-case e
      (nelisp--write-stdout-bytes
       (format "AN3 defs=%S\n"
               (and (boundp 'emacs-cl-macros--struct-defs)
                    (car (cdr (assq 'eieio--class
                                    emacs-cl-macros--struct-defs))))))
    (error (nelisp--write-stdout-bytes (format "AN3 ERR %S\n" e))))

  (condition-case e
      (let ((names nil))
        (mapatoms (lambda (s)
                    (let ((n (symbol-name s)))
                      (when (and (> (length n) 12)
                                 (equal (substring n 0 13) "eieio--class-"))
                        (push n names)))))
        (nelisp--write-stdout-bytes
         (format "AN4 count=%d names=%S\n" (length names) (sort names #'string<))))
    (error (nelisp--write-stdout-bytes (format "AN4 ERR %S\n" e))))

  (nelisp--write-stdout-bytes "AN-DONE\n")
  0)
