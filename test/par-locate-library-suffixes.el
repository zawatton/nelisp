;;; par-locate-library-suffixes.el --- suffix expansion and PATH -*- lexical-binding: t; -*-

(let ((root (make-temp-file "par-locate-library-" t)))
  (unwind-protect
      (let ((load-suffixes '(".probe"))
            (load-file-rep-suffixes '(".encoded")))
        (write-region "" nil (expand-file-name "library.probe.encoded" root) nil 'silent)
        (prin1 (mapcar (lambda (file) (and file (file-name-nondirectory file)))
                      (list (locate-library "library" nil (list root))
                            (locate-library "library" t (list root))
                            (locate-library "library.probe" t (list root)))))
        (terpri)
        (let ((load-file-rep-suffixes nil))
          (write-region "" nil (expand-file-name "library.probe" root) nil 'silent)
          (prin1 (and (locate-library "library" nil (list root)) t))
          (terpri)))
    (delete-directory root t)))
