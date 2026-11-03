(internal-subr-documentation
 (internal-subr-documentation 'car)
 (internal-subr-documentation 'not-a-primitive))
(Snarf-documentation
 ;; An empty name with the default doc-directory opens a directory on Linux.
 ;; GNU 31.1's scanner does not check a negative read result, and can segfault
 ;; even when this is the only form in a fresh process.  Use a regular malformed
 ;; DOC file: the unknown record type for the existing symbol car produces the
 ;; same (error "DOC file invalid at position 0") without an invalid C read.
 (let ((file (make-temp-file "ccore-doc-" nil nil "\037Xcar\ninvalid\n")))
   (unwind-protect
       (let ((doc-directory file))
         (condition-case e (Snarf-documentation "") (error e)))
     (delete-file file)))
 (condition-case e (Snarf-documentation nil) (error e)))
(text-quoting-style
 (let ((text-quoting-style nil)) (text-quoting-style))
 (let ((text-quoting-style 'straight)) (text-quoting-style))
 (let ((text-quoting-style 'grave)) (text-quoting-style)))
