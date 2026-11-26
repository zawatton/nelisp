;;; par-rss-scratch-io.el --- Directory scratch growth and key comparisons -*- lexical-binding: t; -*-

(let ((root (make-temp-file "par-rss-" t))
      (stem "directory-entry-with-a-long-name-")
      (odd "a name\nwith a newline"))
  (unwind-protect
      (let ((default-directory (file-name-as-directory root)))
        (prin1 (list :empty (directory-files "." nil "^[^.]")
                     (condition-case err (directory-files "missing") (error (car err)))))
        (terpri)
        ;; The names fill more than one 8 KiB system read and exceed the
        ;; initial 4 KiB output capacity more than once.
        (dotimes (i (if (getenv "PAR_RSS_IO_MUTATION") 399 400))
          (write-region "" nil (format "%s%03d" stem i) nil 'silent))
        (write-region "" nil odd nil 'silent)
        (let ((names (directory-files "." nil "^[^.]")))
          (prin1 (list :grown (length names) (equal (car names) odd)
                       (equal (car (last names)) (concat stem "399"))))
          (terpri))
        (prin1 (list :small (equal (directory-files "." nil "^a name") (list odd))))
        (terpri))
    (delete-directory root t)))

(let ((a [leaf]) (b [leaf]) (c [other])
      (table (make-hash-table :test 'equal)))
  (dotimes (_ 16)
    (setq a (vector a) b (vector b) c (vector c)))
  (puthash a 'hit table)
  (prin1 (list :keys (gethash b table 'miss) (gethash c table 'miss)
               (gethash a table 'miss)))
  (terpri))
