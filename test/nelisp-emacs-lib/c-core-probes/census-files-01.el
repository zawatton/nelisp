;;; census-files-01.el --- canonical probes  -*- lexical-binding: t; -*-

(add-name-to-file
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (progn (add-name-to-file f g) (list (file-exists-p g) (equal (nth 10 (file-attributes f)) (nth 10 (file-attributes g))))))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (write-region "old" nil g nil 'silent)
         (progn (add-name-to-file f g t) (with-temp-buffer (insert-file-contents g) (buffer-string))))
     (delete-directory d t)))
 (condition-case e (add-name-to-file 7 "unused") (error e)))

(autoload
 (let ((s (make-symbol "ccore-autoload")))
   (autoload s "ccore-not-loaded" "probe doc" t)
   (let ((a (symbol-function s)))
     (list (autoloadp a) (nth 1 a) (nth 2 a) (nth 3 a))))
 (let* ((s (make-symbol "ccore-autoload-existing")) (f '(lambda () 9)))
   (fset s f)
   (autoload s "ccore-not-loaded")
   (equal (symbol-function s) f))
 (condition-case e (autoload 7 "ccore-not-loaded") (error e)))

(autoload-do-load
 (equal (autoload-do-load '(lambda () 7)) '(lambda () 7))
 (equal (autoload-do-load '(autoload "ccore-not-loaded" nil nil nil) nil 'macro)
        '(autoload "ccore-not-loaded" nil nil nil))
 (condition-case e (autoload-do-load) (error (car e))))

(comp-el-to-eln-rel-filename
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (let ((r (comp-el-to-eln-rel-filename f)))
           (list (stringp r) (file-name-absolute-p r) (string-suffix-p ".eln" r))))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (let ((before (comp-el-to-eln-rel-filename f)))
           (write-region "abcd" nil f nil 'silent)
           (equal before (comp-el-to-eln-rel-filename f))))
     (delete-directory d t)))
 (condition-case e (comp-el-to-eln-rel-filename 7) (error e)))

(copy-file
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (progn (copy-file f g) (with-temp-buffer (insert-file-contents g) (buffer-string))))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (write-region "old" nil g nil 'silent)
         (progn (copy-file f g t) (with-temp-buffer (insert-file-contents g) (buffer-string))))
     (delete-directory d t)))
 (condition-case e (copy-file 7 "unused") (error e)))

(default-file-modes
 (let ((m (default-file-modes))) (and (integerp m) (<= 0 m) (<= m #o777)))
 (= (default-file-modes) (default-file-modes))
 (condition-case e (default-file-modes 7) (error (car e))))

(directory-file-name
 (directory-file-name "alpha/beta/")
 (list (directory-file-name "/") (directory-file-name ""))
 (condition-case e (directory-file-name 7) (error e)))

(directory-files
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (directory-files d nil "^[^.]"))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (write-region "z" nil g nil 'silent)
         (directory-files d nil "^[^.]"))
     (delete-directory d t)))
 (condition-case e (directory-files 7) (error e)))

(directory-files-and-attributes
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (mapcar (lambda (row) (list (car row) (car (cdr row)) (nth 7 (cdr row))))
                 (directory-files-and-attributes d nil "^[^.]")))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (directory-files-and-attributes d nil "^absent$"))
     (delete-directory d t)))
 (condition-case e (directory-files-and-attributes 7) (error e)))

(dump-emacs-portable--sort-predicate
 (condition-case e (dump-emacs-portable--sort-predicate) (error (car e)))
 (condition-case e (dump-emacs-portable--sort-predicate nil nil nil) (error (car e))))

(expand-file-name
 (file-relative-name (expand-file-name "a/../b" "/ccore-root/") "/ccore-root/")
 (file-relative-name (expand-file-name "../leaf" "/ccore-root/sub/") "/ccore-root/")
 (condition-case e (expand-file-name 7) (error e)))

(featurep
 (featurep 'emacs)
 (featurep (make-symbol "ccore-absent-feature"))
 (condition-case e (featurep 7) (error e)))

(file-accessible-directory-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-accessible-directory-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (file-accessible-directory-p d) (file-accessible-directory-p g)))
     (delete-directory d t)))
 (condition-case e (file-accessible-directory-p 7) (error e)))

(file-attributes
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (let ((a (file-attributes f))) (list (length a) (car a) (nth 7 a))))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (car (file-attributes d)) (file-attributes g)))
     (delete-directory d t)))
 (condition-case e (file-attributes 7) (error e)))

(file-directory-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-directory-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (file-directory-p d) (file-directory-p g)))
     (delete-directory d t)))
 (condition-case e (file-directory-p 7) (error e)))

(file-executable-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (set-file-modes f #o700)
         (file-executable-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (set-file-modes f #o600)
         (file-executable-p f))
     (delete-directory d t)))
 (condition-case e (file-executable-p 7) (error e)))

(file-exists-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-exists-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (file-exists-p d) (file-exists-p g)))
     (delete-directory d t)))
 (condition-case e (file-exists-p 7) (error e)))

(file-locked-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-locked-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-locked-p g))
     (delete-directory d t)))
 (condition-case e (file-locked-p 7) (error e)))

(file-modes
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (set-file-modes f #o640)
         (file-modes f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-modes g))
     (delete-directory d t)))
 (condition-case e (file-modes 7) (error e)))

(file-name-absolute-p
 (file-name-absolute-p "/ccore/leaf")
 (list (file-name-absolute-p "relative/leaf") (file-name-absolute-p ""))
 (condition-case e (file-name-absolute-p 7) (error e)))

(file-name-as-directory
 (file-name-as-directory "alpha")
 (list (file-name-as-directory "alpha/") (file-name-as-directory ""))
 (condition-case e (file-name-as-directory 7) (error e)))

(file-name-case-insensitive-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (eq (file-name-case-insensitive-p f) (file-name-case-insensitive-p d)))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-name-case-insensitive-p g))
     (delete-directory d t)))
 (condition-case e (file-name-case-insensitive-p 7) (error e)))

(file-name-concat
 (file-name-concat "alpha" "beta" "leaf")
 (list (file-name-concat "alpha/" "" "beta") (file-name-concat "" "leaf"))
 (condition-case e (file-name-concat 7 "leaf") (error e)))

(file-name-directory
 (file-name-directory "alpha/beta/leaf")
 (list (file-name-directory "leaf") (file-name-directory "/"))
 (condition-case e (file-name-directory 7) (error e)))

(file-name-nondirectory
 (file-name-nondirectory "alpha/beta/leaf")
 (list (file-name-nondirectory "alpha/") (file-name-nondirectory ""))
 (condition-case e (file-name-nondirectory 7) (error e)))

(file-newer-than-file-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-newer-than-file-p f f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (write-region "xyz" nil g nil 'silent)
         (set-file-times f '(0 1 0 0))
         (set-file-times g '(0 2 0 0))
         (list (file-newer-than-file-p g f) (file-newer-than-file-p f g)))
     (delete-directory d t)))
 (condition-case e (file-newer-than-file-p 7 "unused") (error e)))

(file-readable-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-readable-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (file-readable-p d) (file-readable-p g)))
     (delete-directory d t)))
 (condition-case e (file-readable-p 7) (error e)))

(file-regular-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-regular-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (list (file-regular-p d) (file-regular-p g)))
     (delete-directory d t)))
 (condition-case e (file-regular-p 7) (error e)))

(file-symlink-p
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (file-symlink-p f))
     (delete-directory d t)))
 (let* ((d (make-temp-file "ccore-files-" t))
        (f (expand-file-name "sample" d))
        (g (expand-file-name "other" d)))
   (unwind-protect
       (progn
         (write-region "abc" nil f nil 'silent)
         (make-symbolic-link "sample" g)
         (file-symlink-p g))
     (delete-directory d t)))
 (condition-case e (file-symlink-p 7) (error e)))
