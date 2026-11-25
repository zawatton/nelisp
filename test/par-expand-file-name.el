;;; -*- lexical-binding: t; -*-

(defun par-expand-row (name thunk)
  (prin1 (list name (condition-case err (funcall thunk) (error (if (eq (cadr err) 'filenamep) (butlast err) err)))))
  (terpri))

(let ((default-directory "/base/dir/")
      (file-name-handler-alist nil)
      (process-environment '("HOME=/test-home/")))
  (dolist (args '(("/tmp/a") ("/tmp/a//b/../") ("//srv/a") ("///srv//a")
                  ("//.") ("//./") ("//a/..") ("/../x") ("/../../x")
                  ("child") ("child/") ("") (".") ("./") ("..") ("../")
                  ("child" "/base/other") ("child" "/base/other/")
                  ("" "/base/other/") ("child" "other") ("child" 42)
                  ("~") ("~/") ("~/a") ("~root/") ("~root/a/../b")
                  ("~nelisp-no-such-account-2d19/x")
                  ("/:/a/../b") ("文/../字/") ("/bad\0name") (nil)))
    (par-expand-row (if (equal args '("/bad\0name")) 'nul-name args) (lambda () (apply #'expand-file-name args))))
  (let ((default-directory "relative")
        (saved (symbol-value 'invocation-directory)))
    (unwind-protect
        (progn
          (set 'invocation-directory "/launcher/bin/")
          (par-expand-row 'relative-current-directory (lambda () (expand-file-name "file"))))
      (set 'invocation-directory saved))))

(let ((file-name-handler-alist nil)
      (default-directory "/base/")
      (process-environment (list "UNRELATED=x" (copy-sequence "HOME=/a"))))
  (par-expand-row 'mutable-home
                  (lambda ()
                    (let ((first (expand-file-name "~/q")))
                      (aset (cadr process-environment) 6 ?b)
                      (list first (expand-file-name "~/q")
                            (let ((process-environment '("HOME=/c")))
                              (expand-file-name "~/q")))))))

(let ((file-name-handler-alist nil)
      (default-directory (copy-sequence "/a"))
      (absolute (copy-sequence "/a/x")))
  (par-expand-row 'mutable-paths
                  (lambda ()
                    (expand-file-name absolute)
                    (aset absolute 3 ?.)
                    (let ((first (expand-file-name "x")))
                      (aset default-directory 1 ?b)
                      (list first (expand-file-name "x") (expand-file-name absolute))))))

(defvar par-expand-events nil)

(defun par-expand-handler (operation &rest args)
  (push (cons operation args) par-expand-events)
  (pcase operation
    ('expand-file-name
     (if (eq (length args) 2)
         (concat "/handled/" (if (equal (car args) "leaf") "leaf" "path"))
       "/wrong-arity"))
    ('directory-file-name "/virtual:dir")
    (_ 'handled)))

(put 'par-expand-handler 'operations
     '(expand-file-name directory-file-name file-directory-p file-regular-p
       file-readable-p file-exists-p directory-files file-attributes))
(put 'par-expand-read-only 'operations '(file-readable-p))

(let ((default-directory "/local/")
      (file-name-handler-alist '(("\\`/virtual:" . par-expand-handler))))
  (dolist (row '((input expand-file-name "/virtual:name")
                 (base expand-file-name "leaf" "/virtual:dir/")
                 (current expand-file-name "/absolute")
                 (result expand-file-name "/local/../virtual:name")
                 (quoted-handler find-file-name-handler "/virtual:name" expand-file-name)))
    (setq par-expand-events nil)
    (par-expand-row (car row)
                    (lambda ()
                      (let ((default-directory (if (eq (car row) 'current)
                                                    "/virtual:/" default-directory)))
                        (list (apply (cadr row) (cddr row)) (reverse par-expand-events))))))
  (setq par-expand-events nil)
  (let ((inhibit-file-name-operation 'expand-file-name)
        (inhibit-file-name-handlers '(par-expand-handler)))
    (let ((file-name-handler-alist
           '(("\\`/virtual:" . par-expand-handler) ("dir/\\'" . par-expand-handler))))
      (par-expand-row 'inhibition
                      (lambda ()
                        (list (expand-file-name "/virtual:dir/")
                              (file-directory-p "/virtual:dir/")
                              (file-regular-p "/virtual:dir/")
                              (file-exists-p "/virtual:dir/")
                              (file-readable-p "/virtual:dir/")
                              (directory-files "/virtual:dir/" t "x" nil 3)
                              (file-attributes "/virtual:dir/" 'string)
                              (reverse par-expand-events)))))))

(let ((file-name-handler-alist '(("\\`/ssh:" . par-expand-handler)))
      (default-directory "/local/"))
  (setq par-expand-events nil)
  (par-expand-row 'remote-base
                  (lambda ()
                    (list (expand-file-name "leaf" "/ssh:host:/folder/")
                          (reverse par-expand-events)))))

(par-expand-row 'standard-handlers
                (lambda ()
                  (list (find-file-name-handler "/:/tmp/a" 'expand-file-name)
                        (find-file-name-handler "/tmp/pkg.el.gz" 'load)
                        (find-file-name-handler "/tmp/pkg.el.gz" 'expand-file-name))))

(let ((file-name-handler-alist
       '(("x" . par-expand-handler) ("y" . par-expand-read-only)
         ("y" . par-expand-handler) ("y" . par-expand-tie)
         . ignored-tail))
      (case-fold-search t))
  (string-match "\\(b\\)" "abc")
  (par-expand-row 'find-handler
                  (lambda ()
                    (list (find-file-name-handler "xy" 'file-readable-p)
                          (find-file-name-handler "xy" 'file-directory-p)
                          (find-file-name-handler "XY" 'file-directory-p)
                          (match-data))))
  (let ((inhibit-file-name-operation 'file-readable-p)
        (inhibit-file-name-handlers '(par-expand-read-only par-expand-handler)))
    (par-expand-row 'find-inhibited
                    (lambda () (find-file-name-handler "xy" 'file-readable-p)))))

(let ((file-name-handler-alist '(("\\`/:" . file-name-non-special)))
      (default-directory "/base/")
      (process-environment '("HOME=/test-home")))
  (dolist (args '(("/:~/file") ("/:/tmp/./") ("/:/../tmp")
                  ("file" "/:/tmp/") ("/:/tmp" "/:/base/")))
    (par-expand-row (cons 'quoted args) (lambda () (apply #'expand-file-name args)))))

(let* ((root (make-temp-file "par-expand-" t))
       (default-directory (file-name-as-directory root)))
  (unwind-protect
      (progn
        (make-directory (concat root "/dir"))
        (write-region "data" nil (concat root "/file") nil 'silent)
        (make-symbolic-link "dir" (concat root "/dir-link"))
        (make-symbolic-link "file" (concat root "/file-link"))
        (make-symbolic-link "missing" (concat root "/dangling"))
        (dolist (name '("" "." "dir" "dir/" "dir-link" "dir-link/"
                        "file" "file/" "file-link" "file-link/" "missing" "dangling"))
          (par-expand-row (cons 'predicates name)
                          (lambda () (list (file-directory-p name) (file-regular-p name)
                                           (file-exists-p name) (file-readable-p name)))))
        (par-expand-row 'quoted-local
                        (lambda ()
                          (let ((file-name-handler-alist '(("\\`/:" . file-name-non-special))))
                            (list (file-directory-p (concat "/:" root "/dir/"))
                                  (file-exists-p (concat "/:" root "/file"))))))
        (par-expand-row 'relative-and-absolute
                        (lambda () (list (file-directory-p (concat root "/dir"))
                                         (file-directory-p "dir")
                                         (directory-files "dir")
                                         (car (file-attributes "dir-link/")))))
        (let ((file-name-handler-alist nil)
              (load-path (list root))
              (normal-top-level-add-subdirs-inode-list nil))
          (make-directory (concat root "/a/nested") t)
          (make-directory (concat root "/CVS"))
          (make-directory (concat root "/.hidden"))
          (make-directory (concat root "/skip"))
          (write-region "" nil (concat root "/skip/.nosearch") nil 'silent)
          (normal-top-level-add-subdirs-to-load-path)
          (par-expand-row 'walk-order
                          (lambda () (mapcar (lambda (name)
                                              (substring name (length root)))
                                            load-path)))))
    (delete-directory root t)))
