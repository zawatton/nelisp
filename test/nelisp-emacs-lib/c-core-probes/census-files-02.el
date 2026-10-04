;;; census-files-02.el --- canonical probes  -*- lexical-binding: t; -*-

(file-writable-p
 (let ((f (make-temp-file "ccore-writable-")))
   (unwind-protect (file-writable-p f) (delete-file f)))
 (condition-case e (file-writable-p 7) (error e)))

(find-file-name-handler
 (let ((file-name-handler-alist '(("\\`probe:" . probe-handler))))
   (save-match-data (find-file-name-handler "probe:item" 'insert-file-contents)))
 (let ((file-name-handler-alist nil))
   (save-match-data (find-file-name-handler "plain.txt" 'write-region)))
 (condition-case e (find-file-name-handler 7 'load) (error e)))

(get-load-suffixes
 (let ((load-suffixes '(".elc" ".el")) (load-file-rep-suffixes '("" ".gz")))
   (get-load-suffixes))
 (let ((load-suffixes nil) (load-file-rep-suffixes '(""))) (get-load-suffixes)))

(insert-file-contents
 (let ((f (make-temp-file "ccore-insert-" nil nil "abcdef")))
   (unwind-protect
       (with-temp-buffer (let ((r (insert-file-contents f))) (list (cadr r) (buffer-string))))
     (delete-file f)))
 (let ((f (make-temp-file "ccore-insert-" nil nil "abcdef")))
   (unwind-protect
       (with-temp-buffer (let ((r (insert-file-contents f nil 1 4))) (list (cadr r) (buffer-string))))
     (delete-file f)))
 (condition-case e (insert-file-contents 7) (error e)))

(load
 (let ((f (make-temp-file "ccore-load-" nil ".el" ";;; -*- lexical-binding: t; -*-\n(+ 20 22)\n"))
       (load-history nil) (current-load-list nil))
   (unwind-protect (load f nil t t) (delete-file f)))
 (condition-case e (load 7 nil t) (error e)))

(locate-file-internal
 (let ((f (make-temp-file "ccore-locate-")))
   (unwind-protect
       (equal (locate-file-internal (file-name-nondirectory f) (list (file-name-directory f))) f)
     (delete-file f)))
 (locate-file-internal "ccore-no-search-path" nil))

(lock-file
 (let ((f (make-temp-file "ccore-lock-")) (create-lockfiles t))
   (unwind-protect (progn (lock-file f) (not (null (file-locked-p f))))
     (unlock-file f) (delete-file f)))
 (condition-case e (lock-file 7) (error e)))

(make-symbolic-link
 (let* ((d (make-temp-file "ccore-link-" t)) (f (expand-file-name "link" d)))
   (unwind-protect (progn (make-symbolic-link "target" f) (file-symlink-p f))
     (delete-directory d t)))
 (condition-case e (make-symbolic-link 7 "unused") (error e)))

(make-temp-name
 (string-prefix-p "ccore-name-" (make-temp-name "ccore-name-"))
 (not (equal (make-temp-name "ccore-name-") (make-temp-name "ccore-name-")))
 (condition-case e (make-temp-name 7) (error e)))

(native-comp-available-p
 (eq (native-comp-available-p) (native-comp-available-p))
 (condition-case e (native-comp-available-p 7) (error (car e))))

(native-comp-function-p
 (native-comp-function-p (symbol-function 'car))
 (native-comp-function-p '(lambda (x) x)))

(native-comp-unit-file
 (condition-case e (native-comp-unit-file nil) (error e))
 (condition-case e (native-comp-unit-file 7) (error e)))

(native-comp-unit-set-file
 (condition-case e (native-comp-unit-set-file nil "probe.eln") (error e))
 (condition-case e (native-comp-unit-set-file 7 "probe.eln") (error e)))

(open-dribble-file
 (open-dribble-file nil)
 (condition-case e (open-dribble-file 7) (error e)))

(provide
 (eval '(let ((features nil) (current-load-list nil) (s (make-symbol "ccore-feature")))
            (eq (provide s) s)) nil)
 (eval '(let ((features nil) (current-load-list nil) (s (make-symbol "ccore-subfeatures")))
            (provide s '(alpha beta))
            (list (featurep s) (featurep s 'alpha) (featurep s 'gamma))) nil)
 (eval '(let ((features nil) (current-load-list nil))
            (condition-case e (provide 7) (error e))) nil))

(rename-file
 (let* ((d (make-temp-file "ccore-rename-" t))
        (a (expand-file-name "a" d)) (b (expand-file-name "b" d)))
   (unwind-protect
       (progn (write-region "abc" nil a nil 'silent) (rename-file a b)
              (list (file-exists-p a) (file-exists-p b)))
     (delete-directory d t)))
 (condition-case e (rename-file 7 "unused") (error e)))

(require
 (eval '(let ((features '(ccore-required)) (current-load-list nil))
            (eq (require 'ccore-required) 'ccore-required)) nil)
 (eval '(let ((features nil) (current-load-list nil))
            (condition-case e (require 7) (error e))) nil))

(set-default-file-modes
 (let ((old (default-file-modes)))
   (unwind-protect (progn (set-default-file-modes #o600) (default-file-modes))
     (set-default-file-modes old)))
 (condition-case e (set-default-file-modes 'bad) (error e)))

(set-file-modes
 (let ((f (make-temp-file "ccore-modes-")))
   (unwind-protect (progn (set-file-modes f #o640) (file-modes f)) (delete-file f)))
 (condition-case e (set-file-modes 7 #o600) (error e)))

(set-file-times
 (let ((f (make-temp-file "ccore-times-")))
   (unwind-protect
       (list (set-file-times f '(0 12345 0 0))
             (equal (nth 5 (file-attributes f)) '(0 12345 0 0)))
     (delete-file f)))
 (condition-case e (set-file-times 7 '(0 12345 0 0)) (error e)))

(set-visited-file-modtime
 (with-temp-buffer (set-visited-file-modtime '(0 12345 0 0)) (visited-file-modtime))
 (with-temp-buffer (set-visited-file-modtime 0) (visited-file-modtime)))

(subr-native-comp-unit
 (null (subr-native-comp-unit (symbol-function 'car)))
 (condition-case e (subr-native-comp-unit 7) (error e)))

(substitute-in-file-name
 (substitute-in-file-name "folder/plain.txt")
 (substitute-in-file-name "folder/$$literal.txt")
 (condition-case e (substitute-in-file-name 7) (error e)))

(unhandled-file-name-directory
 (unhandled-file-name-directory "folder/plain.txt")
 (unhandled-file-name-directory "plain.txt")
 (condition-case e (unhandled-file-name-directory 7) (error e)))

(unlock-file
 (let ((f (make-temp-file "ccore-unlock-")) (create-lockfiles t))
   (unwind-protect
       (progn (lock-file f) (unlock-file f) (file-locked-p f))
     (unlock-file f) (delete-file f)))
 (condition-case e (unlock-file 7) (error e)))

(verify-visited-file-modtime
 (with-temp-buffer (verify-visited-file-modtime (current-buffer)))
 (with-temp-buffer (set-visited-file-modtime '(0 12345 0 0))
                   (verify-visited-file-modtime (current-buffer))))

(visited-file-modtime
 (with-temp-buffer (visited-file-modtime))
 (with-temp-buffer (set-visited-file-modtime '(0 12345 0 0)) (visited-file-modtime)))

(write-region
 (let ((f (make-temp-file "ccore-write-")))
   (unwind-protect
       (progn (write-region "abc" nil f nil 'silent)
              (with-temp-buffer (insert-file-contents f) (buffer-string)))
     (delete-file f)))
 (let ((f (make-temp-file "ccore-write-" nil nil "abc")))
   (unwind-protect
       (progn (write-region "def" nil f t 'silent)
              (with-temp-buffer (insert-file-contents f) (buffer-string)))
     (delete-file f)))
 (condition-case e (write-region "abc" nil 7 nil 'silent) (error e)))
