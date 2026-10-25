;;; nelisp-parity6-test.el --- Automatic locals and unintern -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'bytecomp)

(defvar p6-auto 100)
(defvar p6-explicit 100)
(defconst p6--root
  (file-name-directory (directory-file-name
                       (file-name-directory (or load-file-name buffer-file-name)))))
(defconst p6--cases
  '((automatic-set
     (with-temp-buffer
       (setq p6-auto 5)
       (list (local-variable-p 'p6-auto) p6-auto (default-value 'p6-auto)))
     (t 5 100))
    (automatic-set-function
     (with-temp-buffer
       (set 'p6-auto 9)
       (list (local-variable-p 'p6-auto) p6-auto (default-value 'p6-auto)))
     (t 9 100))
    (local-let
     (with-temp-buffer
       (setq p6-auto 5)
       (list (let ((p6-auto 6)) (list p6-auto (default-value 'p6-auto)))
             p6-auto (default-value 'p6-auto)))
     ((6 100) 5 100))
    (default-let-no-local
     (with-temp-buffer
       (list (let ((p6-auto 7))
               (setq p6-auto 8)
               (list p6-auto (local-variable-p 'p6-auto) (default-value 'p6-auto)))
             p6-auto (local-variable-p 'p6-auto)))
     ((8 nil 8) 100 nil))
    (default-let-switch
     (with-temp-buffer
       (let ((b (generate-new-buffer " p6-other")))
         (unwind-protect
             (list (let ((p6-auto 7))
                     (with-current-buffer b (setq p6-auto 8))
                     (list p6-auto (with-current-buffer b p6-auto)))
                   p6-auto (with-current-buffer b p6-auto))
           (kill-buffer b))))
     ((7 8) 100 8))
    (explicit-local-switch-unwind
     (with-temp-buffer
       (make-local-variable 'p6-explicit)
       (setq p6-explicit 5)
       (let ((a (current-buffer)) (b (generate-new-buffer " p6-explicit-other")))
         (unwind-protect
             (list (condition-case nil
                       (let ((p6-explicit 6)) (set-buffer b)
                         (list p6-explicit (with-current-buffer a p6-explicit)))
                     (error 'failed))
                   (with-current-buffer a p6-explicit) p6-explicit
                   (default-value 'p6-explicit))
           (kill-buffer b))))
     ((100 6) 5 100 100))
    (unintern-zero (unintern) (wrong-number-of-arguments unintern 0))
    (unintern-one (unintern "p6-absent") (wrong-number-of-arguments unintern 1))
    (unintern-three (unintern "p6-absent" nil nil) (wrong-number-of-arguments unintern 3))
    (unintern-two (unintern "p6-absent" nil) nil)))

(defun p6--included-p (mode row)
  ;; GNU bytecode's invalid-arity datum is an unreadable subr object;
  ;; compare complete conditions through the interpreter. Valid calls and
  ;; all buffer-local behavior still run in both substrates.
  (not (and (eq mode 'bytecode)
            (memq (car row) '(unintern-zero unintern-one unintern-three)))))
(defvar p6--results nil)
(defvar p6--attempted nil)
(defun p6--run ()
  (or p6--results
      (let* ((binary (or (getenv "NELISP_PARITY6_BIN") (ert-skip "Set NELISP_PARITY6_BIN")))
             (out (expand-file-name (or (getenv "NELISP_PARITY6_OUT") "target/parity6/ert") p6--root))
             (fixture (expand-file-name "cases.el" out))
             (stderr (expand-file-name "runtime.stderr" out)))
        (when p6--attempted
          (ert-fail "Previous native batch failed; inspect runtime.stdout/runtime.stderr"))
        (setq p6--attempted t)
        (make-directory out t)
        (with-temp-file fixture
          (insert ";;; -*- lexical-binding: t; -*-\n")
          (dolist (form '((defvar p6-auto 100) (defvar p6-explicit 100)
                          (make-variable-buffer-local 'p6-auto)))
            (prin1 form (current-buffer)) (insert "\n"))
          (dolist (mode '(interpreted bytecode))
            (dolist (row (cl-remove-if-not (lambda (r) (p6--included-p mode r)) p6--cases))
              (let* ((form (cadr row))
                     (call (if (eq mode 'interpreted) `(eval ',form nil)
                             `(funcall ',(byte-compile `(lambda () ,form))))))
                (prin1 `(progn (prin1 (list ',mode ',(car row)
                                           (condition-case e ,call (error e))))
                               (terpri)) (current-buffer))
                (insert "\n"))))
          (insert "(princ \"PARITY6 COMPLETE\\n\")\n(kill-emacs 0)\n"))
        (with-temp-buffer
          (let ((rc (apply #'call-process binary nil (list t stderr) nil
                           (append (when (getenv "NELISP_PARITY6_GNU") '("-Q" "--batch"))
                                   (list "--load" fixture)))))
            (write-region (point-min) (point-max) (expand-file-name "runtime.stdout" out) nil 'silent)
            (should (equal rc 0))
            (should (= (file-attribute-size (file-attributes stderr)) 0))
            (goto-char (point-min))
            (dotimes (_ (- (* 2 (length p6--cases)) 3)) (push (read (current-buffer)) p6--results))
            (skip-chars-forward " \n\r")
            (should (equal (buffer-substring (point) (point-max)) "PARITY6 COMPLETE\n"))
            (setq p6--results (nreverse p6--results)))))))

(dolist (mode '(interpreted bytecode))
  (dolist (row (cl-remove-if-not (lambda (r) (p6--included-p mode r)) p6--cases))
    (eval `(ert-deftest ,(intern (format "parity6-%s-%s" mode (car row))) ()
             (should (equal (nth 2 (cl-find-if
                                    (lambda (r) (and (eq (car r) ',mode)
                                                     (eq (cadr r) ',(car row))))
                                    (p6--run))) ',(nth 2 row)))) t)))
(defvar p6-source-root (or (getenv "P6_SOURCE_ROOT") default-directory))
(defun p6-source-function (file name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file p6-source-root))
    (goto-char (point-min))
    (re-search-forward (format "(defun %s " name))
    (goto-char (match-beginning 0))
    (let ((form (read (current-buffer))))
      (eval (cons 'lambda (cddr form)) t))))
(ert-deftest p6-source-auto-registration ()
  (let* ((name (make-symbol "p6-registration")) (slot nil)
         (f (p6-source-function "scripts/nelisp-stdlib-prelude.el" 'make-variable-buffer-local)))
    (cl-letf (((symbol-function 'nelisp--env-globals-op)
               (lambda (op _name &optional value)
                 (pcase op
                   ('variable-entry-slot nil)
                   ('variable-entry-slot-create slot)
                   ('variable-entry-slot-set (setq slot (cdr value)))))))
      (should (eq (funcall f name) name))
      (should (equal slot [nelisp--current-buffer nil make-local-variable]))
      (should (default-boundp name))
      (should-not (default-value name))
      (let ((buffer (current-buffer)) (cell (make-symbol "local")))
        (aset slot 1 (list (cons buffer cell)))
        (funcall f name)
        (should (eq (cdar (aref slot 1)) cell)))
      (should (eq (condition-case e (funcall f nil) (error (car e))) 'setting-constant)))))
(ert-deftest p6-source-bootstrap-registration ()
  ;; The early case-table registrations run before the alias provider.
  (let* ((name (make-symbol "p6-bootstrap")) (slot nil)
         (f (p6-source-function "scripts/nelisp-stdlib-prelude.el" 'make-variable-buffer-local)))
    (cl-letf (((symbol-function 'indirect-variable) nil)
              ((symbol-function 'nelisp--env-globals-op)
               (lambda (op _name &optional value)
                 (pcase op
                   ('variable-entry-slot nil)
                   ('variable-entry-slot-create slot)
                   ('variable-entry-slot-set (setq slot (cdr value)))))))
      (should (eq (funcall f name) name))
      (should (equal slot [nelisp--current-buffer nil make-local-variable])))))
(ert-deftest p6-source-unintern-arity ()
  (let ((f (p6-source-function "scripts/nelisp-stdlib-prelude.el" 'unintern)))
    (dolist (args '(nil ("absent") ("absent" nil nil)))
      (should (equal (condition-case e (apply f args) (error e))
                     (list 'wrong-number-of-arguments 'unintern (length args)))))))

(provide 'nelisp-parity6-test)
