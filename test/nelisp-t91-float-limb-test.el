;;; nelisp-t91-float-limb-test.el --- T91 exact limb kernel -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defconst nelisp-t91--root
  (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
(let ((root (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))
(require 'nelisp-cc-evalport-str-to-float)

(defun nelisp-t91--fn (name)
  (cl-find-if (lambda (x) (and (consp x) (eq (car x) 'defun)
                               (eq (cadr x) name)))
              (cdr nelisp-cc-evalport-str-to-float--source)))

(ert-deftest nelisp-t91-limb-kernel-is-self-contained-in-float-object ()
  (dolist (name '(nlf_big_zero nlf_big_mul_small nlf_big_trim
                  nlf_big_bitlen nlf_big_cmp nlf_big_sub nlf_big_shl1
                  nlf_big_div53 nlf_big_add_small nlf_big_mul10_add
                  nlf_exp_sat))
    (should (nelisp-t91--fn name))))

(ert-deftest nelisp-t91-div53-retains-exact-remainder-for-ties ()
  (let ((form (prin1-to-string (nelisp-t91--fn 'nlf_big_div53))))
    (should (string-match-p "nlf_big_shl1" form))
    (should (string-match-p "nlf_big_cmp" form))
    (should (string-match-p "nlf_big_sub" form))))

(ert-deftest nelisp-t91-independent-python-oracle-corpus ()
  "Run the standalone parser and compare every result with Python struct bits."
  (let* ((script (expand-file-name "test/nelisp-t91-float-oracle.py"
                                  nelisp-t91--root))
         (output (generate-new-buffer " *nelisp-t91-oracle*"))
         (rc (call-process "python3" nil output nil script)))
    (unwind-protect
        (progn
          (let ((report (with-current-buffer output
                          (buffer-string))))
            (cond
             ((= rc 77)
              (ert-skip (string-trim report)))
             ((/= rc 0)
              (message "%s" report)
              (should (= rc 0))))))
      (kill-buffer output))))

(provide 'nelisp-t91-float-limb-test)
