;;; nelisp-fresh-frame-rbx-pad-test.el --- Policy save-area pad -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(load (expand-file-name "nelisp-aot-fresh-frame-test.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defconst nelisp-rbx-pad--fixture
  (expand-file-name "fixtures/nelisp-rbx-pad-unselected.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-rbx-pad-nonpolicy-complete-bytes-unchanged ()
  (let ((cases (with-temp-buffer (insert-file-contents nelisp-rbx-pad--fixture)
                                (goto-char (point-min)) (read (current-buffer)))))
    (should (= (length cases) 6))
    (dolist (case cases)
      (let* ((ir (nelisp-fresh-frame-test--definition (nth 1 case) (nth 2 case)))
             (bytes (nelisp-fresh-frame-test--emit ir nil (car case))))
        (should (equal (secure-hash 'sha256 bytes) (nth 3 case)))))))

(ert-deftest nelisp-rbx-pad-policy-store-follows-rbx-save ()
  ;; Independent acceptance matrix covers odd/even spill and temporary
  ;; counts, no temporary region, and a displacement requiring disp32.
  (dolist (spec '((0 0) (1 1) (3 3) (4 16) (6 256)))
    (let* ((n (car spec)) (r (cadr spec))
           (params (cl-subseq '(a b c d e f) 0 n))
           (ir (nelisp-fresh-frame-test--definition
                `(defun fresh_probe ,params ,(if params (car params) 7)) r))
           (bytes (nelisp-fresh-frame-test--emit ir t))
           (p-rounded (* 2 (/ (+ n 1) 2)))
           (r-rounded (* 2 (/ (+ r 1) 2)))
           (save-disp (- (* 8 (+ p-rounded r-rounded 1))))
           (pad-disp (- (* 8 (+ p-rounded r-rounded 2))))
           (save (nelisp-asm-x86_64-make-buffer))
           (zero (concat (unibyte-string #x48 #xc7 #x85)
                         (nelisp-asm-x86_64--imm32-bytes pad-disp)
                         (unibyte-string 0 0 0 0))))
      (nelisp-asm-x86_64-mov-mem-reg-disp8 save 'rbp save-disp 'rbx)
      (should (string-match-p
               (regexp-quote (concat (nelisp-asm-x86_64-resolve-fixups save) zero)) bytes)))))

(provide 'nelisp-fresh-frame-rbx-pad-test)
