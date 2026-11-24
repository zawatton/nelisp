;;; par-charprop.el --- GNU 31.1 Unicode property and character folding parity -*- lexical-binding: t; -*-

;; Run with GNU Emacs -Q --batch -l to regenerate par-charprop.gnu.

(defmacro par-charprop--report (label &rest body)
  `(progn
     (prin1 (list ,label (condition-case err (progn ,@body)
                           (error (list :error (car err))))))
     (terpri)))

(dolist (char (list ?A ?5 ?\( ?é ?½ ?² #x0301 #x4e00 #xac01 #x1f600
                    #x0378 (max-char)))
  (par-charprop--report char
    (mapcar (lambda (property) (get-char-code-property char property))
            '(general-category decomposition name canonical-combining-class
              bidi-class mirroring numeric-value digit-value old-name))))

(par-charprop--report 'general-category-description
  (char-code-property-description 'general-category 'Lu))
(par-charprop--report 'decomposition-table
  (char-table-p (unicode-property-table-internal 'decomposition)))
(par-charprop--report 'require-char-fold
  (require 'char-fold))
(par-charprop--report 'fold-A
  (char-fold-to-regexp "A"))
(par-charprop--report 'fold-combining
  (char-fold-to-regexp "e\u0301"))
(par-charprop--report 'fold-search
  (with-temp-buffer
    (insert "café")
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (let ((result (char-fold-search-forward "cafe" nil t)))
        (list result (and result (match-string 0)))))))

(par-charprop--report 'map-decoded-value
  (let ((table (make-char-table 'char-code-property-table))
        (char #x0301))
    (set-char-table-extra-slot table 1 0)
    (set-char-table-extra-slot table 2 1)
    (set-char-table-extra-slot table 4 [nil Lu Mn])
    (put-unicode-property-internal table char 'Mn)
    (list (aref table char)
          (get-unicode-property-internal table char)
          (catch 'mapped
            (map-char-table (lambda (key value)
                              (when (eql key char)
                                (throw 'mapped value)))
                            table)))))
