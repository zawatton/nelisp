;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
;; The current syntax table is per buffer (E7).  `with-syntax-table' sets the current
;; buffer's table and restores it; a new buffer starts with the standard table;
;; `set-syntax-table' touches only the current buffer; each buffer shows its own table
;; after a switch.  Spec = GNU 31.1 (par-syntax-local.gnu).
(set-syntax-table (standard-syntax-table))
(defvar sl-main (generate-new-buffer " main") "Work buffer: batch's own buffer may be read-only.")
(set-buffer sl-main)
(defmacro row (n f)
  `(progn (set-buffer sl-main)
          (princ (format "%-30s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e))))))
          (set-buffer sl-main)))
(defun sl-tab (&rest specs)
  "A fresh syntax table with SPECS, a list of (CHAR . DESCRIPTOR)."
  (let ((st (make-syntax-table)))
    (dolist (s specs) (modify-syntax-entry (car s) (cdr s) st))
    st))
(defun sl-probe ()
  "What the current buffer's table says, through five readers."
  (let ((s "a_b-c"))
    (list (mapcar #'char-syntax '(?a ?_ ?- ?\s))
          (string-match "\\w+" s)
          (and (string-match "\\w+" s) (match-end 0))
          (progn (erase-buffer) (insert s) (goto-char 1)
                 (list (looking-at "\\w+") (match-end 0)))
          (progn (goto-char 1) (forward-word 1) (point))
          (progn (goto-char 1) (skip-syntax-forward "w_") (point)))))
(defvar sl-w (sl-tab '(?_ . "w")))
(defvar sl-s (sl-tab '(?a . "_") '(?- . "w")))
(row "L1 fresh buf inside wst"
     (with-syntax-table sl-w
       (with-temp-buffer (list (eq (syntax-table) (standard-syntax-table)) (sl-probe)))))
(row "L2 same, outer probe"
     (with-syntax-table sl-w
       (list (eq (syntax-table) sl-w) (with-temp-buffer (sl-probe)) (eq (syntax-table) sl-w))))
(row "L3 table after wst exits"
     (let ((before (syntax-table)))
       (with-syntax-table sl-w nil)
       (list (eq before (syntax-table)) (eq (syntax-table) (standard-syntax-table)))))
(row "L4 after temp-buffer exits"
     (with-syntax-table sl-w
       (with-temp-buffer (set-syntax-table sl-s) (sl-probe))
       (list (eq (syntax-table) sl-w) (eq (syntax-table) sl-s))))
(row "L5 set in A, switch B and back"
     (let ((a (generate-new-buffer " a")) (b (generate-new-buffer " b")) r)
       (unwind-protect
           (progn
             (with-current-buffer a (set-syntax-table sl-w))
             (with-current-buffer b (set-syntax-table sl-s))
             (setq r (list (with-current-buffer a (list (eq (syntax-table) sl-w) (sl-probe)))
                           (with-current-buffer b (list (eq (syntax-table) sl-s) (sl-probe)))
                           (with-current-buffer a (eq (syntax-table) sl-w))
                           (eq (syntax-table) (standard-syntax-table))))
             (set-buffer a) (setq r (append r (list (eq (syntax-table) sl-w))))
             (set-buffer b) (setq r (append r (list (eq (syntax-table) sl-s)))))
         (kill-buffer a) (kill-buffer b))
       r))
(row "L6 set-buffer leaves global"
     (let ((a (generate-new-buffer " a")) (cur (current-buffer)) r)
       (unwind-protect
           (progn (set-buffer a) (set-syntax-table sl-w)
                  (setq r (list (eq (syntax-table) sl-w)))
                  (set-buffer cur)
                  (setq r (append r (list (eq (syntax-table) sl-w)
                                          (eq (syntax-table) (standard-syntax-table))
                                          (progn (erase-buffer) (sl-probe))))))
         (kill-buffer a))
       r))
(row "L7 wst in A, body switches to B"
     (let ((a (generate-new-buffer " a")) (b (generate-new-buffer " b")) r)
       (unwind-protect
           (progn
             (with-current-buffer a
               (with-syntax-table sl-w
                 (set-buffer b)
                 (setq r (list (eq (syntax-table) (standard-syntax-table)) (sl-probe)))
                 (set-buffer a)
                 (setq r (append r (list (eq (syntax-table) sl-w))))))
             (setq r (append r (list (with-current-buffer a
                                       (eq (syntax-table) (standard-syntax-table)))))))
         (kill-buffer a) (kill-buffer b))
       r))
(row "L8 kill-buffer then new buffer"
     (let ((a (generate-new-buffer " a")))
       (with-current-buffer a (set-syntax-table sl-w))
       (kill-buffer a)
       (let ((c (generate-new-buffer " c")))
         (prog1 (with-current-buffer c (list (eq (syntax-table) (standard-syntax-table)) (sl-probe)))
           (kill-buffer c)))))
(row "L9 nested wst"
     (with-syntax-table sl-w
       (let ((o1 (syntax-table)))
         (list (with-syntax-table sl-s (list (eq (syntax-table) sl-s) (sl-probe)))
               (eq (syntax-table) o1) (eq o1 sl-w) (sl-probe)))))
(row "L10 wst non-local exit"
     (list (catch 'x (with-syntax-table sl-w (throw 'x (eq (syntax-table) sl-w))))
           (eq (syntax-table) (standard-syntax-table))
           (condition-case nil (with-syntax-table sl-w (error "boom")) (error (eq (syntax-table) (standard-syntax-table))))))
(row "L11 fresh buf standard id"
     (list (with-temp-buffer (eq (syntax-table) (standard-syntax-table)))
           (with-temp-buffer (sl-probe))
           (with-temp-buffer (set-syntax-table sl-w) (with-temp-buffer (eq (syntax-table) (standard-syntax-table))))))
(row "L12 string-match in temp inside wst"
     (cl-flet ((m () (list (string-match "\\w+" "-_a") (match-end 0))))
       (list (with-syntax-table sl-w (with-temp-buffer (m)))
             (with-syntax-table sl-w (m))
             (with-syntax-table sl-w (with-temp-buffer (set-syntax-table sl-s) (m)))
             (m))))
(row "L13 wst in killed buffer"
     (let ((a (generate-new-buffer " a")))
       (with-current-buffer a (with-syntax-table sl-w (kill-buffer a)))
       (eq (syntax-table) (standard-syntax-table))))
(row "L14 set-syntax-table returns"
     (with-temp-buffer (list (eq (set-syntax-table sl-w) sl-w) (condition-case e (set-syntax-table 5) (error (car e))))))
