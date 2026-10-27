;;; native-row-census.el --- re-harvest every native row and compare  -*- lexical-binding: t; -*-
;; usage (via tools/ai/native-row-census.sh): NRC_REPO=DIR BINARY --load tools/ai/native-row-census.el
;; For every row of lisp/nelisp-fxir-native-data.el in NRC_REPO: define the row's
;; defun from NRC_REPO's current sources (lisp/*.el, scripts/*.el; a top-level form,
;; or one under unless/when/progn/eval-and-compile/if; the LAST definition found in
;; file-name order wins, as the later load would), as a lexical closure the way the
;; image holds it; compile it with the harvest machinery (tools/ai/nc1-harvest.el's
;; register + `nelisp-fxir-recompile'); compare with the stored row.  A name with no
;; source definition uses the image's closure.  Lines printed:
;;   STALE NAME ID reason     DUP-ID ID NAME NAME2     DUP-NAME NAME ID ID2
;;   NRC-NOTE NAME ...        (informational: source origin)
;; and a final "native-row-census: rows=N stale=S dup=D".
(load (expand-file-name "test/nelisp-fxir-eval.el" (getenv "NRC_REPO")) nil t)
(defvar nrc--repo (file-name-as-directory (getenv "NRC_REPO")))
(defun nrc--read-forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((forms nil) (form t))
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (push form forms))
      (nreverse forms))))
(defun nrc--defuns (form want tbl file)
  "Push onto TBL (name -> list of (FORM . FILE)) the defuns of WANT inside FORM."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'defun)
    (when (memq (nth 1 form) want)
      (puthash (nth 1 form) (cons (cons form file) (gethash (nth 1 form) tbl)) tbl)))
   ((memq (car form) '(unless when progn eval-and-compile if))
    (dolist (f (cdr form)) (nrc--defuns f want tbl file)))))
;; Definitions the standalone build embeds as concatenated string literals
;; ("(defun NAME ...\n" "...") are read back out of the strings.
(defun nrc--embedded (form want tbl file)
  (when (consp form)
    (let ((l form) (run nil))
      (while (consp l)
        (let ((x (car l)))
          (cond ((stringp x) (push x run))
                (t (nrc--embedded-flush run want tbl file) (setq run nil)
                   (when (consp x) (nrc--embedded x want tbl file)))))
        (setq l (cdr l)))
      (nrc--embedded-flush run want tbl file))))
(defun nrc--embedded-flush (run want tbl file)
  (when run
    (let ((text (apply #'concat (reverse run))) (pos 0))
      (while (string-match "(defun \\([^ \n()]+\\) " text pos)
        (let ((start (match-beginning 0)) (name (intern (match-string 1 text))))
          (setq pos (match-end 0))
          (when (memq name want)
            (let ((form (condition-case nil (car (read-from-string text start)) (error nil))))
              (when (eq (car-safe form) 'defun)
                (puthash name (cons (cons form file) (gethash name tbl)) tbl)))))))))
(defun nrc--sources (want)
  (let ((tbl (make-hash-table :test 'eq)) (miss nil)
        (files (append (directory-files (concat nrc--repo "lisp") t "\\.el\\'")
                       (directory-files (concat nrc--repo "scripts") t "\\.el\\'"))))
    (nrc--scan files want tbl)
    tbl))
(defun nrc--scan (files want tbl)
  (progn
    (dolist (file files)
      (when (and (not (string-match-p "native-data\\|native-corpus" file))
                 (with-temp-buffer (insert-file-contents file) (search-forward "(defun " nil t)))
        (dolist (form (condition-case nil (nrc--read-forms file) (error nil)))
          (nrc--defuns form want tbl (file-name-nondirectory file))
          (when (string-match-p "standalone-build" file)
            (nrc--embedded form want tbl (file-name-nondirectory file))))))))
;; Compile NAME's current definition (CAND = (FORM . FILE) to define first, or nil for
;; the image's closure) and return nil when it reproduces ROW, else a reason string.
(defun nrc--check (name row cand)
  "Check ROW against definition CAND of NAME (nil: the image closure); nil when current.
NAME keeps its image definition afterwards."
  (let ((saved (and (fboundp name) (symbol-function name))))
    (unwind-protect (nrc--check-1 name row cand)
      (when (and cand saved) (fset name saved)))))
(defun nrc--check-1 (name row cand)
  (let ((id (nth 1 row)) (reason nil))
    (when cand
      (condition-case e (eval (car cand) t)
        (error (setq reason (format "eval-error %S" e)))))
    (let ((fn (and (fboundp name) (symbol-function name))))
      (when (and (eq (car-safe fn) 'closure) (eq (gethash (nthcdr 3 fn) nelisp--fx-table) 'none))
        (remhash (nthcdr 3 fn) nelisp--fx-table))
      (let* ((rec (and (eq (car-safe fn) 'closure)
                       (progn (nelisp--fx-register name) (gethash (nthcdr 3 fn) nelisp--fx-table))))
             (c (and (vectorp rec) (/= (aref rec 1) -1) (nelisp-fxir-recompile fn))))
        (cond
         (reason reason)
         ((not (eq (car-safe fn) 'closure)) (format "no-closure %S" (type-of fn)))
         ((not (vectorp rec)) (format "record=%S" rec))
         ((or (null c) (null (nth 2 c)) (not (nelisp-fxir-code-same-p (car c) (aref rec 0))))
          "recompile-differs")
         (t (let ((new (list name id (max 0 (nth 1 c)) (nelisp-fxir-callees (nth 2 c) (car c)) (nth 2 c)
                             (list (aref rec 2) (aref rec 5) (aref rec 6)))))
              (cond ((equal new row) nil)
                    ((not (equal (nth 4 new) (nth 4 row))) "ir")
                    ((not (equal (nth 3 new) (nth 3 row))) "callees")
                    (t "nlocals/arity")))))))))
(let* ((rows (let ((r nil))
               (dolist (form (nrc--read-forms (concat nrc--repo "lisp/nelisp-fxir-native-data.el")))
                 (when (and (eq (car-safe form) 'defconst) (eq (nth 1 form) 'nelisp-fxir-native-data))
                   (setq r (eval (nth 2 form) t))))
               r))
       (src (nrc--sources (mapcar #'car rows)))
       (stale 0) (dup 0) (ids (make-hash-table :test 'eql)) (names (make-hash-table :test 'eq)))
  (dolist (row rows)
    (let* ((name (car row)) (id (nth 1 row)) (cands (reverse (gethash name src)))
           (prev-id (gethash id ids)) (prev-name (gethash name names)) (reason nil))
      (when prev-id (setq dup (1+ dup)) (princ (format "DUP-ID %S %S %S\n" id prev-id name)))
      (when prev-name (setq dup (1+ dup)) (princ (format "DUP-NAME %S %S %S\n" name prev-name id)))
      (puthash id name ids) (puthash name id names)
      (if (null cands)
          (if (not (eq (car-safe (and (fboundp name) (symbol-function name))) 'closure))
              (princ (format "NRC-NOTE %S skipped: no source definition and the image holds a primitive\n" name))
            (progn (princ (format "NRC-NOTE %S no-source-def, image closure used\n" name))
                 (setq reason (nrc--check name row nil))))
        ;; Several definitions (prelude fallbacks, per-package copies): the row is
        ;; current when any of them reproduces it.
        (let ((reasons nil) (ok nil))
          (dolist (cand cands)
            (unless ok
              (let ((r (nrc--check name row cand)))
                (if r (push (format "%s@%s" r (cdr cand)) reasons) (setq ok t)))))
          (unless ok (setq reason (mapconcat #'identity (nreverse reasons) ",")))))
      (when reason
        (setq stale (1+ stale))
        (princ (format "STALE %S %S %s\n" name id reason)))))
  (princ (format "native-row-census: rows=%d stale=%d dup=%d\n" (length rows) stale dup))
  (kill-emacs (if (and (= stale 0) (= dup 0)) 0 1)))
