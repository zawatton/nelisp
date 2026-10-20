;;; standalone-sqlite-owned-check.el --- SQLite owned-buffer parity -*- lexical-binding: t; -*-

;;; Commentary:
;; Also runs unchanged on GNU Emacs with SQLite support. Set
;; NELISP_SQLITE_FFI_SOURCE to test the external compatibility package used by
;; Anvil; otherwise exercise this checkout's compatibility implementation.

;;; Code:

(declare-function nelisp--exit-process "ext:nelisp-runtime" (code))

(defvar sqlite-owned--prepared 0)
(defvar sqlite-owned--finalized 0)
(defvar sqlite-owned--opened 0)
(defvar sqlite-owned--closed 0)
(defvar sqlite-owned--original-call nil)

(when (fboundp 'nl-ffi-call)
  (load "packages/nl-ffi/src/nl-ffi-memory.el" nil t)
  (load (or (getenv "NELISP_SQLITE_FFI_SOURCE")
            "packages/nelisp-emacs-io/src/emacs-sqlite-ffi.el") nil t)
  (setq sqlite-owned--original-call (symbol-function 'emacs-sqlite-ffi--call))
  (fset 'emacs-sqlite-ffi--call
        (lambda (name &rest args)
          (let ((result (apply sqlite-owned--original-call name args)))
            (cond ((and (equal name "sqlite3_prepare_v2") (= result 0))
                   (setq sqlite-owned--prepared (1+ sqlite-owned--prepared)))
                  ((equal name "sqlite3_finalize")
                   (setq sqlite-owned--finalized (1+ sqlite-owned--finalized)))
                  ((and (equal name "sqlite3_open_v2") (= result 0))
                   (setq sqlite-owned--opened (1+ sqlite-owned--opened)))
                  ((and (equal name "sqlite3_close_v2") (= result 0))
                   (setq sqlite-owned--closed (1+ sqlite-owned--closed))))
            result))))

(unless (sqlite-available-p) (error "SQLITE-OWNED: SQLite unavailable"))

(let ((db (sqlite-open nil)))
  (unwind-protect
      (progn
        (sqlite-execute db "create table sample (id integer, text text, n integer)")
        (sqlite-execute db "insert into sample values (?1, ?2, ?3)"
                        (vector 7 "日本語の点検・SQLite" 123456))
        (garbage-collect)
        (let ((rows (sqlite-select db "select id, text, n from sample where text = ?"
                                   '("日本語の点検・SQLite"))))
          (unless (equal rows '((7 "日本語の点検・SQLite" 123456)))
            (error "SQLITE-OWNED: unexpected rows %S" rows))
          (prin1 rows) (princ "\n"))
        ;; The FFI wrapper finalizes every statement even after a binding
        ;; failure. A subsequent query must remain usable on the same DB.
        (condition-case nil
            (sqlite-select db "select ?" '("a" "b"))
          (error nil))
        (unless (equal (sqlite-select db "select count(*) from sample") '((1)))
          (error "SQLITE-OWNED: query after binding error failed")))
    (sqlite-close db)))

(when sqlite-owned--original-call
  (fset 'emacs-sqlite-ffi--call sqlite-owned--original-call)
  (unless (and (> sqlite-owned--prepared 0)
               (= sqlite-owned--prepared sqlite-owned--finalized)
               (> sqlite-owned--opened 0)
               (= sqlite-owned--opened sqlite-owned--closed))
    (error "SQLITE-OWNED: statements or databases were not closed")))

(princ "SQLITE-OWNED: PASS\n")
;; Keep the isolated check's complete stdout identical to GNU batch output.
(when (fboundp 'nelisp--exit-process) (nelisp--exit-process 0))

;;; standalone-sqlite-owned-check.el ends here
