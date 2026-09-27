;;; emacs-sqlite-ffi-test.el --- ERT tests for emacs-sqlite-ffi -*- lexical-binding: t; -*-

;;; Commentary:

;; v2 (2026-09-04): the adapter now sits on the NeLisp v1.2.0 reader's
;; name-dispatch `nl-ffi-call' plus `alloc-bytes' / `ptr-*'.  Host Emacs
;; has none of those, so the tests below stand up a fake byte memory and
;; a fake `nl-ffi-call' with `cl-letf' and exercise the marshalling and
;; the row / cursor logic through them.  The real backend is proven by
;; nelisp's `standalone-reader-ffi-smoke' ([ffi-smoke S1 sqlite]) and by
;; the anvil MCP driver's worklog / memory tools on the reader; nothing
;; here talks to a real SQLite.
;;
;; The previous tests covered a shared-library path resolver that no
;; longer exists.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-sqlite-ffi)

;;;; --- fake memory + fake FFI --------------------------------------------

(defvar emacs-sqlite-ffi-test--mem nil
  "Hash table ADDRESS -> byte for the fake pointer primitives.")

(defvar emacs-sqlite-ffi-test--next 4096
  "Next address handed out by the fake `alloc-bytes'.")

(defvar emacs-sqlite-ffi-test--calls nil
  "Reverse-ordered log of (NAME . ARGS) seen by the fake `nl-ffi-call'.")

(defvar emacs-sqlite-ffi-test--ffi nil
  "Function (NAME ARGS) -> value backing the fake `nl-ffi-call'.")

(defun emacs-sqlite-ffi-test--alloc (n _align)
  (let ((p emacs-sqlite-ffi-test--next))
    (setq emacs-sqlite-ffi-test--next (+ p n 16))
    p))

(defun emacs-sqlite-ffi-test--read-u8 (p off)
  (or (gethash (+ p off) emacs-sqlite-ffi-test--mem) 0))

(defun emacs-sqlite-ffi-test--write-u8 (p off byte)
  (puthash (+ p off) byte emacs-sqlite-ffi-test--mem)
  byte)

(defun emacs-sqlite-ffi-test--read-u64 (p off)
  (let ((v 0) (i 7))
    (while (>= i 0)
      (setq v (+ (* v 256) (emacs-sqlite-ffi-test--read-u8 p (+ off i))))
      (setq i (1- i)))
    v))

(defun emacs-sqlite-ffi-test--write-u64 (p off v)
  (let ((i 0))
    (while (< i 8)
      (emacs-sqlite-ffi-test--write-u8 p (+ off i) (% v 256))
      (setq v (/ v 256))
      (setq i (1+ i)))))

(defun emacs-sqlite-ffi-test--store-cstring (string)
  "Put STRING into fake memory NUL-terminated; return its address."
  (let* ((bytes (encode-coding-string string 'utf-8 t))
         (p (emacs-sqlite-ffi-test--alloc (1+ (length bytes)) 1))
         (i 0))
    (while (< i (length bytes))
      (emacs-sqlite-ffi-test--write-u8 p i (aref bytes i))
      (setq i (1+ i)))
    (emacs-sqlite-ffi-test--write-u8 p (length bytes) 0)
    p))

(defun emacs-sqlite-ffi-test--fetch-cstring (p)
  "Read the NUL-terminated string at P from fake memory."
  (let ((acc nil) (i 0) (b nil))
    (while (/= (setq b (emacs-sqlite-ffi-test--read-u8 p i)) 0)
      (push b acc)
      (setq i (1+ i)))
    (decode-coding-string (apply #'unibyte-string (nreverse acc)) 'utf-8 t)))

(defmacro emacs-sqlite-ffi-test--with-fake-backend (ffi &rest body)
  "Run BODY with fake pointer primitives and FFI as the `nl-ffi-call' body."
  (declare (indent 1))
  `(let ((emacs-sqlite-ffi-test--mem (make-hash-table :test 'eql))
         (emacs-sqlite-ffi-test--next 4096)
         (emacs-sqlite-ffi-test--calls nil)
         (emacs-sqlite-ffi-test--ffi ,ffi))
     (cl-letf (((symbol-function 'alloc-bytes) #'emacs-sqlite-ffi-test--alloc)
               ((symbol-function 'ptr-read-u8) #'emacs-sqlite-ffi-test--read-u8)
               ((symbol-function 'ptr-write-u8) #'emacs-sqlite-ffi-test--write-u8)
               ((symbol-function 'ptr-read-u64) #'emacs-sqlite-ffi-test--read-u64)
               ((symbol-function 'nl-ffi-call)
                (lambda (name &rest args)
                  (push (cons name args) emacs-sqlite-ffi-test--calls)
                  (funcall emacs-sqlite-ffi-test--ffi name args))))
       ,@body)))

;;;; --- a tiny scripted sqlite ---------------------------------------------

(defun emacs-sqlite-ffi-test--scripted (rows)
  "Return an FFI function that serves ROWS (lists of (TYPE . VALUE)) for one
statement, records SQL and binds, and answers the bookkeeping calls."
  (let ((remaining nil)
        (current nil)
        (col-names '("a" "b" "c")))
    (lambda (name args)
      (pcase name
        ("sqlite3_libversion" (emacs-sqlite-ffi-test--store-cstring "3.51.1"))
        ("sqlite3_open_v2"
         (emacs-sqlite-ffi-test--write-u64 (nth 1 args) 0 #x1000)
         0)
        ("sqlite3_close_v2" 0)
        ("sqlite3_errmsg" (emacs-sqlite-ffi-test--store-cstring "scripted error"))
        ("sqlite3_prepare_v2"
         (setq remaining rows current nil)
         (emacs-sqlite-ffi-test--write-u64 (nth 3 args) 0 #x2000)
         0)
        ("sqlite3_step"
         (if remaining
             (progn (setq current (pop remaining)) 100)
           (setq current nil) 101))
        ("sqlite3_finalize" 0)
        ("sqlite3_changes" 3)
        ("sqlite3_column_count" (length (car (or rows '(nil)))))
        ("sqlite3_column_name"
         (emacs-sqlite-ffi-test--store-cstring (nth (nth 1 args) col-names)))
        ("sqlite3_column_type" (car (nth (nth 1 args) current)))
        ("sqlite3_column_int64" (cdr (nth (nth 1 args) current)))
        ("sqlite3_column_double" (cdr (nth (nth 1 args) current)))
        ("sqlite3_column_text"
         (emacs-sqlite-ffi-test--store-cstring (cdr (nth (nth 1 args) current))))
        ("sqlite3_column_bytes"
         (length (encode-coding-string (cdr (nth (nth 1 args) current)) 'utf-8 t)))
        ((or "sqlite3_bind_null" "sqlite3_bind_int64" "sqlite3_bind_double"
             "sqlite3_bind_text")
         0)
        (_ (error "unscripted FFI call %s" name))))))

;;;; --- tests --------------------------------------------------------------

(ert-deftest emacs-sqlite-ffi-test/host-emacs-stays-inert ()
  "Loading the adapter under host Emacs never shadows the C sqlite subrs."
  (should-not (emacs-sqlite-ffi-backend-p))
  (should-not (emacs-sqlite-ffi-available-p))
  (should-not (emacs-sqlite-ffi-install))
  (when (fboundp 'sqlite-open)
    (should (subrp (symbol-function 'sqlite-open)))))

(ert-deftest emacs-sqlite-ffi-test/cstring-roundtrip-utf8 ()
  "A multibyte string is written as NUL-terminated UTF-8 and read back."
  (emacs-sqlite-ffi-test--with-fake-backend (lambda (_n _a) 0)
    (let* ((s "é日本 ok")
           (p (emacs-sqlite-ffi--cstring s)))
      (should (= (emacs-sqlite-ffi-test--read-u8 p 0) #xC3))
      (should (= (emacs-sqlite-ffi-test--read-u8
                  p (length (encode-coding-string s 'utf-8 t)))
                 0))
      (should (equal (emacs-sqlite-ffi--read-cstring p) s))
      (should-not (emacs-sqlite-ffi--read-cstring 0)))))

(ert-deftest emacs-sqlite-ffi-test/bulk-primitives-take-the-fast-path ()
  "With ptr-write-bytes / ptr-read-bytes present, no per-byte walk happens."
  (emacs-sqlite-ffi-test--with-fake-backend (lambda (_n _a) 0)
    (let ((bulk-writes nil) (bulk-reads nil))
      (cl-letf (((symbol-function 'ptr-write-bytes)
                 (lambda (p bytes)
                   (push (cons p bytes) bulk-writes)
                   (let ((i 0))
                     (while (< i (length bytes))
                       (emacs-sqlite-ffi-test--write-u8 p i (aref bytes i))
                       (setq i (1+ i))))
                   (length bytes)))
                ((symbol-function 'ptr-read-bytes)
                 (lambda (p n)
                   (push (cons p n) bulk-reads)
                   (let ((acc nil) (i 0))
                     (while (< i n)
                       (push (emacs-sqlite-ffi-test--read-u8 p i) acc)
                       (setq i (1+ i)))
                     (apply #'unibyte-string (nreverse acc)))))
                ((symbol-function 'ptr-write-u8)
                 (lambda (p off byte)
                   ;; Only the terminating NUL may still go through here.
                   (should (= byte 0))
                   (emacs-sqlite-ffi-test--write-u8 p off byte))))
        (let* ((s "é日本 ok")
               (p (emacs-sqlite-ffi--cstring s)))
          (should (= (length bulk-writes) 1))
          (should (equal (emacs-sqlite-ffi--read-cstring p) s))
          (should bulk-reads)
          (should (equal (car (car bulk-reads))
                         p)))))))

(ert-deftest emacs-sqlite-ffi-test/read-bytes-crosses-chunk-boundary ()
  "Text longer than the gather chunk comes back intact."
  (emacs-sqlite-ffi-test--with-fake-backend (lambda (_n _a) 0)
    (let* ((s (make-string (+ emacs-sqlite-ffi--read-chunk 37) ?x))
           (p (emacs-sqlite-ffi--cstring s)))
      (should (equal (emacs-sqlite-ffi--read-bytes p (length s)) s)))))

(ert-deftest emacs-sqlite-ffi-test/available-p-needs-the-rows ()
  "A backend whose nl-ffi-call answers nil for sqlite3_libversion is unavailable."
  (emacs-sqlite-ffi-test--with-fake-backend (lambda (_n _a) nil)
    (should (emacs-sqlite-ffi-backend-p))
    (should-not (emacs-sqlite-ffi-available-p)))
  (emacs-sqlite-ffi-test--with-fake-backend
      (lambda (n _a) (if (equal n "sqlite3_libversion") #x3000 nil))
    (should (emacs-sqlite-ffi-available-p))))

(ert-deftest emacs-sqlite-ffi-test/open-passes-flags-and-yields-handle ()
  "open_v2 gets the UTF-8 name, READWRITE|CREATE, and the handle is read back."
  (emacs-sqlite-ffi-test--with-fake-backend (emacs-sqlite-ffi-test--scripted nil)
    (let ((db (emacs-sqlite-ffi-open nil)))
      (should (emacs-sqlite-ffi-sqlitep db))
      (should (= (nth 1 db) #x1000))
      (should (equal (nth 2 db) ":memory:"))
      (let ((call (assoc "sqlite3_open_v2" emacs-sqlite-ffi-test--calls)))
        (should call)
        (should (equal (emacs-sqlite-ffi-test--fetch-cstring (nth 1 call)) ":memory:"))
        (should (= (nth 3 call) emacs-sqlite-ffi--open-flags))
        (should (= (nth 4 call) 0)))
      (emacs-sqlite-ffi-close db)
      (should (= (nth 1 db) 0))
      (should-error (emacs-sqlite-ffi-execute db "select 1") :type 'sqlite-error))))

(ert-deftest emacs-sqlite-ffi-test/open-failure-signals-sqlite-error ()
  (emacs-sqlite-ffi-test--with-fake-backend
      (lambda (n args)
        (pcase n
          ("sqlite3_open_v2"
           (emacs-sqlite-ffi-test--write-u64 (nth 1 args) 0 #x1000) 14)
          ("sqlite3_errmsg" (emacs-sqlite-ffi-test--store-cstring "unable to open"))
          ("sqlite3_close_v2" 0)
          (_ 0)))
    (let ((err (should-error (emacs-sqlite-ffi-open "/nope/x.db") :type 'sqlite-error)))
      (should (equal (cadr err) "unable to open")))))

(ert-deftest emacs-sqlite-ffi-test/select-decodes-every-column-type ()
  (emacs-sqlite-ffi-test--with-fake-backend
      (emacs-sqlite-ffi-test--scripted
       (list (list (cons 1 42) (cons 2 2.5) (cons 3 "é日本"))
             (list (cons 5 nil) (cons 1 -7) (cons 3 ""))))
    (let* ((db (emacs-sqlite-ffi-open nil))
           (rows (emacs-sqlite-ffi-select db "select a, b, c from t")))
      (should (equal rows '((42 2.5 "é日本") (nil -7 ""))))
      (should (equal (emacs-sqlite-ffi-select db "select a, b, c from t" nil 'full)
                     '(("a" "b" "c") (42 2.5 "é日本") (nil -7 ""))))
      (let ((prep (assoc "sqlite3_prepare_v2" emacs-sqlite-ffi-test--calls)))
        (should (equal (emacs-sqlite-ffi-test--fetch-cstring (nth 2 prep))
                       "select a, b, c from t"))
        (should (= (nth 3 prep) -1))))))

(ert-deftest emacs-sqlite-ffi-test/bind-dispatches-on-lisp-type ()
  (emacs-sqlite-ffi-test--with-fake-backend (emacs-sqlite-ffi-test--scripted nil)
    (let ((db (emacs-sqlite-ffi-open nil)))
      (should (= (emacs-sqlite-ffi-execute db "insert into t values (?,?,?,?)"
                                           (list 7 1.5 "sé" nil))
                 3))
      (let ((names (mapcar #'car (reverse emacs-sqlite-ffi-test--calls))))
        (should (equal (seq-filter (lambda (n) (string-prefix-p "sqlite3_bind_" n)) names)
                       '("sqlite3_bind_int64" "sqlite3_bind_double"
                         "sqlite3_bind_text" "sqlite3_bind_null"))))
      (let ((text (assoc "sqlite3_bind_text" emacs-sqlite-ffi-test--calls)))
        (should (= (nth 2 text) 3))
        (should (equal (emacs-sqlite-ffi-test--fetch-cstring (nth 3 text)) "sé"))
        (should (= (nth 4 text) 3))
        (should (= (nth 5 text) emacs-sqlite-ffi--transient)))
      (should-error (emacs-sqlite-ffi-execute db "insert into t values (?)" (list 'sym))
                    :type 'wrong-type-argument)
      (should (= (emacs-sqlite-ffi-execute db "insert into t values (?)" (vector 1)) 3)))))

(ert-deftest emacs-sqlite-ffi-test/set-cursor-walks-like-emacs ()
  "`set' returns a statement: more-p stays t until next steps past the end."
  (emacs-sqlite-ffi-test--with-fake-backend
      (emacs-sqlite-ffi-test--scripted
       (list (list (cons 1 1) (cons 1 2) (cons 1 3))
             (list (cons 1 4) (cons 1 5) (cons 1 6))))
    (let* ((db (emacs-sqlite-ffi-open nil))
           (set (emacs-sqlite-ffi-select db "select * from t" nil 'set)))
      (should (equal (emacs-sqlite-ffi-columns set) '("a" "b" "c")))
      (should (emacs-sqlite-ffi-more-p set))
      (should (equal (emacs-sqlite-ffi-next set) '(1 2 3)))
      (should (emacs-sqlite-ffi-more-p set))
      (should (equal (emacs-sqlite-ffi-next set) '(4 5 6)))
      (should (emacs-sqlite-ffi-more-p set))
      (should-not (emacs-sqlite-ffi-next set))
      (should-not (emacs-sqlite-ffi-more-p set))
      (should-not (emacs-sqlite-ffi-next set))
      (emacs-sqlite-ffi-finalize set)
      (emacs-sqlite-ffi-finalize set)
      (should (= (cl-count "sqlite3_finalize" emacs-sqlite-ffi-test--calls
                           :key #'car :test #'equal)
                 1)))))

(ert-deftest emacs-sqlite-ffi-test/step-error-signals-and-finalizes ()
  (emacs-sqlite-ffi-test--with-fake-backend
      (lambda (n args)
        (pcase n
          ("sqlite3_open_v2" (emacs-sqlite-ffi-test--write-u64 (nth 1 args) 0 #x1000) 0)
          ("sqlite3_prepare_v2" (emacs-sqlite-ffi-test--write-u64 (nth 3 args) 0 #x2000) 0)
          ("sqlite3_column_count" 1)
          ("sqlite3_column_name" (emacs-sqlite-ffi-test--store-cstring "x"))
          ("sqlite3_step" 5)
          ("sqlite3_errmsg" (emacs-sqlite-ffi-test--store-cstring "database is locked"))
          (_ 0)))
    (let* ((db (emacs-sqlite-ffi-open nil))
           (err (should-error (emacs-sqlite-ffi-select db "select x from t")
                              :type 'sqlite-error)))
      (should (equal (cadr err) "database is locked"))
      (should (assoc "sqlite3_finalize" emacs-sqlite-ffi-test--calls)))))

(ert-deftest emacs-sqlite-ffi-test/pragma-and-transactions-execute-sql ()
  (emacs-sqlite-ffi-test--with-fake-backend (emacs-sqlite-ffi-test--scripted nil)
    (let ((db (emacs-sqlite-ffi-open nil)))
      (should (eq (emacs-sqlite-ffi-pragma db "journal_mode = wal") t))
      (should (eq (emacs-sqlite-ffi-transaction db) t))
      (should (eq (emacs-sqlite-ffi-commit db) t))
      (should (eq (emacs-sqlite-ffi-rollback db) t))
      (let ((sqls (delq nil
                        (mapcar (lambda (c)
                                  (and (equal (car c) "sqlite3_prepare_v2")
                                       (emacs-sqlite-ffi-test--fetch-cstring (nth 2 c))))
                                (reverse emacs-sqlite-ffi-test--calls)))))
        (should (equal sqls '("PRAGMA journal_mode = wal" "BEGIN" "COMMIT" "ROLLBACK")))))))

(ert-deftest emacs-sqlite-ffi-test/install-with-fake-backend-aliases-api ()
  "With a backend and no C sqlite, `emacs-sqlite-ffi-install' aliases sqlite-*."
  (emacs-sqlite-ffi-test--with-fake-backend (emacs-sqlite-ffi-test--scripted nil)
    (cl-letf (((symbol-function 'emacs-sqlite-ffi--host-sqlite-p) (lambda () nil)))
      (let ((saved (mapcar (lambda (pair)
                             (cons (car pair)
                                   (and (fboundp (car pair))
                                        (symbol-function (car pair)))))
                           emacs-sqlite-ffi--aliases)))
        (unwind-protect
            (progn
              (should (emacs-sqlite-ffi-install))
              (should (eq (symbol-function 'sqlite-open) 'emacs-sqlite-ffi-open))
              (should (equal (sqlite-version) "3.51.1")))
          (dolist (pair saved)
            (if (cdr pair)
                (fset (car pair) (cdr pair))
              (fmakunbound (car pair)))))))))

(provide 'emacs-sqlite-ffi-test)
;;; emacs-sqlite-ffi-test.el ends here
