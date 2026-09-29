;;; nelisp-pos-cache-standalone-test.el --- char<->byte position cache  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standalone keeps a per-string char count (SCHARS) and a last
;; (charpos . bytepos) cursor for tag-5 strings in one global table, and the
;; buffer motion primitives scan in bounded chunks.  These tests run the
;; standalone binary and compare against host Emacs for mutation-then-access
;; sequences on multibyte, unibyte and raw-byte strings and on buffers, and
;; assert that sequential access is amortized O(1) with generous thresholds.

;;; Code:

(require 'ert)

(defconst nelisp-pos-cache-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-pos-cache-test--binary ()
  (let ((binary (or (getenv "NELISP_BIN")
                    (expand-file-name "target/nelisp"
                                      nelisp-pos-cache-test--root))))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built"))
    binary))

(defun nelisp-pos-cache-test--run (binary form)
  "Return (STATUS STDOUT STDERR) of BINARY --eval FORM."
  (let ((stderr-file (make-temp-file "pos-cache-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (call-process binary nil (list t stderr-file) nil
                                       "--eval" form))
                 (stderr (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (buffer-string))))
            (list status (buffer-string) stderr)))
      (delete-file stderr-file))))

(defun nelisp-pos-cache-test--standalone (expressions)
  "Evaluate EXPRESSIONS (source strings) on the standalone; return the printed list."
  (let* ((binary (nelisp-pos-cache-test--binary))
         (form (format
                "(progn (princ \"<<\") (prin1 (mapcar (lambda (source) (condition-case err (eval (car (read-from-string source)) t) (error (list 'uncaught-error err)))) '%S)) (princ \">>\") nil)"
                expressions))
         (result (nelisp-pos-cache-test--run binary form)))
    (unless (string-empty-p (nth 2 result))
      (ert-fail (format "unexpected standalone stderr: %s" (nth 2 result))))
    (unless (eql (nth 0 result) 0)
      (ert-fail (format "standalone failed: %S" result)))
    (unless (string-match "<<\\(\\(?:.\\|\n\\)*\\)>>" (nth 1 result))
      (ert-fail (format "no batch value: %s" (nth 1 result))))
    (match-string 1 (nth 1 result))))

(defun nelisp-pos-cache-test--host (expressions)
  (prin1-to-string
   (mapcar (lambda (source)
             (condition-case err
                 (eval (car (read-from-string source)) t)
               (error (list 'uncaught-error err))))
           expressions)))

(defun nelisp-pos-cache-test--parity (expressions)
  (should (equal (nelisp-pos-cache-test--standalone expressions)
                 (nelisp-pos-cache-test--host expressions))))

;; A multibyte text of >= 64 bytes with 1-, 2-, 3- and 4-byte characters, so
;; that its char count and cursor live in the cache table.
(defconst nelisp-pos-cache-test--mb
  "(defun f-\u3042 (x) \"\u65e5\u672c\u8a9e \u00e9\u00e8 \U0001F600 doc\" (+ x 1)) ; \u00fc tail\n(defvar v-\u3044 '(a b \u3046) \"second \u65e5\")\n(quote (\u00e9 . \u00e8))\n")

(defun nelisp-pos-cache-test--src (form)
  "Return FORM (which mentions the free variable S) as a source string
prefixed with the shared multibyte text binding."
  (format "(let ((s (copy-sequence %S))) %S)" nelisp-pos-cache-test--mb form))

(ert-deftest nelisp-pos-cache-string-length-and-aref-parity ()
  (nelisp-pos-cache-test--parity
   (mapcar
    #'nelisp-pos-cache-test--src
    '((length s)
      ;; length then walk in both directions, length again
      (list (length s) (aref s 30) (aref s 5) (aref s 60) (aref s 0)
            (aref s (1- (length s))) (length s))
      (let ((acc nil) (i 0)) (while (< i (length s)) (push (aref s i) acc) (setq i (1+ i))) acc)
      (let ((acc nil) (i (1- (length s)))) (while (>= i 0) (push (aref s i) acc) (setq i (1- i))) acc)
      (list (length s) (progn (aset s 3 ?Z) (length s)) (aref s 3) (aref s 4) (length s))
      (list (substring s 10 40) (substring s 0 3) (substring s 5) (length (substring s 2 50)))
      (condition-case e (aref s (length s)) (args-out-of-range (car e)))
      (condition-case e (substring s 3 (1+ (length s))) (args-out-of-range (car e)))))))

(ert-deftest nelisp-pos-cache-string-raw-byte-and-unibyte-parity ()
  (nelisp-pos-cache-test--parity
   '("(let ((u (concat (make-string 40 ?a) \"\\300\\301\\377\" (make-string 40 ?b)))) (list (length u) (multibyte-string-p u) (aref u 41) (aref u 42) (length u)))"
     "(let ((u (apply #'unibyte-string (append (make-list 70 97) '(200 201 202))))) (list (length u) (multibyte-string-p u) (aref u 70) (progn (aset u 70 ?x) (list (aref u 70) (length u)))))"
     "(let* ((m (concat (make-string 70 ?a) \"\\u3042\\u3044\")) (n (length m))) (list n (progn (aset m 0 ?Q) (length m)) (aref m 70) (aref m 71) (length m) (substring m 69)))")))

(ert-deftest nelisp-pos-cache-read-from-string-offsets-parity ()
  (nelisp-pos-cache-test--parity
   (mapcar
    #'nelisp-pos-cache-test--src
    '(;; sequential reads through the whole text, each from the previous cdr
      (let ((pos 0) (acc nil))
        (condition-case nil
            (while t (let ((r (read-from-string s pos))) (push r acc) (setq pos (cdr r))))
          (end-of-file nil))
        (list (length acc) pos (nreverse acc)))
      ;; explicit START/END, out of order, then a sequential read
      (list (read-from-string s 1 20) (read-from-string s 0) (read-from-string s 10)
            (read-from-string s 3 (length s)))
      ;; mutate (ASCII for ASCII), then read again
      (let ((a (read-from-string s 0)))
        (aset s 1 ?D)
        (list a (read-from-string s 0) (read-from-string s (cdr a)) (length s)))
      (condition-case e (read-from-string s (1+ (length s))) (error (car e)))))))

(ert-deftest nelisp-pos-cache-buffer-mutation-then-access-parity ()
  (nelisp-pos-cache-test--parity
   (mapcar
    (lambda (body)
      (format "(let ((txt (concat %S %S))) (with-temp-buffer %S))"
              nelisp-pos-cache-test--mb nelisp-pos-cache-test--mb body))
    '((progn (insert txt) (goto-char (point-min))
             (list (buffer-size) (point-max) (char-after 1) (char-after 30) (char-before 31)
                   (buffer-substring 5 25) (progn (forward-line 1) (point))
                   (progn (forward-line 1) (point)) (line-beginning-position) (line-end-position)
                   (forward-line 100) (point)))
      (progn (insert txt) (goto-char 20) (insert "\u3042\u3044xyz")
             (list (point) (buffer-size) (char-after 20) (char-after 22) (buffer-substring 18 30)
                   (progn (goto-char (point-min)) (forward-line 2) (point)) (char-after)))
      (progn (insert txt) (delete-region 10 40)
             (list (buffer-size) (char-after 10) (char-after 9) (buffer-substring 1 20)
                   (progn (goto-char 1) (forward-line 1) (point)) (line-end-position)))
      (progn (insert txt) (goto-char 50)
             (list (progn (beginning-of-line) (point)) (progn (end-of-line) (point))
                   (progn (forward-line -1) (point)) (progn (forward-line -5) (point))
                   (forward-line -1) (point)))
      (progn (insert txt) (erase-buffer) (insert "\u3042bc\ndef\n\u3044")
             (list (buffer-size) (char-after 1) (char-after 4) (progn (goto-char 1) (forward-line 1) (point))
                   (forward-line 1) (point) (forward-line 1) (point) (char-after (point-max))))
      (progn (insert txt) (goto-char 30) (insert "a") (goto-char 5)
             (list (char-after) (progn (forward-char 30) (char-after)) (point)
                   (buffer-substring (- (point) 3) (+ (point) 3))))))))

(ert-deftest nelisp-pos-cache-long-line-motion-parity ()
  ;; Lines longer than the first scan chunk exercise the chunk growth path.
  (nelisp-pos-cache-test--parity
   '("(with-temp-buffer (insert (make-string 700 ?a) \"\\u3042\" (make-string 900 ?b) \"\\n\" (make-string 300 ?c) \"\\n\\n\" (make-string 5 ?d)) (goto-char 1) (list (progn (forward-line 1) (point)) (progn (forward-line 1) (point)) (progn (forward-line 1) (point)) (forward-line 1) (point) (progn (goto-char 1200) (line-beginning-position)) (line-end-position) (progn (goto-char (point-max)) (line-beginning-position)) (progn (forward-line -1) (point)) (progn (forward-line -3) (point))))")))

(ert-deftest nelisp-pos-cache-sequential-access-is-amortized-constant ()
  "Sequential access over a large multibyte text must not cost O(text) per op."
  (let* ((form
          "(progn
             (defun pb--time (name n thunk)
               (let ((t0 (float-time)) (i 0))
                 (while (< i n) (funcall thunk i) (setq i (1+ i)))
                 (cons name (/ (* 1e6 (- (float-time) t0)) n))))
             (let* ((parts nil) (j 0))
               (while (< j 2400) (push (format \"(defun foo-%d (x) \\\"\\u65e5\\u672c doc\\\" (+ x %d))\\n\" j j) parts) (setq j (1+ j)))
               (let* ((s (decode-coding-string (encode-coding-string (apply #'concat (nreverse parts)) 'utf-8-unix) 'utf-8-unix)) (pos 0) (r nil))
                 (push (pb--time 'length 100 (lambda (_) (length s))) r)
                 (push (pb--time 'aref 300 (lambda (i) (aref s (* i 300)))) r)
                 (push (pb--time 'read 300 (lambda (_) (setq pos (cdr (read-from-string s pos))))) r)
                 (with-temp-buffer (insert s) (goto-char 1)
                   (push (pb--time 'forward-line 100 (lambda (_) (forward-line 1))) r))
                 (prin1 r))))")
         (file (make-temp-file "pos-cache-timing-" nil ".el"))
         (result (unwind-protect
                     (progn
                       (with-temp-file file (insert form))
                       ;; `--load' (not `--eval'): the loaded path is the one
                       ;; whose per-op costs were measured before the caches.
                       (let ((stderr-file (make-temp-file "pos-cache-stderr-")))
                         (unwind-protect
                             (with-temp-buffer
                               (let ((status (call-process
                                              (nelisp-pos-cache-test--binary)
                                              nil (list t stderr-file) nil
                                              "--load" file "--")))
                                 (list status (buffer-string)
                                       (with-temp-buffer
                                         (insert-file-contents stderr-file)
                                         (buffer-string)))))
                           (delete-file stderr-file))))
                   (delete-file file))))
    (should (string-empty-p (nth 2 result)))
    (should (eql (nth 0 result) 0))
    (let* ((alist (car (read-from-string (nth 1 result))))
           (get (lambda (k) (cdr (assq k alist)))))
            ;; (The calls sit inside thunks run through `funcall': written inline in
      ;; a loop, a pure `length'/`aref' call is optimized differently and
      ;; measures nothing meaningful.)
      ;; Before the caches: length ~800us, read ~2900us, forward-line ~22000us
      ;; on this text.  Thresholds leave ample room for a loaded machine.
      (should (< (funcall get 'length) 300))
      (should (< (funcall get 'aref) 300))
      (should (< (funcall get 'read) 1500))
      (should (< (funcall get 'forward-line) 14000)))))

(provide 'nelisp-pos-cache-standalone-test)
;;; nelisp-pos-cache-standalone-test.el ends here
