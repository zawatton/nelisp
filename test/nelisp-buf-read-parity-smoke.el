;;; nelisp-buf-read-parity-smoke.el --- buffer/marker/string `read' parity probe  -*- lexical-binding: t; -*-
;; Prints one line per case; nelisp-buf-read-parity-smoke.sh diffs the output of
;; host GNU Emacs against the standalone.  Cases beyond the original buffer
;; ones cover the reader-level fixes (pool sizing, iterative lists, native
;; decline reasons, `(a . )', `1.', `?\N{U+..}', `#@N', `#(..)').
(defun bp--show (label thunk)
  (princ (format "%s => %S\n" label
                 (condition-case e (funcall thunk)
                   (error (list 'ERR (car e) (if (bufferp (cadr e)) 'BUFFER (cdr e))))))))
(defun bp--reads (text &optional narrow)
  "Read every form from TEXT in a temp buffer; return (FORM . POINT) list + terminal error."
  (with-temp-buffer
    (insert text) (goto-char (point-min))
    (when narrow (narrow-to-region (car narrow) (cdr narrow)) (goto-char (point-min)))
    (let (out done)
      (while (not done)
        (condition-case e
            (let ((f (read (current-buffer)))) (push (cons f (point)) out))
          (error (push (list 'ERR (car e) (if (bufferp (cadr e)) 'BUFFER (cdr e)) (point)) out) (setq done t))))
      (nreverse out))))
(defmacro bp--case (label text &optional narrow)
  `(bp--show ,label (lambda () (bp--reads ,text ,narrow))))
(bp--case "simple" "(a b) c \"str\" 12 ?a")
(bp--case "multibyte" "(\"日本語\" ?あ) 'ñ \"é\" (ü . ö)")
(bp--case "comments" "; c1\n(a) ; c2\n\n  ;; c3\n b ; tail")
(bp--case "eof-in-list" "(a b")
(bp--case "eof-in-string" "\"abc")
(bp--case "empty" "")
(bp--case "ws-only" "  \n\t ")
(bp--case "only-comment" "; hi")
(bp--case "vector-record" "[1 2 (3)] #s(foo 1 2) #s(hash-table data (a 1))")
(bp--show "circle" (lambda () (with-temp-buffer (insert "(#1=(a) #1# #2=(x #2#))") (goto-char 1) (let ((f (read (current-buffer)))) (list (eq (car f) (cadr f)) (point) (eq (caddr f) (cadr (caddr f))))))))
(bp--case "charesc" "?\\C-x ?\\M-a ?\\^? ?\\x41 ?\\N{U+263A} ?\\s ?\\d")
(bp--case "strescapes" "\"a\\nb\\x41\\u00e9\\C-a\\\"q\"")
(bp--case "quotes" "'a `(b ,c ,@d) #'f")
(bp--case "invalid-close" "(a) ) b")
(bp--case "invalid-dot" "(a . )")
(bp--case "dot-list" "(a . b) (1 . (2))")
(bp--case "numbers" "1 -2 3.5 1e3 #x1f #b101 #o17 1. +5 .5 ?\\n")
(bp--case "symbols" "foo-bar a\\ b \\,x 1+ ## nil t")
(bp--case "sharp-at" "#@5 skip(a) b")
(bp--show "sharp-dollar" (lambda () (let ((r (bp--reads "#$ x"))) (list (stringp (caar r)) (cdar r) (cadr r) (caddr r)))))
(bp--show "raw-byte" (lambda () (let ((r (bp--reads (concat "(" (string 4194303) ") x")))) (list (length (symbol-name (car (caar r)))) (cdar r) (cadr r) (caddr r)))))
(bp--case "emoji" "(\"😀\" ?😀) end")
(bp--case "narrowed-basic" "aaa (b c) (d e) fff" '(5 . 13))
(bp--case "narrowed-cut-form" "(a b c) (d e f)" '(1 . 12))
(bp--case "narrowed-cut-eof" "xx (a b c" '(3 . 6))
;; point placement / repositioning
(bp--show "goto-then-read"
  (lambda ()
    (with-temp-buffer
      (insert "(one) (two) (three)")
      (goto-char 7) (let ((a (read (current-buffer)))) (list a (point)))
      (let ((r1 (progn (goto-char 7) (read (current-buffer)))) (p1 (point))
            (r2 (read (current-buffer))) (p2 (point)))
        (list r1 p1 r2 p2)))))
(bp--show "modify-between-reads"
  (lambda ()
    (with-temp-buffer
      (insert "(a) (b) (c)") (goto-char 1)
      (let* ((r1 (read (current-buffer))) (p1 (point)))
        (goto-char (point-max)) (insert " (d)") (goto-char p1)
        (let* ((r2 (read (current-buffer))) (r3 (read (current-buffer))) (r4 (read (current-buffer)))
               (p4 (point)))
          (delete-region 1 5) (goto-char 1)
          (list r1 p1 r2 r3 r4 p4 (read (current-buffer)) (point) (buffer-string)))))))
(bp--show "insert-at-point-after-read"
  (lambda ()
    (with-temp-buffer
      (insert "(a) (b)") (goto-char 1) (read (current-buffer)) (insert "X") (list (point) (buffer-string)))))
(bp--show "multibyte-repeated-modify"
  (lambda ()
    (with-temp-buffer
      (insert "(あ) (い) (う)") (goto-char 1)
      (let ((a (read (current-buffer))) (p (point)))
        (goto-char 1) (insert "えお ") (list a p (read (current-buffer)) (point) (read (current-buffer)) (point))))))
;; markers
(bp--show "marker-read"
  (lambda ()
    (with-temp-buffer
      (insert "(a) 日本 (c)") (let ((m (copy-marker 1)))
        (let* ((r1 (read m)) (p1 (marker-position m)) (r2 (read m)) (p2 (marker-position m))
               (r3 (read m)) (p3 (marker-position m)))
          (list r1 p1 r2 p2 r3 p3 (point)))))))
(bp--show "marker-eof"
  (lambda () (with-temp-buffer (insert "(a)") (let ((m (copy-marker 4))) (condition-case e (read m) (error e))))))
(bp--show "buffer-eof-data"
  (lambda () (with-temp-buffer (insert "(a") (goto-char 1)
     (condition-case e (read (current-buffer)) (error (list (car e) (bufferp (cadr e)) (cddr e)))))))
(bp--show "read-from-buffer-name"
  (lambda () (let ((b (generate-new-buffer "rdtest")))
     (unwind-protect (progn (with-current-buffer b (insert "(x y) z")) (list (read b) (with-current-buffer b (point))))
       (kill-buffer b)))))
(bp--show "string-stream" (lambda () (list (read "(a b) c") (read-from-string "  (a b) c") (read-from-string "x y" 1))))
(bp--case "stray-bracket-lines" "a\n bb\n  ) x")
(bp--case "stray-square" "  ] y")
(bp--case "stray-multibyte" "é ) 日")
(bp--case "prefix-eof" "'")
(bp--case "prefix-eof2" "a `,@")
(bp--case "atom-eof" "ab")
(bp--case "atom-then" "ab c")
(bp--case "qmark-paren" "(?\\( ?\\) ?a)")
(bp--case "string-parens" "(\"(\" \")\" \"a\\\"(\") z")
(bp--case "comment-paren" "(a ; )\n b) c")
(bp--case "sym-escaped-paren" "(a\\)b c)")
(bp--case "nested-eof" "(a (b [c \"d\" (e)]")
(bp--case "hash-forms" "#'car #s(a b) #:g #xff")
(bp--case "narrowed-close" "(a) ) (b)" '(4 . 10))
(bp--case "narrowed-eof-exact" "(a) (b c)" '(5 . 8))
(bp--show "marker-narrow-eof"
  (lambda () (with-temp-buffer (insert "(a) (b c) z") (narrow-to-region 1 8)
     (let ((m (copy-marker 5))) (list (condition-case e (read m) (error e)) (marker-position m))))))
(bp--show "marker-stray"
  (lambda () (with-temp-buffer (insert "x ) y") (let ((m (copy-marker 2))) (list (condition-case e (read m) (error e)) (marker-position m))))))
(bp--show "widen-after-eof"
  (lambda () (with-temp-buffer (insert "(a) (b c) z") (narrow-to-region 1 8) (goto-char 5)
     (list (condition-case e (read (current-buffer)) (error (car e))) (point) (progn (widen) (goto-char 5) (read (current-buffer))) (point)))))
(bp--show "point-outside-narrow-after-widen"
  (lambda () (with-temp-buffer (insert "(a) (b) (c)") (narrow-to-region 5 9) (goto-char 5) (list (read (current-buffer)) (point) (condition-case e (read (current-buffer)) (error (car e))) (point)))))

;; ---- string reader (`read-from-string') cases ----
(defun bp--rfs (label text &optional keep-data)
  (bp--show label
            (lambda ()
              (condition-case e (read-from-string text)
                (error (if keep-data (list 'ERR (car e) (cdr e)) (list 'ERR (car e))))))))
(bp--rfs "rfs-dotted" "(a . b)")
(bp--rfs "rfs-dotted2" "(a b . c) x")
(bp--rfs "rfs-dotted3" "(a . (b . (c)))")
(bp--rfs "rfs-dot-empty" "(a . )" t)
(bp--rfs "rfs-dot-first" "(. b)" t)
(bp--rfs "rfs-dot-extra" "(a . b c)" t)
(bp--rfs "rfs-dot-eof" "(a . b")
(bp--rfs "rfs-dot-paren" "(a .)")
(bp--rfs "rfs-stray" ")" t)
(bp--rfs "rfs-stray-bracket" "]" t)
(bp--rfs "rfs-quote-stray" "')" t)
(bp--rfs "rfs-eof-list" "(a b")
(bp--rfs "rfs-eof-string" "\"abc")
(bp--rfs "rfs-eof-vector" "[1 2")
(bp--rfs "rfs-eof-quote" "'")
(bp--rfs "rfs-empty" "")
(bp--rfs "rfs-numbers" "1.")
(bp--rfs "rfs-numbers2" "-1.")
(bp--rfs "rfs-numbers3" "+1. x")
(bp--rfs "rfs-numbers4" "(1. 2.5 3.e2 .5 -.5 1e2 12345678901234567890.)")
(bp--show "rfs-numbers5" (lambda () (let ((r (read-from-string "1.5."))) (list (symbolp (car r)) (symbol-name (car r)) (cdr r)))))
(bp--show "rfs-numbers6" (lambda () (let ((r (read-from-string "(1.. 1.+ 1.a)"))) (list (mapcar (function symbol-name) (car r)) (cdr r)))))
(bp--rfs "rfs-charN" "?\\N{U+263A}")
(bp--rfs "rfs-charN2" "(?\\N{U+41} ?\\N{U+1F600} ?a)")
(bp--rfs "rfs-charN-bad" "?\\N{U+110000}")
(bp--rfs "rfs-charN-bad2" "?\\N{U+}")
(bp--rfs "rfs-strN" "\"a\\N{U+263A}b\"")
(bp--rfs "rfs-sharp-at" "#@3 abc d")
(bp--rfs "rfs-sharp-at0" "#@0 q")
(bp--rfs "rfs-sharp-at00" "#@00 x")
(bp--rfs "rfs-sharp-at-list" "(a #@00 b)")
(bp--rfs "rfs-sharp-at-after" "a #@2 bc d")
(bp--rfs "rfs-sharp-at-bare" "#@")
(bp--rfs "rfs-meta-string" "\"\\M-n\"")
(bp--rfs "rfs-escapes" "(\"\\x41\\n\\t\\e\\C-a\\^b\\101\" ?\\C-a ?\\^? ?\\M-a)")
;; Native-decline reasons that used to be misclassified: comments and strings
;; that merely CONTAIN escape-looking text must not force the slow reader.
(bp--rfs "rfs-comment-escapes" "(a ; \\Sw \\N \\M- ## #@ #(\n b \"doc ; \\w\" ?\\;)")
(bp--rfs "rfs-string-hashes" "(\"## #@3 #( ?\\N \\w\" x)")
(bp--rfs "rfs-string-then-comment" "(\"a\" ; \\S\n \"b\" ; \\N\n c)")
(bp--rfs "rfs-charquote" "(?\" ?\\\" \"s\\\"t\" ?\\\\ y)")
(bp--rfs "rfs-empty-sym" "(## a ##)")
;; text properties
(bp--show "rfs-textprops"
  (lambda ()
    (let* ((r (read-from-string "#(\"abcd\" 0 2 (face bold) 2 4 (mouse-face highlight))"))
           (s (car r)))
      (list (substring-no-properties s) (cdr r)
            (get-text-property 0 'face s) (get-text-property 1 'face s)
            (get-text-property 2 'face s) (get-text-property 2 'mouse-face s)
            (get-text-property 3 'mouse-face s)))))
(bp--show "rfs-textprops-nested"
  (lambda ()
    (let* ((r (read-from-string "(x #(\"ab\" 1 2 (k v)) y)")) (s (cadr (car r))))
      (list (car (car r)) (substring-no-properties s) (car (cddr (car r))) (cdr r)
            (get-text-property 0 'k s) (get-text-property 1 'k s)))))
(bp--rfs "rfs-textprops-empty" "#(\"ab\")")
(bp--rfs "rfs-textprops-range" "#(\"ab\" 0 5 (k v))")
;; ---- list/vector scaling and the pool ----
(defun bp--flat (n) (concat "(" (mapconcat (lambda (i) (format "a%d" i)) (number-sequence 1 n) " ") ")"))
(bp--show "big-flat-list"
  (lambda () (let* ((r (read-from-string (bp--flat 6000))) (l (car r)))
               (list (length l) (car l) (car (last l)) (cdr r)))))
(bp--show "big-flat-buffer"
  (lambda () (with-temp-buffer (insert (bp--flat 6000) " tail") (goto-char 1)
               (let* ((l (read (current-buffer))) (p (point)))
                 (list (length l) (nth 2999 l) p (read (current-buffer)))))))
(bp--show "big-dotted"
  (lambda () (let* ((s (concat "(" (mapconcat #'number-to-string (number-sequence 1 3000) " ") " . end)"))
                    (l (car (read-from-string s))))
               (let ((c l) (n 0)) (while (consp c) (setq n (1+ n) c (cdr c))) (list n c)))))
(bp--show "big-vector"
  (lambda () (let* ((s (concat "[" (mapconcat #'number-to-string (number-sequence 1 4000) " ") "]"))
                    (v (car (read-from-string s))))
               (list (length v) (aref v 0) (aref v 3999)))))
(bp--show "deep-nesting"
  (lambda () (let* ((n 400) (s (concat (make-string n ?\() "x" (make-string n ?\))))
                    (l (car (read-from-string s))) (d 0))
               (while (consp l) (setq d (1+ d) l (car l)))
               (list d l))))
(bp--show "deep-nesting-mixed"
  (lambda () (let* ((n 200) (s (concat (apply #'concat (make-list n "(a [")) "z" (apply #'concat (make-list n "] )"))))
                    (l (car (read-from-string s))) (d 0))
               (while (consp l) (setq d (1+ d) l (aref (cadr l) 0)) (when (symbolp l) (setq l nil)))
               d)))
(bp--show "many-forms-sequential"
  (lambda () (let* ((s (mapconcat (lambda (i) (format "(defun f%d (x) \"doc %d\" (+ x %d))" i i i)) (number-sequence 1 500) "\n"))
                    (pos 0) (n 0) (last nil))
               (condition-case nil
                   (while t (let ((r (read-from-string s pos))) (setq last (car r) pos (cdr r) n (1+ n))))
                 (end-of-file nil))
               (list n last pos (length s)))))
(bp--show "labels-in-long-list"
  (lambda () (let* ((s (concat "(#1=(a b) " (mapconcat #'number-to-string (number-sequence 1 500) " ") " #1# #2=[x #2#])"))
                    (l (car (read-from-string s))))
               (list (length l) (eq (car l) (nth 501 l)) (eq (nth 502 l) (aref (nth 502 l) 1))))))
(bp--rfs "rfs-nil-elements" "(nil a nil (nil) [nil nil] . nil)")
(bp--rfs "rfs-quote-family" "('a `(b ,c ,@d) #'e . f)")
nil
