;;; nelisp-reader-cursor-hint-smoke.el --- buffer read cursor hint  -*- lexical-binding: t -*-
;; Run: BIN --load test/nelisp-reader-cursor-hint-smoke.el
;; Prints one PASS/FAIL line per case and ends with SMOKE-RESULT.  With
;; NL_HINT_NEGCTL=1 it also runs a negative control that corrupts the hint
;; the way a missing invalidation would; that case must report FAIL.
(defvar rh--fail 0)
(defun rh--check (name ok)
  (unless ok (setq rh--fail (1+ rh--fail)))
  (princ (format "%s %s\n" (if ok "PASS" "FAIL") name)))

(defun rh--text (n)
  (let ((parts nil))
    (dotimes (i n)
      (push (format "(f%d \"%s\" ?あ %d)\n" i (if (= 0 (% i 3)) "日本語ab" "xyz") i) parts))
    (mapconcat #'identity (nreverse parts) "")))

(defun rh--reference (text)
  "All forms of TEXT read with `read-from-string' (no hint)."
  (let ((pos 0) (forms nil) (r nil))
    (while (setq r (condition-case nil (read-from-string text pos) (end-of-file nil)))
      (push (car r) forms) (setq pos (cdr r)))
    (nreverse forms)))

(defun rh--read-all (buf &optional gc-every)
  (let ((forms nil) (f nil) (n 0))
    (while (not (eq (setq f (condition-case nil (read buf) (end-of-file 'rh-eof))) 'rh-eof))
      (setq n (1+ n))
      (when (and gc-every (= 0 (% n gc-every))) (garbage-collect))
      (push f forms))
    (nreverse forms)))

(let* ((text (rh--text 400)) (ref (rh--reference text)))
  (rh--check "text is large and multibyte" (and (> (length text) 2048) (> (string-bytes text) (length text))))
  ;; 1. sequential read equals read-from-string
  (let ((buf (generate-new-buffer " rh1")))
    (with-current-buffer buf (insert text) (goto-char (point-min)))
    (rh--check "sequential buffer read" (equal (rh--read-all buf) ref)))
  ;; 2. reads interleaved with collections
  (let ((buf (generate-new-buffer " rh2")))
    (with-current-buffer buf (insert text) (goto-char (point-min)))
    (rh--check "read with GC every 7 forms" (equal (rh--read-all buf 7) ref)))
  ;; 3. backward seek (goto-char to an earlier form) after reads
  (let ((buf (generate-new-buffer " rh3")) a b)
    (with-current-buffer buf (insert text) (goto-char (point-min)))
    (dotimes (_ 50) (read buf))
    (setq a (read buf))
    (with-current-buffer buf (goto-char (point-min)))
    (setq b (read buf))
    (rh--check "backward seek rereads first form" (and (equal b (nth 0 ref)) (equal a (nth 50 ref))))
    (with-current-buffer buf (goto-char (point-min)))
    (rh--check "after seek whole text again" (equal (rh--read-all buf) ref)))
  ;; 4. edit between reads changes byte lengths before and after point
  (let ((buf (generate-new-buffer " rh4")) f1 f2)
    (with-current-buffer buf (insert text) (goto-char (point-min)))
    (dotimes (_ 20) (read buf))
    (with-current-buffer buf
      (let ((p (point)))
        (goto-char (point-min)) (insert "(あいうえお 1)\n" "(ascii 2)\n")
        (goto-char (+ p (length "(あいうえお 1)\n(ascii 2)\n")))))
    (setq f1 (read buf))
    (rh--check "read after insertion before point" (equal f1 (nth 20 ref)))
    (with-current-buffer buf (goto-char (point-min)))
    (setq f2 (rh--read-all buf))
    (rh--check "whole text after edit"
               (equal f2 (rh--reference (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))))))
  ;; 5. narrowed read does not use the full-text hint
  (let ((buf (generate-new-buffer " rh5")))
    (with-current-buffer buf (insert text) (goto-char (point-min)) (read buf))
    (with-current-buffer buf
      (narrow-to-region (point-min) (+ (point-min) 40)) (goto-char (point-min)))
    (rh--check "narrowed read first form" (equal (read buf) (nth 0 ref))))
  ;; 6. read-from-string START/END on multibyte and unibyte, and aset between reads
  (let* ((s (copy-sequence text)) (r1 (read-from-string s 5)) (r2 (read-from-string s (cdr r1) (length s))))
    (aset s 1 ?Z)
    (rh--check "aset ASCII between reads"
               (equal (read-from-string s (cdr r1)) (read-from-string text (cdr r1))))
    (rh--check "START/END offsets" (and (consp r2) (> (cdr r2) (cdr r1)))))
  (let ((u (string-to-unibyte "(a 1) (b 2) (c 3)")))
    (rh--check "unibyte START" (equal (read-from-string u 6) '((b 2) . 11)))))

;; 7. negative control: a cursor whose byte no longer matches its char, as left
;; by a missing invalidation after the text changed.
(when (equal (getenv "NL_HINT_NEGCTL") "1")
  (let* ((t1 (rh--text 400)) (b1 (generate-new-buffer " rhn1")) ok)
    (with-current-buffer b1 (insert t1) (goto-char (point-min)))
    (read b1)
    (let ((cell (cdr (cdr (cdr nelisp--rd-hint)))))
      (setcdr cell (1+ (cdr cell))))
    (setq ok (condition-case nil
                 (equal (rh--read-all b1) (cdr (rh--reference t1)))
               (error nil)))
    (rh--check "NEGCTL stale cursor detected (must FAIL)" ok)))
(princ (format "SMOKE-RESULT fail=%d\n" rh--fail))
(kill-emacs (if (= rh--fail 0) 0 1))
