;;; strings-image-b.el --- check the strings of a cold image after it is loaded  -*- lexical-binding:t -*-
;; Run as: BIN --cold-load-from OUT.img --load strings-image-b.el
;; Prints one line per case, "STRINGS-IMAGE name ok|FAIL", and a final tally.

(defvar strings-image--fails 0)
(defun strings-image--check (name ok)
  (unless ok (setq strings-image--fails (1+ strings-image--fails)))
  (princ (format "STRINGS-IMAGE %s %s\n" name (if ok "ok" "FAIL"))))

(strings-image--check "ascii-content" (equal strings-image--ascii "alpha"))
(strings-image--check "ascii-length" (= (length strings-image--ascii) 5))
(strings-image--check "ascii-unibyte" (not (multibyte-string-p strings-image--ascii)))
(strings-image--check "multibyte-flag" (multibyte-string-p strings-image--multibyte))
(strings-image--check "multibyte-content" (equal strings-image--multibyte "beta"))
(strings-image--check "wide-length" (= (length strings-image--wide) 3))
(strings-image--check "wide-chars" (equal (append strings-image--wide nil) '(26085 26412 35486)))
(strings-image--check "wide-bytes" (= (string-bytes strings-image--wide) 9))
(strings-image--check "props-content" (equal strings-image--props "gamma"))
(strings-image--check "props-face" (eq (get-text-property 0 'face strings-image--props) 'bold))
(strings-image--check "props-copy"
                      (eq (get-text-property 2 'face (substring strings-image--props 1 4)) 'bold))
(strings-image--check "props-concat"
                      (eq (get-text-property 6 'face (concat "plain-" strings-image--props)) 'bold))
(strings-image--check "empty" (and (equal strings-image--empty "") (= (length strings-image--empty) 0)))
(strings-image--check "made" (equal strings-image--made "xxxxx"))
(strings-image--check "long-length" (= (length strings-image--long) 200000))
(strings-image--check "long-ends" (and (= (aref strings-image--long 0) ?q)
                                       (= (aref strings-image--long 199999) ?q)))
(strings-image--check "list-identity"
                      (and (eq (nth 0 strings-image--list) strings-image--ascii)
                           (eq (nth 6 strings-image--list) strings-image--long)))
(strings-image--check "shared-identity"
                      (eq (car strings-image--shared) (cadr strings-image--shared)))
(strings-image--check "eq-table-ascii" (eq (gethash strings-image--ascii strings-image--eq-table) 'ascii))
(strings-image--check "eq-table-wide" (eq (gethash strings-image--wide strings-image--eq-table) 'wide))
(strings-image--check "eq-table-miss" (null (gethash (copy-sequence strings-image--ascii)
                                                     strings-image--eq-table)))
(strings-image--check "equal-table" (and (eq (gethash "alpha" strings-image--equal-table) 1)
                                         (eq (gethash "日本語" strings-image--equal-table) 2)))
(strings-image--check "symbol" (equal (symbol-name strings-image--symbol) "strings-image--sym-κ"))
(strings-image--check "string=" (and (string= strings-image--ascii "alpha")
                                     (not (string= strings-image--ascii "alphb"))))
(strings-image--check "string<" (string< "alpha" strings-image--multibyte))
(strings-image--check "concat" (equal (concat strings-image--ascii "-" strings-image--multibyte)
                                      "alpha-beta"))
(strings-image--check "substring" (equal (substring strings-image--ascii 1 3) "lp"))
(strings-image--check "format" (equal (format "%s/%s" strings-image--ascii strings-image--wide)
                                      "alpha/日本語"))
(strings-image--check "sxhash" (= (sxhash-equal strings-image--ascii) (sxhash-equal (copy-sequence "alpha"))))

;; Writes into image strings (copy on write is not needed: the image is private).
(let ((s strings-image--ascii))
  (aset s 0 ?A)
  (strings-image--check "aset-image" (and (equal s "Alpha") (equal strings-image--ascii "Alpha"))))
(let ((s strings-image--made))
  (clear-string s)
  (strings-image--check "clear-string-image" (equal s (make-string 5 0))))
(let ((s (copy-sequence strings-image--multibyte)))
  (put-text-property 1 3 'k 'v s)
  (strings-image--check "props-on-new" (eq (get-text-property 0 'k (substring s 1 3)) 'v)))
(strings-image--check "props-image-put"
                      (progn (put-text-property 0 3 'z 1 strings-image--ascii)
                             (eq (get-text-property 1 'z strings-image--ascii) 1)))

;; The collector must leave image strings, and the pool strings that point at
;; them, alone: churn the pool, collect, and look again.
(let ((junk nil) (i 0))
  (while (< i 20000)
    (setq junk (cons (concat "junk" (number-to-string i)) (and (< (% i 50) 3) junk)))
    (setq i (1+ i)))
  (garbage-collect)
  (garbage-collect))
(strings-image--check "after-gc-ascii" (equal (substring strings-image--ascii 1) "lpha"))
(strings-image--check "after-gc-wide" (equal strings-image--wide "日本語"))
(strings-image--check "after-gc-long" (= (length strings-image--long) 200000))
(strings-image--check "after-gc-eq-table" (eq (gethash strings-image--wide strings-image--eq-table) 'wide))
(setq strings-image--made (concat strings-image--made "pool"))
(garbage-collect)
(strings-image--check "pool-from-image" (equal strings-image--made (concat (make-string 5 0) "pool")))

(princ (format "STRINGS-IMAGE-TOTAL fails=%d\n" strings-image--fails))
