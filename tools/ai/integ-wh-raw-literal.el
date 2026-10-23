;;; integ-wh-raw-literal.el --- string literals with raw-byte escapes read as GNU reads them  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same bytes.  A literal whose non-ASCII
;; content is raw-byte escapes (\300, \xc0) is a unibyte string of that many bytes; the byte-code strings of an
;; .elc are such literals.  The reader copied the lexer's unibyte token with the multibyte-by-count allocator and
;; then wrote 14 into the view's tag byte, so "\300\207" read as one multibyte character and `aref' of the last
;; byte of a byte-code string signalled args-out-of-range (the census's GUI image: printing a byte-code function).

(defun iwh-rl (tag s)
  (princ (format "RL %-16s mb=%S len=%S bytes=%S last=%S eq=%S\n" tag (multibyte-string-p s) (length s)
                 (string-bytes s) (aref s (1- (length s))) (equal s (copy-sequence s)))))

(iwh-rl "oct2" "\300\207")
(iwh-rl "oct1" "\207")
(iwh-rl "hex2" "\xc0\x87")
(iwh-rl "ascii+hi" "ab\300")
(iwh-rl "mixed" "a\300\207b")
(iwh-rl "vector" (aref (vector "\300\207") 0))
(iwh-rl "bytecode-short" (aref #[0 "\300\301\207" [] 1] 1))
(iwh-rl "bytecode-long" (aref #[257 "\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\207" [a] 3] 1))
(iwh-rl "read-string" (car (read-from-string "\"\\300\\207\\201\"")))
(iwh-rl "unibyte-string" (unibyte-string 192 135))
(iwh-rl "ascii" "abc")
(iwh-rl "utf8" "é日")
(iwh-rl "u-escape" "é")
(princ (format "RL printed %S\n" (prin1-to-string "\300\207")))
(princ "INTEG-WH-RAW-LITERAL DONE\n")
