;;; elprop-forms-auto.el --- property-comparison corpus (Doc 03 §6.3) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Each entry is evaluated on host Emacs (the oracle) and on the vendored
;; NeLisp binary by `bin/elprop-run'; their printed results must match.
;; Seed entries below are hand-curated pure forms.  `scripts/extract-elprop-
;; forms.el' can generate an alternative corpus from host ERT `*-tests.el'
;; files (loaded by the runner via the ELPROP_FORMS env var).

;;; Code:

(defvar elprop-forms
  '(;; arithmetic
    (:id "arith-add"      :type value :form (+ 1 2 3))
    (:id "arith-sub"      :type value :form (- 10 3 2))
    (:id "arith-mul"      :type value :form (* 2 3 4))
    (:id "arith-div"      :type value :form (/ 7 2))
    (:id "arith-mod"      :type value :form (mod -3 5))
    (:id "arith-max"      :type value :form (max 3 7 2))
    (:id "arith-min"      :type value :form (min 3 7 2))
    (:id "arith-abs"      :type value :form (abs -5))
    (:id "arith-expt"     :type value :form (expt 2 10))
    (:id "arith-1+"       :type value :form (1+ 41))
    ;; comparison / logic
    (:id "cmp-eq-num"     :type value :form (= 2 2))
    (:id "cmp-lt-chain"   :type value :form (< 1 2 3))
    (:id "cmp-gt-chain"   :type value :form (> 3 2 1))
    (:id "logic-and"      :type value :form (and t 1 2))
    (:id "logic-or"       :type value :form (or nil nil 3))
    (:id "logic-not"      :type value :form (not nil))
    ;; lists
    (:id "list-car"       :type value :form (car '(1 2 3)))
    (:id "list-cdr"       :type value :form (cdr '(1 2 3)))
    (:id "list-cons"      :type value :form (cons 1 '(2 3)))
    (:id "list-list"      :type value :form (list 1 2 3))
    (:id "list-append"    :type value :form (append '(1 2) '(3 4)))
    (:id "list-length"    :type value :form (length '(a b c)))
    (:id "list-nth"       :type value :form (nth 2 '(a b c d)))
    (:id "list-reverse"   :type value :form (reverse '(1 2 3)))
    (:id "list-member"    :type value :form (member 2 '(1 2 3)))
    (:id "list-assoc"     :type value :form (assoc 'b '((a . 1) (b . 2))))
    (:id "list-mapcar"    :type value :form (mapcar #'1+ '(1 2 3)))
    ;; strings
    (:id "str-concat"     :type value :form (concat "a" "b" "c"))
    (:id "str-substring"  :type value :form (substring "hello" 1 3))
    (:id "str-length"     :type value :form (length "hello"))
    (:id "str-upcase"     :type value :form (upcase "abc"))
    (:id "str-format"     :type value :form (format "%d-%s" 3 "x"))
    (:id "str-split"      :type value :form (split-string "a,b,c" ","))
    (:id "str-to-number"  :type value :form (string-to-number "42"))
    (:id "str-number-to"  :type value :form (number-to-string 42))
    ;; control flow
    (:id "ctl-if"         :type value :form (if t 1 2))
    (:id "ctl-cond"       :type value :form (cond ((= 1 2) 'a) (t 'b)))
    (:id "ctl-let"        :type value :form (let ((x 10)) (* x x)))
    (:id "ctl-let*"       :type value :form (let* ((x 2) (y (* x x))) (+ x y)))
    (:id "ctl-progn"      :type value :form (progn 1 2 3))
    (:id "ctl-funcall"    :type value :form (funcall #'+ 1 2))
    (:id "ctl-apply"      :type value :form (apply #'+ '(1 2 3)))
    ;; predicates
    (:id "pred-consp"     :type value :form (consp '(1)))
    (:id "pred-numberp"   :type value :form (numberp 5))
    (:id "pred-stringp"   :type value :form (stringp "x"))
    (:id "pred-null"      :type value :form (null nil))
    ;; --- probing batch: areas likely to reveal NeLisp compat gaps ---
    ;; float arithmetic (the +/-/* arms are i64-only)
    (:id "float-div"       :type value :form (/ 7.0 2))
    (:id "float-add"       :type value :form (+ 1.5 2.5))
    (:id "float-floor"     :type value :form (floor 3.7))
    (:id "float-ceiling"   :type value :form (ceiling 3.2))
    (:id "float-truncate"  :type value :form (truncate 3.9))
    (:id "float-round"     :type value :form (round 2.4))
    (:id "num-float"       :type value :form (float 3))
    ;; bitwise
    (:id "bit-logand"      :type value :form (logand 12 10))
    (:id "bit-logior"      :type value :form (logior 12 1))
    (:id "bit-logxor"      :type value :form (logxor 5 3))
    (:id "bit-ash-l"       :type value :form (ash 1 4))
    (:id "bit-ash-r"       :type value :form (ash 16 -2))
    ;; vectors / sequences
    (:id "vec-aref"        :type value :form (aref [10 20 30] 1))
    (:id "seq-elt-list"    :type value :form (elt '(a b c) 2))
    (:id "seq-elt-str"     :type value :form (elt "abc" 1))
    (:id "vec-length"      :type value :form (length [1 2 3]))
    (:id "seq-nreverse"    :type value :form (nreverse (list 1 2 3)))
    (:id "seq-sort"        :type value :form (sort (list 3 1 2) #'<))
    (:id "seq-numseq"      :type value :form (number-sequence 1 5))
    ;; strings
    (:id "str-char-to"     :type value :form (char-to-string 65))
    (:id "str-to-char"     :type value :form (string-to-char "A"))
    (:id "str-make"        :type value :form (make-string 3 ?x))
    (:id "str-format-hex"  :type value :form (format "%x" 255))
    (:id "str-prefix-p"    :type value :form (string-prefix-p "ab" "abc"))
    (:id "str-suffix-p"    :type value :form (string-suffix-p "bc" "abc"))
    (:id "str-trim"        :type value :form (string-trim "  hi  "))
    (:id "str-join"        :type value :form (string-join '("a" "b" "c") "-"))
    ;; alist / plist
    (:id "plist-get"       :type value :form (plist-get '(:a 1 :b 2) :b))
    (:id "alist-get"       :type value :form (alist-get 'b '((a . 1) (b . 2))))
    (:id "list-assq"       :type value :form (assq 'b '((a . 1) (b . 2))))
    (:id "list-rassq"      :type value :form (rassq 2 '((a . 1) (b . 2))))
    (:id "list-delete"     :type value :form (delete 2 (list 1 2 3 2)))
    (:id "list-butlast"    :type value :form (butlast '(1 2 3)))
    ;; predicates / misc
    (:id "pred-zerop"      :type value :form (zerop 0))
    (:id "pred-characterp" :type value :form (characterp ?A))
    (:id "pred-vectorp"    :type value :form (vectorp [1]))
    (:id "pred-keywordp"   :type value :form (keywordp :x))
    ;; error-class (both engines must signal)
    (:id "err-car-int"    :type error :form (car 5))
    (:id "err-add-str"    :type error :form (+ 1 "x")))
  "List of (:id :type :form) property-comparison entries.
TYPE is `value' (printed results must match) or `error' (both must signal).")

(provide 'elprop-forms-auto)
;;; elprop-forms-auto.el ends here
