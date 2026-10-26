;;; nelisp-timefns.el --- GNU timefns.c time functions with glibc zone rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reference implementation (Lisp source) of: format-time-string, decode-time,
;; encode-time, time-add, time-subtract, time-less-p, time-equal-p,
;; time-convert, current-time-string, current-time-zone, set-time-zone-rule and
;; an exact float-time front end.  The algorithms follow GNU Emacs 31.1
;; src/timefns.c (decode_lisp_time, time_arith, time_cmp, tzlookup),
;; lib/strftime.c (the nstrftime directive machine, C locale) and glibc's
;; tzset/tzfile/mktime (TZif v2+ files, POSIX TZ strings, __tz_compute).
;;
;; The prelude embeds the GNU-byte-compiled form of this file between the
;; "BEGIN GENERATED nelisp-timefns" and "END GENERATED nelisp-timefns" markers
;; of scripts/nelisp-stdlib-prelude.el; regenerate with
;;   tools/ai/timefns-genblock.sh
;; Demoting back to interpreted Lisp is: replace that block by this file.
;; Speed notes (measured on the standalone VM): `floor', `mod', `ash', `abs',
;; `*' and let* frames of more than ~6 variables are slow, so the hot paths
;; use `/' `%' `+' plus small tables instead.

;;; Code:

;;; tf-core.el --- GNU timefns.c time values, zones and arithmetic  -*- lexical-binding: t; -*-
;; Part 1 of 2 of the time-function block spliced into scripts/nelisp-stdlib-prelude.el.
;; Mirrors GNU Emacs 31.1 src/timefns.c (decode_lisp_time, time_arith, time_cmp,
;; time-convert, tzlookup) and glibc TZ semantics (TZif v2+, POSIX TZ strings,
;; __tz_compute rules, gnulib mktime probing), locale = C.

;; `fixnump' is a slow call on this VM for values above 2^30 (~1.5 us); `integerp'
;; plus two compares against FIXNUM-range literals is not (a bignum literal is).
(defmacro nelisp--tf-fix (x)
  "Non-nil when X is a fixnum."
  `(let ((tf-v ,x)) (and (integerp tf-v) (<= tf-v 2305843009213693951) (>= tf-v -2305843009213693952))))

;;;; ---- fixnum helpers (the interpreter's floor/mod/ash/abs are slow) -----
(defmacro nelisp--tf-fl (a b)
  "Floor of fixnum A divided by positive fixnum B."
  `(let* ((tf-a ,a) (tf-b ,b) (tf-q (/ tf-a tf-b)))
     (if (< (% tf-a tf-b) 0) (1- tf-q) tf-q)))
(defmacro nelisp--tf-md (a b)
  "Fixnum A modulo positive fixnum B."
  `(let* ((tf-r (% ,a ,b))) (if (< tf-r 0) (+ tf-r ,b) tf-r)))
(defmacro nelisp--tf-abs (x)
  `(let ((tf-x ,x)) (if (< tf-x 0) (- tf-x) tf-x)))

;;;; ---- big-integer helpers (native bignum division stops at 2^31) -------
(defun nelisp--tf-bitlen (a)
  (let ((k 0))
    (while (> a 0) (setq a (ash a -30) k (+ k 30)))
    k))

(defun nelisp--tf-fdiv (n d)
  "floor (N / D) for integer N and positive integer D, any size."
  (cond
   ((and (nelisp--tf-fix n) (nelisp--tf-fix d)) (nelisp--tf-fl n d))
   ((nelisp--tf-fix n) (if (< n 0) -1 0))
   ((<= d 2147483648) (floor n d))
   ;; D a power of two (float timestamps): an arithmetic shift is the floor.
   ((= (logand d (1- d)) 0) (ash n (- 1 (nelisp--tf-ilen d))))
   (t (let* ((neg (< n 0)) (a (abs n)) (q 0) (r 0) (i (nelisp--tf-bitlen a)))
        (while (> i 0)
          (setq i (1- i))
          (setq r (+ r r (logand (ash a (- i)) 1)) q (+ q q))
          (when (>= r d) (setq r (- r d) q (1+ q))))
        (if neg (if (= r 0) (- q) (- (1+ q))) q)))))

(defun nelisp--tf-fmod (n d)
  (if (and (nelisp--tf-fix n) (nelisp--tf-fix d)) (nelisp--tf-md n d)
    (- n (* d (nelisp--tf-fdiv n d)))))

(defun nelisp--tf-muldiv (a m d)
  "floor (A * M / D) with M, D positive."
  (if (and (nelisp--tf-fix m) (nelisp--tf-fix d))
      (let ((g (nelisp--tf-gcd m d)))
        (nelisp--tf-fdiv (* a (/ m g)) (/ d g)))
    (nelisp--tf-fdiv (* a m) d)))

(defun nelisp--tf-gcd (a b)
  (setq a (if (< a 0) (- a) a) b (if (< b 0) (- b) b))
  (while (/= b 0)
    (let ((r (if (and (nelisp--tf-fix a) (nelisp--tf-fix b)) (% a b) (- a (* b (nelisp--tf-fdiv a b))))))
      (setq a b b r)))
  a)

;;;; ---- time values -------------------------------------------------------
(defun nelisp--tf-bad-spec ()
  (signal 'error (list "Invalid time specification")))
(defun nelisp--tf-overflow ()
  (signal 'error (list "Specified time is not representable")))

(defun nelisp--tf-float-spec (f)
  "Return (TICKS . HZ) exactly equal to the double F (GNU decode_float_time)."
  (cond
   ((/= f f) (nelisp--tf-bad-spec))
   ((and (/= f 0.0) (= f (* f 2.0))) (nelisp--tf-overflow))
   ((= f 0.0) (cons 0 1))
   (t
    (let* ((e (logb f))
           (scale (if (>= e -1022) (- 52 e) 1074))
           (x f) (k scale))
      (if (< scale 0)
          (cons (truncate f) 1)
        (while (> k 60)
          (setq x (* x 1152921504606846976.0) k (- k 60)))
        (setq x (* x (float (ash 1 k))))
        (cons (truncate x) (ash 1 scale)))))))

(defun nelisp--tf-spec (time)
  "Decode TIME like GNU `decode_lisp_time' into a fresh (TICKS . HZ)."
  (cond
   ((integerp time) (cons time 1))
   ((null time)
    (let ((us (nl-unix-time-usec))) (cons (* us 1000) 1000000000)))
   ((consp time)
    (let ((high (car time)) (low (cdr time)))
      (if (consp low)
          (let ((hz 1) (usec 0) (psec 0) (tail (cdr low)))
            (setq low (car low))
            (cond
             ((consp tail)
              (setq usec (car tail) tail (cdr tail))
              (if (consp tail) (setq psec (car tail) hz 1000000000000)
                (setq hz 1000000)))
             (tail (setq usec tail hz 1000000)))
            (unless (and (nelisp--tf-fix usec) (nelisp--tf-fix psec) (integerp high) (integerp low))
              (nelisp--tf-bad-spec))
            (let* ((us (+ usec (nelisp--tf-fl psec 1000000)))
                   (carry (nelisp--tf-fl us 1000000))
                   (ps (nelisp--tf-md psec 1000000))
                   (us (nelisp--tf-md us 1000000))
                   (s (+ (* high 65536) low carry)))
              (cons (cond ((eq hz 1) s)
                          ((eq hz 1000000) (+ (* s 1000000) us))
                          (t (+ (* s 1000000000000) (* us 1000000) ps)))
                    hz)))
        (unless (and (integerp high) (integerp low) (> low 0))
          (nelisp--tf-bad-spec))
        (cons high low))))
   ((floatp time) (nelisp--tf-float-spec time))
   (t (nelisp--tf-bad-spec))))

(defun nelisp--tf-timespec (time)
  "Return (SEC . NSEC) for TIME (GNU lisp_time_argument)."
  (let ((s (nelisp--tf-spec time)))
    (let ((ticks (car s)) (hz (cdr s)))
      (cond
       ((eq hz 1) (cons ticks 0))
       ((eq hz 1000000000)
        (let ((q (if (nelisp--tf-fix ticks) (nelisp--tf-fl ticks 1000000000) (floor ticks 1000000000))))
          (cons q (- ticks (* q 1000000000)))))
       (t (let* ((q (nelisp--tf-muldiv ticks 1000000000 hz))
                 (sec (if (nelisp--tf-fix q) (nelisp--tf-fl q 1000000000) (floor q 1000000000))))
            (cons sec (- q (* sec 1000000000)))))))))

(defun nelisp--tf-seconds (time)
  "Whole seconds of TIME (GNU lisp_seconds_argument); sub-second parts unchecked."
  (cond
   ((integerp time) time)
   ((and (consp time) (consp (cdr time)))
    (let ((high (car time)) (low (car (cdr time))))
      (unless (and (integerp high) (integerp low)) (nelisp--tf-bad-spec))
      (+ (* high 65536) low)))
   (t (let ((s (nelisp--tf-spec time))) (nelisp--tf-fdiv (car s) (cdr s))))))

(defun nelisp--tf-trillion-factor-p (hz)
  (and (integerp hz) (> hz 0) (<= hz 1000000000000) (= (% 1000000000000 hz) 0)))

(defun nelisp--tf-list4 (ticks hz)
  (let* ((q (nelisp--tf-muldiv ticks 1000000000000 hz))
         (rest (nelisp--tf-fdiv q 1000000000000))
         (full (- q (* rest 1000000000000))))
    (list (if (nelisp--tf-fix rest) (nelisp--tf-fl rest 65536) (floor rest 65536)) (if (nelisp--tf-fix rest) (nelisp--tf-md rest 65536) (mod rest 65536)) (/ full 1000000) (% full 1000000))))

(defun nelisp--tf-arith (a b subtract)
  (let* ((ta (nelisp--tf-spec a)) (tb (nelisp--tf-spec b))
         (na (car ta)) (da (cdr ta)) (nb (car tb)) (db (cdr tb)) ticks hz)
    (if (eql da db)
        (setq hz da ticks (if subtract (- na nb) (+ na nb)))
      (let* ((g (nelisp--tf-gcd da db))
             (fa (nelisp--tf-fdiv da g)) (fb (nelisp--tf-fdiv db g))
             (ihz (* fa db))
             (it (if subtract (- (* fb na) (* fa nb)) (+ (* fb na) (* fa nb))))
             (hzmin (min da db))
             (ig (nelisp--tf-gcd it ihz)))
        (when (> ig 1)
          (setq it (nelisp--tf-fdiv it ig) ihz (nelisp--tf-fdiv ihz ig))
          (when (< ihz hzmin)
            (let ((rescale (- (nelisp--tf-fdiv (- hzmin) ihz))))
              (setq it (* it rescale) ihz (* ihz rescale)))))
        (setq ticks it hz ihz)))
    (cond
     ((eql hz 1) ticks)
     ((or (not current-time-list)
          (and (consp a) (not (consp (cdr a))))
          (and (consp b) (not (consp (cdr b))))
          (not (nelisp--tf-trillion-factor-p hz)))
      (cons ticks hz))
     (t (nelisp--tf-list4 ticks hz)))))

(unless (fboundp 'time-add)
  (defun time-add (a b)
    "Return the sum of two time values A and B, as a time value."
    (nelisp--tf-arith a b nil)))

(unless (fboundp 'time-subtract)
  (defun time-subtract (a b)
    "Return the difference between two time values A and B, as a time value."
    (if (and (eq a b) (not (floatp a)))
        (if current-time-list (list 0 0 0 0) (cons 0 1000000000))
      (nelisp--tf-arith a b t))))

(defun nelisp--tf-cmp (a b)
  "Negative, 0, positive as time value A <, =, > B (GNU time_cmp)."
  (cond
   ((eq a b) 0)
   ((and (integerp a) (integerp b)) (- a b))
   (t (let ((x a) (y b))
        (when (and (consp a) (consp b) (eq (cdr a) (cdr b)))
          (setq x (car a) y (car b)))
        (if (and (nelisp--tf-fix x) (nelisp--tf-fix y))
            (- x y)
          (let* ((ta (nelisp--tf-spec a)) (tb (nelisp--tf-spec b))
                 (za (car ta)) (zb (car tb)))
            (unless (eql (cdr ta) (cdr tb))
              (setq za (* za (cdr tb)) zb (* zb (cdr ta))))
            (cond ((< za zb) -1) ((> za zb) 1) (t 0))))))))

(unless (fboundp 'time-less-p)
  (defun time-less-p (a b)
    "Return non-nil if time value A is less than time value B."
    (< (nelisp--tf-cmp a b) 0)))

(unless (fboundp 'time-equal-p)
  (defun time-equal-p (a b)
    "Return non-nil if A and B are equal time values."
    (and (eq (null a) (null b)) (= (nelisp--tf-cmp a b) 0))))

(unless (fboundp 'time-convert)
  (defun time-convert (time &optional form)
    "Convert TIME value to a Lisp timestamp of the given FORM."
    (let* ((s (nelisp--tf-spec time))
           (ticks (car s)) (hz (cdr s)))
      (setq form (if form form (if current-time-list 'list t)))
      (cond
       ((eq form 'list) (nelisp--tf-list4 ticks hz))
       ((eq form 'integer) (if (integerp time) time (nelisp--tf-fdiv ticks hz)))
       (t
        (when (eq form t) (setq form hz))
        (cond
         ((and (consp time) (eq form (cdr time))) time)
         ((eql form hz) (cons ticks form))
         (t
          (unless (and (integerp form) (> form 0))
            (signal 'error (list "Invalid time frequency" form)))
          (cons (nelisp--tf-muldiv ticks form hz) form))))))))

;;;; ---- current-time ------------------------------------------------------
(unless (fboundp 'current-time)
  (defun current-time ()
    "Return the current time, as the number of seconds since the epoch."
    (let ((us (nl-unix-time-usec)))
      (if current-time-list
          (let ((s (/ us 1000000)))
            (list (/ s 65536) (% s 65536) (% us 1000000) 0))
        (cons (* us 1000) 1000000000)))))

;;;; ---- float-time (exact, GNU frac_to_double) ---------------------------
(defun nelisp--tf-ilen (a)
  "Number of bits of positive integer A."
  (let ((k (nelisp--tf-bitlen a)))
    (while (and (> k 0) (= (ash a (- 1 k)) 0)) (setq k (1- k)))
    k))

(defun nelisp--tf-scale2 (x k)
  "X * 2^K for float X, exactly."
  (while (> k 60) (setq x (* x 1152921504606846976.0) k (- k 60)))
  (while (< k -60) (setq x (* x 8.673617379884035e-19) k (+ k 60)))
  (cond ((> k 0) (* x (float (ash 1 k))))
        ((< k 0) (/ x (float (ash 1 (- k)))))
        (t x)))

(defun nelisp--tf-frac-double (n d)
  "N / D rounded to the nearest double (D > 0)."
  (cond
   ((= n 0) 0.0)
   ((and (nelisp--tf-fix n) (nelisp--tf-fix d) (= (% n d) 0)) (float (/ n d)))
   (t
    (let* ((neg (< n 0)) (a (abs n))
           (scale (+ (- (nelisp--tf-ilen d) (nelisp--tf-ilen a)) 53)) q r incr)
      (if (< scale 0)
          (setq d (ash d (- scale)))
        (setq scale (min scale 1074) a (ash a scale)))
      (setq q (nelisp--tf-fdiv a d) r (- a (* q d)))
      (if (<= (nelisp--tf-ilen q) 53)
          (let ((r2 (* 2 r)))
            (setq incr (if (or (> r2 d) (and (= r2 d) (= (logand q 1) 1))) 1 0)))
        (setq incr (if (and (= (logand q 1) 1) (or (= (logand (ash q -1) 1) 1) (/= r 0))) 2 0)))
      (setq q (+ q incr))
      (when (> (nelisp--tf-ilen q) 53) (setq q (- q (logand q 1))))
      (let ((x (nelisp--tf-scale2 (float q) (- scale))))
        (if neg (- x) x))))))

(defun nelisp--tf-float-time (time)
  (let ((s (nelisp--tf-spec time)))
    (nelisp--tf-frac-double (car s) (cdr s))))

;; `float-time' itself is the native subr: it answers the clock, floats, integers, integer lists
;; and small (TICKS . HZ) pairs, and hands every other TIME to `nelisp--tf-float-time' above
;; (scripts/nelisp-standalone-build.el, the "float-time" dispatch arm).

;;; tf-zone.el --- time zones: TZif, POSIX TZ rules, glibc semantics  -*- lexical-binding: t; -*-
;; Zone objects:  [0 INFO]                         fixed offset
;;                [1 STD DST R0 R1 STDOFF DSTOFF]  POSIX rule zone
;;                [2 TRANS TYPEIDX TYPES FOOTER FIRST]  TZif zone
;; INFO is the list (UTOFF ISDST ABBR) answered for a UTC instant.

(defvar nelisp--tf-zone-cache (make-hash-table :test 'equal))
(defvar nelisp--tf-utc-zone (vector 0 (list 0 nil "GMT")))
(defvar nelisp--tf-override nil "(TZENV . ZONE) set by `set-time-zone-rule'.")
(defvar nelisp--tf-local-key 'unset)
(defvar nelisp--tf-local-zone nil)
(defvar nelisp--tf-mk-offset 0 "gnulib mktime's localtime_offset heuristic.")

;;; -- calendar --------------------------------------------------------------
(defconst nelisp--tf-cum-days
  [0 31 59 90 120 151 181 212 243 273 304 334])

(defun nelisp--tf-leap-p (y)
  (and (= (% y 4) 0) (or (/= (% y 100) 0) (= (% y 400) 0))))

(defun nelisp--tf-days-from-civil (y m d)
  "Days since 1970-01-01 of proleptic Gregorian Y-M-D (M in 1..12)."
  (let* ((y (if (<= m 2) (1- y) y))
         (era (nelisp--tf-fl y 400))
         (yoe (- y (* era 400)))
         (doy (+ (/ (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (/ yoe 4) (- (/ yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

;; March-based calendar tables (day of the 400-year era -> year-of-era start,
;; day of March-based year -> month, day).
(defconst nelisp--tf-ystart
  [0 365 730 1095 1461 1826 2191 2556 2922 3287 3652 4017 4383 4748 5113 5478 5844 6209 6574
   6939 7305 7670 8035 8400 8766 9131 9496 9861 10227 10592 10957 11322 11688 12053 12418
   12783 13149 13514 13879 14244 14610 14975 15340 15705 16071 16436 16801 17166 17532 17897
   18262 18627 18993 19358 19723 20088 20454 20819 21184 21549 21915 22280 22645 23010 23376
   23741 24106 24471 24837 25202 25567 25932 26298 26663 27028 27393 27759 28124 28489 28854
   29220 29585 29950 30315 30681 31046 31411 31776 32142 32507 32872 33237 33603 33968 34333
   34698 35064 35429 35794 36159 36524 36889 37254 37619 37985 38350 38715 39080 39446 39811
   40176 40541 40907 41272 41637 42002 42368 42733 43098 43463 43829 44194 44559 44924 45290
   45655 46020 46385 46751 47116 47481 47846 48212 48577 48942 49307 49673 50038 50403 50768
   51134 51499 51864 52229 52595 52960 53325 53690 54056 54421 54786 55151 55517 55882 56247
   56612 56978 57343 57708 58073 58439 58804 59169 59534 59900 60265 60630 60995 61361 61726
   62091 62456 62822 63187 63552 63917 64283 64648 65013 65378 65744 66109 66474 66839 67205
   67570 67935 68300 68666 69031 69396 69761 70127 70492 70857 71222 71588 71953 72318 72683
   73048 73413 73778 74143 74509 74874 75239 75604 75970 76335 76700 77065 77431 77796 78161
   78526 78892 79257 79622 79987 80353 80718 81083 81448 81814 82179 82544 82909 83275 83640
   84005 84370 84736 85101 85466 85831 86197 86562 86927 87292 87658 88023 88388 88753 89119
   89484 89849 90214 90580 90945 91310 91675 92041 92406 92771 93136 93502 93867 94232 94597
   94963 95328 95693 96058 96424 96789 97154 97519 97885 98250 98615 98980 99346 99711 100076
   100441 100807 101172 101537 101902 102268 102633 102998 103363 103729 104094 104459 104824
   105190 105555 105920 106285 106651 107016 107381 107746 108112 108477 108842 109207 109572
   109937 110302 110667 111033 111398 111763 112128 112494 112859 113224 113589 113955 114320
   114685 115050 115416 115781 116146 116511 116877 117242 117607 117972 118338 118703 119068
   119433 119799 120164 120529 120894 121260 121625 121990 122355 122721 123086 123451 123816
   124182 124547 124912 125277 125643 126008 126373 126738 127104 127469 127834 128199 128565
   128930 129295 129660 130026 130391 130756 131121 131487 131852 132217 132582 132948 133313
   133678 134043 134409 134774 135139 135504 135870 136235 136600 136965 137331 137696 138061
   138426 138792 139157 139522 139887 140253 140618 140983 141348 141714 142079 142444 142809
   143175 143540 143905 144270 144636 145001 145366 145731])
(defconst nelisp--tf-mtab
  [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4
   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
   5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7
   7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
   8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 10 10 10 10 10 10 10
   10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 11 11 11 11 11
   11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 12 12 12 12 12 12
   12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
   2 2 2 2 2 2 2])
(defconst nelisp--tf-dtab
  [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 1 2 3
   4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 1 2 3 4 5 6 7 8
   9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 1 2 3 4 5 6 7 8 9 10
   11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 1 2 3 4 5 6 7 8 9 10 11 12 13
   14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
   18 19 20 21 22 23 24 25 26 27 28 29 30 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
   21 22 23 24 25 26 27 28 29 30 31 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22
   23 24 25 26 27 28 29 30 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
   26 27 28 29 30 31 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
   28 29 30 31 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29])

(defconst nelisp--tf-era400 [-4000 -3600 -3200 -2800 -2400 -2000 -1600 -1200 -800 -400 0 400 800 1200 1600 2000 2400 2800 3200 3600 4000 4400 4800 5200 5600 6000 6400 6800 7200 7600 8000 8400 8800 9200 9600 10000 10400 10800 11200 11600 12000])
(defmacro nelisp--tf-mul400 (x)
  "X * 400, from a table for the usual range (the VM's `*' is slow)."
  `(let ((tf-e ,x)) (if (and (>= tf-e -10) (<= tf-e 30)) (aref nelisp--tf-era400 (+ tf-e 10)) (* tf-e 400))))

(defmacro nelisp--tf-yday (y m d)
  "Zero-based day of the year of Y-M-D (variables)."
  `(+ (aref nelisp--tf-cum-days (1- ,m)) ,d -1
      (if (and (> ,m 2) (= (% ,y 4) 0) (or (/= (% ,y 100) 0) (= (% ,y 400) 0))) 1 0)))

(defmacro nelisp--tf-with-civil (days vars &rest body)
  "Bind VARS = (Y M D) for epoch DAYS (a variable, fixnum) around BODY."
  `(let* ((tf-z (+ ,days 719468))
          (tf-doe (nelisp--tf-md tf-z 146097))
          (tf-yoe (/ (- (+ tf-doe (/ tf-doe 36524)) (/ tf-doe 1460) (/ tf-doe 146096)) 365))
          (tf-doy (- tf-doe (aref nelisp--tf-ystart tf-yoe)))
          (,(nth 1 vars) (aref nelisp--tf-mtab tf-doy))
          (,(nth 2 vars) (aref nelisp--tf-dtab tf-doy))
          (,(nth 0 vars) (+ tf-yoe (nelisp--tf-mul400 (/ (- tf-z tf-doe) 146097))
                            (if (<= ,(nth 1 vars) 2) 1 0))))
     ,@body))

(defun nelisp--tf-civil (days)
  "Return (YEAR MONTH DAY WDAY YDAY) for epoch DAYS."
  (nelisp--tf-with-civil days (y m d)
    (list y m d (nelisp--tf-md (+ days 4) 7) (nelisp--tf-yday y m d))))

;;; -- POSIX TZ strings ------------------------------------------------------
(defun nelisp--tf-digits (s i)
  "Return (VALUE . NEXT) of the decimal digits of S at I, or nil."
  (let ((j i) (n (length s)))
    (while (and (< j n) (>= (aref s j) ?0) (<= (aref s j) ?9)) (setq j (1+ j)))
    (and (> j i) (cons (string-to-number (substring s i j)) j))))

(defun nelisp--tf-parse-offset (s i signed)
  "Parse [+-]hh[:mm[:ss]] at I.  Return (SECONDS . NEXT), seconds as written."
  (let ((n (length s)) (neg nil) h m (sec 0) r)
    (when (and signed (< i n) (memq (aref s i) '(?+ ?-)))
      (setq neg (eq (aref s i) ?-) i (1+ i)))
    (setq r (nelisp--tf-digits s i))
    (when r
      (setq h (min (car r) 24) i (cdr r) m 0)
      (when (and (< i n) (eq (aref s i) ?:))
        (let ((r2 (nelisp--tf-digits s (1+ i))))
          (when r2
            (setq m (min (car r2) 59) i (cdr r2))
            (when (and (< i n) (eq (aref s i) ?:))
              (let ((r3 (nelisp--tf-digits s (1+ i))))
                (when r3 (setq sec (min (car r3) 59) i (cdr r3))))))))
      (let ((v (+ (* h 3600) (* m 60) sec)))
        (cons (if neg (- v) v) i)))))

(defun nelisp--tf-parse-name (s i)
  "Parse a zone abbreviation at I.  Return (NAME . NEXT) or nil."
  (let ((n (length s)))
    (cond
     ((and (< i n) (eq (aref s i) ?<))
      (let ((j (1+ i)))
        (while (and (< j n) (not (eq (aref s j) ?>))) (setq j (1+ j)))
        (and (< j n) (>= (- j i 1) 3) (cons (substring s (1+ i) j) (1+ j)))))
     (t
      (let ((j i))
        (while (and (< j n) (let ((c (aref s j)))
                              (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)))))
          (setq j (1+ j)))
        (and (>= (- j i) 3) (cons (substring s i j) j)))))))

(defun nelisp--tf-parse-rule (s i)
  "Parse Jn | n | Mm.w.d [/time] at I.  Return ((TYPE A B C SECS) . NEXT) or nil."
  (let ((n (length s)) type a b c r (secs 7200))
    (cond
     ((>= i n) nil)
     ((eq (aref s i) ?J)
      (setq r (nelisp--tf-digits s (1+ i)))
      (when (and r (>= (car r) 1) (<= (car r) 365)) (setq type 'j a (car r) i (cdr r))))
     ((eq (aref s i) ?M)
      (setq r (nelisp--tf-digits s (1+ i)))
      (when (and r (>= (car r) 1) (<= (car r) 12) (< (cdr r) n) (eq (aref s (cdr r)) ?.))
        (setq a (car r) r (nelisp--tf-digits s (1+ (cdr r))))
        (when (and r (>= (car r) 1) (<= (car r) 5) (< (cdr r) n) (eq (aref s (cdr r)) ?.))
          (setq b (car r) r (nelisp--tf-digits s (1+ (cdr r))))
          (when (and r (<= (car r) 6))
            (setq c (car r) i (cdr r) type 'm)))))
     (t (setq r (nelisp--tf-digits s i))
        (when (and r (<= (car r) 365)) (setq type 'n a (car r) i (cdr r)))))
    (when type
      (if (and (< i n) (eq (aref s i) ?/))
          (let ((o (nelisp--tf-parse-offset s (1+ i) t)))
            (if o (setq secs (car o) i (cdr o)) (setq type nil)))))
    (and type (cons (list type a b c secs) i))))

(defun nelisp--tf-rule-utc (rule year utoff)
  "UTC instant at which RULE falls in YEAR, the wall clock then being UTOFF.
Like glibc compute_change, years <= 1970 are based on 1970-01-01."
  (let* ((type (car rule)) (a (nth 1 rule)) (b (nth 2 rule)) (c (nth 3 rule))
         (secs (nth 4 rule))
         (rjan1 (nelisp--tf-days-from-civil year 1 1))
         (jan1 (if (> year 1970) rjan1 0))
         (day
          (cond
           ((eq type 'j) (+ jan1 (1- a) (if (and (nelisp--tf-leap-p year) (>= a 60)) 1 0)))
           ((eq type 'n) (+ jan1 a))
           (t (let* ((first (nelisp--tf-days-from-civil year a 1))
                     (wd (nelisp--tf-md (+ first 4) 7))
                     (d (+ first (nelisp--tf-md (- c wd) 7) (* 7 (1- b))))
                     (ml (if (= a 12) 31
                           (- (nelisp--tf-days-from-civil year (1+ a) 1) first))))
                (while (>= (- d first) ml) (setq d (- d 7)))
                (+ jan1 (- d rjan1)))))))
    (- (+ (* day 86400) secs) utoff)))

(defun nelisp--tf-rule-info (z secs)
  (if (and (> (length z) 7) (aref z 7) (>= secs 2140668000))
      (nelisp--tf-rule-info (aref z 7) secs)
    (nelisp--tf-rule-info-1 z secs)))

(defun nelisp--tf-rule-info-1 (z secs)
  (let* ((year (car (nelisp--tf-civil (nelisp--tf-fl secs 86400))))
         (start (nelisp--tf-rule-utc (aref z 3) year (aref z 5)))
         (end (nelisp--tf-rule-utc (aref z 4) year (aref z 6))))
    (if (if (> start end) (or (< secs end) (>= secs start)) (and (>= secs start) (< secs end)))
        (aref z 2) (aref z 1))))

(defun nelisp--tf-posix (s)
  "Build a zone from POSIX TZ string S following glibc __tzset_parse_tz."
  (let* ((std (nelisp--tf-parse-name s 0)))
    (if (null std)
        (vector 0 (list 0 nil ""))
      (let* ((i (cdr std)) (so (nelisp--tf-parse-offset s i t))
             (stdoff (if so (- (car so)) 0)) (n (length s)))
        (if (or (null so) (>= (cdr so) n))
            (vector 0 (list stdoff nil (car std)))
          (setq i (cdr so))
          (let ((dst (nelisp--tf-parse-name s i)))
            (if (null dst)
                (vector 0 (list stdoff nil (car std)))
              (setq i (cdr dst))
              (let* ((do (nelisp--tf-parse-offset s i t))
                     (dstoff (if do (- (car do)) (+ stdoff 3600)))
                     (r0 nil) (r1 nil))
                (when do (setq i (cdr do)))
                (when (and (< i n) (eq (aref s i) ?,))
                  (let ((p0 (nelisp--tf-parse-rule s (1+ i))))
                    (when (and p0 (< (cdr p0) n) (eq (aref s (cdr p0)) ?,))
                      (let ((p1 (nelisp--tf-parse-rule s (1+ (cdr p0)))))
                        (when p1 (setq r0 (car p0) r1 (car p1)))))))
                (if r0
                    (vector 1 (list stdoff nil (car std)) (list dstoff t (car dst))
                            r0 r1 stdoff dstoff nil)
                  ;; No rule: glibc reads the transitions of `posixrules'
                  ;; (America/New_York) and applies the user's offsets: DST
                  ;; starts at the New York start instant shifted by
                  ;; (STDOFF - NY STD), ends at the New York end instant, and
                  ;; times past 2037 use New York's own names and offsets.
                  (vector 1 (list stdoff nil (car std)) (list dstoff t (car dst))
                          (list 'm 3 2 0 7200) (list 'm 11 1 0 7200)
                          (- -36000 stdoff) -14400
                          (nelisp--tf-posix "EST5EDT,M3.2.0,M11.1.0")))))))))))

;;; -- TZif ------------------------------------------------------------------
(defun nelisp--tf-be (s pos n)
  (let ((v 0) (k 0))
    (while (< k n) (setq v (+ (* v 256) (aref s (+ pos k))) k (1+ k)))
    v))

(defun nelisp--tf-bes (s pos n)
  (let ((v (nelisp--tf-be s pos n)))
    (if (>= (aref s pos) 128) (- v (ash 1 (* 8 n))) v)))

(defun nelisp--tf-read-file (path)
  (condition-case nil
      (and (file-regular-p path) (file-readable-p path)
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally path)
             (buffer-string)))
    (error nil)))

(defun nelisp--tf-tzif (s)
  "Parse TZif data S into a zone, or nil when it is not TZif."
  (when (and (> (length s) 44) (string= (substring s 0 4) "TZif"))
    (let* ((ver (aref s 4)) (base 0) (w 4)
           (isut (nelisp--tf-be s 20 4)) (isstd (nelisp--tf-be s 24 4))
           (leap (nelisp--tf-be s 28 4)) (timecnt (nelisp--tf-be s 32 4))
           (typecnt (nelisp--tf-be s 36 4)) (charcnt (nelisp--tf-be s 40 4)))
      (when (>= ver ?2)
        (setq base (+ 44 (* timecnt 4) timecnt (* typecnt 6) charcnt (* leap 8) isstd isut)
              w 8
              isut (nelisp--tf-be s (+ base 20) 4) isstd (nelisp--tf-be s (+ base 24) 4)
              leap (nelisp--tf-be s (+ base 28) 4) timecnt (nelisp--tf-be s (+ base 32) 4)
              typecnt (nelisp--tf-be s (+ base 36) 4) charcnt (nelisp--tf-be s (+ base 40) 4)))
      (let* ((p (+ base 44))
             (trans (make-vector timecnt 0)) (idx (make-vector timecnt 0))
             (types (make-vector typecnt nil)) (k 0) (tp 0))
        (while (< k timecnt)
          (aset trans k (nelisp--tf-bes s (+ p (* k w)) w))
          (setq k (1+ k)))
        (setq p (+ p (* timecnt w)) k 0)
        (while (< k timecnt) (aset idx k (aref s (+ p k))) (setq k (1+ k)))
        (setq p (+ p timecnt) tp p)
        (let ((cp (+ p (* typecnt 6))))
          (setq k 0)
          (while (< k typecnt)
            (let* ((q (+ tp (* k 6))) (ai (aref s (+ q 5))) (e (+ cp ai)))
              (while (/= (aref s e) 0) (setq e (1+ e)))
              (aset types k (list (nelisp--tf-bes s q 4) (/= (aref s (+ q 4)) 0)
                                  (substring s (+ cp ai) e))))
            (setq k (1+ k)))
          (setq p (+ cp charcnt (* leap (+ w 4)) isstd isut)))
        (let ((footer nil) (first nil) (i 0))
          (when (and (>= ver ?2) (< p (length s)) (eq (aref s p) ?\n))
            (let ((e (1+ p)))
              (while (and (< e (length s)) (/= (aref s e) ?\n)) (setq e (1+ e)))
              (when (> e (1+ p)) (setq footer (nelisp--tf-posix (substring s (1+ p) e))))))
          (while (and (< i typecnt) (nth 1 (aref types i))) (setq i (1+ i)))
          (setq first (aref types (if (= i typecnt) 0 i)))
          (vector 2 trans idx types footer first))))))

(defun nelisp--tf-tzif-info (z secs)
  (let* ((trans (aref z 1)) (n (length trans)))
    (cond
     ((or (= n 0) (< secs (aref trans 0))) (aref z 5))
     ((>= secs (aref trans (1- n)))
      (if (aref z 4) (nelisp--tf-info (aref z 4) secs)
        (aref (aref z 3) (aref (aref z 2) (1- n)))))
     (t (let ((lo 0) (hi (1- n)))
          (while (< lo hi)
            (let ((mid (/ (+ lo hi 1) 2)))
              (if (<= (aref trans mid) secs) (setq lo mid) (setq hi (1- mid)))))
          (aref (aref z 3) (aref (aref z 2) lo)))))))

(defun nelisp--tf-info (z secs)
  "Answer (UTOFF ISDST ABBR) of zone Z at UTC instant SECS."
  (let ((k (aref z 0)))
    (cond ((eq k 0) (aref z 1))
          ((eq k 1) (nelisp--tf-rule-info z secs))
          (t (nelisp--tf-tzif-info z secs)))))

;;; -- zone lookup -----------------------------------------------------------
(defun nelisp--tf-string-zone (tz)
  "Zone for TZ string TZ; nil TZ is the system wall clock (/etc/localtime)."
  (cond
   ((null tz)
    (or (let ((d (nelisp--tf-read-file "/etc/localtime"))) (and d (nelisp--tf-tzif d)))
        (nelisp--tf-posix "UTC0")))
   ((string= tz "") (vector 0 (list 0 nil "Universal")))
   (t
    (let* ((name (if (eq (aref tz 0) ?:) (substring tz 1) tz))
           (dir (or (getenv "TZDIR") "/usr/share/zoneinfo"))
           (path (if (and (> (length name) 0) (eq (aref name 0) ?/)) name
                   (concat dir "/" name)))
           (data (and (> (length name) 0) (nelisp--tf-read-file path))))
      (or (and data (nelisp--tf-tzif data))
          (nelisp--tf-posix name))))))

(defun nelisp--tf-offset-string (off prec-sign abbr)
  "TZ string GNU tzlookup builds for numeric zone OFF."
  (let* ((a (abs off)) (h (/ a 3600)) (r (% a 3600)) (mi (/ r 60)) (se (% r 60)))
    (concat (if abbr (concat "<" abbr ">") (concat "<" (if (< off 0) "-" "+")
                                                   (nelisp--tf-pad2 h)
                                                   (if (/= r 0) (nelisp--tf-pad2 mi) "")
                                                   (if (/= se 0) (nelisp--tf-pad2 se) "") ">"))
            (if (< off 0) "" "-") (number-to-string h) ":" (nelisp--tf-pad2 mi) ":" (nelisp--tf-pad2 se))))

(defun nelisp--tf-pad2 (n) (if (< n 10) (concat "0" (number-to-string n)) (number-to-string n)))

(defun nelisp--tf-zone (zone)
  "Resolve ZONE (GNU tzlookup) to a zone object."
  (cond
   ((null zone) (nelisp--tf-local-zone))
   ((or (eq zone t) (eql zone 0)) nelisp--tf-utc-zone)
   ((eq zone 'wall) (nelisp--tf-cached 'wall nil))
   ((stringp zone) (nelisp--tf-cached zone zone))
   ((nelisp--tf-fix zone)
    (or (gethash zone nelisp--tf-zone-cache)
        (puthash zone (nelisp--tf-posix (nelisp--tf-offset-string zone nil nil))
                 nelisp--tf-zone-cache)))
   ((and (consp zone) (nelisp--tf-fix (car zone)) (consp (cdr zone)))
    (let ((abbr (car (cdr zone))))
      (unless (stringp abbr) (signal 'wrong-type-argument (list 'stringp abbr)))
      (let ((key (nelisp--tf-offset-string (car zone) nil abbr)))
        (nelisp--tf-cached key key))))
   (t (signal 'error (list "Invalid time zone specification" zone)))))

(defun nelisp--tf-cached (key tz)
  (or (gethash key nelisp--tf-zone-cache)
      (progn
        (when (> (hash-table-count nelisp--tf-zone-cache) 200) (clrhash nelisp--tf-zone-cache))
        (puthash key (nelisp--tf-string-zone tz) nelisp--tf-zone-cache))))

(defun nelisp--tf-local-zone ()
  (let ((tz (getenv "TZ")))
    (cond
     ((and nelisp--tf-override (equal (car nelisp--tf-override) tz)) (cdr nelisp--tf-override))
     ((and (not (eq nelisp--tf-local-key 'unset)) (equal nelisp--tf-local-key tz)) nelisp--tf-local-zone)
     (t (setq nelisp--tf-local-zone (nelisp--tf-cached (or tz 'wall) tz)
              nelisp--tf-local-key tz)
        nelisp--tf-local-zone))))

(unless (fboundp 'set-time-zone-rule)
  (defun set-time-zone-rule (tz)
    "Set the Emacs local time zone using TZ."
    (setq nelisp--tf-override
          (cons (getenv "TZ") (nelisp--tf-zone (if (null tz) 'wall tz))))
    nil))

;;; tf-fmt.el --- decode-time, encode-time, format-time-string  -*- lexical-binding: t; -*-
;; Mirrors GNU timefns.c Fdecode_time/Fencode_time/format_time_string and
;; lib/strftime.c (gnulib nstrftime) with the C locale.

(defconst nelisp--tf-d2
  (let ((v (make-vector 100 nil)) (i 0))
    (while (< i 100) (aset v i (concat (number-to-string (/ i 10)) (number-to-string (% i 10)))) (setq i (1+ i)))
    v))
(defconst nelisp--tf-sp2
  (let ((v (make-vector 100 nil)) (i 0))
    (while (< i 100) (aset v i (if (< i 10) (concat " " (number-to-string i)) (aref nelisp--tf-d2 i))) (setq i (1+ i)))
    v))
(defconst nelisp--tf-d3
  (let ((v (make-vector 367 nil)) (i 0))
    (while (< i 367) (aset v i (concat (if (< i 100) "0" "") (if (< i 10) "0" "") (number-to-string i))) (setq i (1+ i)))
    v))
(defconst nelisp--tf-wday-abbr ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
(defconst nelisp--tf-wday-full ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
(defconst nelisp--tf-mon-abbr ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
(defconst nelisp--tf-mon-full ["January" "February" "March" "April" "May" "June" "July" "August"
                               "September" "October" "November" "December"])

(defun nelisp--tf-check-secs (secs)
  (when (or (> secs 100000000000000000) (< secs -100000000000000000)) (nelisp--tf-overflow)))

(defun nelisp--tf-localtime (secs zone)
  "Return (YEAR MONTH DAY DAYS HOUR MIN SEC INFO) of UTC SECS in ZONE."
  (when (or (> secs 100000000000000000) (< secs -100000000000000000)) (nelisp--tf-overflow))
  (let* ((info (if (eq (aref zone 0) 0) (aref zone 1) (nelisp--tf-info zone secs)))
         (loc (+ secs (car info)))
         (sod (nelisp--tf-md loc 86400))
         (days (/ (- loc sod) 86400)))
    (nelisp--tf-with-civil days (y m d)
      (when (or (> y 2147485547) (< y -2147481748)) (nelisp--tf-overflow))
      (list y m d days (/ sod 3600) (/ (% sod 3600) 60) (% sod 60) info))))

(unless (fboundp 'decode-time)
  (defun decode-time (&optional specified-time zone form)
    "Decode a timestamp into (SEC MIN HOUR DAY MON YEAR DOW DST UTCOFF)."
    (let (secs hz ticks)
      (if (eq form t)
          (let ((s (nelisp--tf-spec specified-time)))
            (setq ticks (car s) hz (cdr s) secs (nelisp--tf-fdiv ticks hz)))
        (setq secs (nelisp--tf-seconds specified-time) hz 1))
      (let* ((lt (nelisp--tf-localtime secs (nelisp--tf-zone zone)))
             (info (nth 7 lt)))
        (list (if (eql hz 1) (nth 6 lt)
                (cons (+ (* hz (nth 6 lt)) (nelisp--tf-fmod ticks hz)) hz))
              (nth 5 lt) (nth 4 lt) (nth 2 lt) (nth 1 lt) (nth 0 lt) (nelisp--tf-md (+ (nth 3 lt) 4) 7)
              (nth 1 info) (car info))))))

;;; -- encode-time (gnulib mktime) -------------------------------------------
(defun nelisp--tf-isdst-differ (a b)
  (and (not (eq (= a 0) (= b 0))) (>= a 0) (>= b 0)))

(defun nelisp--tf-mktime (sec min hour mday mon1 year zone isdst)
  "Port of gnulib __mktime_internal.  MON1 is 1-based.  Return UTC seconds."
  (let* ((mon (1- mon1))
         (y (+ year (nelisp--tf-fl mon 12)))
         (m (1+ (nelisp--tf-md mon 12)))
         (lsec 0))
    (setq lsec (+ (* (nelisp--tf-days-from-civil y m 1) 86400) (* (1- mday) 86400)
                  (* hour 3600) (* min 60) sec))
    (nelisp--tf-check-secs lsec)
    (if (eq zone nelisp--tf-utc-zone)
        lsec
      (let* ((t0 (+ lsec nelisp--tf-mk-offset))
             (tt t0) (t1 t0) (t2 t0) (dst2 nil) (probes 12) (found nil) (info nil) (dt 0))
        (while (not found)
          (setq info (nelisp--tf-info zone tt))
          (setq dt (- lsec (+ tt (car info))))
          (cond
           ((= dt 0) (setq found 'match))
           ((and (eql tt t1) (not (eql tt t2))
                 (if (eq isdst -1)
                     (<= (if dst2 1 0) (if (nth 1 info) 1 0))
                   (not (eq (/= isdst 0) (and (nth 1 info) t)))))
            (setq found 'oscillate))
           (t (setq probes (1- probes))
              (when (= probes 0) (nelisp--tf-overflow))
              (setq t1 t2 t2 tt tt (+ tt dt) dst2 (and (nth 1 info) t)))))
        (when (and (eq found 'match)
                   (nelisp--tf-isdst-differ isdst (if (nth 1 info) 1 0)))
          ;; glibc (pre-2.42 gnulib) heuristic: probe both directions for a time
          ;; with the requested isdst, else assume a one-hour DST difference.
          (let ((stride 601200) (delta 601200) (bound (+ 268227000 601200)) (done nil)
                (dstdiff (- (if (= isdst 0) 1 0) (if (nth 1 info) 0 1))))
            (while (and (not done) (< delta bound))
              (let ((dir -1))
                (while (and (not done) (<= dir 1))
                  (let* ((ot (+ tt (* delta dir))) (oinfo (nelisp--tf-info zone ot)))
                    (unless (nelisp--tf-isdst-differ isdst (if (nth 1 oinfo) 1 0))
                      (setq tt (- lsec (car oinfo)) done t)))
                  (setq dir (+ dir 2))))
              (setq delta (+ delta stride)))
            (unless done (setq tt (+ tt (* 3600 dstdiff))))))
        (setq nelisp--tf-mk-offset (- tt lsec))
        tt))))

(unless (fboundp 'encode-time)
  (defun encode-time (time &rest obsolescent-arguments)
    "Convert TIME to a timestamp."
    (let ((zone nil) (isdst -1) secarg minarg hourarg mdayarg monarg yeararg
          (nargs (1+ (length obsolescent-arguments))))
      (cond
       ((= nargs 1)
        (let ((tail time) (i 0))
          (while (< i 6)
            (unless (consp tail) (signal 'wrong-type-argument (list 'consp tail)))
            (setq tail (cdr tail) i (1+ i))))
        (setq secarg (car time) time (cdr time)
              minarg (car time) time (cdr time)
              hourarg (car time) time (cdr time)
              mdayarg (car time) time (cdr time)
              monarg (car time) time (cdr time)
              yeararg (car time) time (cdr time))
        (when time
          (unless (consp time) (signal 'wrong-type-argument (list 'consp time)))
          (setq time (cdr time))
          (unless (consp time) (signal 'wrong-type-argument (list 'consp time)))
          (let ((dstflag (car time)))
            (setq time (cdr time))
            (unless (consp time) (signal 'wrong-type-argument (list 'consp time)))
            (setq zone (car time))
            (when (and (symbolp dstflag) (not (nelisp--tf-fix zone)) (not (consp zone)))
              (setq isdst (if dstflag 1 0))))))
       ((< nargs 6)
        (signal 'wrong-number-of-arguments (list 'encode-time nargs)))
       (t
        (when (> nargs 6) (setq zone (car (last obsolescent-arguments))))
        (setq secarg time minarg (nth 0 obsolescent-arguments)
              hourarg (nth 1 obsolescent-arguments) mdayarg (nth 2 obsolescent-arguments)
              monarg (nth 3 obsolescent-arguments) yeararg (nth 4 obsolescent-arguments))))
      (let* ((th (nelisp--tf-spec secarg))
             (hz (cdr th)) sec subticks)
        (if (eql hz 1) (setq sec (car th) subticks 0)
          (setq sec (nelisp--tf-fdiv (car th) hz) subticks (- (car th) (* sec hz))))
        (dolist (v (list sec minarg hourarg mdayarg monarg yeararg))
          (unless (nelisp--tf-fix v) (signal 'wrong-type-argument (list 'fixnump v)))
          (when (or (> v 2147483647) (< v -2147483648)) (nelisp--tf-overflow)))
        (let* ((z (nelisp--tf-zone zone))
               (value (nelisp--tf-mktime sec minarg hourarg mdayarg monarg yeararg z isdst)))
          (if (eql hz 1)
              (if current-time-list (list (nelisp--tf-fl value 65536) (nelisp--tf-md value 65536)) value)
            (cons (+ (* value hz) subticks) hz)))))))

;;; -- strftime --------------------------------------------------------------
(defun nelisp--tf-num (v digits pad width neg always-sign &optional colons)
  "Format V (|V| when NEG) like strftime's do_number_body.
PAD is ?0 (zero), ?_ (space), ?- (none), ?+ (sign) or ?z (default ZERO_PAD)."
  (let* ((u (if neg (nelisp--tf-abs v) v))
         (body (number-to-string u))
         (body (if colons (nelisp--tf-colonize body colons) body))
         (pad (if (eq pad ?z) ?0 pad))
         (w (if (< width 0) digits width))
         (sign (cond (neg "-") (always-sign "+") (t "")))
         (nd (length body))
         (shortage (- w (length sign) nd))
         (padding (if (or (eq pad ?-) (<= shortage 0)) 0 shortage)))
    (cond
     ((= padding 0) (concat sign body))
     ((eq pad ?_) (concat (make-string padding ?\s) sign body))
     (t (concat sign (make-string padding ?0) body)))))

(defun nelisp--tf-colonize (body mask)
  "Insert ':' into digit string BODY per MASK (bit i = before i-th digit from right)."
  (let ((out nil) (i 0) (n (length body)))
    (while (or (< i n) (/= mask 0))
      (when (/= (logand mask 1) 0) (setq out (cons ":" out)))
      (setq mask (ash mask -1))
      (setq out (cons (if (< i n) (string (aref body (- n i 1))) "0") out))
      (setq i (1+ i)))
    (apply #'concat out)))

(defun nelisp--tf-text (s pad width up low)
  "Emit text S with strftime width/pad semantics."
  (when up (setq s (upcase s)))
  (when low (setq s (downcase s)))
  (let ((w (if (or (eq pad ?-) (< width 0)) 0 width)) (n (length s)))
    (if (< n w)
        (concat (make-string (- w n) (if (memq pad '(?0 ?+)) ?0 ?\s)) s)
      s)))

(defvar nelisp--tf-plan-cache (make-hash-table :test 'equal))

(defconst nelisp--tf-ycache (make-vector 9000 nil))

(defun nelisp--tf-simple-code (c pad width up chg colons mod)
  "Fast-path code of a plain directive (no flags, width or modifier), else nil."
  (and (eq pad ?z) (< width 0) (not up) (not chg) (eq mod 0) (eq colons 0)
       (cond ((eq c ?Y) 1) ((eq c ?m) 2) ((eq c ?d) 3) ((eq c ?H) 4) ((eq c ?M) 5)
             ((eq c ?S) 6) ((eq c ?a) 7) ((eq c ?b) 8) ((eq c ?e) 9) ((eq c ?j) 10)
             ((eq c ?T) 11) ((eq c ?F) 12) ((eq c ?y) 13) ((eq c ?p) 14) ((eq c ?Z) 15)
             ((eq c ?z) 16) ((eq c ?A) 17) ((eq c ?B) 18) ((eq c ?s) 19) ((eq c ?I) 20)
             ((eq c ?R) 21) ((eq c ?c) 22) ((eq c ?D) 23) ((eq c ?k) 24))))

(defun nelisp--tf-parse-format (fmt)
  "Parse FMT into a list of literal strings and directive vectors
[CHAR PAD WIDTH UP CHG COLONS MOD TEXT] (TEXT = the directive source)."
  (let ((n (length fmt)) (i 0) (start 0) (plan nil))
    (while (< i n)
      (if (not (eq (aref fmt i) ?%))
          (setq i (1+ i))
        (when (> i start) (setq plan (cons (substring fmt start i) plan)))
        (let ((percent i) (pad ?z) (up nil) (chg nil) (width -1) (mod 0) (c nil) (colons 0) (bad nil))
          (setq i (1+ i))
          (let ((go t))
            (while (and go (< i n))
              (let ((ch (aref fmt i)))
                (cond ((eq ch ?_) (setq pad ?_ i (1+ i)))
                      ((eq ch ?-) (setq pad ?- i (1+ i)))
                      ((eq ch ?+) (setq pad ?+ i (1+ i)))
                      ((eq ch ?0) (setq pad ?0 i (1+ i)))
                      ((eq ch ?^) (setq up t i (1+ i)))
                      ((eq ch ?#) (setq chg t i (1+ i)))
                      (t (setq go nil))))))
          (when (and (< i n) (>= (aref fmt i) ?0) (<= (aref fmt i) ?9))
            (setq width 0)
            (while (and (< i n) (>= (aref fmt i) ?0) (<= (aref fmt i) ?9))
              (setq width (+ (* width 10) (- (aref fmt i) ?0)) i (1+ i))))
          (when (and (< i n) (memq (aref fmt i) '(?E ?O)))
            (setq mod (aref fmt i) i (1+ i)))
          (cond
           ((>= i n)                    ; '%' at end: GNU outputs text up to previous char
            (setq plan (cons (vector ?\0 pad width up chg 0 mod (substring fmt percent n) nil) plan)
                  start n))
           (t
            (setq c (aref fmt i))
            (cond
             ((eq c ?:)
              (let ((j i))
                (while (and (< j n) (eq (aref fmt j) ?:)) (setq j (1+ j)))
                (if (and (< j n) (eq (aref fmt j) ?z))
                    (setq colons (- j i) i j c ?z)
                  ;; bad: GNU copies "%...:" and resumes after the first colon.
                  (setq bad t))))
             ((and (eq c ?%) (/= (1- i) percent))
              ;; 'E%' etc.: output text before the '%' and re-scan from it
              (setq plan (cons (vector ?\0 pad width up chg 0 mod (substring fmt percent i) nil) plan)
                    start i)
              (setq c nil)))
            (cond
             (bad (setq plan (cons (vector ?\0 pad width up chg 0 mod (substring fmt percent (1+ i)) nil) plan)
                        i (1+ i) start i))
             (c (setq i (1+ i))
                (setq plan (cons (vector c pad width up chg colons mod (substring fmt percent i)
                                       (nelisp--tf-simple-code c pad width up chg colons mod))
                                 plan)
                      start i))))))))
    (when (> n start) (setq plan (cons (substring fmt start n) plan)))
    (nreverse plan)))

(defun nelisp--tf-iso (year yday wday)
  "Return (ISOYEAR-ADJUST . DAYS) as GNU's %V/%g/%G computation."
  (let* ((leap (lambda (y) (and (= (% y 4) 0) (or (/= (% y 100) 0) (= (% y 400) 0)))))
         (tm-year (- year 1900))
         (yr (+ tm-year (if (< tm-year 0) (% 1900 400) (- (% 1900 400) 400))))
         (adj 0)
         (days (nelisp--tf-iso-days yday wday)))
    (if (< days 0)
        (setq adj -1
              days (nelisp--tf-iso-days (+ yday 365 (if (funcall leap (1- yr)) 1 0)) wday))
      (let ((d (nelisp--tf-iso-days (- yday (+ 365 (if (funcall leap yr) 1 0))) wday)))
        (when (>= d 0) (setq adj 1 days d))))
    (cons adj days)))

(defun nelisp--tf-iso-days (yday wday)
  (- (+ yday 4) (% (+ (- yday wday) 4 378) 7) 1))

(defun nelisp--tf-run (plan lt secs nsec zone-info up0 yr-spec width0)
  "Execute PLAN against local time LT.  Returns a string."
  (let ((out nil) (width width0) (code nil) (s nil)
        (fast (and (< width0 0) (not up0) (eq yr-spec ?z))))
    (while plan
      (let ((e (car plan)))
        (setq plan (cdr plan))
        (cond
         ((stringp e) (setq out (cons e out) width -1))
         ((and fast (setq code (aref e 8))
               (setq s (cond ((eq code 4) (aref nelisp--tf-d2 (nth 4 lt)))
                             ((eq code 5) (aref nelisp--tf-d2 (nth 5 lt)))
                             ((eq code 6) (aref nelisp--tf-d2 (nth 6 lt)))
                             ((eq code 2) (aref nelisp--tf-d2 (nth 1 lt)))
                             ((eq code 3) (aref nelisp--tf-d2 (nth 2 lt)))
                             ((eq code 7) (aref nelisp--tf-wday-abbr (nelisp--tf-md (+ (nth 3 lt) 4) 7)))
                             ((eq code 8) (aref nelisp--tf-mon-abbr (1- (nth 1 lt))))
                             ((eq code 9) (aref nelisp--tf-sp2 (nth 2 lt)))
                             ((eq code 10) (aref nelisp--tf-d3 (1+ (nelisp--tf-yday (nth 0 lt) (nth 1 lt) (nth 2 lt)))))
                             ((eq code 11) (concat (aref nelisp--tf-d2 (nth 4 lt)) ":" (aref nelisp--tf-d2 (nth 5 lt)) ":"
                                                   (aref nelisp--tf-d2 (nth 6 lt))))
                             ((eq code 14) (if (>= (nth 4 lt) 12) "PM" "AM"))
                             ((eq code 15) (nth 2 zone-info))
                             ((eq code 17) (aref nelisp--tf-wday-full (nelisp--tf-md (+ (nth 3 lt) 4) 7)))
                             ((eq code 18) (aref nelisp--tf-mon-full (1- (nth 1 lt))))
                             ((eq code 19) (number-to-string secs))
                             ((eq code 20) (aref nelisp--tf-d2 (let ((h (nth 4 lt))) (cond ((> h 12) (- h 12)) ((= h 0) 12) (t h)))))
                             ((eq code 21) (concat (aref nelisp--tf-d2 (nth 4 lt)) ":" (aref nelisp--tf-d2 (nth 5 lt))))
                             ((eq code 24) (aref nelisp--tf-sp2 (nth 4 lt)))
                             ((eq code 16)
                              (let ((off (car zone-info)))
                                (cond ((= off 0) (if (let ((ab (nth 2 zone-info))) (and (> (length ab) 0) (eq (aref ab 0) ?-))) nil "+0000"))
                                      ((and (= (% off 60) 0) (< off 360000) (> off -360000))
                                       (let ((a (if (< off 0) (- off) off)))
                                         (concat (if (< off 0) "-" "+") (aref nelisp--tf-d2 (/ a 3600))
                                                 (aref nelisp--tf-d2 (% (/ a 60) 60))))))))
                             ((and (>= (nth 0 lt) 1000) (<= (nth 0 lt) 9999))
                              (let ((ys (or (aref nelisp--tf-ycache (- (nth 0 lt) 1000))
                                            (aset nelisp--tf-ycache (- (nth 0 lt) 1000) (number-to-string (nth 0 lt))))))
                                (cond ((eq code 1) ys)
                                      ((eq code 12) (concat ys "-" (aref nelisp--tf-d2 (nth 1 lt)) "-" (aref nelisp--tf-d2 (nth 2 lt))))
                                      ((eq code 13) (aref nelisp--tf-d2 (% (nth 0 lt) 100)))
                                      ((eq code 22) (concat (aref nelisp--tf-wday-abbr (nelisp--tf-md (+ (nth 3 lt) 4) 7)) " "
                                                            (aref nelisp--tf-mon-abbr (1- (nth 1 lt))) " "
                                                            (aref nelisp--tf-sp2 (nth 2 lt)) " " (aref nelisp--tf-d2 (nth 4 lt)) ":"
                                                            (aref nelisp--tf-d2 (nth 5 lt)) ":" (aref nelisp--tf-d2 (nth 6 lt)) " " ys))
                                      ((eq code 23) (concat (aref nelisp--tf-d2 (nth 1 lt)) "/" (aref nelisp--tf-d2 (nth 2 lt)) "/"
                                                            (aref nelisp--tf-d2 (% (nth 0 lt) 100))))))))))
          (setq out (cons s out)))
         (t
          (setq out (cons (nelisp--tf-directive e (if (< (aref e 2) 0) width (aref e 2)) lt secs nsec zone-info up0 yr-spec) out)
                width -1)))))
    (apply #'concat (nreverse out))))

(defun nelisp--tf-sub (fmt pad width up lt secs nsec info yr-spec)
  (let ((plan (nelisp--tf-plan fmt)))
    (nelisp--tf-run plan lt secs nsec info up pad width)))

(defun nelisp--tf-plan (fmt)
  (or (gethash fmt nelisp--tf-plan-cache)
      (progn
        (when (> (hash-table-count nelisp--tf-plan-cache) 256) (clrhash nelisp--tf-plan-cache))
        (puthash fmt (nelisp--tf-parse-format fmt) nelisp--tf-plan-cache))))

(defun nelisp--tf-directive (e w lt secs nsec info up0 yr-spec)
  (let* ((c (aref e 0)) (pad (aref e 1)) (up (or up0 (aref e 3))) (chg (aref e 4))
         (colons (aref e 5)) (mod (aref e 6))
         (year (nth 0 lt)) (mon (nth 1 lt)) (mday (nth 2 lt)) (hour (nth 4 lt)) (min (nth 5 lt)) (sec (nth 6 lt))
         (wday (nelisp--tf-md (+ (nth 3 lt) 4) 7)) (yday (nelisp--tf-yday year mon mday))
         (hour12 (cond ((> hour 12) (- hour 12)) ((= hour 0) 12) (t hour)))
         (tm-year (- year 1900)) (low nil))
    (cond
     ((eq c ?%) (nelisp--tf-text "%" pad w up low))
     ((eq c ?n) (nelisp--tf-text "\n" pad w up low))
     ((eq c ?t) (nelisp--tf-text "\t" pad w up low))
     ((eq c ?a) (if (/= mod 0) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (aref nelisp--tf-wday-abbr wday) pad w (or up chg) nil)))
     ((eq c ?A) (if (/= mod 0) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (aref nelisp--tf-wday-full wday) pad w (or up chg) nil)))
     ((or (eq c ?b) (eq c ?h)) (if (eq mod ?E) (nelisp--tf-bad e pad w up)
                                 (nelisp--tf-text (aref nelisp--tf-mon-abbr (1- mon)) pad w (or up chg) nil)))
     ((eq c ?B) (if (eq mod ?E) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (aref nelisp--tf-mon-full (1- mon)) pad w (or up chg) nil)))
     ((or (eq c ?p) (eq c ?P))
      (let ((s (if (>= hour 12) "PM" "AM")))
        (nelisp--tf-text s pad w (and up (not chg) (not (eq c ?P))) (or (eq c ?P) chg))))
     ((eq c ?Z) (nelisp--tf-text (nth 2 info) pad w (and up (not chg)) chg))
     ((eq c ?z)
      (if (> colons 3) (nelisp--tf-bad e pad w up)
        (let* ((diff (car info)) (neg (< diff 0))
               (ad (nelisp--tf-abs diff)) (hd (/ ad 3600)) (md (% (/ ad 60) 60)) (sd (% ad 60)))
          (when (= diff 0)
            (setq neg (and (> (length (nth 2 info)) 0) (eq (aref (nth 2 info) 0) ?-))))
          (cond
           ((= colons 0) (nelisp--tf-num (+ (* hd 100) md) 5 pad w neg t))
           ((= colons 1) (nelisp--tf-num (+ (* hd 100) md) 6 pad w neg t 4))
           ((= colons 2) (nelisp--tf-num (+ (* hd 10000) (* md 100) sd) 9 pad w neg t 20))
           (t (cond ((/= sd 0) (nelisp--tf-num (+ (* hd 10000) (* md 100) sd) 9 pad w neg t 20))
                    ((/= md 0) (nelisp--tf-num (+ (* hd 100) md) 6 pad w neg t 4))
                    (t (nelisp--tf-num hd 3 pad w neg t))))))))
     ((eq c ?Y) (if (eq mod ?E) (nelisp--tf-num-year year 4 pad w yr-spec t)
                  (nelisp--tf-num-year year 4 pad w yr-spec nil)))
     ((eq c ?C)
      (let* ((negy (< tm-year -1900)) (z (and (not negy) (< tm-year 0)))
             (cent (+ (/ (- tm-year (if z 99 0)) 100) 19)))
        (nelisp--tf-yearish cent 2 negy pad w yr-spec)))
     ((eq c ?y)
      (let ((yy (% tm-year 100)))
        (when (< yy 0) (setq yy (if (< tm-year -1900) (- yy) (+ yy 100))))
        (nelisp--tf-yearish yy 2 nil pad w yr-spec)))
     ((eq c ?m) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num mon 2 pad w nil nil)))
     ((eq c ?d) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num mday 2 pad w nil nil)))
     ((eq c ?e) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num mday 2 (if (eq pad ?z) ?_ pad) w nil nil)))
     ((eq c ?H) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num hour 2 pad w nil nil)))
     ((eq c ?I) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num hour12 2 pad w nil nil)))
     ((eq c ?k) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num hour 2 (if (eq pad ?z) ?_ pad) w nil nil)))
     ((eq c ?l) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num hour12 2 (if (eq pad ?z) ?_ pad) w nil nil)))
     ((eq c ?M) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num min 2 pad w nil nil)))
     ((eq c ?S) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num sec 2 pad w nil nil)))
     ((eq c ?j) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num (1+ yday) 3 pad w nil nil)))
     ((eq c ?q) (nelisp--tf-num (1+ (ash (* (1- mon) 11) -5)) 1 pad w nil nil))
     ((eq c ?u) (nelisp--tf-num (1+ (% (+ wday 6) 7)) 1 pad w nil nil))
     ((eq c ?w) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num wday 1 pad w nil nil)))
     ((eq c ?U) (if (eq mod ?E) (nelisp--tf-bad e pad w up) (nelisp--tf-num (/ (+ (- yday wday) 7) 7) 2 pad w nil nil)))
     ((eq c ?W) (if (eq mod ?E) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-num (/ (+ (- yday (% (+ wday 6) 7)) 7) 7) 2 pad w nil nil)))
     ((or (eq c ?V) (eq c ?g) (eq c ?G))
      (if (eq mod ?E) (nelisp--tf-bad e pad w up)
        (let* ((r (nelisp--tf-iso year yday wday)) (adj (car r)) (days (cdr r)))
          (cond
           ((eq c ?g)
            (let ((yy (% (+ (% tm-year 100) adj) 100)))
              (nelisp--tf-yearish (cond ((>= yy 0) yy) ((< tm-year (- -1900 adj)) (- yy)) (t (+ yy 100)))
                                  2 nil pad w yr-spec)))
           ((eq c ?G) (nelisp--tf-yearish (+ tm-year 1900 adj) 4 (< tm-year (- -1900 adj)) pad w yr-spec))
           (t (nelisp--tf-num (1+ (/ days 7)) 2 pad w nil nil))))))
     ((eq c ?N)
      (if (eq mod ?E) (nelisp--tf-bad e pad w up)
        (let* ((n nsec) (width (if (<= w 0) 9 w)) (nd 9))
          (while (or (< width nd) (and (< 1 nd) (= (% n 10) 0)))
            (setq nd (1- nd) n (/ n 10)))
          (let* ((digits (nelisp--tf-pad-left (number-to-string n) nd))
                 (pd (if (eq pad ?z) ?0 pad))
                 (extra (if (or (eq pd ?-) (<= (- width nd) 0)) 0 (- width nd))))
            (concat digits (make-string extra (if (memq pd '(?0 ?+)) ?0 ?\s)))))))
     ((eq c ?s)
      (let ((v secs))
        (nelisp--tf-num v 1 pad w (< v 0) nil)))
     ((eq c ?D) (if (/= mod 0) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (nelisp--tf-sub "%m/%d/%y" pad -1 up lt secs nsec info pad) pad w nil nil)))
     ((eq c ?x) (if (eq mod ?O) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (nelisp--tf-sub "%m/%d/%y" pad -1 up lt secs nsec info pad) pad w nil nil)))
     ((eq c ?X) (if (eq mod ?O) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (nelisp--tf-sub "%H:%M:%S" pad -1 up lt secs nsec info pad) pad w nil nil)))
     ((eq c ?T) (nelisp--tf-text (nelisp--tf-sub "%H:%M:%S" pad -1 up lt secs nsec info pad) pad w nil nil))
     ((eq c ?R) (nelisp--tf-text (nelisp--tf-sub "%H:%M" pad -1 up lt secs nsec info pad) pad w nil nil))
     ((eq c ?r) (nelisp--tf-text (nelisp--tf-sub "%I:%M:%S %p" pad -1 up lt secs nsec info pad) pad w nil nil))
     ((eq c ?c) (if (eq mod ?O) (nelisp--tf-bad e pad w up)
                  (nelisp--tf-text (nelisp--tf-sub "%a %b %e %H:%M:%S %Y" pad -1 up lt secs nsec info pad) pad w nil nil)))
     ((eq c ?F)
      (if (/= mod 0) (nelisp--tf-bad e pad w up)
        (let* ((sign-pad (and (eq pad ?z) (< w 0)))
               (subw (if sign-pad 4 (max 0 (- w 6))))
               (ypad (if sign-pad ?+ pad)))
          (nelisp--tf-text (nelisp--tf-sub "%Y-%m-%d" ypad subw up lt secs nsec info ypad) pad w nil nil))))
     (t (nelisp--tf-bad e pad w up)))))

(defun nelisp--tf-pad-left (s n)
  (if (< (length s) n) (concat (make-string (- n (length s)) ?0) s) s))

(defun nelisp--tf-bad (e pad w up)
  (nelisp--tf-text (aref e 7) pad w up nil))

(defun nelisp--tf-yearish (v digits neg pad w yr-spec)
  (let ((pad (if (eq pad ?z) yr-spec pad)))
    (nelisp--tf-num v digits pad w neg
                    (and (eq pad ?+) (or (> (nelisp--tf-abs v) (if (= digits 2) 99 9999)) (< digits w))))))

(defun nelisp--tf-num-year (year digits pad w yr-spec era)
  (nelisp--tf-yearish year digits (< year 0) pad w yr-spec))

(unless (fboundp 'format-time-string)
  (defun format-time-string (format-string &optional time zone)
    "Use FORMAT-STRING to format the time value TIME in ZONE."
    (unless (stringp format-string)
      (signal 'wrong-type-argument (list 'stringp format-string)))
    (let* ((ts (if (integerp time) nil (nelisp--tf-timespec time)))
           (secs (if ts (car ts) time)) (nsec (if ts (cdr ts) 0))
           (z (nelisp--tf-zone zone))
           (lt (nelisp--tf-localtime secs z)))
      (nelisp--tf-run (nelisp--tf-plan format-string) lt secs nsec (nth 7 lt) nil ?z -1))))

(unless (fboundp 'current-time-string)
  (defun current-time-string (&optional specified-time zone)
    "Return the current local time, as a human-readable string."
    (let* ((secs (nelisp--tf-seconds specified-time))
           (lt (nelisp--tf-localtime secs (nelisp--tf-zone zone))))
      (concat (aref nelisp--tf-wday-abbr (nelisp--tf-md (+ (nth 3 lt) 4) 7)) " " (aref nelisp--tf-mon-abbr (1- (nth 1 lt)))
              (if (< (nth 2 lt) 10) "  " " ") (number-to-string (nth 2 lt)) " "
              (aref nelisp--tf-d2 (nth 4 lt)) ":" (aref nelisp--tf-d2 (nth 5 lt)) ":"
              (aref nelisp--tf-d2 (nth 6 lt)) " " (number-to-string (nth 0 lt))))))

(unless (fboundp 'current-time-zone)
  (defun current-time-zone (&optional specified-time zone)
    "Return the offset and name for the local time zone as (OFFSET NAME)."
    (let* ((secs (nelisp--tf-seconds specified-time))
           (z (nelisp--tf-zone zone))
           (lt (nelisp--tf-localtime secs z))
           (info (nth 7 lt)) (off (car info)) (name (nth 2 info)))
      (when (= (length name) 0)
        (let* ((a (abs off)) (h (/ a 3600)) (r (% a 3600)))
          (setq name (concat (if (< off 0) "-" "+") (nelisp--tf-pad2 h)
                             (if (/= r 0) (concat (nelisp--tf-pad2 (/ r 60))
                                                  (if (/= (% r 60) 0) (nelisp--tf-pad2 (% r 60)) ""))
                               "")))))
      (list off name))))

(provide 'nelisp-timefns)
;;; nelisp-timefns.el ends here
