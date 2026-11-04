;;; census-probe.el --- print the Doc 238 A0 representation census  -*- lexical-binding: t; -*-
;; Run on an image built with NELISP_REPR_CENSUS=1, after (or around) a workload.
;; (nelisp--repr-census RESET) returns 64 counters, row*8+shape; non-nil RESET zeroes them.
(let* ((rows '("nl_word_data" "nl_word_length" "nl_str_target" "nl_bi_strptr"
               "nl_sexp_clone_into[producer]" "nl_word_clone[producer]"
               "m5_fx_word_ptr[producer]" "(spare)"))
       (shapes '("legacy-view(000)" "direct-cons(010)" "direct-string(100)"
                 "vectorlike(110)" "immediate(odd)" "-" "-" "-"))
       (counts (nelisp--repr-census nil))
       (r 0))
  (princ "accessor/producer\tshape\tcount\n")
  (while counts
    (let ((s 0))
      (while (< s 8)
        (when (and (> (car counts) 0) (< s 5))
          (princ (format "%s\t%s\t%d\n" (nth r rows) (nth s shapes) (car counts))))
        (setq counts (cdr counts) s (1+ s))))
    (setq r (1+ r))))
