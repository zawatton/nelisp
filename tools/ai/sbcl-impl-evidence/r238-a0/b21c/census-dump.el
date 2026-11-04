;; Doc 238 A0.2: census dump helper (inserted into a copy of the ON driver).
(defun r238-census-dump (label)
  (let* ((rows '("nl_word_data" "nl_word_length" "nl_str_target" "nl_bi_strptr"
                 "nl_sexp_clone_into[producer]" "nl_word_clone[producer]"
                 "m5_fx_word_ptr[producer]" "(spare)"))
         (shapes '("legacy-view(000)" "direct-cons(010)" "direct-string(100)"
                   "vectorlike(110)" "immediate(odd)" "-" "-" "-"))
         (counts (nelisp--repr-census t)) (r 0))
    (while counts
      (let ((s 0))
        (while (< s 8)
          (when (> (car counts) 0)
            (princ (format "CENSUS %s\t%s\t%s\t%d\n" label (nth r rows) (nth s shapes) (car counts))))
          (setq counts (cdr counts) s (1+ s))))
      (setq r (1+ r)))))
