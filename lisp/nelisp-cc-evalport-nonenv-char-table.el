;;; nelisp-cc-evalport-nonenv-char-table.el --- lowered -*- lexical-binding: t; -*-
;;; Commentary:
;; CharTable storage kernel.  The entries array holds sorted, disjoint,
;; inclusive run-length intervals, 48 bytes each:
;;   +0  start  (i64)
;;   +8  value  (32-byte Sexp)
;;   +40 end    (i64)
;; Lookup is a binary search; `nl_ct_set_range' trims/splits/replaces the
;; overlapped intervals in one pass and coalesces `eq' neighbours, so a
;; set over any range (even 0..#x3FFFFF) is O(log n) plus one array shift.
;; Slot value nil (or no interval) falls through to the table default and
;; then to the parent chain, exactly like GNU `char_table_ref'.
;; Doc 216 S4: the 80-byte body begins with kind/size, followed by subtype
;; WORD@8, default WORD@16, entries/cap/len@24/32/40, parent body@48 and
;; extras/cap/len@56/64/72.  Entry and extra strides remain 48 and 32.
;;; Code:
(defconst nelisp-cc-evalport-nonenv-char-table--source
  (quote
   (seq
    (defun nl_ct_copy_u64s (src dst i n)
      (if (= i n) 1
        (let* ((word_off (* i 8))
               (val (heap-read-u64 (+ src word_off) 0)))
          (seq (ptr-write-u64 (+ dst word_off) 0 val)
               (nl_ct_copy_u64s src dst (+ i 1) n)))))
    (defun nl_ct_copy_entries (src_ptr dst_ptr i n)
      (if (= i n) 1
        (let* ((entry_off (* i 48)))
          (seq (nl_ct_copy_u64s (+ src_ptr entry_off) (+ dst_ptr entry_off) 0 6)
               (nl_ct_copy_entries src_ptr dst_ptr (+ i 1) n)))))
    (defun nl_ct_grow_entries (box old_ptr old_cap old_len)
      (let* ((new_cap (if (< (* old_cap 2) 4) 4 (* old_cap 2)))
             (new_ptr (nelisp_alloc_bytes (* new_cap 48) 8)))
        (seq (nl_ct_copy_entries old_ptr new_ptr 0 old_len)
             (if (> old_cap 0) (nelisp_dealloc_bytes old_ptr (* old_cap 48) 8) 1)
             (seq (ptr-write-u64 (+ box 24) 0 new_ptr)
                  (ptr-write-u64 (+ box 32) 0 new_cap))
             new_ptr)))
    ;; Ensure room for NEED entries; returns the (possibly new) array pointer.
    (defun nl_ct_reserve (box need)
      (let* ((eptr (heap-read-u64 (+ box 24) 0))
             (ecap (heap-read-u64 (+ box 32) 0))
             (elen (heap-read-u64 (+ box 40) 0)))
        (if (<= need ecap) eptr (nl_ct_grow_entries box eptr ecap elen))))
    (defun nl_ct_start (eptr i) (heap-read-u64 (+ eptr (* i 48)) 0))
    (defun nl_ct_end (eptr i) (heap-read-u64 (+ eptr (+ (* i 48) 40)) 0))
    (defun nl_ct_val (eptr i) (+ eptr (+ (* i 48) 8)))
    ;; Move COUNT entries from index FROM to index TO (overlap safe).
    (defun nl_ct_move_entries (eptr from to count)
      (let* ((src (+ eptr (* from 48)))
             (dst (+ eptr (* to 48)))
             (nw (* count 6))
             (k 0))
        (if (< dst src)
            (while (< k nw)
              (ptr-write-u64 (+ dst (* k 8)) 0 (heap-read-u64 (+ src (* k 8)) 0))
              (setq k (+ k 1)))
          (seq (setq k nw)
               (while (> k 0)
                 (setq k (- k 1))
                 (ptr-write-u64 (+ dst (* k 8)) 0 (heap-read-u64 (+ src (* k 8)) 0)))))
        1))
    ;; First index whose end >= X (N if none).
    (defun nl_ct_lower_end (eptr n x)
      (let* ((lo 0) (hi n) (mid 0))
        (while (< lo hi)
          (setq mid (/ (+ lo hi) 2))
          (if (>= (nl_ct_end eptr mid) x)
              (setq hi mid)
            (setq lo (+ mid 1))))
        lo))
    ;; First index whose start > X (N if none).
    (defun nl_ct_upper_start (eptr n x)
      (let* ((lo 0) (hi n) (mid 0))
        (while (< lo hi)
          (setq mid (/ (+ lo hi) 2))
          (if (> (nl_ct_start eptr mid) x)
              (setq hi mid)
            (setq lo (+ mid 1))))
        lo))
    ;; Effective lookup: interval value, else default, else parent chain.
    (defun nl_ct_get_box (box idx out)
      (nl_abi_word_to_rc (nl_ct_get_box_w box idx 0) out))
    ;; `char-table-range' on a (FROM . TO) cons: slot value, else default,
    ;; never the parent chain (verified against host Emacs).
    ;; Doc 215 word ABI: result WORD, 0 after the signal is stashed.
    ;; Doc 225 S3: the value at IDX through GNU's slot vector
    ;; (`nl_ct2_contents_ref'), nil answering the default
    ;; (chartab.c:252-368 char_table_ref_and_range at FROM).
    (defun nl_char_table_get_local_raw_w (arg idx _pad)
      (let* ((box (nl_word_payload arg)) (value (nl_ct2_contents_ref box idx)))
        (if (/= (nl_val_tag value) 0) value (heap-read-u64 box 16))))
    (defun nl_char_table_get_local_raw (arg idx out)
      (nl_abi_word_to_rc (nl_char_table_get_local_raw_w arg idx 0) out))
    (defun nl_char_table_get_raw (arg idx out)
      (let* ((tag (nl_val_tag arg)))
        (if (= tag 9)
            (nl_ct_get_box (nl_word_payload arg) idx out)
          1)))
    ;; Doc 215 builtin word ABI: a storage slot as a value WORD (Int -> immediate, nil 3, t 7).

    ;; Effective lookup returning the value WORD (the table's own child, no clone).
    ;; Doc 225 S3: GNU CHAR_TABLE_REF (chartab.c:225-249, lisp.h:2133-2155):
    ;; the slot vector's value, then the default, then the parent chain.
    (defun nl_ct_get_box_w (box idx _pad)
      (let* ((value (nl_ct2_contents_ref box idx)))
        (if (/= (nl_val_tag value) 0) value
          (let* ((parent (heap-read-u64 box 48)) (default (heap-read-u64 box 16)))
            (if (and (= (nl_val_tag default) 0) (/= parent 0))
                (nl_ct_get_box_w parent idx 0) default)))))
    ;; Doc 215 builtin word ABI: the value WORD, or 0 after a stashed error for a non-char-table.
    (defun nl_char_table_get_raw_w (arg idx _pad)
      (if (= (nl_val_tag arg) 9)
          (nl_ct_get_box_w (nl_word_payload arg) idx 0)
        (extern-call nl_abi_fail 1 0)))
    ;; Merge entry P with its right / left neighbour when adjacent and `eq'.
    (defun nl_ct_coalesce (box p)
      (let* ((eptr (heap-read-u64 (+ box 24) 0))
             (n (heap-read-u64 (+ box 40) 0)))
        (if (< (+ p 1) n)
            (if (= (nl_ct_start eptr (+ p 1)) (+ (nl_ct_end eptr p) 1))
                (if (= (wf_bytecode_w_eqw (heap-read-u64 (nl_ct_val eptr p) 0) (heap-read-u64 (nl_ct_val eptr (+ p 1)) 0)) 1)
                    (seq (ptr-write-u64 (+ eptr (+ (* p 48) 40)) 0
                                        (nl_ct_end eptr (+ p 1)))
                         (nl_ct_move_entries eptr (+ p 2) (+ p 1) (- n (+ p 2)))
                         (setq n (- n 1))
                         (ptr-write-u64 (+ box 40) 0 n))
                  0)
              0)
          0)
        (if (> p 0)
            (if (= (+ (nl_ct_end eptr (- p 1)) 1) (nl_ct_start eptr p))
                (if (= (wf_bytecode_w_eqw (heap-read-u64 (nl_ct_val eptr (- p 1)) 0) (heap-read-u64 (nl_ct_val eptr p) 0)) 1)
                    (seq (ptr-write-u64 (+ eptr (+ (* (- p 1) 48) 40)) 0
                                        (nl_ct_end eptr p))
                         (nl_ct_move_entries eptr (+ p 1) p (- n (+ p 1)))
                         (setq n (- n 1))
                         (ptr-write-u64 (+ box 40) 0 n))
                  0)
              0)
          0)
        1))
    (defun nl_ct_write_entry (eptr i from to val)
      (let* ((ent (+ eptr (* i 48))))
        (seq (ptr-write-u64 ent 0 from)
             (nl_ct_store_word (+ ent 8) val)
             (ptr-write-u64 (+ ent 40) 0 to)
             1)))
    ;; The interval at LO..HI overlaps [FROM,TO].  Trim/split the border
    ;; intervals, replace the fully covered ones with one new interval.
    (defun nl_ct_set_range2 (box eptr n lo hi from to val)
      (let* ((keepl (if (< lo hi) (if (< (nl_ct_start eptr lo) from) 1 0) 0))
             (keepr (if (< lo hi) (if (> (nl_ct_end eptr (- hi 1)) to) 1 0) 0))
             (eptr2 eptr) (pl lo) (pr hi) (r 0) (n2 n))
        (if (= (+ keepl keepr) 2)
            (if (= (- hi lo) 1)
                ;; One interval strictly contains the range: split in three.
                (seq
                 (setq eptr2 (nl_ct_reserve box (+ n 2)))
                 (nl_ct_move_entries eptr2 hi (+ hi 2) (- n hi))
                 (ptr-write-u64 (+ eptr2 (* (+ hi 1) 48)) 0 (+ to 1))
                 (nl_ct_store_word (nl_ct_val eptr2 (+ hi 1))
                                   (heap-read-u64 (nl_ct_val eptr2 lo) 0))
                 (ptr-write-u64 (+ eptr2 (+ (* (+ hi 1) 48) 40)) 0
                                (nl_ct_end eptr2 lo))
                 (nl_ct_write_entry eptr2 hi from to val)
                 (ptr-write-u64 (+ eptr2 (+ (* lo 48) 40)) 0 (- from 1))
                 (ptr-write-u64 (+ box 40) 0 (+ n 2))
                 1)
              0)
          0)
        (if (if (= (+ keepl keepr) 2) (= (- hi lo) 1) 0)
            1
          (seq
           (if (= keepl 1)
               (ptr-write-u64 (+ eptr (+ (* lo 48) 40)) 0 (- from 1))
             0)
           (if (= keepr 1)
               (ptr-write-u64 (+ eptr (* (- hi 1) 48)) 0 (+ to 1))
             0)
           (setq pl (+ lo keepl))
           (setq pr (- hi keepr))
           (setq r (- pr pl))
           (if (= r 0)
               (seq (setq eptr2 (nl_ct_reserve box (+ n 1)))
                    (nl_ct_move_entries eptr2 pl (+ pl 1) (- n pl))
                    (setq n2 (+ n 1)))
             (seq (nl_ct_move_entries eptr pr (+ pl 1) (- n pr))
                  (setq n2 (- n (- r 1)))))
           (nl_ct_write_entry eptr2 pl from to val)
           (ptr-write-u64 (+ box 40) 0 n2)
           (nl_ct_coalesce box pl)
           1))))
    ;; Set every char in [FROM,TO] (FROM <= TO, both valid chars) to VAL.
    (defun nl_ct_value_eq (slot value)
      (wf_bytecode_w_eqw (heap-read-u64 slot 0) value))
    (defun nl_ct_set_range (box from to val)
      (let* ((eptr (heap-read-u64 (+ box 24) 0))
             (n (heap-read-u64 (+ box 40) 0))
             (lo (nl_ct_lower_end eptr n from))
             (hi (nl_ct_upper_start eptr n to))
             (same (if (= (- hi lo) 1)
                       (if (<= (nl_ct_start eptr lo) from)
                           (if (>= (nl_ct_end eptr lo) to)
                               (nl_ct_value_eq (nl_ct_val eptr lo) val)
                             0)
                         0)
                     0)))
        (if (= same 1)
            1
          (nl_ct_set_range2 box eptr n lo hi from to val))))
    (defun nl_char_table_set_raw (arg idx val out) 1)
    ;; Doc 215 builtin word ABI: set and answer VAL as a WORD (an immediate stays as it is).
    ;; Doc 225 S3: char-tables store GNU's slot vector, which only the
    ;; prelude's char_table_set writes; the interval rows are no longer
    ;; storage, so the raw setters refuse instead of writing them.
    (defun nl_char_table_set_raw_w (arg idx val _pad)
      (extern-call nl_abi_fail 1 0))
    (defun nl_char_table_set_range_raw (arg from to val out) 1)
    ;; Doc 215 word ABI: the result WORD, or 0 after a stashed generic error when
    ;; the legacy form answered nonzero.  NOTE: the legacy form answers rc 1 for a plain
    ;; miss WITHOUT a signal; callers that treat that as a miss must stay on `nl_char_table_set_range_raw'.
    (defun nl_char_table_set_range_raw_w (arg from to val _pad)
      (extern-call nl_abi_fail 1 0))))
  "CharTable get kernel over GNU's slot vector (Doc 225 S3).")
(provide (quote nelisp-cc-evalport-nonenv-char-table))
;;; nelisp-cc-evalport-nonenv-char-table.el ends here
