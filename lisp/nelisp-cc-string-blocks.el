;;; nelisp-cc-string-blocks.el --- GNU string storage (Doc 227) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; GNU Emacs 31.1 alloc.c:1318-2055. Headers have the exact Lisp_String
;; layout: size, size_byte, intervals, data. A WORD is header+4.
;; OS-backed header blocks are stable under the legacy arena collector.
;; The 32 KiB alignment/type trailer agrees with gc-port's ablock boundary.
;; Small sblocks retain GNU's 8192-byte size and oldest-first compaction.
;; These are allocation/tagged-object operations (native policy clause 1),
;; not additional Lisp-visible reader builtins.

;;; Code:

(defconst nelisp-cc-string-blocks--source
  '(seq
    ;; State: headers, free, oldest, current, large, live-count, live-bytes,
    ;; pinned, legacy-fixups, collections, moved-bytes, allocated-bytes.
    (defun nl_string_alloc_mapping (n)
      (let* ((p (nl_os_alloc_chunk n)))
        (if (= p 0) 0
          (if (= (nl_os_commit_range p 0 n) 0)
              (seq (nl_os_free_chunk p n) 0) p))))
    ;; GNU classifies a pointer's allocation before live_string_holding.
    ;; This base has no shared mem_node tree. Its String-only page index
    ;; performs that ownership query without walking unrelated sblocks.
    ;; Entry: live/deleted/empty, 4-KiB page, block base | private kind.
    (defun nl_string_page_probe (rows cap key)
      (let* ((hash (* (/ key 4096) 2654435761))
             (i (logand (logxor hash (sar hash 32)) (- cap 1)))
             (row (+ rows (* i 24))) (go 1))
        (while (= go 1)
          (if (= (heap-read-u64 row 0) 0) (setq go 0)
            (if (= (heap-read-u64 row 8) key) (setq go 0)
              (seq (setq i (logand (+ i 1) (- cap 1)))
                   (setq row (+ rows (* i 24))))))) row))
(defun nl_string_mapping_op (w end kind)
  ;; An allocated-header query (kind 1) first asks the last header it found
  ;; (st+136): string code reads the same few strings repeatedly. GNU's
  ;; live_string_p walks the block mem_find returned; the page index answers
  ;; here. A sweep clears the memo, the only time a header is freed. Zero is
  ;; the cleared memo, never a header, so it never hits.
  (if (if (= kind 1) (if (= w 0) 0 (= w (heap-read-u64 (data-addr nl_strings_state) 136))) 0) 1
  (if (< kind 3)
      (let*
	  ((st (data-addr nl_strings_state))
	   (lo (heap-read-u64 st 96)) (data-lo (heap-read-u64 st 112))
	   (hi (heap-read-u64 st 104)) (map (heap-read-u64 st 144)))
	(if (or (= lo 0) (and (/= data-lo 0) (< data-lo lo)))
	    (setq lo data-lo)
	  0)
	(if (> (heap-read-u64 st 120) hi)
	    (setq hi (heap-read-u64 st 120))
	  0)
	(if (or (= map 0) (< w lo) (>= w hi)) 0
	  (let*
	      ((row
		(nl_string_page_probe (+ map 24) (heap-read-u64 map 8)
				      (logand w -4096))))
	    (if (/= (heap-read-u64 row 0) 1) 0
	      (let*
		  ((packed (heap-read-u64 row 16))
		   (base (logand packed -8))
		   (block-kind (logand packed 7))
		   (end
		    (if (= block-kind 3) (+ base 32736)
		      (if (= block-kind 4) (+ base 8192)
			(if (= block-kind 6) (heap-read-u64 st 168)
			  (heap-read-u64 base 8))))))
		(if (and (>= w base) (< w end))
		    (if (= kind 1)
			(if
			    (and (or (= block-kind 3) (= block-kind 6)) (= (logand w 31) 0)
				 (/= (heap-read-u64 w 24) 0))
			    (seq (ptr-write-u64 st 136 w) 1)
			  0)
		      packed)
		  0))))))
    (let*
	((st (data-addr nl_strings_state))
	 (map (heap-read-u64 st 144))
	 (cap (if (= map 0) 0 (heap-read-u64 map 8)))
	 (used (if (= map 0) 0 (heap-read-u64 map 16)))
	 (page (logand w -4096))
	 (pages (/ (+ (- end page) 4095) 4096)) (ok 1))
      (if (or (= map 0) (> (* (+ used pages) 2) cap))
	  (let* ((live 0) (i 0) (nextcap (if (= cap 0) 64 cap)))
	    (while (< i cap)
	      (if (= (heap-read-u64 (+ map 24) (* i 24)) 1)
		  (setq live (+ live 1))
		0)
	      (setq i (+ i 1)))
	    (while (< nextcap (* (+ live pages) 2))
	      (setq nextcap (* nextcap 2)))
	    (let*
		((fresh
		  (nl_string_alloc_mapping (+ 24 (* nextcap 24)))))
	      (if (= fresh 0) (setq ok 0)
		(seq (ptr-write-u64 fresh 8 nextcap)
		     (ptr-write-u64 fresh 16 live) (setq i 0)
		     (while (< i cap)
		       (let* ((old (+ map (+ 24 (* i 24)))))
			 (if (= (heap-read-u64 old 0) 1)
			     (let*
				 ((row
				   (nl_string_page_probe (+ fresh 24)
							 nextcap
							 (heap-read-u64
							  old 8))))
			       (ptr-write-u64 row 8
					      (heap-read-u64 old 8))
			       (ptr-write-u64 row 16
					      (heap-read-u64 old 16))
			       (ptr-write-u64 row 0 1))
			   0))
		       (setq i (+ i 1)))
		     (if (= map 0) 0
		       (seq
			(ptr-write-u64 map 0 (heap-read-u64 st 152))
			(ptr-write-u64 st 152 map)))
		     (ptr-write-u64 st 144 fresh) (setq map fresh)
		     (setq cap nextcap)))))
	0)
      (if (= ok 0) 0
	(seq
	 (while (< page end)
	   (let* ((row (nl_string_page_probe (+ map 24) cap page)))
	     (if (= (heap-read-u64 row 0) 0)
		 (ptr-write-u64 map 16 (+ (heap-read-u64 map 16) 1))
	       0)
	     (ptr-write-u64 row 8 page)
	     (ptr-write-u64 row 16 (logior w kind))
	     (ptr-write-u64 row 0 1))
	   (setq page (+ page 4096)))
	 1))))))

    (defun nl_string_register_mapping (base end kind)
  (nl_string_mapping_op base end kind))

    (defun nl_string_unregister_mapping (base end)
      (let* ((map (heap-read-u64 (data-addr nl_strings_state) 144))
             (cap (if (= map 0) 0 (heap-read-u64 map 8)))
             (page (logand base -4096)))
        (if (= map 0) 0
          (seq
           (while (< page end)
             (let* ((row (nl_string_page_probe (+ map 24) cap page)))
               (if (and (= (heap-read-u64 row 0) 1)
                        (= (logand (heap-read-u64 row 16) -8) base))
                   (ptr-write-u64 row 0 2) 0))
             (setq page (+ page 4096))) 0))))
    (defun nl_string_mapping_holding (w) (nl_string_mapping_op w 0 0))

    (defun nl_string_reap_page_tables ()
      (let* ((st (data-addr nl_strings_state)) (map (heap-read-u64 st 152)))
        (ptr-write-u64 st 152 0)
        (while (/= map 0)
          (let* ((next (heap-read-u64 map 0)) (cap (heap-read-u64 map 8)))
            (nl_os_free_chunk map (+ 24 (* cap 24))) (setq map next))) 0))
    (defun nl_string_sdata_size (n)
      (logand (+ (if (< n 7) 16 (+ n 9)) 7) -8))
    (defun nl_string_nbytes (s)
      (let* ((n (heap-read-u64 s 8)))
        (if (< n 0) (logand (heap-read-u64 s 0) 9223372036854775807) n)))
    (defun nl_string_header_p (s) (nl_string_mapping_op s 0 1))
    ;; Pool-less strings of a restored cold image (pdumper.c:2084 dump_string):
    ;; the image's last block is a footer naming its header area, a run of
    ;; 32-byte headers.  Registering the area as mapping kind 6 makes every
    ;; header validate like a pool header; nothing ever sweeps, moves or marks
    ;; them (pdumper_marked_p covers dump strings in GNU; strings have no
    ;; children, so a skipped mark loses nothing).  1 when an area was attached.
    (defun nl_string_image_attach (ds slen)
      (let* ((st (data-addr nl_strings_state)) (f (+ ds (- slen 32))))
        (if (< slen 64) 0
          (if (/= (heap-read-u64 f 0) 32) 0
            (if (/= (heap-read-u64 f 8) 21754191157611598) 0
              (let* ((lo (+ ds (heap-read-u64 f 16))) (hi (+ ds (heap-read-u64 f 24))))
                (if (or (> lo hi) (> hi f) (/= (logand lo 31) 0)) 0
                  (if (= (nl_string_register_mapping lo hi 6) 0) 0
                    (seq (ptr-write-u64 st 160 lo) (ptr-write-u64 st 168 hi)
                         (if (or (= (heap-read-u64 st 96) 0) (< lo (heap-read-u64 st 96)))
                             (ptr-write-u64 st 96 lo) 0)
                         (if (> hi (heap-read-u64 st 104)) (ptr-write-u64 st 104 hi) 0)
                         1)))))))))

    (defun nl_string_word_p (w)
      (if (= (logand w 7) 4) (nl_string_header_p (- w 4)) 0))
    (defun nl_string_kind (s)
      (if (< (heap-read-u64 s 8) 0) 14 5))
    (defun nl_string_capacity (s)
      (if (< (heap-read-u64 s 8) 0) s (logior s 4611686018427387904)))
    ;; Physical reads of arbitrary raw byte buffers remain physical. Only
    ;; validated canonical WORDs select the legacy logical field mapping.
    (defun nl_string_field_header (p)
      (if (= (logand p 7) 4)
          (let* ((s (logand (- p 4) -32)))
            (if (= (nl_string_header_p s) 1) s 0)) 0))
    (defun nl_string_read64 (p off)
      ;; OFF=-1 is the old tag-byte read. Both widths return an i64 and
      ;; share ownership validation; no second private getter is needed.
      ;; Every generic slot read of the runtime passes through here, so the
      ;; common case (a legacy slot or buffer address, low bits not 100) is
      ;; decided by one test, before any string state is loaded.
      (if (/= (logand p 7) 4)
          (if (= off -1) (heap-read-u8 p 0) (heap-read-u64 p off))
        (let* ((st (data-addr nl_strings_state))
               (header (if (and (>= p (+ (heap-read-u64 st 96) 4))
                                (< p (+ (heap-read-u64 st 104) 4)))
                           (nl_string_field_header p) 0)))
          (if (= off -1)
              (if (and (/= header 0) (= p (+ header 4)))
                  (nl_string_kind header) (heap-read-u8 p 0))
            (if (= header 0) (heap-read-u64 p off)
              (let* ((s header))
                (setq off (+ off (- p (+ s 4))))
                (if (= off 0) (nl_string_kind s)
                  (if (= off 8) (nl_string_capacity s)
                    (if (= off 16) (heap-read-u64 s 24)
                      (if (= off 24) (nl_string_nbytes s) (heap-read-u64 p off)))))))))))
    (defun nl_string_write64 (p off value)
      (if (= (nl_string_word_p p) 0) (ptr-write-u64 p off value)
        (let* ((s (- p 4)))
          (if (= off 8)
              (seq (ptr-write-u64 s 8
                     (if (= (logand value 4611686018427387904) 0)
                         (if (= (logand (heap-read-u64 s 0) 9223372036854775807)
                                (nl_string_nbytes s)) -1 (nl_string_nbytes s))
                       (nl_string_nbytes s))) 1)
            (if (= off 0) (nl_string_write8 p 0 value)
              (if (= off 16) (seq (ptr-write-u64 s 24 value) 1)
                (if (= off 24) (seq (ptr-write-u64 s 8 value) 1)
                  (ptr-write-u64 p off value))))))))
    (defun nl_string_write8 (p off value)
      (if (and (= off 0) (= (nl_string_word_p p) 1))
          (let* ((s (- p 4)))
            (seq (ptr-write-u64 s 8 (if (= value 14) -1 (nl_string_nbytes s))) 1))
        (ptr-write-u8 p off value)))
    (defun nl_string_copy_bytes (src dst n)
      (let* ((i 0))
        (while (<= (+ i 8) n)
          (ptr-write-u64 dst i (heap-read-u64 src i)) (setq i (+ i 8)))
        (while (< i n)
          (ptr-write-u8 dst i (heap-read-u8 src i)) (setq i (+ i 1))) 1))
    (defun nl_string_char_count (data n)
      (let* ((i 0) (chars 0))
        (while (< i n)
          (if (/= (logand (heap-read-u8 data i) 192) 128)
              (setq chars (+ chars 1)) 0)
          (setq i (+ i 1))) chars))
    (defun nl_string_allocate_header ()
      (let* ((st (data-addr nl_strings_state)) (s (heap-read-u64 st 8)))
        (if (= s 0)
            (let* ((origin (nl_string_alloc_mapping 65536))
                   (b (if (= origin 0) 0 (logand (+ origin 32767) -32768))) (i 1022))
              (if (= b 0) 0
                (if (= (nl_string_register_mapping b (+ b 32736) 3) 0)
                    (seq (nl_os_free_chunk origin 65536) (setq s 0))
                (seq
                 (ptr-write-u64 b 32736 origin)
                 (ptr-write-u64 b 32744 (heap-read-u64 st 0))
                 (ptr-write-u64 b 32752 3)
                 (ptr-write-u64 st 0 b)
                 (if (or (= (heap-read-u64 st 96) 0) (< b (heap-read-u64 st 96)))
                     (ptr-write-u64 st 96 b) 0)
                 (if (> (+ b 32736) (heap-read-u64 st 104))
                     (ptr-write-u64 st 104 (+ b 32736)) 0)
                 (while (>= i 0)
                   (let* ((h (+ b (* i 32))))
                     (ptr-write-u64 h 0 (heap-read-u64 st 8))
                     (ptr-write-u64 h 24 0)
                     (ptr-write-u64 st 8 h))
                   (setq i (- i 1)))
                 (setq s (heap-read-u64 st 8)))))) 0)
        (if (= s 0) 0
          (seq (ptr-write-u64 st 8 (heap-read-u64 s 0))
               (ptr-write-u64 s 0 0) (ptr-write-u64 s 8 0)
               (ptr-write-u64 s 16 0) s))))
    ;; One allocation entry owns header, sdata and the pool lock.
(defun nl_string_new (bytes n unibyte)
  (let* ((lock (+ (data-addr nl_strings_state) 128)) (word 0))
    (while (= (atomic-compare-exchange lock 0 1) 0) 0)
    (setq word
	  (let*
	      ((s (nl_string_allocate_header)) (nbytes n)
	       (nchars
		(if (or (= unibyte 1) (= bytes 0)) n
		  (nl_string_char_count bytes n))))
	    (if (= s 0) 0
	      (let*
		  ((allocated
		    (let*
			((st (data-addr nl_strings_state))
			 (needed (nl_string_sdata_size nbytes))
			 (b (heap-read-u64 st 24)) (data 0))
		      (if (> nbytes 1024)
			  (seq
			   (setq b
				 (nl_string_alloc_mapping
				  (+ needed 16)))
			   (if (= b 0) 0
			     (if
				 (=
				  (nl_string_register_mapping b
							      (+ b
								 needed
								 16)
							      5)
				  0)
				 (seq
				  (nl_os_free_chunk b (+ needed 16))
				  (setq b 0))
			       (seq
				(ptr-write-u64 b 0
					       (heap-read-u64 st 32))
				(ptr-write-u64 st 32 b)
				(setq data (+ b 16))))))
			(seq
			 (if
			     (or (= b 0)
				 (> (+ (heap-read-u64 b 8) needed)
				    (+ b 8192)))
			     (let*
				 ((fresh
				   (nl_string_alloc_mapping 8192)))
			       (if (= fresh 0) (setq b 0)
				 (if
				     (=
				      (nl_string_register_mapping
				       fresh (+ fresh 8192) 4)
				      0)
				     (seq
				      (nl_os_free_chunk fresh 8192)
				      (setq b 0))
				   (seq (ptr-write-u64 fresh 0 0)
					(ptr-write-u64 fresh 8
						       (+ fresh 16))
					(if (= b 0)
					    (ptr-write-u64 st 16 fresh)
					  (ptr-write-u64 b 0 fresh))
					(setq b fresh)
					(ptr-write-u64 st 24 b)))))
			   0)
			 (if (/= b 0) (setq data (heap-read-u64 b 8))
			   0)))
		      (if (= data 0) 0
			(seq
			 (if
			     (or (= (heap-read-u64 st 112) 0)
				 (< b (heap-read-u64 st 112)))
			     (ptr-write-u64 st 112 b)
			   0)
			 (let*
			     ((end
			       (if (> nbytes 1024) (+ b needed 16)
				 (+ b 8192))))
			   (if (> end (heap-read-u64 st 120))
			       (ptr-write-u64 st 120 end)
			     0))
			 (ptr-write-u64 data 0 s)
			 (ptr-write-u64 b 8 (+ data needed))
			 (ptr-write-u64 s 0 nchars)
			 (ptr-write-u64 s 8
					(if (= unibyte 1) -1 nbytes))
			 (ptr-write-u64 s 24 (+ data 8))
			 (ptr-write-u64 st 136 s)
			 (ptr-write-u8 (+ data 8) nbytes 0)
			 (ptr-write-u64 (data-addr nl_gc_stats) 0
					(+
					 (heap-read-u64
					  (data-addr nl_gc_stats) 0)
					 (+ 32 needed)))
			 (ptr-write-u64 st 88
					(+ (heap-read-u64 st 88)
					   (+ 32 needed)))
			 s)))))
		(if (= allocated 0) 0
		  (seq
		   (if (= bytes 0) 0
		     (nl_string_copy_bytes bytes (heap-read-u64 s 24)
					   n))
		   (+ s 4)))))))
    (atomic-compare-exchange lock 1 0) word))
    ;; Out slots are caller-owned ABI transport. They never allocate an
    ;; alias object. nl_val_clone_into recovers the canonical header WORD.
    (defun nl_string_store_view (word out)
      (let* ((s (- word 4)) (bytes (heap-read-u64 s 8)))
        ;; Decode size_byte once when packing the compatibility transport.
        (if (< bytes 0)
            (seq (ptr-write-u64 out 0 14)
                 (ptr-write-u64 out 8 (logior s 2305843009213693952))
                 (ptr-write-u64 out 24 (logand (heap-read-u64 s 0) 9223372036854775807)))
          (seq (ptr-write-u64 out 0 5)
               (ptr-write-u64 out 8 (logior s 6917529027641081856))
               (ptr-write-u64 out 24 bytes)))
        (ptr-write-u64 out 16 (heap-read-u64 s 24)) out))
    (defun nl_string_view_header (sp)
      (let* ((tag (heap-read-u8 sp 0)))
        (if (or (= tag 5) (= tag 14))
            (let* ((cap (heap-read-u64 sp 8))
                   (s (logand cap 2305843009213693951)))
              ;; Bit 61 identifies a caller-owned view issued by this
              ;; kernel. As with GNU XSTRING, its precise edge is trusted;
              ;; arbitrary legacy capacities still need ownership lookup.
              (if (/= (logand cap 2305843009213693952) 0)
                  (if (/= (heap-read-u64 s 24) 0) s 0)
                (if (= (nl_string_header_p s) 1) s 0))) 0)))
    (defun nl_string_from_view (sp)
      ;; The canonical header owns representation flags. A caller's old
      ;; view must not resurrect multibyte state after clear-string.
      (let* ((s (nl_string_view_header sp)))
        (if (= s 0) 0 (+ s 4))))
    (defun nl_string_mark (s)
      (if (= (nl_string_header_p s) 0) 0
        (if (if (>= s (heap-read-u64 (data-addr nl_strings_state) 160))
                (< s (heap-read-u64 (data-addr nl_strings_state) 168)) 0) 0
        (if (< (heap-read-u64 s 0) 0) 0
          (seq (ptr-write-u64 s 0 (logior (heap-read-u64 s 0) -9223372036854775808))
               (if (/= (heap-read-u64 s 16) 0)
                   (nl_gc_mark_word (heap-read-u64 s 16)) 0) 1)))))
    (defun nl_string_unmark ()
      (let* ((st (data-addr nl_strings_state)) (b (heap-read-u64 st 0)))
        (while (/= b 0)
          (let* ((i 0))
            (while (< i 1023)
              (let* ((s (+ b (* i 32))))
                (if (/= (heap-read-u64 s 24) 0)
                    (ptr-write-u64 s 0 (logand (heap-read-u64 s 0) 9223372036854775807)) 0))
              (setq i (+ i 1))))
          (setq b (heap-read-u64 b 32744)))
        (ptr-write-u64 st 56 0) (ptr-write-u64 st 64 0) 0))
    (defun nl_string_pin_word (w)
      (let* ((st (data-addr nl_strings_state))
             (mapping (nl_string_mapping_holding w))
             (kind (logand mapping 7)) (base (logand mapping -8)) (s 0))
        (if (= kind 3)
            (let* ((header (logand w -32)))
              (if (/= (heap-read-u64 header 24) 0) (setq s header) 0))
          (if (or (= kind 4) (= kind 5))
              (let* ((d (+ base 16)) (end (heap-read-u64 base 8)))
                (while (and (< d end) (= s 0))
                  (let* ((owner (heap-read-u64 d 0))
                         (n (if (= owner 0) (heap-read-u64 d 8) (nl_string_nbytes owner))))
                    (if (and (/= owner 0) (>= w (+ d 8)) (<= w (+ d 8 n)))
                        (setq s owner) 0)
                    (setq d (+ d (nl_string_sdata_size n)))))) 0))
        (if (= s 0) 0
          (seq (nl_gc_mark_word (+ s 4)) (ptr-write-u64 st 56 1) 1))))
    (defun nl_compact_small_strings ()
      (let* ((st (data-addr nl_strings_state)) (tb (heap-read-u64 st 16)))
        (if (or (= tb 0) (/= (heap-read-u64 st 56) 0)
                (= (nl_gc_conserv_pinned_p) 1)) 0
          (let* ((to (+ tb 16)) (b tb))
            (while (/= b 0)
              (let* ((from (+ b 16)) (end (heap-read-u64 b 8)))
                (while (< from end)
                  (let* ((s (heap-read-u64 from 0))
                         (n (if (= s 0) (heap-read-u64 from 8) (nl_string_nbytes s)))
                         (size (nl_string_sdata_size n)) (next (+ from size)))
                    (if (/= s 0)
                        (seq
                         (if (> (+ to size) (+ tb 8192))
                             (seq (ptr-write-u64 tb 8 to)
                                  (setq tb (heap-read-u64 tb 0)) (setq to (+ tb 16))) 0)
                         (if (/= from to)
                             (seq (nl_string_copy_bytes from to size)
                                  (ptr-write-u64 s 24 (+ to 8))
                                  (ptr-write-u64 st 80 (+ (heap-read-u64 st 80) size))) 0)
                         (setq to (+ to size))) 0)
                    (setq from next))))
              (setq b (heap-read-u64 b 0)))
            (setq b (heap-read-u64 tb 0))
            (while (/= b 0)
              (let* ((next (heap-read-u64 b 0)))
                (nl_string_unregister_mapping b (+ b 8192))
                (nl_os_free_chunk b 8192) (setq b next)))
            (ptr-write-u64 tb 8 to) (ptr-write-u64 tb 0 0)
            (ptr-write-u64 st 24 tb) 1))))
    (defun nl_sweep_strings ()
      (let* ((st (data-addr nl_strings_state)) (b (heap-read-u64 st 0))
             (kept 0) (empty 0) (count 0) (bytes 0))
        (ptr-write-u64 st 8 0)
        (ptr-write-u64 st 136 0)
        (while (/= b 0)
          (let* ((next (heap-read-u64 b 32744)) (before (heap-read-u64 st 8))
                 (live 0) (i 0))
            (while (< i 1023)
              (let* ((s (+ b (* i 32))))
                (if (and (/= (heap-read-u64 s 24) 0) (< (heap-read-u64 s 0) 0))
                    (seq (ptr-write-u64 s 0 (logand (heap-read-u64 s 0) 9223372036854775807))
                         (setq live (+ live 1)) (setq count (+ count 1))
                         (setq bytes (+ bytes (+ 32 (nl_string_sdata_size (nl_string_nbytes s))))))
                  (seq
                   (if (/= (heap-read-u64 s 24) 0)
                       (let* ((d (- (heap-read-u64 s 24) 8)) (n (nl_string_nbytes s)))
                         (ptr-write-u64 d 0 0) (ptr-write-u64 d 8 n)) 0)
                   (ptr-write-u64 s 24 0) (ptr-write-u64 s 0 (heap-read-u64 st 8))
                   (ptr-write-u64 st 8 s))))
              (setq i (+ i 1)))
            (if (and (= live 0) (>= empty 2))
                (seq (ptr-write-u64 st 8 before)
                     (seq (nl_string_unregister_mapping b (+ b 32736)) (nl_os_free_chunk (heap-read-u64 b 32736) 65536)))
              (seq (if (= live 0) (setq empty (+ empty 1)) 0)
                   (ptr-write-u64 b 32744 kept) (setq kept b)))
            (setq b next)))
        (ptr-write-u64 st 0 kept)
        (setq b (heap-read-u64 st 32)) (setq kept 0)
        (while (/= b 0)
          (let* ((next (heap-read-u64 b 0)) (end (heap-read-u64 b 8)))
            (if (= (heap-read-u64 b 16) 0) (seq (nl_string_unregister_mapping b end) (nl_os_free_chunk b (- end b)))
              (seq (ptr-write-u64 b 0 kept) (setq kept b))) (setq b next)))
        (ptr-write-u64 st 32 kept)
        (nl_compact_small_strings)
        (nl_string_reap_page_tables)
        (let* ((fix (heap-read-u64 st 64)))
          (while (/= fix 0)
            (let* ((sp (heap-read-u64 fix 0)) (s (nl_string_view_header sp)))
              (if (/= s 0) (ptr-write-u64 sp 16 (heap-read-u64 s 24)) 0))
            (setq fix (heap-read-u64 fix 8))))
        (ptr-write-u64 st 64 0) (ptr-write-u64 st 56 0)
        (ptr-write-u64 st 40 count) (ptr-write-u64 st 48 bytes)
        (ptr-write-u64 st 72 (+ (heap-read-u64 st 72) 1))
        (ptr-write-u64 (data-addr nl_gc_stats) 16
                       (+ (heap-read-u64 (data-addr nl_gc_stats) 16) bytes))
        count)))
  "GNU-style string allocation, marking, sweeping and sdata compaction.")

(provide 'nelisp-cc-string-blocks)
;;; nelisp-cc-string-blocks.el ends here
