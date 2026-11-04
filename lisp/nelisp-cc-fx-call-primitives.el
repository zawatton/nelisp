;;; nelisp-cc-fx-call-primitives.el --- VM primitive WORD calls -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Evaluator transport only (native-surface clauses 1/2/5). No new Lisp
;; primitive or native sorting implementation: sort remains ordinary Lisp.
;; Inspect the live resolved definition, retaining normal APPLY on decline.

(defconst nelisp-cc-fx-call-primitives--source
  '(seq
    ;; GNU bytecode.c:803 reads the called symbol's function cell directly.
    ;; The symbol cell (Doc 221 M02) replaces the per-site entry cache that
    ;; was validated against the table, its count and its buckets on every
    ;; call; CACHE's slot 0 is the call site's symbol, owned by CACHE.
    ;; Alias symbols and malformed/immediate definitions retain the full
    ;; checked resolver: this answers 0 for them.  The live function cell
    ;; is read every time, including after FSET and FMAKUNBOUND.
    ;; A symbol object is its own entry (Doc 226 D9, `nelisp_mirror_lookup_entry_owned'),
    ;; so the function cell is its field at +24 and the lookup is skipped for it; a direct
    ;; cons (the usual definition, `(builtin NAME)' or a closure) is no unbound marker.
    (defun m5_fx_cached_function (cache env)
      (let* ((sym (nl_vector_slot_word cache 0))
             (entry (if (= (logand sym 7) 6)
                        (let* ((k (logand (ptr-read-u64 (- sym 6) 0) 63)))
                          (if (= k 4) sym
                            (if (= k 16) sym (nelisp_mirror_lookup_entry_owned env sym))))
                      (nelisp_mirror_lookup_entry_owned env sym))))
        (if (or (= entry 0) (= entry 3)) 0
          (let* ((fn (let ((w0 (nl_record_slot_word entry 1)))
                       ;; w2subr: a Subr object answers its legacy list.
                       (if (= (logand w0 7) 6)
                           (if (= (logand (heap-read-u8 (- w0 6) 0) 63) 20)
                               (heap-read-u64 (- w0 6) 8) w0)
                         w0))))
            (if (= (logand fn 7) 2) fn
              (if (= (wf_bytecode_w_unbound_p env fn) 1) 0
                (if (= (m5_fx_word_cons_p fn 0) 1) fn 0)))))))
    ;; NAME's WORD when FN is (builtin NAME), else 0.
    (defun m5_fx_builtin_name (fn _pad)
      (if (= (m5_fx_word_cons_p fn 0) 0) 0
        (let* ((head (nl_cons_car_word fn)) (tail (nl_cons_cdr_word fn)))
          (if (= (logand head 1) 1) 0
            (if (= (sexp-name-eq head "builtin") 0) 0
              (if (= (m5_fx_word_cons_p tail 0) 0) 0
                (nl_cons_car_word tail)))))))
    (defun m5_fx_primitive (fn stk sp argc resw env scratch)
      (let* ((name (m5_fx_builtin_name fn 0)))
        (if (= name 0) -3
          (m5_fx_primitive_named name stk sp argc resw env scratch))))
    ;; The primitives VM code calls without an argument list, by id: the
    ;; names are compared once per call site and definition (the call-out's
    ;; inline cache, `m5_fx_callout'), not on every call.  0: no primitive
    ;; for NAME with ARGC arguments.
    (defun m5_fx_primitive_id (name argc)
      (if (= argc 1)
          (cond
           ((= (sexp-name-eq name "car") 1) 1)
           ((= (sexp-name-eq name "cdr") 1) 2)
           ((or (= (sexp-name-eq name "null") 1) (= (sexp-name-eq name "not") 1)) 3)
           ((= (sexp-name-eq name "consp") 1) 4)
           ((= (sexp-name-eq name "length") 1) -16)
           ((= (sexp-name-eq name "funcall") 1) 13)
           ;; nc3: `fboundp' (`bf_fboundp_w') on the operand word.
           ((= (sexp-name-eq name "fboundp") 1) -19)
           ;; callpath2: `symbol-function' (GNU Bsymbol_function) on the operand word.
           ((= (sexp-name-eq name "symbol-function") 1) -21)
           (t 0))
        (if (= argc 2)
            (cond
             ((= (sexp-name-eq name "setcdr") 1) 5)
             ((= (sexp-name-eq name "setcar") 1) 6)
             ((= (sexp-name-eq name "eq") 1) 7)
             ((= (sexp-name-eq name "<") 1) 8)
             ((= (sexp-name-eq name "string<") 1) 9)
             ((= (sexp-name-eq name "nth") 1) 10)
             ((= (sexp-name-eq name "nthcdr") 1) 11)
             ((= (sexp-name-eq name "aref") 1) -14)
             ((= (sexp-name-eq name "%") 1) -17)
             ((= (sexp-name-eq name "logand") 1) -18)
             ((= (sexp-name-eq name "funcall") 1) 13)
             (t 0))
          ;; Negative internal kinds do not overlap the interpreter's
          ;; positive descriptor IDs or FX globals21/22. The site's signed
          ;; fixnum cache preserves them; -3 remains a return status only.
          (if (if (= argc 3) (= (sexp-name-eq name "aset") 1) 0) -15
            ;; nc3: the builtin arm of `nelisp--string-search-chars' on the
            ;; operand words.
            (if (if (= argc 3) (= (sexp-name-eq name "nelisp--string-search-chars") 1) 0) -20
              (if (if (> argc 0) (= (sexp-name-eq name "funcall") 1) 0) 13 0))))))
    ;; bufedit/bufstore: the buffer text is a gap buffer of bytes in Emacs's internal encoding
    ;; (see `nelisp--gb-char-reference' in the prelude).  GNU buffer.h: BEG_ADDR..GPT_ADDR | gap |
    ;; GAP_END_ADDR..Z_ADDR, one array of bytes; a character is one byte in a unibyte buffer and its
    ;; internal (UTF-8 based, character.h) sequence in a multibyte one, a raw byte 0x80..0xFF being
    ;; the two bytes C0/C1 xx.  The record's STORE slot (1) is a unibyte string of CAP bytes: the
    ;; GBYTE bytes before the gap, the gap, then ZBYTE - GBYTE bytes at the end (physical byte =
    ;; logical + CAP - ZBYTE from GBYTE on).  Positions handed in and out stay character indexes:
    ;; BLEN (GPT) and LEN (Z) count characters, GBYTE (GPT_BYTE) and ZBYTE (Z_BYTE) bytes, and
    ;; CCHAR/CBYTE is one known character/byte pair (marker.c's cached_charpos/cached_bytepos).
    ;; When ZBYTE = LEN every character is one byte and a position is its own byte position (the
    ;; Z == Z_BYTE early return of buf_charpos_to_bytepos); otherwise a position is found by
    ;; scanning from the nearest known pair (BEG, GPT, Z, the cache: best_below/best_above).  Slot
    ;; K of the record is the word at payload + 16 + 8K: store 24, unibyte 32, modified 40, markers
    ;; 48, overlays 56, narrow-start 64, narrow-end 72, text-properties 80, tick 96, pend 104, blen
    ;; 112, len 120, ins 136, gbyte 144, zbyte 152, cchar 160, cbyte 168.  A string word (tag 4) has
    ;; its header at word-4: nchars 0, nbytes 8 (negative: unibyte), intervals 16, data pointer 24
    ;; (read again after any allocation: string data can move).  GNU: insdel.c make_gap/
    ;; move_gap_both/insert_1_both/copy_text/del_range_2, marker.c buf_charpos_to_bytepos/
    ;; buf_bytepos_to_charpos, search.c find_newline, editfns.c Fchar_after/make_buffer_string.
    ;; The header of the store when the record's text state is consistent, else 0.
    (defun nl_gb_ok (r)
      (let* ((s (ptr-read-u64 r 24)))
        (if (/= (logand s 7) 4) 0
          (let* ((h (- s 4)) (bw (ptr-read-u64 r 112)) (lw (ptr-read-u64 r 120))
                 (gw (ptr-read-u64 r 144)) (zw (ptr-read-u64 r 152)))
            (if (>= (ptr-read-u64 h 8) 0) 0
              (if (/= (logand (logand bw lw) (logand gw zw) 3) 1) 0
                (if (/= (logand (logior bw lw) (logior gw zw) 3) 1) 0
                  (let* ((blen (sar bw 2)) (len (sar lw 2)) (gb (sar gw 2)) (zb (sar zw 2))
                         (cap (logand (ptr-read-u64 h 0) 9223372036854775807)))
                    (if (< blen 0) 0
                      (if (> blen len) 0
                        (if (> zb cap) 0
                          (if (> gb zb) 0
                            (if (< gb blen) 0
                              (if (< zb len) 0
                                (if (> (- gb blen) (- zb len)) 0 h)))))))))))))))
    ;; The byte length of the character whose internal sequence starts with the byte X.
    (defun nl_gb_blen (x)
      (if (< x 128) 1 (if (< x 224) 2 (if (< x 240) 3 (if (< x 248) 4 5)))))
    ;; The physical byte P after N characters from physical byte P inside one side of the gap:
    ;; eight bytes per step while they are ASCII.
    (defun nl_gb_fwd (d p n)
      (seq
       (while (> n 0)
         (if (if (>= n 8) (= (logand (ptr-read-u64 d p) -9187201950435737472) 0) 0)
             (seq (setq p (+ p 8)) (setq n (- n 8)))
           (seq (setq p (+ p (nl_gb_blen (ptr-read-u8 d p)))) (setq n (- n 1)))))
       p))
    ;; The physical byte N characters before the character boundary P (one side of the gap).
    (defun nl_gb_bwd (d p n)
      (seq
       (while (> n 0)
         (if (if (>= n 8) (= (logand (ptr-read-u64 d (- p 8)) -9187201950435737472) 0) 0)
             (seq (setq p (- p 8)) (setq n (- n 8)))
           (seq (setq p (- p 1))
                (while (= (logand (ptr-read-u8 d p) 192) 128) (setq p (- p 1)))
                (setq n (- n 1)))))
       p))
    ;; The number of characters (bytes that are not 10xxxxxx continuation bytes) at physical
    ;; [P, E): four bytes per step, the continuation lanes counted with one multiply.
    (defun nl_gb_leads (d p e)
      (let* ((n 0))
        (seq
         (while (<= (+ p 4) e)
           (let* ((w (ptr-read-u32 d p))
                  (c (sar (logand (logand w (logxor (shl w 1) 4294967295)) 2155905152) 7)))
             (seq (setq n (+ n (- 4 (logand (sar (* c 16843009) 24) 255))))
                  (setq p (+ p 4)))))
         (while (< p e)
           (seq (if (= (logand (ptr-read-u8 d p) 192) 128) 0 (setq n (+ n 1)))
                (setq p (+ p 1))))
         n)))
    ;; Record the known pair CHARPOS/BYTEPOS of R (the cache) and answer BYTEPOS.
    (defun nl_gb_cache (r c b)
      (seq (ptr-write-u64 r 160 (+ (shl c 2) 1)) (ptr-write-u64 r 168 (+ (shl b 2) 1)) b))
    ;; buf_charpos_to_bytepos: the logical byte position of the character index POS (0 <= POS
    ;; <= LEN) of record R with store data D.
    (defun nl_gb_c2b (r d pos)
      (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2)))
        (if (= zb len) pos
          (let* ((blen (sar (ptr-read-u64 r 112) 2)) (gb (sar (ptr-read-u64 r 144) 2))
                 (cc (sar (ptr-read-u64 r 160) 2)) (cb (sar (ptr-read-u64 r 168) 2))
                 (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807) zb))
                 (lc 0) (lb 0) (hc len) (hb zb))
            (seq
             ;; best_below / best_above among BEG, GPT, Z and the cache
             (if (<= blen pos) (seq (setq lc blen) (setq lb gb)) (seq (setq hc blen) (setq hb gb)))
             (if (if (<= cc pos) (> cc lc) 0) (seq (setq lc cc) (setq lb cb)) 0)
             (if (if (>= cc pos) (< cc hc) 0) (seq (setq hc cc) (setq hb cb)) 0)
             (if (= pos lc) lb
               (if (= pos hc) hb
                 (nl_gb_cache r pos
                              (if (<= (- pos lc) (- hc pos))
                                  (if (< lb gb) (nl_gb_fwd d lb (- pos lc))
                                    (- (nl_gb_fwd d (+ lb gapb) (- pos lc)) gapb))
                                (if (<= hb gb) (nl_gb_bwd d hb (- hc pos))
                                  (- (nl_gb_bwd d (+ hb gapb) (- hc pos)) gapb)))))))))))
    ;; buf_bytepos_to_charpos: the character index of the logical character boundary BPOS.
    (defun nl_gb_b2c (r d bpos)
      (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2)))
        (if (= zb len) bpos
          (let* ((blen (sar (ptr-read-u64 r 112) 2)) (gb (sar (ptr-read-u64 r 144) 2))
                 (cc (sar (ptr-read-u64 r 160) 2)) (cb (sar (ptr-read-u64 r 168) 2))
                 (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807) zb))
                 (lc 0) (lb 0) (hc len) (hb zb))
            (seq
             (if (<= gb bpos) (seq (setq lc blen) (setq lb gb)) (seq (setq hc blen) (setq hb gb)))
             (if (if (<= cb bpos) (> cb lb) 0) (seq (setq lc cc) (setq lb cb)) 0)
             (if (if (>= cb bpos) (< cb hb) 0) (seq (setq hc cc) (setq hb cb)) 0)
             (let* ((c (if (<= (- bpos lb) (- hb bpos))
                           (+ lc (if (< lb gb) (nl_gb_leads d lb bpos)
                                   (nl_gb_leads d (+ lb gapb) (+ bpos gapb))))
                         (- hc (if (<= hb gb) (nl_gb_leads d bpos hb)
                                 (nl_gb_leads d (+ bpos gapb) (+ hb gapb)))))))
               (seq (nl_gb_cache r c bpos) c)))))))
    ;; The character at the physical byte P of a multibyte store (STRING_CHAR).
    (defun nl_gb_pchar (d p)
      (let* ((x (ptr-read-u8 d p)))
        (if (< x 128) x (sar (nl_gb_dec d p (+ p 8)) 3))))
    ;; The character at character index I (0 <= I < LEN) of record R with store data D.
    (defun nl_gb_at (r d i)
      (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2))
             (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807) zb)))
        (if (= zb len)
            (ptr-read-u8 d (if (< i (sar (ptr-read-u64 r 112) 2)) i (+ i gapb)))
          (let* ((lb (nl_gb_c2b r d i)))
            (nl_gb_pchar d (if (< lb (sar (ptr-read-u64 r 144) 2)) lb (+ lb gapb)))))))
    ;; 1 when the 32-bit word W has a byte equal to C (K = C in every byte), else 0.
    (defun nl_gb_hasb (w k)
      (let* ((y (logxor w k)))
        (if (= (logand (logior (+ (logand y 2139062143) 2139062143) y) 2155905152) 2155905152) 0 1)))
    ;; Last physical byte in [LO, HI] equal to C scanning down, or -1: eight bytes per step.
    (defun nl_gb_rscan (d lo hi c)
      (let* ((p hi) (res -1) (go 1) (k (* c 16843009)))
        (seq
         (while (if (= go 1) (>= (- p 7) lo) 0)
           (if (= (logior (nl_gb_hasb (ptr-read-u32 d (- p 3)) k) (nl_gb_hasb (ptr-read-u32 d (- p 7)) k)) 0)
               (setq p (- p 8))
             (setq go 0)))
         (while (if (= res -1) (>= p lo) 0)
           (if (= (ptr-read-u8 d p) c) (setq res p) (setq p (- p 1))))
         res)))
    ;; First physical byte in [LO, HI] equal to C scanning up, or -1.
    (defun nl_gb_fscan (d lo hi c)
      (let* ((p lo) (res -1) (go 1) (k (* c 16843009)))
        (seq
         (while (if (= go 1) (<= (+ p 7) hi) 0)
           (if (= (logior (nl_gb_hasb (ptr-read-u32 d p) k) (nl_gb_hasb (ptr-read-u32 d (+ p 4)) k)) 0)
               (setq p (+ p 8))
             (setq go 0)))
         (while (if (= res -1) (<= p hi) 0)
           (if (= (ptr-read-u8 d p) c) (setq res p) (setq p (+ p 1))))
         res)))
    ;; Last logical byte in [FL, I] equal to C, or -1 (GPT byte GB, gap GAPB bytes).
    (defun nl_gb_rfindb (d gb gapb i fl c)
      (let* ((res -1))
        (seq
         (if (>= i gb)
             (let* ((lo2 (if (> fl gb) fl gb))
                    (p (nl_gb_rscan d (+ lo2 gapb) (+ i gapb) c)))
               (seq (if (>= p 0) (setq res (- p gapb)) 0)
                    (setq i (- gb 1))))
           0)
         (if (< res 0)
             (if (>= i fl) (setq res (nl_gb_rscan d fl i c)) 0)
           0)
         res)))
    ;; First logical byte in [I, END] equal to C, or -1.
    (defun nl_gb_ffindb (d gb gapb i end c)
      (let* ((res -1))
        (seq
         (if (< i gb)
             (let* ((e2 (if (< end gb) end (- gb 1)))
                    (p (nl_gb_fscan d i e2 c)))
               (seq (if (>= p 0) (setq res p) 0)
                    (setq i gb)))
           0)
         (if (< res 0)
             (if (<= i end)
                 (let* ((p (nl_gb_fscan d (+ i gapb) (+ end gapb) c)))
                   (if (>= p 0) (setq res (- p gapb)) 0))
               0)
           0)
         res)))
    ;; Last character index in [FL, I] holding the byte-sized character C (ASCII, or any byte of a
    ;; unibyte buffer), or -1 (GNU find_newline backward).
    (defun nl_gb_rfind (r d i fl c)
      (if (< i fl) -1
        (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2))
               (gb (sar (ptr-read-u64 r 144) 2))
               (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807) zb)))
          (if (= zb len) (nl_gb_rfindb d gb gapb i fl c)
            (let* ((lb (nl_gb_c2b r d fl)) (hb (nl_gb_c2b r d i))
                   (j (nl_gb_rfindb d gb gapb hb lb c)))
              (if (< j 0) -1 (nl_gb_b2c r d j)))))))
    ;; First character index in [I, END] holding C, or -1 (GNU find_newline forward).
    (defun nl_gb_ffind (r d i end c)
      (if (> i end) -1
        (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2))
               (gb (sar (ptr-read-u64 r 144) 2))
               (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807) zb)))
          (if (= zb len) (nl_gb_ffindb d gb gapb i end c)
            (let* ((hb (nl_gb_c2b r d end)) (lb (nl_gb_c2b r d i))
                   (j (nl_gb_ffindb d gb gapb lb hb c)))
              (if (< j 0) -1 (nl_gb_b2c r d j)))))))
    ;; Number of newline bytes at physical [LO, HI): eight per step, zero lanes counted exactly.
    (defun nl_gb_pcount (d lo hi)
      (let* ((p lo) (n 0) (k 168430090))
        (seq
         (while (<= (+ p 4) hi)
           (let* ((y (logxor (ptr-read-u32 d p) k))
                  (z (logand (logxor (logior (+ (logand y 2139062143) 2139062143) y) 4294967295) 2155905152)))
             (seq (if (= z 0) 0 (setq n (+ n (logand (sar (* (sar z 7) 16843009) 24) 255))))
                  (setq p (+ p 4)))))
         (while (< p hi)
           (seq (if (= (ptr-read-u8 d p) 10) (setq n (+ n 1)) 0)
                (setq p (+ p 1))))
         n)))
    ;; Number of newlines at character indexes [S, E) of record R.
    (defun nl_gb_count (r d s e)
      (let* ((sb (nl_gb_c2b r d s)) (eb (nl_gb_c2b r d e))
             (gb (sar (ptr-read-u64 r 144) 2))
             (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807)
                      (sar (ptr-read-u64 r 152) 2)))
             (n 0))
        (seq
         (if (< sb gb)
             (seq (setq n (nl_gb_pcount d sb (if (< eb gb) eb gb)))
                  (setq sb gb))
           0)
         (if (< sb eb) (+ n (nl_gb_pcount d (+ sb gapb) (+ eb gapb))) n))))
    ;; Copy N bytes within the store from physical SRC to DST (memmove: overlap safe), eight at a
    ;; time.
    (defun nl_gb_copy (d dst src n)
      (let* ((i 0))
        (seq
         (if (< dst src)
             (seq (while (<= (+ i 8) n)
                    (seq (ptr-write-u64 d (+ dst i) (ptr-read-u64 d (+ src i)))
                         (setq i (+ i 8))))
                  (while (< i n)
                    (seq (ptr-write-u8 d (+ dst i) (ptr-read-u8 d (+ src i)))
                         (setq i (+ i 1)))))
           (seq (setq i n)
                (while (>= i 8)
                  (seq (setq i (- i 8))
                       (ptr-write-u64 d (+ dst i) (ptr-read-u64 d (+ src i)))))
                (while (> i 0)
                  (seq (setq i (- i 1))
                       (ptr-write-u8 d (+ dst i) (ptr-read-u8 d (+ src i)))))))
         0)))
    ;; Copy N bytes from SRC to DST (distinct memory: copy_text's memcpy): 32 bytes per step.
    (defun nl_gb_mcopy (src dst n)
      (let* ((i 0))
        (seq
         (while (<= (+ i 32) n)
           (let* ((a (ptr-read-u64 src i)) (b (ptr-read-u64 src (+ i 8)))
                  (c (ptr-read-u64 src (+ i 16))) (e (ptr-read-u64 src (+ i 24))))
             (seq (ptr-write-u64 dst i a) (ptr-write-u64 dst (+ i 8) b)
                  (ptr-write-u64 dst (+ i 16) c) (ptr-write-u64 dst (+ i 24) e)
                  (setq i (+ i 32)))))
         (while (<= (+ i 8) n)
           (seq (ptr-write-u64 dst i (ptr-read-u64 src i)) (setq i (+ i 8))))
         (while (< i n)
           (seq (ptr-write-u8 dst i (ptr-read-u8 src i)) (setq i (+ i 1))))
         1)))
    ;; Move the gap of the (valid) store H of record R to character index POS (move_gap_both).
    (defun nl_gb_move (r h pos)
      (let* ((d (ptr-read-u64 h 24)) (gb (sar (ptr-read-u64 r 144) 2)) (zb (sar (ptr-read-u64 r 152) 2))
             (gapb (- (logand (ptr-read-u64 h 0) 9223372036854775807) zb))
             (pb (nl_gb_c2b r d pos)))
        (seq
         (if (< pb gb) (nl_gb_copy d (+ pb gapb) pb (- gb pb))
           (if (> pb gb) (nl_gb_copy d gb (+ gb gapb) (- pb gb)) 0))
         (ptr-write-u64 r 112 (+ (shl pos 2) 1))
         (ptr-write-u64 r 144 (+ (shl pb 2) 1))
         0)))
    ;; Make the gap of record R at least N bytes (make_gap_larger): a new store with the gap kept
    ;; where it is, GNU's GAP_BYTES_DFL (2000) more than asked, or half the text when that is more
    ;; (no realloc here, so appends grow the store geometrically).  1 when the gap is large enough
    ;; afterwards, 0 when memory is short.
    (defun nl_gb_grow (r n)
      (let* ((h (nl_gb_ok r)))
        (if (= h 0) 0
          (let* ((gb (sar (ptr-read-u64 r 144) 2)) (zb (sar (ptr-read-u64 r 152) 2))
                 (cap (logand (ptr-read-u64 h 0) 9223372036854775807)))
            (if (>= (- cap zb) n) 1
              (let* ((need (+ zb n)) (ncap (+ need (if (< (/ zb 2) 2000) 2000 (/ zb 2))))
                     (w (nl_string_new 0 ncap 1)))
                (if (= w 0) 0
                  (let* ((h2 (ptr-read-u64 r 24)) (old (- h2 4))
                         (od (ptr-read-u64 old 24)) (nd (ptr-read-u64 (- w 4) 24))
                         (tail (- zb gb)))
                    (seq (nl_gb_mcopy od nd gb)
                         (nl_gb_mcopy (+ od (- cap tail)) (+ nd (- ncap tail)) tail)
                         (wf_dirty)
                         (ptr-write-u64 r 24 w)
                         1)))))))))
    ;; The character (shifted left 3) and byte count of the internal sequence at byte BI of the
    ;; NB bytes at SD, as character*8 + length, or -1 for a truncated or malformed one.
    (defun nl_gb_dec (sd bi nb)
      (let* ((b0 (ptr-read-u8 sd bi)))
        (if (< b0 128) (+ (shl b0 3) 1)
          (if (< b0 194)
              (if (< (+ bi 1) nb)
                  (+ (shl (+ 4194176 (* (logand b0 1) 64) (logand (ptr-read-u8 sd (+ bi 1)) 63)) 3) 2)
                -1)
            (if (< b0 224)
                (if (< (+ bi 1) nb)
                    (+ (shl (logior (* (logand b0 31) 64) (logand (ptr-read-u8 sd (+ bi 1)) 63)) 3) 2)
                  -1)
              (if (< b0 240)
                  (if (< (+ bi 2) nb)
                      (+ (shl (logior (logior (* (logand b0 15) 4096)
                                              (* (logand (ptr-read-u8 sd (+ bi 1)) 63) 64))
                                      (logand (ptr-read-u8 sd (+ bi 2)) 63)) 3) 3)
                    -1)
                (if (< b0 248)
                    (if (< (+ bi 3) nb)
                        (+ (shl (logior (logior (* (logand b0 7) 262144)
                                                (* (logand (ptr-read-u8 sd (+ bi 1)) 63) 4096))
                                        (logior (* (logand (ptr-read-u8 sd (+ bi 2)) 63) 64)
                                                (logand (ptr-read-u8 sd (+ bi 3)) 63))) 3) 4)
                      -1)
                  (if (= b0 248)
                      (if (< (+ bi 4) nb)
                          (+ (shl (logior (logior (* (logand (ptr-read-u8 sd (+ bi 1)) 63) 262144)
                                                  (* (logand (ptr-read-u8 sd (+ bi 2)) 63) 4096))
                                          (logior (* (logand (ptr-read-u8 sd (+ bi 3)) 63) 64)
                                                  (logand (ptr-read-u8 sd (+ bi 4)) 63))) 3) 5)
                        -1)
                    -1))))))))
    ;; regfix: a legacy string box (tag 5 or 14: the result of a native string builtin such as
    ;; `number-to-string', `upcase' or `decode-coding-string' that a VM local or an argument list
    ;; still holds) as the String WORD the store operations take, as `nl_val_clone_into' makes it
    ;; for a stored value; any other word comes back unchanged.
    (defun nl_gb_strw (w)
      (if (/= (logand w 7) 0) w
        (if (< w 4096) w
          (let* ((tg (heap-read-u8 w 0)))
            (if (or (= tg 5) (= tg 14))
                (let* ((v (nl_string_from_view w)))
                  (if (/= v 0) v
                    (let* ((o (nl_sym_name_string w))) (if (= o 0) w o))))
              w)))))
    ;; The number of bytes at SD (N of them) that are 128 or more: eight per step.
    (defun nl_gb_highs (sd n)
      (let* ((k 0) (m 0))
        (seq
         (while (<= (+ k 8) n)
           (seq (if (= (logand (ptr-read-u64 sd k) -9187201950435737472) 0) 0
                  (let* ((j k) (e (+ k 8)))
                    (while (< j e)
                      (seq (if (>= (ptr-read-u8 sd j) 128) (setq m (+ m 1)) 0) (setq j (+ j 1))))))
                (setq k (+ k 8))))
         (while (< k n)
           (seq (if (>= (ptr-read-u8 sd k) 128) (setq m (+ m 1)) 0) (setq k (+ k 1))))
         m)))
    ;; The bytes the string with header H2 takes in a buffer (UNI 1: unibyte), copy_text's count:
    ;; its bytes when the representations agree, one per character into a unibyte buffer, two for
    ;; each byte >= 128 of a unibyte string into a multibyte buffer.
    (defun nl_gb_need (h2 uni)
      (let* ((n (logand (ptr-read-u64 h2 0) 9223372036854775807)) (sb (ptr-read-u64 h2 8)))
        (if (= uni 1) n
          (if (>= sb 0) sb (+ n (nl_gb_highs (ptr-read-u64 h2 24) n))))))
    ;; Write the string with header H2 at physical byte P of the store data D (insert_1_both /
    ;; copy_text): the bytes as they are when the representations agree (memcpy), a unibyte byte
    ;; >= 128 as the raw-byte character's two bytes in a multibyte buffer (UNI 0), a multibyte
    ;; character's low byte in a unibyte one.  The number of bytes written, or -1 for a
    ;; malformed string.
    (defun nl_gb_put (d p h2 uni)
      (let* ((n (logand (ptr-read-u64 h2 0) 9223372036854775807)) (sb (ptr-read-u64 h2 8))
             (sd (ptr-read-u64 h2 24)))
        (if (if (= uni 1) (if (< sb 0) 1 (= sb n)) (>= sb 0))
            (seq (nl_gb_mcopy sd (+ d p) (if (< sb 0) n sb)) (if (< sb 0) n sb))
          (if (= uni 0)
              ;; unibyte string into a multibyte buffer
              (let* ((k 0) (o p))
                (seq
                 (while (< k n)
                   (if (if (<= (+ k 8) n) (= (logand (ptr-read-u64 sd k) -9187201950435737472) 0) 0)
                       (seq (ptr-write-u64 d o (ptr-read-u64 sd k)) (setq k (+ k 8)) (setq o (+ o 8)))
                     (let* ((c (ptr-read-u8 sd k)))
                       (seq (if (< c 128) (seq (ptr-write-u8 d o c) (setq o (+ o 1)))
                              (seq (ptr-write-u8 d o (logior 192 (logand (sar c 6) 1)))
                                   (ptr-write-u8 d (+ o 1) (logior 128 (logand c 63)))
                                   (setq o (+ o 2))))
                            (setq k (+ k 1))))))
                 (- o p)))
            ;; multibyte string into a unibyte buffer: each character's low byte
            (let* ((k 0) (bi 0) (bad 0))
              (seq
               (while (if (= bad 0) (< k n) 0)
                 (let* ((x (nl_gb_dec sd bi sb)))
                   (if (< x 0) (setq bad 1)
                     (seq (ptr-write-u8 d (+ p k) (logand (sar x 3) 255))
                          (setq bi (+ bi (logand x 7)))
                          (setq k (+ k 1))))))
               (if (= bad 1) -1 n)))))))
    ;; 1 when the string with header H2 is multibyte text beyond ASCII (its byte count differs from
    ;; its character count), which makes the slices of a multibyte buffer multibyte strings too.
    (defun nl_gb_wide_p (h2)
      (let* ((sb (ptr-read-u64 h2 8)))
        (if (< sb 0) 0
          (if (= sb (logand (ptr-read-u64 h2 0) 9223372036854775807)) 0 1))))
    ;; The internal length of character C.
    (defun nl_gb_enclen (c)
      (if (< c 128) 1 (if (< c 2048) 2 (if (< c 65536) 3 (if (< c 2097152) 4 (if (< c 4194176) 5 2))))))
    ;; Write the internal form of C at DST; its length.
    (defun nl_gb_enc (dst c)
      (if (< c 128) (seq (ptr-write-u8 dst 0 c) 1)
        (if (< c 2048)
            (seq (ptr-write-u8 dst 0 (logior 192 (sar c 6)))
                 (ptr-write-u8 dst 1 (logior 128 (logand c 63))) 2)
          (if (< c 65536)
              (seq (ptr-write-u8 dst 0 (logior 224 (sar c 12)))
                   (ptr-write-u8 dst 1 (logior 128 (logand (sar c 6) 63)))
                   (ptr-write-u8 dst 2 (logior 128 (logand c 63))) 3)
            (if (< c 2097152)
                (seq (ptr-write-u8 dst 0 (logior 240 (sar c 18)))
                     (ptr-write-u8 dst 1 (logior 128 (logand (sar c 12) 63)))
                     (ptr-write-u8 dst 2 (logior 128 (logand (sar c 6) 63)))
                     (ptr-write-u8 dst 3 (logior 128 (logand c 63))) 4)
              (if (< c 4194176)
                  (seq (ptr-write-u8 dst 0 248)
                       (ptr-write-u8 dst 1 (logior 128 (logand (sar c 18) 63)))
                       (ptr-write-u8 dst 2 (logior 128 (logand (sar c 12) 63)))
                       (ptr-write-u8 dst 3 (logior 128 (logand (sar c 6) 63)))
                       (ptr-write-u8 dst 4 (logior 128 (logand c 63))) 5)
                (seq (ptr-write-u8 dst 0 (logior 192 (logand (sar (- c 4194048) 6) 1)))
                     (ptr-write-u8 dst 1 (logior 128 (logand (- c 4194048) 63))) 2)))))))
    ;; 1 when every byte at physical [LO, HI) is below 128 (eight per load).
    (defun nl_gb_ascii_p (d lo hi)
      (let* ((p lo) (acc 0))
        (seq
         (while (<= (+ p 8) hi)
           (seq (setq acc (logior acc (ptr-read-u64 d p))) (setq p (+ p 8))))
         (while (< p hi)
           (seq (setq acc (logior acc (ptr-read-u8 d p))) (setq p (+ p 1))))
         (if (= (logand acc -9187201950435737472) 0) 1 0))))
    ;; Adjust the cache of record R for an insertion of N characters, NB bytes at the gap start
    ;; BLEN: a pair after it moves with the text (one at it stays: the new text starts there).
    (defun nl_gb_cache_ins (r blen n nb)
      (let* ((cc (sar (ptr-read-u64 r 160) 2)))
        (if (> cc blen)
            (seq (ptr-write-u64 r 160 (+ (ptr-read-u64 r 160) (shl n 2)))
                 (ptr-write-u64 r 168 (+ (ptr-read-u64 r 168) (shl nb 2))))
          0)))
    ;; make_buffer_string: the text of the record B at character indexes [S, E) as a new string
    ;; word, or 0 (declined: bad range or state, or no memory).  Unibyte for a unibyte buffer or
    ;; ASCII text; the bytes are copied as they are (the internal encodings agree).
    (defun nl_gb_substring (b s e)
      (let* ((r (- b 6)) (h (nl_gb_ok r)))
        (if (= h 0) 0
          (let* ((len (sar (ptr-read-u64 r 120) 2)))
            (if (< s 0) 0
              (if (> s e) 0
                (if (> e len) 0
                  (let* ((d (ptr-read-u64 h 24))
                         (sb (nl_gb_c2b r d s)) (eb (nl_gb_c2b r d e))
                         (gb (sar (ptr-read-u64 r 144) 2))
                         (gapb (- (logand (ptr-read-u64 h 0) 9223372036854775807) (sar (ptr-read-u64 r 152) 2)))
                         (uni (if (= (ptr-read-u64 r 32) 3) 0 1))
                         ;; the two physical ranges of [SB, EB)
                         (a1 sb) (z1 (if (< eb gb) eb gb))
                         (a2 (+ (if (> sb gb) sb gb) gapb)) (z2 (+ eb gapb))
                         (mb (if (= uni 1) 0
                               (if (= (ptr-read-u64 r 136) 7) 1
                                 (if (= (nl_gb_ascii_p d a1 z1) 1)
                                     (if (= (nl_gb_ascii_p d a2 z2) 1) 0 1) 1))))
                         (nbytes (- eb sb))
                         (w (nl_string_new 0 nbytes (if (= mb 1) 0 1))))
                    (if (= w 0) 0
                      (let* ((h3 (nl_gb_ok r)))
                        (if (= h3 0) 0
                          (let* ((d2 (ptr-read-u64 h3 24)) (dst (ptr-read-u64 (- w 4) 24))
                                 (n1 (if (< a1 z1) (- z1 a1) 0)))
                            (seq (if (> n1 0) (nl_gb_mcopy (+ d2 a1) dst n1) 0)
                                 (if (< a2 z2) (nl_gb_mcopy (+ d2 a2) (+ dst n1) (- z2 a2)) 0)
                                 (if (= mb 1)
                                     (seq (ptr-write-u64 (- w 4) 0 (- e s))
                                          (ptr-write-u64 (- w 4) 8 nbytes))
                                   0)
                                 w)))))))))))))
    ;; Insert the string word SW at the gap start of record B (reserving room): the character
    ;; count as a fixnum word, or 0 (declined).
    (defun nl_gb_insert_w (b sw0)
      (let* ((r (- b 6)) (sw (nl_gb_strw sw0)))
        (if (/= (logand sw 7) 4) 0
          (if (= (nl_gb_ok r) 0) 0
            (let* ((uni0 (if (= (ptr-read-u64 r 32) 3) 0 1))
                   (n (logand (ptr-read-u64 (- sw 4) 0) 9223372036854775807))
                   (nb (nl_gb_need (- sw 4) uni0)))
              (if (= (nl_gb_grow r nb) 0) 0
                (let* ((h (nl_gb_ok r)))
                  (if (= h 0) 0
                    (let* ((blen (sar (ptr-read-u64 r 112) 2)) (gb (sar (ptr-read-u64 r 144) 2))
                           (k (nl_gb_put (ptr-read-u64 h 24) gb (- sw 4) uni0)))
                      (if (/= k nb) 0
                        (seq (wf_dirty)
                             (if (= uni0 1) 0 (if (= (nl_gb_wide_p (- sw 4)) 1) (ptr-write-u64 r 136 7) 0))
                             (nl_gb_cache_ins r blen n nb)
                             (ptr-write-u64 r 112 (+ (ptr-read-u64 r 112) (shl n 2)))
                             (ptr-write-u64 r 120 (+ (ptr-read-u64 r 120) (shl n 2)))
                             (ptr-write-u64 r 144 (+ (ptr-read-u64 r 144) (shl nb 2)))
                             (ptr-write-u64 r 152 (+ (ptr-read-u64 r 152) (shl nb 2)))
                             (+ (shl n 2) 1))))))))))))
    ;; Overwrite the characters at index S.. with those of the string SW (same count) when they
    ;; take the same bytes and the run lies wholly before or wholly after the gap; 3 on success,
    ;; 0 when declined.
    (defun nl_gb_overwrite_w (b s sw0)
      (let* ((r (- b 6)) (h (nl_gb_ok r)) (sw (nl_gb_strw sw0)))
        (if (= h 0) 0
          (if (/= (logand sw 7) 4) 0
            (let* ((n (logand (ptr-read-u64 (- sw 4) 0) 9223372036854775807))
                   (len (sar (ptr-read-u64 r 120) 2))
                   (uni (if (= (ptr-read-u64 r 32) 3) 0 1)))
              (if (< s 0) 0
                (if (> (+ s n) len) 0
                  (let* ((d (ptr-read-u64 h 24)) (gb (sar (ptr-read-u64 r 144) 2))
                         (gapb (- (logand (ptr-read-u64 h 0) 9223372036854775807) (sar (ptr-read-u64 r 152) 2)))
                         (sb (nl_gb_c2b r d s)) (eb (nl_gb_c2b r d (+ s n)))
                         (nb (nl_gb_need (- sw 4) uni)))
                    (if (/= nb (- eb sb)) 0
                      (if (if (<= eb gb) 0 (< sb gb)) 0
                        (if (< (nl_gb_put d (if (<= eb gb) sb (+ sb gapb)) (- sw 4) uni) 0) 0
                          (seq (if (= uni 1) 0 (if (= (nl_gb_wide_p (- sw 4)) 1) (ptr-write-u64 r 136 7) 0)) 3))))))))))))
    ;; The insert fast path (id 58, GNU Finsert -> insert_from_string_1 for plain strings): every
    ;; argument a string without intervals, the buffer without markers, overlays or text
    ;; properties to adjust, and the gap large enough (the reference reserves).  Writes the
    ;; characters at the point (moving the gap to the pending point first) and answers nil.
    (defun nl_gb_insert_leaf (stk sp argc resw r h)
      (let* ((total 0) (i 0) (ok 1) (ne (ptr-read-u64 r 72)) (tick (ptr-read-u64 r 96))
             (pend (ptr-read-u64 r 104)))
        (if (= argc 0) (seq (ptr-write-u64 resw 0 3) 0)
        (if (/= (ptr-read-u64 r 48) 3) -3
          (if (/= (ptr-read-u64 r 56) 3) -3
            (if (/= (ptr-read-u64 r 80) 3) -3
              (if (/= (logand tick 3) 1) -3
                (if (/= ne 3) (if (/= (logand ne 3) 1) -3 (nl_gb_insert_run stk sp argc resw r h total ne tick pend))
                  (nl_gb_insert_run stk sp argc resw r h total ne tick pend)))))))))
    (defun nl_gb_insert_run (stk sp argc resw r h total ne tick pend)
      (let* ((i 0) (ok 1) (uni (if (= (ptr-read-u64 r 32) 3) 0 1)))
        (seq
         (while (if (= ok 1) (< i argc) 0)
           (let* ((w0 (heap-read-u64 (+ stk (* (+ (- sp argc) i) 8)) 0))
                  (w (nl_gb_strw w0)))
             (seq
              (if (/= w w0) (ptr-write-u64 (+ stk (* (+ (- sp argc) i) 8)) 0 w) 0)
              (if (/= (logand w 7) 4) (setq ok 0)
                (let* ((h2 (- w 4)))
                  (if (/= (ptr-read-u64 h2 16) 0) (setq ok 0)
                    (seq (setq total (+ total (nl_gb_need h2 uni)))
                         (setq i (+ i 1)))))))))
         (if (= ok 0) -3
           (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2))
                  (cap (logand (ptr-read-u64 h 0) 9223372036854775807)))
             (if (< (- cap zb) total) -3
               (if (if (= pend 3) 0 (if (/= (logand pend 3) 1) 1 (if (< (sar pend 2) 1) 1 (if (> (sar pend 2) (+ len 1)) 1 0)))) -3
                 (seq
                  (if (= pend 3) 0
                    (seq (nl_gb_move r h (- (sar pend 2) 1))
                         (ptr-write-u64 r 104 3)))
                  (let* ((d (ptr-read-u64 h 24)) (p (sar (ptr-read-u64 r 144) 2))
                         (blen (sar (ptr-read-u64 r 112) 2))
                         (k 0) (kc 0) (j 0) (bad 0) (wd 0))
                    (seq
                     (while (if (= bad 0) (< j argc) 0)
                       (let* ((w (heap-read-u64 (+ stk (* (+ (- sp argc) j) 8)) 0))
                              (n (nl_gb_put d (+ p k) (- w 4) uni)))
                         (if (< n 0) (setq bad 1)
                           (seq (setq k (+ k n))
                                (setq kc (+ kc (logand (ptr-read-u64 (- w 4) 0) 9223372036854775807)))
                                (setq j (+ j 1))
                                (if (= uni 1) 0 (if (= (nl_gb_wide_p (- w 4)) 1) (setq wd 1) 0))))))
                     (if (= bad 1) -3
                       (seq (wf_dirty)
                            (if (= wd 1) (ptr-write-u64 r 136 7) 0)
                            (nl_gb_cache_ins r blen kc k)
                            (ptr-write-u64 r 112 (+ (shl (+ blen kc) 2) 1))
                            (ptr-write-u64 r 120 (+ (shl (+ len kc) 2) 1))
                            (ptr-write-u64 r 144 (+ (shl (+ p k) 2) 1))
                            (ptr-write-u64 r 152 (+ (shl (+ zb k) 2) 1))
                            (if (= ne 3) 0 (ptr-write-u64 r 72 (+ ne (shl kc 2))))
                            (ptr-write-u64 r 40 7)
                            (ptr-write-u64 r 96 (+ tick (shl argc 2)))
                            (ptr-write-u64 resw 0 3)
                            0))))))))))))
    ;; Line start and line end positions (the bodies of ids 54, 55) for the forward-line loops.
    (defun nl_gb_bol (r d pos lo)
      (if (<= pos lo) lo
        (let* ((j (nl_gb_rfind r d (- pos 2) (- lo 1) 10)))
          (if (>= j 0) (+ j 2) lo))))
    (defun nl_gb_eol (r d cur hi)
      (if (>= cur hi) hi
        (let* ((j (nl_gb_ffind r d (- cur 1) (- hi 2) 10)))
          (if (>= j 0) (+ j 1) hi))))
    ;; forward-line (id 64, GNU Fforward_line / scan_newline_from_point): the point moves N lines
    ;; (nil is 1) clamped to the accessible portion; the value is the shortage, a final line
    ;; without a newline counting as one line moved.  N a fixnum or nil.
    (defun nl_gb_forward_line (stk sp argc resw r)
      (let* ((h (nl_gb_ok r)))
        (if (= h 0) -3
          (let* ((blen (sar (ptr-read-u64 r 112) 2)) (len (sar (ptr-read-u64 r 120) 2))
                 (pend (ptr-read-u64 r 104)) (ns (ptr-read-u64 r 64)) (ne (ptr-read-u64 r 72))
                 (pt (if (= pend 3) (+ blen 1) (if (= (logand pend 3) 1) (sar pend 2) -9)))
                 (lo (if (= ns 3) 1 (if (= (logand ns 3) 1) (sar ns 2) -9)))
                 (hi (if (= ne 3) (+ len 1) (if (= (logand ne 3) 1) (sar ne 2) -9)))
                 (a (if (= argc 1) (heap-read-u64 (+ stk (* (- sp 1) 8)) 0) 3)))
            (if (< pt 1) -3
              (if (< lo 1) -3
                (if (> hi (+ len 1)) -3
                  (if (< hi lo) -3
                    (if (> pt hi) -3
                      (if (< pt lo) -3
                        (if (if (> argc 1) 1 (if (= a 3) 0 (if (= (logand a 3) 1) 0 1))) -3
                          (let* ((n (if (= a 3) 1 (sar a 2))) (d (ptr-read-u64 h 24))
                                 (cur pt) (rem n) (ran 0) (stall 0))
                            (seq
                             (if (= n 0)
                                 (seq (setq cur (nl_gb_bol r d pt lo)) (setq rem 0))
                               (if (> n 0)
                                   (seq
                                    (while (if (> rem 0) (= ran 0) 0)
                                      (if (>= cur hi) (setq ran 1)
                                        (let* ((nl (nl_gb_eol r d cur hi)))
                                          (if (< nl hi)
                                              (seq (setq cur (+ nl 1)) (setq rem (- rem 1)))
                                            (seq (setq cur hi) (setq ran 1))))))
                                    ;; GNU: a short count is one less when the point moved to the end
                                    ;; of a nonempty accessible portion that ends in a non-newline
                                    (if (> rem 0)
                                        (if (> hi lo)
                                            (if (/= cur pt)
                                                (if (/= (nl_gb_at r d (- cur 2)) 10)
                                                    (setq rem (- rem 1)) 0)
                                              0)
                                          0)
                                      0))
                                 (seq
                                  (setq cur (nl_gb_bol r d pt lo))
                                  (setq rem (- 0 n))
                                  (while (if (> rem 0) (= stall 0) 0)
                                    (let* ((prev (if (<= cur lo) lo (nl_gb_bol r d (- cur 1) lo))))
                                      (if (= prev cur) (setq stall 1)
                                        (seq (setq cur prev) (setq rem (- rem 1))))))
                                  (setq rem (- 0 rem)))))
                             (wf_dirty)
                             (ptr-write-u64 r 104 (if (= cur (+ blen 1)) 3 (+ (shl cur 2) 1)))
                             (ptr-write-u64 resw 0 (+ (shl rem 2) 1))
                             0)))))))))))))
    ;; The hot readers of the text with every state word read once: 50 char-after, 51 char-before,
    ;; 52 following-char, 53 preceding-char, 54/59 line-beginning-position/pos-bol, 55/60
    ;; line-end-position/pos-eol, 56 bolp, 57 eolp.  0 with RESW written, -3 declined.  They
    ;; allocate nothing.
    (defun nl_gb_fast (id stk sp argc resw r)
      (let* ((h (nl_gb_ok r)))
        (if (= h 0) -3
          (let* ((blen (sar (ptr-read-u64 r 112) 2)) (len (sar (ptr-read-u64 r 120) 2))
                 (pend (ptr-read-u64 r 104)) (ns (ptr-read-u64 r 64)) (ne (ptr-read-u64 r 72))
                 (pt (if (= pend 3) (+ blen 1) (if (= (logand pend 3) 1) (sar pend 2) -9)))
                 (lo (if (= ns 3) 1 (if (= (logand ns 3) 1) (sar ns 2) -9)))
                 (hi (if (= ne 3) (+ len 1) (if (= (logand ne 3) 1) (sar ne 2) -9)))
                 (a (if (= argc 1) (heap-read-u64 (+ stk (* (- sp 1) 8)) 0) 3)))
            (if (< pt 1) -3
              (if (< lo 1) -3
                (if (> hi (+ len 1)) -3
                  (if (< hi lo) -3
                    (if (> pt hi) -3
                      (if (< pt lo) -3
                        (if (if (> argc 1) 1
                              (if (<= id 51) (if (= a 3) 0 (if (= (logand a 3) 1) 0 1))
                                (if (if (= id 54) 1 (if (= id 55) 1 (if (= id 59) 1 (if (= id 60) 1 0))))
                                    (if (= a 3) 0 (if (= a 5) 0 1))
                                  argc)))
                            -3
                          (let* ((d (ptr-read-u64 h 24)))
                            (if (<= id 51)
                                (let* ((pos (if (= a 3) pt (sar a 2))))
                                  (seq
                                   (ptr-write-u64
                                    resw 0
                                    (if (= id 50)
                                        (if (>= pos lo) (if (< pos hi) (+ (shl (nl_gb_at r d (- pos 1)) 2) 1) 3) 3)
                                      (if (> pos lo) (if (<= pos hi) (+ (shl (nl_gb_at r d (- pos 2)) 2) 1) 3) 3)))
                                   0))
                              (if (<= id 53)
                                  (seq
                                   (ptr-write-u64
                                    resw 0
                                    (if (= id 52)
                                        (if (>= pt lo) (if (< pt hi) (+ (shl (nl_gb_at r d (- pt 1)) 2) 1) 1) 1)
                                      (if (> pt lo) (if (<= pt hi) (+ (shl (nl_gb_at r d (- pt 2)) 2) 1) 1) 1)))
                                   0)
                                (if (= id 56)
                                    (seq (ptr-write-u64 resw 0 (if (<= pt lo) 7 (if (= (nl_gb_at r d (- pt 2)) 10) 7 3))) 0)
                                  (if (= id 57)
                                      (seq (ptr-write-u64 resw 0 (if (>= pt hi) 7 (if (= (nl_gb_at r d (- pt 1)) 10) 7 3))) 0)
                                    (if (if (= id 54) 1 (= id 59))
                                        (seq (ptr-write-u64 resw 0 (+ (shl (nl_gb_bol r d pt lo) 2) 1)) 0)
                                      (seq (ptr-write-u64 resw 0 (+ (shl (nl_gb_eol r d pt hi) 2) 1)) 0))))))))))))))))))
    (defun nl_gb_leaf_run (id stk sp argc resw b)
      (if (= id 64) (nl_gb_forward_line stk sp argc resw (- b 6))
      (if (if (<= id 57) 1 (if (= id 59) 1 (= id 60))) (nl_gb_fast id stk sp argc resw (- b 6))
      (let* ((r (- b 6)) (h (nl_gb_ok r)))
        (if (= h 0) -3
          (if (= id 58) (nl_gb_insert_leaf stk sp argc resw r h)
          (if (= id 65) (nl_gb_insert_char stk sp argc resw r h)
            (let* ((blen (sar (ptr-read-u64 r 112) 2)) (len (sar (ptr-read-u64 r 120) 2))
                   (pend (ptr-read-u64 r 104)) (ns (ptr-read-u64 r 64)) (ne (ptr-read-u64 r 72))
                   (pt (if (= pend 3) (+ blen 1) (if (= (logand pend 3) 1) (sar pend 2) -9)))
                   (lo (if (= ns 3) 1 (if (= (logand ns 3) 1) (sar ns 2) -9)))
                   (hi (if (= ne 3) (+ len 1) (if (= (logand ne 3) 1) (sar ne 2) -9))))
              (if (< pt 1) -3
                (if (< lo 1) -3
                  (if (> hi (+ len 1)) -3
                    (if (< hi lo) -3
                      (if (> pt hi) -3
                        (if (< pt lo) -3
                          (if (= id 61) (nl_gb_delete_run stk sp argc resw r h blen len pt lo hi)
                            (if (= id 66) (nl_gb_delete_char stk sp argc resw r h blen len pt lo hi)
                              -3))))))))))))))))
    ;; The deletion itself (GNU del_range_2) of the 1-based range [S, E) inside the accessible
    ;; portion of a buffer without markers, overlays or properties: the gap moves to its end and
    ;; grows over it; the point and the narrowing follow `nelisp-delete-region'.  TICK is the
    ;; record's fixnum tick word.
    (defun nl_gb_delete_core (resw r h blen len pt s e tick)
      (let* ((delta (- e s)) (ns (ptr-read-u64 r 64)) (ne (ptr-read-u64 r 72))
             (np (if (<= pt s) pt (if (>= pt e) (- pt delta) s))))
        (seq
         (wf_dirty)
         (nl_gb_delete_text r h (- s 1) (- e 1))
         (if (= ns 3) 0
           (let* ((om (sar ns 2)))
             (ptr-write-u64 r 64 (+ (shl (if (<= om s) om (if (>= om e) (- om delta) s)) 2) 1))))
         (if (= ne 3) 0
           (let* ((om (sar ne 2)))
             (ptr-write-u64 r 72 (+ (shl (if (<= om s) om (if (>= om e) (- om delta) s)) 2) 1))))
         (ptr-write-u64 r 40 7)
         (ptr-write-u64 r 96 (+ tick 4))
         (ptr-write-u64 r 104 (if (= np s) 3 (+ (shl np 2) 1)))
         (ptr-write-u64 resw 0 3)
         0)))
    ;; Delete the characters at indexes [S, E) of the valid store H of record R: the gap moves to
    ;; E and grows over the range; the cache follows the text.
    (defun nl_gb_delete_text (r h s e)
      (let* ((d (ptr-read-u64 h 24)) (sb (nl_gb_c2b r d s)))
        (seq
         (nl_gb_move r h e)
         (let* ((eb (sar (ptr-read-u64 r 144) 2)) (cc (sar (ptr-read-u64 r 160) 2)))
           (seq
            (if (>= cc e)
                (seq (ptr-write-u64 r 160 (- (ptr-read-u64 r 160) (shl (- e s) 2)))
                     (ptr-write-u64 r 168 (- (ptr-read-u64 r 168) (shl (- eb sb) 2))))
              (if (> cc s) (nl_gb_cache r s sb) 0))
            (ptr-write-u64 r 112 (+ (shl s 2) 1))
            (ptr-write-u64 r 144 (+ (shl sb 2) 1))
            (ptr-write-u64 r 120 (- (ptr-read-u64 r 120) (shl (- e s) 2)))
            (ptr-write-u64 r 152 (- (ptr-read-u64 r 152) (shl (- eb sb) 2)))
            0)))))
    ;; delete-region (id 61): two fixnum positions inside the accessible portion.
    (defun nl_gb_delete_run (stk sp argc resw r h blen len pt lo hi)
      (if (/= argc 2) -3
        (if (/= (ptr-read-u64 r 48) 3) -3
          (if (/= (ptr-read-u64 r 56) 3) -3
            (if (/= (ptr-read-u64 r 80) 3) -3
              (let* ((x (heap-read-u64 (+ stk (* (- sp 2) 8)) 0)) (y (heap-read-u64 (+ stk (* (- sp 1) 8)) 0))
                     (tick (ptr-read-u64 r 96)))
                (if (/= (logand x 3) 1) -3
                  (if (/= (logand y 3) 1) -3
                    (if (/= (logand tick 3) 1) -3
                      (let* ((sx (sar x 2)) (sy (sar y 2))
                             (s (if (< sx sy) sx sy)) (e (if (< sx sy) sy sx)))
                        (if (< s lo) -3
                          (if (> e hi) -3
                            (nl_gb_delete_core resw r h blen len pt s e tick)))))))))))))
    ;; delete-char (id 66): N characters after the point (before it when negative) inside the
    ;; accessible portion; a second argument (KILLFLAG) is ignored as the reference ignores it.
    (defun nl_gb_delete_char (stk sp argc resw r h blen len pt lo hi)
      (if (if (< argc 1) 1 (> argc 2)) -3
        (if (/= (ptr-read-u64 r 48) 3) -3
          (if (/= (ptr-read-u64 r 56) 3) -3
            (if (/= (ptr-read-u64 r 80) 3) -3
              (let* ((x (heap-read-u64 (+ stk (* (- sp argc) 8)) 0)) (tick (ptr-read-u64 r 96)))
                (if (/= (logand x 3) 1) -3
                  (if (/= (logand tick 3) 1) -3
                    (let* ((pos (+ pt (sar x 2))))
                      (if (< pos lo) -3
                        (if (> pos hi) -3
                          (nl_gb_delete_core resw r h blen len pt
                                             (if (< pt pos) pt pos) (if (< pt pos) pos pt) tick))))))))))))
    ;; insert-char (id 65): CHARACTER (a fixnum) COUNT (nil or a fixnum) times at the point, when
    ;; the buffer has no markers, overlays or properties, INHERIT is nil and the gap is large
    ;; enough.  COUNT <= 0 inserts nothing.  A unibyte buffer keeps the low byte.
    (defun nl_gb_insert_char (stk sp argc resw r h)
      (if (if (< argc 1) 1 (> argc 3)) -3
        (let* ((c (heap-read-u64 (+ stk (* (- sp argc) 8)) 0))
               (cw (if (> argc 1) (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0) 3))
               (iw (if (> argc 2) (heap-read-u64 (+ stk (* (+ (- sp argc) 2) 8)) 0) 3))
               (tick (ptr-read-u64 r 96)) (ne (ptr-read-u64 r 72)) (pend (ptr-read-u64 r 104)))
          (if (/= (logand c 3) 1) -3
            (if (/= iw 3) -3
              (if (if (= cw 3) 0 (/= (logand cw 3) 1)) -3
                (let* ((count (if (= cw 3) 1 (sar cw 2))) (ch (sar c 2)))
                  (if (<= count 0) (seq (ptr-write-u64 resw 0 3) 0)
                    (if (< ch 0) -3
                      (if (> ch 4194303) -3
                        (if (/= (ptr-read-u64 r 48) 3) -3
                          (if (/= (ptr-read-u64 r 56) 3) -3
                            (if (/= (ptr-read-u64 r 80) 3) -3
                              (if (/= (logand tick 3) 1) -3
                                (if (if (= ne 3) 0 (/= (logand ne 3) 1)) -3
                                  (let* ((len (sar (ptr-read-u64 r 120) 2)) (zb (sar (ptr-read-u64 r 152) 2))
                                         (uni (if (= (ptr-read-u64 r 32) 3) 0 1))
                                         (cl (if (= uni 1) 1 (nl_gb_enclen ch)))
                                         (cap (logand (ptr-read-u64 h 0) 9223372036854775807)))
                                    (if (< (- cap zb) (* count cl)) -3
                                      (if (if (= pend 3) 0 (if (/= (logand pend 3) 1) 1 (if (< (sar pend 2) 1) 1 (if (> (sar pend 2) (+ len 1)) 1 0)))) -3
                                        (seq
                                         (if (= pend 3) 0
                                           (seq (nl_gb_move r h (- (sar pend 2) 1))
                                                (ptr-write-u64 r 104 3)))
                                         (let* ((d (ptr-read-u64 h 24)) (p (sar (ptr-read-u64 r 144) 2))
                                                (blen (sar (ptr-read-u64 r 112) 2))
                                                (k 0) (nb (* count cl)))
                                           (seq
                                            (wf_dirty)
                                            (if (= cl 1)
                                                (let* ((v (logand ch 255)) (v8 (* v 72340172838076673)))
                                                  (seq
                                                   (while (<= (+ k 8) count)
                                                     (seq (ptr-write-u64 d (+ p k) v8) (setq k (+ k 8))))
                                                   (while (< k count)
                                                     (seq (ptr-write-u8 d (+ p k) v) (setq k (+ k 1))))))
                                              (seq
                                               (nl_gb_enc (+ d p) ch)
                                               (setq k cl)
                                               (while (< k nb)
                                                 (seq (ptr-write-u8 d (+ p k) (ptr-read-u8 d (+ p (- k cl))))
                                                      (setq k (+ k 1))))))
                                            (if (= uni 1) 0 (if (>= ch 128) (ptr-write-u64 r 136 7) 0))
                                            (nl_gb_cache_ins r blen count nb)
                                            (ptr-write-u64 r 112 (+ (shl (+ blen count) 2) 1))
                                            (ptr-write-u64 r 120 (+ (shl (+ len count) 2) 1))
                                            (ptr-write-u64 r 144 (+ (shl (+ p nb) 2) 1))
                                            (ptr-write-u64 r 152 (+ (shl (+ zb nb) 2) 1))
                                            (if (= ne 3) 0 (ptr-write-u64 r 72 (+ ne (shl count 2))))
                                            (ptr-write-u64 r 40 7)
                                            (ptr-write-u64 r 96 (+ tick 4))
                                            (ptr-write-u64 resw 0 3)
                                            0)))))))))))))))))))))
    ;; The column of the point of record R (its store state already validated): characters from the
    ;; line start, a tab advancing to the next multiple of WIDTH (current_column without wide
    ;; characters or invisible text).  The fixnum word, or 0 when the point is not a fixnum.
    (defun nl_gb_column (r d len width)
      (let* ((pend (ptr-read-u64 r 104)) (ns (ptr-read-u64 r 64))
             (blen (sar (ptr-read-u64 r 112) 2))
             (pt (if (= pend 3) (+ blen 1) (if (= (logand pend 3) 1) (sar pend 2) -9)))
             (lo (if (= ns 3) 1 (if (= (logand ns 3) 1) (sar ns 2) -9))))
        (if (< pt 1) 0
          (if (< lo 1) 0
            (if (> pt (+ len 1)) 0
              (let* ((bol (nl_gb_bol r d pt lo)) (i (nl_gb_c2b r d (- bol 1))) (e (nl_gb_c2b r d (- pt 1)))
                     (gb (sar (ptr-read-u64 r 144) 2))
                     (gapb (- (logand (ptr-read-u64 (- (ptr-read-u64 r 24) 4) 0) 9223372036854775807)
                              (sar (ptr-read-u64 r 152) 2)))
                     (col 0))
                (seq
                 (while (< i e)
                   (let* ((x (ptr-read-u8 d (if (< i gb) i (+ i gapb)))))
                     (seq (if (= x 9)
                              (setq col (* width (+ 1 (/ col width))))
                            (if (= (logand x 192) 128) 0 (setq col (+ col 1))))
                          (setq i (+ i 1)))))
                 (+ (shl col 2) 1))))))))
    ;; The record word V when it is a record of the 20 slots of `nelisp-buffer', else 0.
    (defun nl_gb_record (v)
      (if (= (logand v 7) 6)
          (if (> v 4096)
              (if (= (logand (ptr-read-u64 (- v 6) 0) 63) 12)
                  (if (>= (nl_record_length v) 20) v 0)
                0)
            0)
        0))
    ;; The store operations as builtins on the argument list ARGS (ids 70-80): GNU's
    ;; BUF_CHAR/move_gap/make_gap/insert/del_range/make_buffer_string/find_newline.  They answer
    ;; the value WORD, or 0 for an argument shape or state the Lisp reference owns.
    (defun nl_gb_wprim (id args env)
      (let* ((argc (m5_list_len args 0))
             (b (if (> argc 0) (nl_gb_record (wf_arg_word args 0)) 0)))
        (if (= b 0) 0
          (let* ((r (- b 6)) (h (nl_gb_ok r)))
            (if (= h 0) 0
              (let* ((len (sar (ptr-read-u64 r 120) 2))
                     (x (if (> argc 1) (wf_arg_word args 1) 3))
                     (y (if (> argc 2) (wf_arg_word args 2) 3))
                     (fx (if (= (logand x 3) 1) 1 0)) (fy (if (= (logand y 3) 1) 1 0)))
                (if (= id 70)
                    (if (= argc 2)
                        (if (= fx 1)
                            (if (>= (sar x 2) 0)
                                (if (< (sar x 2) len)
                                    (+ (shl (nl_gb_at r (ptr-read-u64 h 24) (sar x 2)) 2) 1)
                                  0)
                              0)
                          0)
                      0)
                  (if (= id 71)
                      (if (= argc 2)
                          (if (= fx 1)
                              (if (>= (sar x 2) 0)
                                  (if (<= (sar x 2) len)
                                      (seq (wf_dirty) (nl_gb_move r h (sar x 2)) 3)
                                    0)
                                0)
                            0)
                        0)
                    (if (= id 72)
                        (if (= argc 2)
                            (if (= fx 1)
                                (if (>= (sar x 2) 0)
                                    (if (= (nl_gb_grow r (sar x 2)) 1) 3 0)
                                  0)
                              0)
                          0)
                      (if (= id 73)
                          (if (= argc 2) (nl_gb_insert_w b x) 0)
                        (if (= id 74)
                            (if (= argc 3)
                                (if (= fx 1)
                                    (if (= fy 1)
                                        (if (>= (sar x 2) 0)
                                            (if (<= (sar x 2) (sar y 2))
                                                (if (<= (sar y 2) len)
                                                    (seq (wf_dirty)
                                                         (nl_gb_delete_text r h (sar x 2) (sar y 2))
                                                         3)
                                                  0)
                                              0)
                                          0)
                                      0)
                                  0)
                              0)
                          (if (= id 75)
                              (if (= argc 3)
                                  (if (= fx 1)
                                      (if (= fy 1) (nl_gb_substring b (sar x 2) (sar y 2)) 0)
                                    0)
                                0)
                            (nl_gb_wprim_more id args r h len argc x y fx fy)))))))))))))
    ;; bufread: insert-file-contents reads straight into the gap (GNU fileio.c Finsert_file_contents:
    ;; emacs_read into BYTE_POS_ADDR (PT_BYTE) after make_gap, then the decoder runs on the gap in
    ;; place, decode_coding_gap).  1 when the byte at physical Q of D is a continuation byte.
    (defun nl_gb_cont (d q) (if (= (logand (ptr-read-u8 d q) 192) 128) 1 0))
    ;; The number of characters of the bytes at physical [LO, HI) of D when they are well-formed
    ;; UTF-8 (shortest form, no surrogates, at most U+10FFFF; ASCII eight bytes per load), else -1.
    ;; Well-formed UTF-8 is its own internal encoding, so no byte moves.
    (defun nl_gb_utf8_chars (d lo hi)
      (let* ((p lo) (n 0) (bad 0))
        (seq
         (while (if (= bad 0) (< p hi) 0)
           (if (if (<= (+ p 8) hi) (= (logand (ptr-read-u64 d p) -9187201950435737472) 0) 0)
               (seq (setq p (+ p 8)) (setq n (+ n 8)))
             (let* ((c (ptr-read-u8 d p)))
               (if (< c 128) (seq (setq p (+ p 1)) (setq n (+ n 1)))
                 (let* ((k (if (< c 194) 0 (if (< c 224) 1 (if (< c 240) 2 (if (< c 245) 3 0))))))
                   (if (= k 0) (setq bad 1)
                     (if (> (+ p k) (- hi 1)) (setq bad 1)
                       (let* ((b1 (ptr-read-u8 d (+ p 1))))
                         (if (= (nl_gb_cont d (+ p 1)) 0) (setq bad 1)
                           (if (if (= c 224) (< b1 160)
                                 (if (= c 237) (>= b1 160)
                                   (if (= c 240) (< b1 144) (if (= c 244) (>= b1 144) 0))))
                               (setq bad 1)
                             (if (if (>= k 2) (= (nl_gb_cont d (+ p 2)) 0) 0) (setq bad 1)
                               (if (if (>= k 3) (= (nl_gb_cont d (+ p 3)) 0) 0) (setq bad 1)
                                 (seq (setq p (+ (+ p k) 1)) (setq n (+ n 1)))))))))))))))
         (if (= bad 0) n -1))))
    ;; The tail of `nl_gb_insert_file': GOT bytes (-1: the read failed) were read at the gap start
    ;; GB of the store data D, OFF of them to be skipped (the BEG offset), M = GOT - OFF being the
    ;; text.  Count its characters under the representation rules (LIT non-nil: the bytes
    ;; undecoded; else UTF-8 text), then account for them exactly as `nl_gb_insert_w' does for a
    ;; string.  The fixnum word of the character count, or the fixnum word -1 (nothing inserted:
    ;; the Lisp route decodes it).
    (defun nl_gb_insert_file_done (r d gb off got lit)
      (if (< got 0) -3
        (let* ((m (if (> got off) (- got off) 0)))
          (seq
           (if (> off 0) (if (> m 0) (nl_gb_copy d gb (+ gb off) m) 0) 0)
           (let* ((uni (if (= (ptr-read-u64 r 32) 3) 0 1))
                  (n (if (= (nl_gb_ascii_p d gb (+ gb m)) 1) m
                       (if (/= lit 3) (if (= uni 1) m -1)
                         (if (= uni 1) -1 (nl_gb_utf8_chars d gb (+ gb m)))))))
             (if (< n 0) -3
               (let* ((blen (sar (ptr-read-u64 r 112) 2)))
                 (seq (wf_dirty)
                      (if (= uni 1) 0 (if (< n m) (ptr-write-u64 r 136 7) 0))
                      (nl_gb_cache_ins r blen n m)
                      (ptr-write-u64 r 112 (+ (ptr-read-u64 r 112) (shl n 2)))
                      (ptr-write-u64 r 120 (+ (ptr-read-u64 r 120) (shl n 2)))
                      (ptr-write-u64 r 144 (+ (ptr-read-u64 r 144) (shl m 2)))
                      (ptr-write-u64 r 152 (+ (ptr-read-u64 r 152) (shl m 2)))
                      (+ (shl n 2) 1)))))))))
    ;; Open the file, read OFF + NB bytes into the gap of the store H of record R (made large
    ;; enough by the caller), close it, and account for the text.
    (defun nl_gb_insert_file_read (args r h off nb lit)
      (let* ((d (ptr-read-u64 h 24)) (gb (sar (ptr-read-u64 r 144) 2))
             (fd (nl_os_open_read (nl_bi_make_cpath (wf_arg_ptr args 1))))
             (want (+ off nb)) (got 0) (stop 0))
        (if (< fd 0) -3
          (seq
           (while (if (= stop 0) (< got want) 0)
             (let* ((k (nl_os_read_file_handle fd (+ (+ d gb) got) (- want got))))
               (if (> k 0) (setq got (+ got k))
                 (seq (setq stop 1) (if (< k 0) (setq got -1) 0)))))
           (nl_os_close_handle fd)
           (nl_gb_insert_file_done r d gb off got lit)))))
    ;; (nelisp--gb-insert-file B PATH OFF NBYTES LITERAL): the NBYTES bytes of the file PATH from
    ;; byte OFF, read into the gap of B and counted as characters; see `nl_gb_insert_file_done'.  0
    ;; for an argument shape or state the Lisp reference owns.
    (defun nl_gb_insert_file (args r)
      (let* ((pw (nl_gb_strw (wf_arg_word args 1))) (beg (wf_arg_word args 2))
             (nbw (wf_arg_word args 3)) (lit (wf_arg_word args 4)))
        (if (/= (logand pw 7) 4) 0
          (if (/= (logand beg 3) 1) 0
            (if (/= (logand nbw 3) 1) 0
              (let* ((off (sar beg 2)) (nb (sar nbw 2)))
                (if (< off 0) 0
                  (if (< nb 0) 0
                    (if (= (nl_gb_grow r (+ off nb)) 0) 0
                      (let* ((h2 (nl_gb_ok r)))
                        (if (= h2 0) 0
                          (nl_gb_insert_file_read args r h2 off nb lit))))))))))))
    (defun nl_gb_wprim_more (id args r h len argc x y fx fy)
      (let* ((d (ptr-read-u64 h 24)))
        (if (= id 76)
            ;; (gb-find-char B C POS LIM DIR): a byte-sized C (ASCII, any byte of a unibyte buffer,
            ;; or nothing to find in an ASCII-only one); a wider character is the reference's
            (if (= argc 5)
                (let* ((pos (wf_arg_word args 2)) (lim (wf_arg_word args 3)) (dir (wf_arg_word args 4)))
                  (if (= fx 1)
                      (if (= (logand pos 3) 1)
                          (if (= (logand lim 3) 1)
                              (if (= (logand dir 3) 1)
                                  (let* ((c (sar x 2)) (p (sar pos 2)) (l (sar lim 2))
                                         (uni (if (= (ptr-read-u64 r 32) 3) 0 1))
                                         (ascii (if (= (sar (ptr-read-u64 r 152) 2) len) 1 0)))
                                    (if (< c 0) 0
                                      (if (> c 2147483647) 0
                                        (if (if (< c 128) 0 (if (= uni 1) 0 (= ascii 0))) 0
                                          (if (> (sar dir 2) 0)
                                              (if (< p 0) 0
                                                (if (> l len) 0
                                                  (+ (shl (if (< p l) (if (< c (if (= uni 1) 256 128)) (nl_gb_ffind r d p (- l 1) c) -1) -1) 2) 1)))
                                            (if (> p len) 0
                                              (if (< l 0) 0
                                                (+ (shl (if (> p l) (if (< c (if (= uni 1) 256 128)) (nl_gb_rfind r d (- p 1) l c) -1) -1) 2) 1))))))))
                                0)
                            0)
                        0)
                    0))
              0)
          (if (= id 77)
              (if (= argc 3)
                  (if (= fx 1)
                      (if (= fy 1)
                          (if (>= (sar x 2) 0)
                              (if (<= (sar x 2) (sar y 2))
                                  (if (<= (sar y 2) len)
                                      (+ (shl (nl_gb_count r d (sar x 2) (sar y 2)) 2) 1)
                                    0)
                                0)
                            0)
                        0)
                    0)
                0)
            (if (= id 78)
                (if (= argc 3)
                    (if (= fx 1)
                        (if (>= (sar x 2) 0) (nl_gb_overwrite_w (wf_arg_word args 0) (sar x 2) y) 0)
                      0)
                  0)
              (if (= id 79)
                  ;; (gb-count-lines B LO HI): the lines between two 1-based positions
                  (if (= argc 3)
                      (if (= fx 1)
                          (if (= fy 1)
                              (if (>= (sar x 2) 1)
                                  (if (<= (sar x 2) (sar y 2))
                                      (if (<= (sar y 2) (+ len 1))
                                          (if (= x y) 1
                                            (+ (shl (+ (nl_gb_count r d (- (sar x 2) 1) (- (sar y 2) 1))
                                                       (if (= (nl_gb_at r d (- (sar y 2) 2)) 10) 0 1)) 2) 1))
                                        0)
                                    0)
                                0)
                            0)
                        0)
                    0)
                (if (= id 80)
                    ;; (gb-column B WIDTH): point's column, tabs to multiples of WIDTH
                    (if (= argc 2)
                        (if (= fx 1)
                            (if (> (sar x 2) 0)
                                (nl_gb_column r d len (sar x 2))
                              0)
                          0)
                      0)
                  (if (= id 81) (if (= argc 5) (nl_gb_insert_file args r) 0) 0))))))))
    ;; The reference route of the ids 50-60 and 70-78: the Lisp reference defun of the prelude on ARGS.
    (defun nl_gb_slow_w (id args env)
      (let* ((buf (alloc-bytes 72 8)))
        (if (= id 50)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8387498203844536419) (ptr-write-u64 (+ buf 16) 0 8243107278864872037) (ptr-write-u64 (+ buf 24) 0 1701015141)
                 (nl_buf_delegate args env buf 28))
              (if (= id 51)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7378411511848462435) (ptr-write-u64 (+ buf 16) 0 7306638986517508719) (ptr-write-u64 (+ buf 24) 0 435459876210)
                 (nl_buf_delegate args env buf 29))
              (if (= id 52)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7956021537170812774) (ptr-write-u64 (+ buf 16) 0 8227357856964488551) (ptr-write-u64 (+ buf 24) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 32))
              (if (= id 53)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7956000603348759152) (ptr-write-u64 (+ buf 16) 0 8227357856964488551) (ptr-write-u64 (+ buf 24) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 32))
              (if (= id 54)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7450469105669138796) (ptr-write-u64 (+ buf 16) 0 8083230630112816745) (ptr-write-u64 (+ buf 24) 0 3273676477860246383) (ptr-write-u64 (+ buf 32) 0 7164775599194924402) (ptr-write-u64 (+ buf 40) 0 101)
                 (nl_buf_delegate args env buf 41))
              (if (= id 55)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7236832896880634220) (ptr-write-u64 (+ buf 16) 0 8028075807021690925) (ptr-write-u64 (+ buf 24) 0 7310016635638787438) (ptr-write-u64 (+ buf 32) 0 6644590)
                 (nl_buf_delegate args env buf 35))
              (if (= id 56)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7378429104001675106) (ptr-write-u64 (+ buf 16) 0 111477728309861)
                 (nl_buf_delegate args env buf 22))
              (if (= id 57)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7378429104001675109) (ptr-write-u64 (+ buf 16) 0 111477728309861)
                 (nl_buf_delegate args env buf 22))
              (if (= id 58)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8227360128952921705) (ptr-write-u64 (+ buf 16) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 24))
              (if (= id 59)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3273113496651526000) (ptr-write-u64 (+ buf 16) 0 7164775599194924402) (ptr-write-u64 (+ buf 24) 0 101)
                 (nl_buf_delegate args env buf 25))
              (if (= id 60)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3273113509536427888) (ptr-write-u64 (+ buf 16) 0 7164775599194924402) (ptr-write-u64 (+ buf 24) 0 101)
                 (nl_buf_delegate args env buf 25))
              (if (= id 61)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8227343644867978596) (ptr-write-u64 (+ buf 16) 0 7309955097514567525) (ptr-write-u64 (+ buf 24) 0 28538298447324518)
                 (nl_buf_delegate args env buf 31))
              (if (= id 62)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8299415468148946274) (ptr-write-u64 (+ buf 16) 0 7453010373645656693) (ptr-write-u64 (+ buf 24) 0 7954890096655036973) (ptr-write-u64 (+ buf 32) 0 25955)
                 (nl_buf_delegate args env buf 34))
              (if (= id 63)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8299415468148946274) (ptr-write-u64 (+ buf 16) 0 7453010373645656693) (ptr-write-u64 (+ buf 24) 0 8101820080786992685) (ptr-write-u64 (+ buf 32) 0 8227358973673894501) (ptr-write-u64 (+ buf 40) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 48))
              (if (= id 64)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3270864992319205222) (ptr-write-u64 (+ buf 16) 0 7378429103817255276) (ptr-write-u64 (+ buf 24) 0 111477728309861)
                 (nl_buf_delegate args env buf 30))
              (if (= id 65)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7146496218384002665) (ptr-write-u64 (+ buf 16) 0 7306638986518356328) (ptr-write-u64 (+ buf 24) 0 435459876210)
                 (nl_buf_delegate args env buf 29))
              (if (= id 66)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7146479734299059556) (ptr-write-u64 (+ buf 16) 0 7306638986518356328) (ptr-write-u64 (+ buf 24) 0 435459876210)
                 (nl_buf_delegate args env buf 29))
              (if (= id 70)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3274786980020183655) (ptr-write-u64 (+ buf 16) 0 7164775599194924402) (ptr-write-u64 (+ buf 24) 0 101)
                 (nl_buf_delegate args env buf 25))
              (if (= id 71)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3271150925299671655) (ptr-write-u64 (+ buf 16) 0 7164775599194924402) (ptr-write-u64 (+ buf 24) 0 101)
                 (nl_buf_delegate args env buf 25))
              (if (= id 72)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8243121572516029031) (ptr-write-u64 (+ buf 16) 0 8243107278864868726) (ptr-write-u64 (+ buf 24) 0 1701015141)
                 (nl_buf_delegate args env buf 28))
              (if (= id 73)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8243121611019739751) (ptr-write-u64 (+ buf 16) 0 7310016635638787444) (ptr-write-u64 (+ buf 24) 0 6644590)
                 (nl_buf_delegate args env buf 27))
              (if (= id 74)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8387229063775609447) (ptr-write-u64 (+ buf 16) 0 7310016635638787429) (ptr-write-u64 (+ buf 24) 0 6644590)
                 (nl_buf_delegate args env buf 27))
              (if (= id 75)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8391158787304415847) (ptr-write-u64 (+ buf 16) 0 7378429103850809714) (ptr-write-u64 (+ buf 24) 0 111477728309861)
                 (nl_buf_delegate args env buf 30))
              (if (= id 76)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 3270860628342694503) (ptr-write-u64 (+ buf 16) 0 7378429104034506851) (ptr-write-u64 (+ buf 24) 0 111477728309861)
                 (nl_buf_delegate args env buf 30))
              (if (= id 77)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8389772277103551079) (ptr-write-u64 (+ buf 16) 0 7306638986517966381) (ptr-write-u64 (+ buf 24) 0 435459876210)
                 (nl_buf_delegate args env buf 29))
              (if (= id 78)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8607053397204230759) (ptr-write-u64 (+ buf 16) 0 7378429103817648498) (ptr-write-u64 (+ buf 24) 0 111477728309861)
                 (nl_buf_delegate args env buf 30))
              (if (= id 79)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8389772277103551079) (ptr-write-u64 (+ buf 16) 0 8227358973757058093) (ptr-write-u64 (+ buf 24) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 32))
              (if (= id 81)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8243121611019739751) (ptr-write-u64 (+ buf 16) 0 8227343610574941556) (ptr-write-u64 (+ buf 24) 0 7305804402515076709)
                 (nl_buf_delegate args env buf 32))
              (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7887329548070380135) (ptr-write-u64 (+ buf 16) 0 7310016635638787438) (ptr-write-u64 (+ buf 24) 0 6644590)
                 (nl_buf_delegate args env buf 27))))))))))))))))))))))))))))))))
    ;; The leaf bodies as builtins (ids 50-61), ARGS a list of any length.
    (defun nl_gb_prim_w (id args env)
      (let* ((argc (m5_list_len args 0)) (stk (alloc-bytes (+ 8 (* argc 8)) 8)) (res (alloc-bytes 8 8)) (i 0))
        (seq
         (while (< i argc)
           (seq (ptr-write-u64 stk (* i 8) (wf_arg_word args i)) (setq i (+ i 1))))
         (let* ((b (nl_buf_current env)))
           (if (= b 0) (nl_gb_slow_w id args env)
             (if (= (nl_gb_leaf_run id stk argc argc res b) 0) (heap-read-u64 res 0)
               (nl_gb_slow_w id args env)))))))
    ;; buffer-substring (62) and buffer-substring-no-properties (63) on the current buffer: the
    ;; accessible range of two fixnums, a buffer without text properties for 62 (a range that
    ;; needs intervals is the reference's); the new string word.
    (defun nl_gb_bufsub_w (id args env)
      (let* ((argc (m5_list_len args 0)) (b (nl_buf_current env)))
        (if (/= argc 2) (nl_gb_slow_w id args env)
          (if (= b 0) (nl_gb_slow_w id args env)
            (let* ((r (- b 6)) (x (wf_arg_word args 0)) (y (wf_arg_word args 1))
                   (len (ptr-read-u64 r 120)) (ns (ptr-read-u64 r 64)) (ne (ptr-read-u64 r 72)))
              (if (/= (logand x 3) 1) (nl_gb_slow_w id args env)
                (if (/= (logand y 3) 1) (nl_gb_slow_w id args env)
                  (if (if (= id 62) (/= (ptr-read-u64 r 80) 3) 0) (nl_gb_slow_w id args env)
                    (if (/= (logand len 3) 1) (nl_gb_slow_w id args env)
                      (let* ((n (sar len 2))
                             (lo (if (= ns 3) 1 (if (= (logand ns 3) 1) (sar ns 2) -9)))
                             (hi (if (= ne 3) (+ n 1) (if (= (logand ne 3) 1) (sar ne 2) -9)))
                             (sx (sar x 2)) (sy (sar y 2))
                             (s (if (< sx sy) sx sy)) (e (if (< sx sy) sy sx)))
                        (if (< lo 1) (nl_gb_slow_w id args env)
                          (if (< s lo) (nl_gb_slow_w id args env)
                            (if (> e hi) (nl_gb_slow_w id args env)
                              (let* ((w (nl_gb_substring b (- s 1) (- e 1))))
                                (if (= w 0) (nl_gb_slow_w id args env) w)))))))))))))))
    ;; The store operations as builtins (ids 70-80).
    (defun nl_gb_wprim_w (id args env)
      (let* ((v (nl_gb_wprim id args env)))
        (if (= v 0) (nl_gb_slow_w id args env) v)))
    ;; bufnative: the buffer read path as leaf WORD bodies (native-surface clause 1: tagged
    ;; object access, a record's slots).  GNU buffer.h/editfns.c: PT is the buffer's point,
    ;; BEGV/ZV its narrowing, Z its size; here the `nelisp-buffer' record keeps the pending
    ;; point (slot 11, nil = the gap position BLEN+1), BLEN (12), the text length LEN (13)
    ;; and the narrowing bounds (6, 7), all fixnum words or nil.  Slot K of the record is the
    ;; word at payload + 16 + 8K (Lisp `aref' index K+1).  Ids: 40 point, 41 point-min,
    ;; 42 point-max, 43 goto-char, 45 buffer-size, 46 bobp, 47 eobp, 48 narrow-to-region,
    ;; 49 widen.  The Lisp references
    ;; are the prelude's nelisp--NAME-reference defuns.  Every body answers 0 with RESW
    ;; written or -3 with nothing written (not a buffer, an argument shape the Lisp owns).
    ;; `nl_symview_object' with the entry validated first: a symbol whose cell vector slot was
    ;; not yet written by a Lisp access (a defun compiled before the first buffer call) can
    ;; answer a small even word, which that function then dereferences.  An object word is a
    ;; tag-6 heap address, so anything else is no object (the Lisp reference route runs and
    ;; makes the cell).
    (defun nl_buf_symobj (view)
      (let* ((mp (nl_symcells_mirror)))
        (if (= mp 0) 0
          (let* ((e (nl_symcell_entry mp view)))
            (if (<= e 4096) 0
              (if (/= (logand e 7) 6) 0
                (if (= (heap-read-u64 (- e 6) 8) 3) 0
                  (let* ((u (heap-read-u64 268436336 0)))
                    (if (= u 0) e
                      (if (= (heap-read-u64 view 16) (heap-read-u64 u 16)) 0 e))))))))))
;; The symbol OBJECT of `nelisp--current-buffer', kept with the compaction epoch it was read at in
    ;; the two words of the BSS block `nl_buf_cache' (zero-fresh on every start, never in the cold
    ;; image: the low-address control page is restored from the image and holds other tables, which
    ;; is what made the first version read a stale 16 there): `nl_gc_compact' bumps
    ;; `nl_mxcache_epoch' before it moves objects, so a stale object is never used.  Cache the
    ;; symbol, never the buffer: a let-binding changes the value cell, not the symbol.  The name
    ;; view needed to find the object is a temporary of this slow path.
    (defun nl_buf_refetch (env)
      (let* ((buf (alloc-bytes 24 1)) (view (alloc-bytes 32 8)))
        (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126)
             (ptr-write-u64 (+ buf 8) 0 3275364210996049251)
             (ptr-write-u64 (+ buf 16) 0 125779835254114)
             (nl_alloc_symbol buf 22 view)
             (let* ((obj (nl_buf_symobj view)))
               (if (= obj 0) 0
                 (seq (ptr-write-u64 (data-addr nl_buf_cache) 0 obj)
                      (ptr-write-u64 (data-addr nl_buf_cache) 8 (heap-read-u64 (data-addr nl_mxcache_epoch) 0))
                      obj))))))
    
    ;; The value of `nelisp--current-buffer' when it is a record of at least the 16 slots of
    ;; `nelisp-buffer' (GNU current_buffer), else 0.  The symbol object's value cell is read
    ;; as `nl_gv_fast' reads it (GNU find_symbol_value on a PLAINVAL: kind 4, redirect 0, no
    ;; worker, no root pin); a let-binding of the variable is a shallow binding, so the cell
    ;; is the current value.  A value that is a record cannot be the unbound marker.
    (defun nl_buf_current (env)
      (let* ((obj (heap-read-u64 (data-addr nl_buf_cache) 0))
             (sym (if (= obj 0) (nl_buf_refetch env)
                    (if (= (heap-read-u64 (data-addr nl_buf_cache) 8) (heap-read-u64 (data-addr nl_mxcache_epoch) 0)) obj
                      (nl_buf_refetch env)))))
        (if (= sym 0) 0
          (if (/= (logand (ptr-read-u64 (- sym 6) 0) 831) 4) 0
            (if (/= (ptr-read-u64 env 168) 0) 0
              (if (/= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0) 0
                (if (= (ptr-read-u64 (data-addr nl_root_pin_control) 48) 2) 0
                  (let* ((g (ptr-read-u64 (- sym 6) 16)))
                    (if (= (logand g 7) 6)
                        (if (> g 4096)
                            (if (= (logand (ptr-read-u64 (- g 6) 0) 63) 12)
                                (if (>= (nl_record_length g) 20) g 0)
                              0)
                          0)
                      0)))))))))
    (defun nl_buf_leaf_run (id stk sp argc resw env)
      (let* ((b (nl_buf_current env)))
        (if (= b 0) -3
          (if (>= id 50) (nl_gb_leaf_run id stk sp argc resw b)
          (let* ((r (- b 6)))
            (if (<= id 42)
                ;; 40 point, 41 point-min, 42 point-max
                (if (= id 40)
                    (let* ((pend (ptr-read-u64 r 104)))
                      (if (= pend 3)
                          (let* ((blen (ptr-read-u64 r 112)))
                            (if (= (logand blen 3) 1) (seq (ptr-write-u64 resw 0 (+ blen 4)) 0) -3))
                        (if (= (logand pend 3) 1) (seq (ptr-write-u64 resw 0 pend) 0) -3)))
                  (if (= id 41)
                      (let* ((ns (ptr-read-u64 r 64)))
                        (if (= ns 3) (seq (ptr-write-u64 resw 0 5) 0)
                          (if (= (logand ns 3) 1) (seq (ptr-write-u64 resw 0 ns) 0) -3)))
                    (let* ((ne (ptr-read-u64 r 72)))
                      (if (= ne 3)
                          (let* ((len (ptr-read-u64 r 120)))
                            (if (= (logand len 3) 1) (seq (ptr-write-u64 resw 0 (+ len 4)) 0) -3))
                        (if (= (logand ne 3) 1) (seq (ptr-write-u64 resw 0 ne) 0) -3)))))
              (if (= id 45)
                  (let* ((len (ptr-read-u64 r 120)))
                    (if (= argc 0)
                        (if (= (logand len 3) 1) (seq (ptr-write-u64 resw 0 len) 0) -3)
                      -3))
                (nl_buf_leaf_more id stk sp argc resw b))))))))
    ;; 43 goto-char, 46 bobp, 47 eobp, 48 narrow-to-region, 49 widen: B the buffer record.
    (defun nl_buf_leaf_more (id stk sp argc resw b)
      (let* ((pend (ptr-read-u64 (- b 6) 104)) (blen (ptr-read-u64 (- b 6) 112))
             (len (ptr-read-u64 (- b 6) 120)) (ns (ptr-read-u64 (- b 6) 64))
             (ne (ptr-read-u64 (- b 6) 72)))
        (if (/= (logand blen 3) 1) -3
          (if (/= (logand len 3) 1) -3
            (let* ((pt (if (= pend 3) (+ blen 4) pend))
                   (pmin (if (= ns 3) 5 ns))
                   (pmax (if (= ne 3) (+ len 4) ne)))
              (if (/= (logand pt 3) 1) -3
                (if (/= (logand pmin 3) 1) -3
                  (if (/= (logand pmax 3) 1) -3
                    (if (= id 46) (seq (ptr-write-u64 resw 0 (if (= pt pmin) 7 3)) 0)
                      (if (= id 47) (seq (ptr-write-u64 resw 0 (if (= pt pmax) 7 3)) 0)
                        (if (= id 43)
                            ;; Fgoto_char on an integer: clamp into BEGV..ZV and record the
                            ;; pending point (nil when it is the gap position, which
                            ;; `nelisp-buffer--settle' relies on); the value is the
                            ;; argument, not the clamp.
                            (let* ((a (if (= argc 1) (heap-read-u64 (+ stk (* (- sp 1) 8)) 0) 3)))
                              (if (= (logand a 3) 1)
                                  (let* ((c (if (< a pmin) pmin (if (> a pmax) pmax a))))
                                    (seq (wf_dirty)
                                         (ptr-write-u64 (- b 6) 104 (if (= c (+ blen 4)) 3 c))
                                         (ptr-write-u64 resw 0 a)
                                         0))
                                -3))
                          (if (= id 48)
                              ;; Fnarrow_to_region on two fixnums inside the whole buffer:
                              ;; START and END swap into order, the bounds are stored and
                              ;; point is moved into them (pend nil when it is the gap
                              ;; position).  Markers, other types and out-of-range positions
                              ;; are the Lisp's (args-out-of-range data).
                              (let* ((x (if (= argc 2) (heap-read-u64 (+ stk (* (- sp 2) 8)) 0) 3))
                                     (y (if (= argc 2) (heap-read-u64 (+ stk (* (- sp 1) 8)) 0) 3)))
                                (if (= (logand x 3) 1)
                                    (if (= (logand y 3) 1)
                                        (let* ((lo (if (< y x) y x)) (hi (if (< y x) x y)))
                                          (if (< lo 5) -3
                                            (if (> hi (+ len 4)) -3
                                              (let* ((np (if (< pt lo) lo (if (> pt hi) hi pt))))
                                                (seq (wf_dirty)
                                                     (ptr-write-u64 (- b 6) 64 lo)
                                                     (ptr-write-u64 (- b 6) 72 hi)
                                                     (ptr-write-u64 (- b 6) 104 (if (= np (+ blen 4)) 3 np))
                                                     (ptr-write-u64 resw 0 3)
                                                     0)))))
                                      -3)
                                  -3))
                            (if (= id 49)
                                (if (= argc 0)
                                    (seq (wf_dirty)
                                         (ptr-write-u64 (- b 6) 64 3)
                                         (ptr-write-u64 (- b 6) 72 3)
                                         (ptr-write-u64 resw 0 3)
                                         0)
                                  -3)
                              -3)))))))))))))
    ;; The reference route of the seven names (the list entry (builtin NAME)): the same leaf
    ;; body on the argument words, else the Lisp reference defun of the prelude on ARGS.
    (defun nl_buf_delegate (args env buf len)
      (let* ((sym_slot (alloc-bytes 32 8)))
        (seq (nl_alloc_symbol buf len sym_slot)
             (let* ((function-word (nl_lookup_fn_autoload_w (+ env 0) (+ env 64) sym_slot 0)))
               (if (and (/= function-word 0) (/= function-word 2))
                   (nl_apply_function_w function-word args env 0)
                 (nl_abi_fail (nl_cons_stash_void_function env sym_slot) 0))))))
    (defun nl_buf_slow_w (id args env)
      (let* ((buf (alloc-bytes 48 8)))
        (if (= id 40)
            (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7309955123267596144) (ptr-write-u64 (+ buf 16) 0 28538298447324518)
                 (nl_buf_delegate args env buf 23))
          (if (= id 41)
              (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7596778124535754608) (ptr-write-u64 (+ buf 16) 0 7310016635638787438) (ptr-write-u64 (+ buf 24) 0 6644590)
                   (nl_buf_delegate args env buf 27))
            (if (= id 42)
                (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7020317372232331120) (ptr-write-u64 (+ buf 16) 0 7310016635638787448) (ptr-write-u64 (+ buf 24) 0 6644590)
                     (nl_buf_delegate args env buf 27))
              (if (= id 43)
                  (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7018969066051497831) (ptr-write-u64 (+ buf 16) 0 7310016635638787442) (ptr-write-u64 (+ buf 24) 0 6644590)
                       (nl_buf_delegate args env buf 27))
                (if (= id 45)
                    (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8299415468148946274) (ptr-write-u64 (+ buf 16) 0 7306638986517510761) (ptr-write-u64 (+ buf 24) 0 435459876210)
                         (nl_buf_delegate args env buf 29))
                  (if (= id 46)
                      (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7378429104001019746) (ptr-write-u64 (+ buf 16) 0 111477728309861)
                           (nl_buf_delegate args env buf 22))
                    (if (= id 47)
                        (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7378429104001019749) (ptr-write-u64 (+ buf 16) 0 111477728309861)
                             (nl_buf_delegate args env buf 22))
                      (if (= id 48)
                          (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 8371478602896793966) (ptr-write-u64 (+ buf 16) 0 7957694959391485295) (ptr-write-u64 (+ buf 24) 0 7954890096655036973) (ptr-write-u64 (+ buf 32) 0 25955)
                               (nl_buf_delegate args env buf 34))
                        (seq (ptr-write-u64 (+ buf 0) 0 3255381746650998126) (ptr-write-u64 (+ buf 8) 0 7309955097346468215) (ptr-write-u64 (+ buf 16) 0 28538298447324518)
                             (nl_buf_delegate args env buf 23))))))))))))
    (defun nl_buf_prim_w (id args env)
      (if (>= id 50) (nl_gb_prim_w id args env)
      (let* ((argc (m5_list_len args 0)) (stk (alloc-bytes 16 8)) (res (alloc-bytes 8 8)))
        (seq
         (if (> argc 0) (ptr-write-u64 stk 0 (wf_arg_word args 0)) 0)
         (if (> argc 1) (ptr-write-u64 stk 8 (wf_arg_word args 1)) 0)
         (if (= (nl_buf_leaf_run id stk (if (> argc 2) 0 argc) (if (> argc 2) 0 argc) res env) 0)
             (heap-read-u64 res 0)
           (nl_buf_slow_w id args env))))))
    ;; Primitive ID on the ARGC words ending at STK[SP-1]: 0 with the value
    ;; in RESW, -3 when it declines (a type it leaves to the full builtin).
    (defun m5_fx_primitive_run (id stk sp argc resw env scratch)
      (if (or (= id -14) (= id -15) (= id -16))
          ;; Only canonical ordinary vectors enter unchecked storage APIs.
          ;; Seven-element vectors include the nemacs char-table bridge;
          ;; leave all of them, other representations and errors to APPLY.
          (if (if (= id -16) (= argc 1) (if (= id -14) (= argc 2) (= argc 3)))
              (let* ((a (heap-read-u64 (+ stk (* (- sp argc) 8)) 0)))
                (if (if (= (logand a 7) 6) (> a 4096) 0)
                    (if (= (logand (ptr-read-u64 (- a 6) 0) 63) 12)
                        ;; corecall: a record (GNU data.c Faref/Faset, RECORDP uses
                        ;; PVSIZE).  Index 0 is the type word, K > 0 is data slot K-1,
                        ;; K up to the slot count; `length' of a record and an aset of
                        ;; its type word stay with the full builtin.
                        (if (= id -16) -3
                          (let* ((n (nl_record_length a))
                                 (b (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0)))
                            (if (= (logand b 3) 1)
                                (let* ((index (sar b 2)))
                                  (if (if (>= index 0) (<= index n) 0)
                                      (if (= id -14)
                                          (seq (ptr-write-u64 resw 0
                                                              (if (= index 0) (nl_record_type_word a)
                                                                (nl_record_slot_word a (- index 1))))
                                               0)
                                        (if (= index 0) -3
                                          (let* ((value (heap-read-u64 (+ stk (* (+ (- sp argc) 2) 8)) 0)))
                                            (seq (wf_dirty)
                                                 (ptr-write-u64 (+ (nl_word_payload a) (+ 16 (* (- index 1) 8))) 0
                                                                (nl_word_canon value))
                                                 (ptr-write-u64 resw 0 value) 0))))
                                    -3))
                              -3)))
                    (if (= (logand (ptr-read-u64 (- a 6) 0) 63) 8)
                        (let* ((n (vector-len a)))
                          (if (if (>= n 0) (/= n 7) 0)
                              (if (= id -16)
                                  (seq (ptr-write-u64 resw 0 (+ (shl n 2) 1)) 0)
                                (let* ((b (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0)))
                                  (if (= (logand b 3) 1)
                                      (let* ((index (sar b 2)))
                                        (if (if (>= index 0) (< index n) 0)
                                            (if (= id -14)
                                                (seq (ptr-write-u64 resw 0 (nl_vector_slot_word a index)) 0)
                                              (let* ((value (heap-read-u64 (+ stk (* (+ (- sp argc) 2) 8)) 0))
                                                     (canonical (nl_word_canon value)))
                                                (seq (wf_dirty)
                                                     (vector-slot-set a index canonical)
                                                     (ptr-write-u64 resw 0 value) 0)))
                                          -3))
                                    -3)))
                            -3))
                      -3))
                  -3))
            -3)
      (let* ((a (if (> argc 0) (heap-read-u64 (+ stk (* (- sp argc) 8)) 0) 3))
             (b (if (> argc 1) (heap-read-u64 (+ stk (* (+ (- sp argc) 1) 8)) 0) 3))
             (tag (nl_word_tag64 a)) (r -1))
        (if (= id 13)
            (m5_fx_primitive_funcall a stk sp (- argc 1) resw env scratch)
          (if (if (>= id 40) (<= id 66) 0)
              ;; bufnative: the buffer read path (GNU buffer.h / editfns.c), below.
              (nl_buf_leaf_run id stk sp argc resw env)
          (seq
           (setq r
            (cond
             ((= id 1) (if (= tag 7) (nl_cons_car_word a) (if (= tag 0) 3 -1)))
             ((= id 2) (if (= tag 7) (nl_cons_cdr_word a) (if (= tag 0) 3 -1)))
             ((= id 3) (if (= (wf_bytecode_w_nilp a) 1) 7 3))
             ((= id 4) (if (= tag 7) 7 3))
             ((= id 5) (if (= tag 7) (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 8 (nl_word_canon b)) b) -1))
             ((= id 6) (if (= tag 7) (seq (wf_dirty) (ptr-write-u64 (nl_word_payload a) 0 (nl_word_canon b)) b) -1))
             ((= id 7) (if (= (wf_bytecode_w_eqw a b) 1) 7 3))
             ((= id 8) (if (and (= (m5_fx_int_p a) 1) (= (m5_fx_int_p b) 1))
                           (if (< (wf_bytecode_w_int a) (wf_bytecode_w_int b)) 7 3) -1))
             ((= id 9) (if (and (= (m5_fx_string_argument_p tag) 1)
                                (= (m5_fx_string_argument_p (nl_word_tag64 b)) 1))
                           (if (= (m5_fx_sort_string_lessp a b) 1) 7 3) -1))
             ((or (= id 10) (= id 11))
              (if (and (= (m5_fx_int_p a) 1) (>= (wf_bytecode_w_int a) 0))
                  (m5_fx_primitive_nth b (wf_bytecode_w_int a) (if (= id 11) 1 0)) -1))
             ;; Tagged arithmetic (native-surface clause 5), not new Lisp
             ;; builtins. Nonfixnums and nonbinary calls keep ordinary APPLY.
             ((= id -17)
              (if (and (= argc 2) (= (logand a 3) 1) (= (logand b 3) 1) (/= b 1))
                  (let* ((x (sar a 2)) (y (sar b 2)))
                    ;; Truncating quotient matches GNU Frem, including
                    ;; negative dividends. Fixnums cannot overflow raw /.
                    (+ (shl (- x (* (/ x y) y)) 2) 1))
                -1))
             ((= id -18)
              (if (and (= argc 2) (= (logand a 3) 1) (= (logand b 3) 1))
                  (logand a b) -1))
             ;; nc3: the builtin arms of `fboundp' (`bf_fboundp_w') and
             ;; `nelisp--string-search-chars' on the operand words, with the
             ;; argument pointers `wf_arg_ptr' would read from a list
             ;; (`m5_fx_word_ptr'); no Lisp runs and nothing collects.  A
             ;; non-symbol `fboundp' operand (the error) declines to APPLY.
             ((= id -19)
              (if (or (= a 3) (= a 7)) 3
                (if (if (= (logand a 7) 6)
                        (if (> a 4096)
                            (or (= (logand (ptr-read-u64 (- a 6) 0) 63) 4)
                                (= (logand (ptr-read-u64 (- a 6) 0) 63) 16))
                          0)
                      0)
                    (let* ((tmp (alloc-bytes 32 8)))
                      (if (= (nelisp_env_lookup_function (+ env 0) (+ env 64) a tmp) 0)
                          (if (= (bf_fboundp_cell_p tmp) 0) 3 7)
                        3))
                  -1)))
             ((= id -20)
              (if (= argc 3)
                  (let* ((r (m5_string_search_chars
                             (m5_fx_word_ptr a) (m5_fx_word_ptr b)
                             (m5_fx_word_ptr (heap-read-u64 (+ stk (* (+ (- sp argc) 2) 8)) 0)))))
                    (if (= r -2) 7 (if (= r -1) 3 (wf_write_int_w 0 r))))
                -1))
             ;; callpath2: GNU data.c Fsymbol_function, XSYMBOL (sym)->u.s.function
             ;; (Bsymbol_function in byte code): an interned or uninterned symbol
             ;; object's cell as `bf_symbol_function_w' answers it (nil when unset,
             ;; a Subr object's legacy list), with no argument list and no name
             ;; dispatch.  nil and t have no function.  Any other operand (the
             ;; error, a symbol view) declines to APPLY.
             ((= id -21)
              (if (or (= a 3) (= a 7)) 3
                (if (if (= (logand a 7) 6)
                        (if (> a 4096)
                            (or (= (logand (ptr-read-u64 (- a 6) 0) 63) 4)
                                (= (logand (ptr-read-u64 (- a 6) 0) 63) 16))
                          0)
                      0)
                    (let* ((v (nelisp_env_lookup_function_w (+ env 0) (+ env 64) a 0)))
                      (if (= v 2) 3
                        (if (= v 0) -1
                          (if (= (bf_fboundp_cell_p v) 0) 3 v))))
                  -1)))
             (t -1)))
           (if (= r -1) -3 (seq (ptr-write-u64 resw 0 r) 0))))))))
    ;; corecall (W2c): the VM's call of a primitive held by the call site's symbol, with the
    ;; SUBR struct in hand (GNU bytecode.c Bcall -> funcall_subr, eval.c:3254).  CACHE is the
    ;; site operand (the compiler's [SYMBOL nil nil nil nil] vector, or the symbol).  The
    ;; symbol's function cell is read directly; when it holds a Subr object (header kind 20,
    ;; immutable, so it IS the builtin whatever the symbol is called, and a redefinition or
    ;; advice replaces the cell), the primitive id the descriptor carries runs
    ;; `m5_fx_primitive_run' on the operand words, exactly as `m5_fx_callout' does after its
    ;; lookup, cache and autoload tests.  0: done, value in RESW; -3: not handled, nothing
    ;; written (the call-out then runs unchanged).  The ids are the leaf bodies that call no
    ;; Lisp, allocate nothing and reach no safepoint; operands and RESW are slots of the
    ;; activation's rooted frame.
    (defun m5_fx_call_subr (env cache stk sp argc resw)
      (let* ((sym (if (= (logand cache 7) 6)
                      (if (< cache 4096) 0
                        (if (= (logand (ptr-read-u64 (- cache 6) 0) 63) 8)
                            (ptr-read-u64 (- cache 6) 8)
                          cache))
                    0))
             (fn (if (= (logand sym 7) 6)
                     (if (< sym 4096) 0
                       (if (= (logand (ptr-read-u64 (- sym 6) 0) 63) 4)
                           (ptr-read-u64 (- sym 6) 24)
                         0))
                   0)))
        (if (= (logand fn 7) 6)
            (if (< fn 4096) -3
              (let* ((hdr (ptr-read-u64 (- fn 6) 0)))
                (if (/= (logand hdr 63) 20) -3
                  (let* ((desc (sar hdr 8))
                         (lo (logand desc 255))
                         (id (- lo 17)))
                    (if (if (= lo 0) 1 (> lo 128)) -3
                      (if (< argc (logand (sar desc 8) 255)) -3
                        (if (if (= (logand (sar desc 16) 255) 127) 0
                              (> argc (logand (sar desc 16) 255)))
                            -3
                          (if (if (>= id 40) (<= id 66) 0)
                              ;; bufnative: the buffer read path, straight to its body
                              (nl_buf_leaf_run id stk sp argc resw env)
                          (if (if (= id 1) 1 (if (= id 10) 1 (if (= id -14) 1 (if (= id -15) 1 (if (= id -16) 1
                                (if (if (>= id 3) (<= id 7) 0) 1 (if (= id 11) 1 0)))))))
                              (m5_fx_primitive_run id stk sp argc resw env 0)
                            -3)))))))))
          -3)))
    ;; callpath: GNU funcall_subr for every Subr object, not only the primitive ids: the
    ;; primitive above, else the argument-array entry the object carries
    ;; (`m5_fx_subr_argv'), with the operand words as its argv -- no list, no name
    ;; lookup, no autoload test (a Subr object is never an autoload cell; GNU
    ;; funcall_general tests SUBRP first).  0 done, -3 not handled (nothing happened:
    ;; the call-out runs), any other value the call's signal status.
    (defun m5_fx_call_subr_any (env cache stk sp argc resw)
      (let* ((st (m5_fx_call_subr env cache stk sp argc resw)))
        (if (/= st -3) st
          (let* ((sym (if (= (logand cache 7) 6)
                          (if (< cache 4096) 0
                            (if (= (logand (ptr-read-u64 (- cache 6) 0) 63) 8)
                                (ptr-read-u64 (- cache 6) 8)
                              cache))
                        0))
                 (fn (if (= (logand sym 7) 6)
                         (if (< sym 4096) 0
                           (if (= (logand (ptr-read-u64 (- sym 6) 0) 63) 4)
                               (ptr-read-u64 (- sym 6) 24)
                             0))
                       0)))
            (if (= (logand fn 7) 6)
                (if (< fn 4096) -3
                  (if (/= (logand (ptr-read-u64 (- fn 6) 0) 63) 20) -3
                    (let* ((pst (m5_fx_subr_prim2 env fn stk sp argc resw)))
                      (if (/= pst -3) pst
                        (m5_fx_subr_argv env sym fn stk sp argc resw)))))
              -3)))))
    ;; callpath2: the interpreter's primitive of a Subr object on the operand words;
    ;; the reader's definition is generated (`nelisp-standalone--argframe-m5-helpers'),
    ;; the link sets without `nl_eic_prim2_run' have none to run.
    (defun m5_fx_subr_prim2 (env fn stk sp argc resw) -3)
    ;; The link sets without argument-array entries have none to call (the reader's
    ;; definition is generated with them, `nelisp-standalone--argframe-m5-helpers').
    (defun m5_fx_subr_argv (env sym fn stk sp argc resw) -3)
    ;; The argument pointer `wf_arg_ptr' gives a builtin arm for the list
    ;; element WORD (`nl_cons_car_ptr'): a legacy view, a direct cons or a
    ;; symbol object as it is, anything else materialised into a fresh view.
    (defun m5_fx_word_ptr (word)
      (if (= (logand word 5) 0) word
        (if (if (= (logand word 7) 6)
                (or (= (logand (ptr-read-u64 (- word 6) 0) 63) 4)
                    (= (logand (ptr-read-u64 (- word 6) 0) 63) 16))
              0)
            word
          (nl_sexp_clone_into word (alloc-bytes 32 8)))))
    (defun m5_fx_primitive_named (name stk sp argc resw env scratch)
      (let* ((id (m5_fx_primitive_id name argc)))
        (if (= id 0) -3 (m5_fx_primitive_run id stk sp argc resw env scratch))))
    (defun m5_fx_string_argument_p (tag)
      (if (or (= tag 4) (= tag 16) (= tag 5) (= tag 6)
              (= tag 14) (= tag 15)) 1 0))
    ;; prim-strcmp integration changes this one expression. Both inputs and
    ;; the result stay WORD/raw values; no FUNCALL or transport box is needed.
    (defun m5_fx_sort_string_lessp (a b)
      (bf_str_lt (bf_str_or_name_w a 0) (bf_str_or_name_w b 0)))
    (defun m5_fx_primitive_nth (items index tail)
      (seq
       (while (and (> index 0) (= (m5_fx_word_cons_p items 0) 1))
         (setq items (nl_cons_cdr_word items))
         (setq index (- index 1)))
       (if (= (nl_word_tag64 items) 0) 3
         (if (/= index 0) -1
           (if (= tail 1) items
             (if (= (m5_fx_word_cons_p items 0) 1) (nl_cons_car_word items) -1))))))
    ;; GNU eval.c:3185-3215 Ffuncall -> funcall_general -> funcall_lambda: a
    ;; compiled callee runs on the already evaluated argument words, with
    ;; its activation recorded, and no argument list.  A closure the VM
    ;; covers for ARGC arguments therefore runs through `m5_fx_call_words'
    ;; directly; anything else answers -3 and keeps the generic apply.
    ;; WS: a rooted WORD slot of the caller's frame, which holds a looked-up
    ;; definition while it runs (0: no symbol lookup, the call declines).
    ;; The VM's closure call needs no scratch (`m5_fx_call_words').
    (defun m5_fx_primitive_funcall (fn stk sp argc resw env ws)
      (m5_fx_funcall_site 0 fn stk sp argc resw env ws))
    ;; GNU's function object carries its own code (Ffuncall, funcall_lambda,
    ;; exec_byte_code on the object's bytecode, eval.c:3185-3215, 3350);
    ;; a closure's VM record lives in `nelisp--fx-table' under its body, a
    ;; hash lookup per call.  A `funcall' call site keeps the last covered
    ;; closure it called and that closure's record in its cache vector's
    ;; slots 3 and 4, so a site that keeps calling one closure skips the
    ;; lookup.  A record retired from the table is marked in its slot 7
    ;; (`nelisp--fx-retire'), which the cached copy checks (its code stays:
    ;; a call already running it must go on), and the arity is checked at
    ;; every call.  CACHE 0: no site cache (a worker, or a
    ;; call with no cache vector).
    (defun m5_fx_funcall_site (cache fn stk sp argc resw env ws)
      (if (if (= cache 0) 0 (= (nl_vector_slot_word cache 3) fn))
          (let* ((rec (nl_vector_slot_word cache 4)))
            (if (if (if (>= (nl_vector_length rec) 8)
                        (= (nl_vector_slot_word rec 7) 3) 0)
                    (>= (m5_fx_arity_code rec argc) 0) 0)
                (m5_fx_call_words rec fn (+ stk (* (- sp argc) 8)) argc
                                  env 0 resw 0)
              (m5_fx_funcall_site 0 fn stk sp argc resw env ws)))
        (let* ((tag (nl_word_tag64 fn)))
          (if (or (= tag 4) (= tag 16))
              ;; applyfn2: GNU Ffuncall -> funcall_general reads the symbol's
              ;; function cell and calls a SUBR there through funcall_subr
              ;; (eval.c:3105, 3254): a Subr object in the cell runs on the
              ;; argument words as the VM's Bcall does (`m5_fx_call_subr_any'),
              ;; before the name lookup and the primitive-name compares below.
              (let* ((sst (if (= ws 0) -3 (m5_fx_call_subr_any env fn stk sp argc resw)))
                     (def (if (/= sst -3) 2
                            (if (= ws 0) 2
                              (nelisp_env_lookup_function_w (+ env 0) (+ env 64) fn 0)))))
                (if (/= sst -3) sst
                (if (= def 2) -3
                  (seq (ptr-write-u64 ws 0 def)
                       (let* ((st (m5_fx_primitive def stk sp argc resw env ws)))
                         (if (/= st -3) st
                           ;; callpath2: GNU Ffuncall -> funcall_general on the
                           ;; symbol's definition (eval.c:3185-3215): a Subr, a
                           ;; byte-code function or a covered closure runs on the
                           ;; argument words, the backtrace naming the symbol as
                           ;; record_in_backtrace names args[0].  An alias (a
                           ;; symbol again) keeps the generic apply, which follows
                           ;; the chain and detects a cycle.
                           (if (or (= (nl_word_tag64 def) 4) (= (nl_word_tag64 def) 16)) -3
                             (m5_fx_funcall_value fn fn def stk sp argc resw env))))))))
            (let* ((st (m5_fx_primitive fn stk sp argc resw env ws)))
              (if (/= st -3) st
                (let* ((rec (m5_fx_covered fn argc env 0)))
                  (if (= rec 0)
                      ;; callpath2: a byte-code function or a Subr object on the
                      ;; words (GNU funcall_general's COMPILEDP / SUBRP arms).  A
                      ;; cons (an uncovered closure or lambda) has neither: it goes
                      ;; straight to the generic apply.
                      (if (if (= (logand fn 7) 2) 1 (= (nl_val_tag fn) 7)) -3
                        (m5_fx_funcall_value fn 0 fn stk sp argc resw env))
                    (seq (if (= cache 0) 0 (m5_fx_funcall_site_fill cache fn rec))
                         (m5_fx_call_words rec fn (+ stk (* (- sp argc) 8)) argc
                                           env 0 resw 0))))))))))
    ;; callpath2: GNU funcall_general for a definition that is no primitive id and
    ;; no covered closure (a Subr object, a byte-code function); the reader's
    ;; definition is generated (`nelisp-standalone--argframe-m5-helpers').
    (defun m5_fx_funcall_value (opr sym def stk sp argc resw env) -3)
    ;; callpath2: a builtin value's arity from its Subr object, for the
    ;; `nelisp--native-subr-arity' arm; 0 (none) outside the reader, whose
    ;; definition is generated (`nelisp-standalone--argframe-m5-helpers').
    (defun bf_builtin_subr_arity_w (fnw env) 0)
    ;; callpath2: `apply' on the operand words (call-out kind 14); the reader's
    ;; definition is generated (`nelisp-standalone--argframe-m5-helpers').
    (defun m5_fx_apply_run (stk sp argc resw env) -3)
    (defun m5_fx_apply_spread (base argc resw env) -3)
    (defun m5_fx_apply_no_word_subr (fw) 0)
    ;; applyfn2: GNU Fapply's spread for a builtin (reader definitions generated
    ;; with the others, `nelisp-standalone--callpath2-m5-defs').
    (defun m5_fx_apply_word_subr (fw) 0)
    (defun m5_fx_apply_list_len (w) -1)
    (defun m5_fx_apply_subr (stk sp argc resw env) -3)
    (defun m5_fx_apply_words_run (stk sp argc resw env) -3)
    ;; The record before the closure, so a reader that sees the closure
    ;; sees its record (the site kind's order, `m5_fx_site_kind').
    (defun m5_fx_funcall_site_fill (cache fn rec)
      (if (if (< (nl_vector_length cache) 5) 1 (< (nl_vector_length rec) 8)) 0
        (seq (wf_dirty)
             (ptr-write-u64 (+ (nl_word_payload cache) 40) 0 rec)
             (ptr-write-u64 (+ (nl_word_payload cache) 32) 0 fn)))))
  "Direct WORD transport for resolved existing VM primitives.")

(provide 'nelisp-cc-fx-call-primitives)
;;; nelisp-cc-fx-call-primitives.el ends here
