;;; nelisp-standalone-build.el --- Pure-elisp standalone NeLisp eval build  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reproducible, INCREMENTAL build of a standalone NeLisp eval binary that
;; needs ZERO Rust (no cargo / rustc / target binary).  The REAL NeLisp
;; evaluator (nl_eval_inner + the combiner cons/apply cluster + bootstrap
;; mirror) is compiled by the pure-elisp Phase 47 compiler into relocatable
;; units and linked by the pure-elisp static linker into a freestanding
;; static ELF.  The single primitive that was Rust-only (nelisp_apply_function,
;; the builtin name -> native op dispatcher) is supplied here in pure elisp.
;;
;; Toolchain (all pure elisp):
;;   each unit source -> `nelisp-phase47-compiler' (my-compile-to-unit) ->
;;   in-memory link-unit -> `nelisp-static-linker' (nelisp-link-units) ->
;;   ELF64 ET_EXEC at target/nelisp-standalone-eval.
;;
;; REBUILD MODEL (the point of this file):
;;   * Whole build:        (nelisp-standalone-build)         [Makefile: standalone-eval]
;;   * Individual .el:     editing one lisp/nelisp-cc-XXX.el invalidates ONLY that
;;                         unit's cached object (target/standalone-units/NAME.unit);
;;                         the next build recompiles just that unit + relinks.
;;   * Force one unit:     (nelisp-standalone-rebuild-one "eq-symbol.o")
;;   * Clean cache:        rm -rf target/standalone-units   [Makefile: standalone-eval-clean]
;;
;; The cached object format is the link-unit elisp datum (text bytes + symbols
;; + relocs) serialized with `print-escape-nonascii' so the cache is plain
;; ASCII and round-trips via `read'.  Freshness = cache newer than the unit's
;; source .el AND newer than the compiler/linker sources.
;;
;; PARAMETRIZED FORM (proves the evaluator really runs, not a fixed exit):
;;   NELISP_FORM_OP / NELISP_FORM_A / NELISP_FORM_B select the embedded form
;;   (OP A B); default (+ 1 2) -> exit 3.  Supported OP: + - * .  The driver
;;   unit is always recompiled (it is tiny and form-dependent); all other
;;   units are cached.

;;; Code:

(require 'cl-lib)
(require 'nelisp-phase47-compiler)
(require 'nelisp-static-linker)
(require 'nelisp-elf-write)

(defconst nelisp-standalone--this-file
  (or load-file-name buffer-file-name)
  "Absolute path of this build script (= source file of the glue units).")

(defconst nelisp-standalone--repo-root
  (file-name-directory
   (directory-file-name (file-name-directory nelisp-standalone--this-file)))
  "Repo root (parent of scripts/).")

(defconst nelisp-standalone--cache-dir
  (expand-file-name "target/standalone-units" nelisp-standalone--repo-root)
  "Per-unit compiled-object cache directory.")

(defconst nelisp-standalone--out
  (expand-file-name "target/nelisp-standalone-eval" nelisp-standalone--repo-root)
  "Output standalone ELF path (baked-form eval).")

(defconst nelisp-standalone--reader-out
  (expand-file-name "target/nelisp-standalone-reader" nelisp-standalone--repo-root)
  "Output standalone ELF path (reader path: text -> AOT reader -> eval).")

(defvar nelisp-standalone--recompiled nil
  "Names of units recompiled in the current build (vs. served from cache).")

(defun nelisp-standalone--dep-files ()
  "Toolchain source files; any newer than a cache entry forces recompile."
  (delq nil (mapcar #'locate-library
                    '("nelisp-phase47-compiler" "nelisp-static-linker"
                      "nelisp-elf-write" "nelisp-cc-runtime"))))

;; ===================================================================
;; my-compile-to-unit — Phase47 source -> in-memory link-unit.
;; reloc :addend normalized to 0 (the linker uses P = va+offset+4; the
;; assembler emits the standard RELA -4 -- normalizing avoids the off-by-4).
;; ===================================================================
(defun nelisp-standalone--compile-to-unit (name source)
  "Compile Phase47 SOURCE to a link-unit labelled NAME."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch 'x86_64)
         (nelisp-phase47-compiler--allow-external-user-calls t)
         (nelisp-phase47-compiler--abi 'sysv)
         (ir (nelisp-phase47-compiler--parse source nil))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (dolist (d defuns) (nelisp-phase47-compiler--emit-defun d buf))
    (let* ((text (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (relocs0 (nelisp-asm-x86_64-extract-relocs buf))
           (exported (mapcar (lambda (d)
                               (let ((nm (nelisp-phase47-compiler--ir-get d :name)))
                                 (if (stringp nm) nm (symbol-name nm))))
                             defuns))
           (symbols (delq nil
                          (mapcar (lambda (cell)
                                    (let ((nm (if (stringp (car cell)) (car cell)
                                                (symbol-name (car cell)))))
                                      (when (member nm exported)
                                        (nelisp-link-symbol nm (cdr cell)
                                                            :section 'text :bind 'global :type 'func))))
                                  labels)))
           (relocs (mapcar (lambda (r)
                             (list :offset (plist-get r :offset) :type (plist-get r :type)
                                   :symbol (plist-get r :symbol) :addend 0 :section 'text))
                           relocs0)))
      (nelisp-link-unit-make name (list (cons 'text text)) symbols relocs))))

;; ===================================================================
;; Incremental cache.
;; ===================================================================
(defun nelisp-standalone--cached-unit (name source source-file)
  "Return the link-unit for NAME, compiling SOURCE only if SOURCE-FILE
or the toolchain is newer than the cached object."
  (let* ((cache (expand-file-name (concat name ".unit") nelisp-standalone--cache-dir))
         (deps (cons source-file (nelisp-standalone--dep-files)))
         (fresh (and (file-exists-p cache)
                     (cl-every (lambda (f) (or (null f) (file-newer-than-file-p cache f))) deps))))
    (if fresh
        (with-temp-buffer
          (insert-file-contents cache)
          (goto-char (point-min))
          (read (current-buffer)))
      (let ((unit (nelisp-standalone--compile-to-unit name source)))
        (make-directory nelisp-standalone--cache-dir t)
        (with-temp-file cache
          (let ((print-escape-nonascii t) (print-length nil) (print-level nil))
            (prin1 unit (current-buffer))))
        (push name nelisp-standalone--recompiled)
        unit))))

;; ===================================================================
;; BOTTOM LAYER (pure elisp) — bump arena over an mmap MAP_FIXED region at
;; 0x10000000.  bump-offset@+0 (init 16), QUIT_FLAG slot@+8 (init 0), data
;; from +16.  Supplies nl_alloc_bytes (the target of every `alloc-bytes' op
;; reloc), nl_dealloc_bytes, nl_quit_flag_ptr.
;; ===================================================================
;; M6 catch/throw stash region lives in the reserved arena bytes [16,96):
;;   +8  (268435464): QUIT_FLAG slot       (init 0)
;;   +16 (268435472): THROW flag           (0 = none in flight, 1 = in flight)
;;   +24 (268435480): stashed TAG Sexp     (32 bytes)
;;   +56 (268435512): stashed VAL Sexp     (32 bytes)
;; The bump offset starts at 96 (was 16) so nl_alloc_bytes never hands out the
;; reserved [16,96) bytes.  The extra reservation is harmless to the baked-form
;; eval path (it never reads the stash); it only shifts the first allocation up.
(defconst nelisp-standalone--arena-source
  '(seq
    (defun nl_seq2 (_a b) b)
    (defun nl_align_up (n a) (logand (+ n (- a 1)) (- 0 a)))
    (defun nl_arena_init ()
      ;; 4 GiB bump arena (virtual; only TOUCHED pages consume RAM, so the
      ;; mapping itself is free).  The bump allocator NEVER frees: every nested
      ;; eval call leaks ~4-5 KiB and a `fib' invocation (which re-expands the
      ;; `cond' macro + runs ~10 nested calls) leaks a MEASURED 89_024 bytes,
      ;; so a single deeply-recursive top-level form needs headroom ∝ its whole
      ;; call count.  The old 1 GiB ceiling SIGSEGV'd at fib(20) (≈13.5k calls ×
      ;; 89 KiB ≈ 1.2 GiB); 4 GiB lifts the single-form ceiling to ≈fib(21-22).
      ;; The binary loads at 0x400000 (≈170 KiB) and this region is
      ;; 0x10000000..0x110000000, far below the high-address stack, so MAP_FIXED
      ;; is collision-free.  4 GiB = 0x100000000 = 4294967296.
      (let ((p (syscall-direct 9 268435456 4294967296 3 50 -1 0)))
        (nl_seq2 (ptr-write-u64 268435456 0 96)
                 (nl_seq2 (ptr-write-u64 268435464 0 0)
                          (nl_seq2 (ptr-write-u64 268435472 0 0) p)))))
    (defun nl_alloc_bytes (size align)
      (let ((cur (ptr-read-u64 268435456 0)))
        (let ((a (nl_align_up cur align)))
          (nl_seq2 (ptr-write-u64 268435456 0 (+ a size)) (+ 268435456 a)))))
    (defun nl_dealloc_bytes (_p _s _a) 1)
    (defun nl_quit_flag_ptr () 268435464)))

;; SHIM: nelisp_eval_call — universal recursion entry.  Bumps rec_cur@env+96
;; against rec_max@env+104, recurses into REAL nl_eval_inner.
(defconst nelisp-standalone--shim-source
  '(seq (defun nelisp_eval_call (form_ptr env out)
          (let* ((rec_cur_addr (+ env 96)) (rec_max_addr (+ env 104)))
            (let* ((rec_cur (ptr-read-u64 rec_cur_addr 0)) (rec_max (ptr-read-u64 rec_max_addr 0)))
              (if (>= rec_cur rec_max) 1
                (nl_seq2 (ptr-write-u64 rec_cur_addr 0 (+ rec_cur 1))
                  (let* ((rc (nl_eval_inner form_ptr env out 0)))
                    (nl_seq2 (ptr-write-u64 rec_cur_addr 0 rec_cur) rc)))))))))

(defun nelisp-standalone--name-u64 (s)
  "Pack STRING S (<=8 ASCII bytes) little-endian into a u64 for symbol-name match."
  (let ((v 0) (i 0))
    (while (< i (length s))
      (setq v (logior v (ash (aref s i) (* i 8))))
      (setq i (1+ i)))
    v))

;; *** nelisp_apply_function — the pure-elisp builtin dispatcher (was Rust-only). ***
;; func -> (builtin . (NAME . nil)); NAME @ car(cdr(func)); args -> cons list of
;; evaluated Sexps; out -> 32B result slot.  Covers arithmetic (+ - * / mod 1+ 1-),
;; comparison/predicate (= < > <= >= eq null not -> T/Nil), list ops
;; (car cdr cons list), hash tables (M4: make-hash-table/puthash/gethash/remhash/
;; hash-table-count/maphash), strings+format (M5: length/concat/substring/
;; make-string/string=/char-to-string/string-to-char/number-to-string/
;; string-to-number/format) and file I/O (M7: wrf/rdf/slen, impls in m7b-fileio.o).
;; rc 0 ok / 1 unknown.  The dispatch chain is generated from `table' at load time.
;;
;; DISPATCH builder (unified, M4/M5): each table entry is (MATCH . IMPL) where
;; MATCH is either (:u8 "NAME") -> u64-packed `wf_name_is' (names <=8 bytes) or
;; (:lit "NAME") -> the `sexp-name-eq' grammar op (full-length compare, ANY
;; length, required for "make-hash-table"/"string-to-number"/... > 8 bytes).
(defconst nelisp-standalone--applyfn-dispatch-table
  '(((:u8 "+")    . (wf_write_int out (wf_sum args 0)))
    ((:u8 "-")    . (wf_write_int out (wf_diff args)))
    ((:u8 "*")    . (wf_write_int out (wf_prod args 1)))
    ((:u8 "/")    . (wf_write_int out (/ (wf_argval args 0) (wf_argval args 1))))
    ((:u8 "mod")  . (wf_write_int out (mod (wf_argval args 0) (wf_argval args 1))))
    ((:u8 "1+")   . (wf_write_int out (+ (wf_argval args 0) 1)))
    ((:u8 "1-")   . (wf_write_int out (- (wf_argval args 0) 1)))
    ((:u8 "=")    . (if (= (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<")    . (if (< (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">")    . (if (> (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "<=")   . (if (<= (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 ">=")   . (if (>= (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "eq")   . (if (= (wf_argval args 0) (wf_argval args 1)) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "null") . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "not")  . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 0) (wf_write_t out) (wf_write_nil out)))
    ((:u8 "car")  . (wf_copy32 out (nl_cons_car_ptr (wf_arg_ptr args 0))))
    ((:u8 "cdr")  . (wf_copy32 out (nl_cons_cdr_ptr (wf_arg_ptr args 0))))
    ((:u8 "cons") . (seq (nelisp_cons_construct (wf_arg_ptr args 0) (wf_arg_ptr args 1) out) 0))
    ((:u8 "list") . (seq (wf_copy32 out args) 0))
    ;; --- M4 hash tables (cons-alist v1) ---
    ((:lit "make-hash-table")  . (wf_ht_make out))
    ((:lit "puthash")          . (wf_ht_put args out))
    ((:lit "gethash")          . (wf_ht_get args out))
    ((:lit "remhash")          . (wf_ht_rem args out))
    ((:lit "hash-table-count") . (wf_write_int out (wf_ht_count (wf_ht_alist_slot (wf_arg_ptr args 0)) 0)))
    ((:lit "maphash")          . (wf_ht_maphash args out))
    ;; --- M5 strings + format ---
    ((:lit "length")           . (wf_write_int out (m5_length (wf_arg_ptr args 0))))
    ((:lit "string=")          . (if (= (m5_streq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                                      (wf_write_t out) (wf_write_nil out)))
    ((:lit "string-to-char")   . (wf_write_int out
                                  (if (= (str-len (wf_arg_ptr args 0)) 0) 0
                                    (str-byte-at (wf_arg_ptr args 0) 0))))
    ((:lit "string-to-number") . (wf_write_int out (m5_s2n (wf_arg_ptr args 0))))
    ((:lit "number-to-string") . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_push_dec ms (ptr-read-u64 (wf_arg_ptr args 0) 8))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "char-to-string")   . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 4)
                                        (mut-str-push-byte ms (ptr-read-u64 (wf_arg_ptr args 0) 8))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "make-string")      . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_push_repeat ms (ptr-read-u64 (wf_arg_ptr args 1) 8)
                                                        (ptr-read-u64 (wf_arg_ptr args 0) 8))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "concat")           . (let* ((ms (alloc-bytes 32 8)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_concat_walk ms args)
                                        (mut-str-finalize ms out) 0)))
    ((:lit "substring")        . (let* ((ms (alloc-bytes 32 8))
                                        (s (wf_arg_ptr args 0))
                                        (slen (str-len s))
                                        (from (ptr-read-u64 (wf_arg_ptr args 1) 8))
                                        (rest (nl_cons_cdr_ptr (nl_cons_cdr_ptr args)))
                                        (to (if (= (ptr-read-u64 rest 0) 7)
                                                (ptr-read-u64 (wf_arg_ptr args 2) 8)
                                              slen)))
                                   (seq (mut-str-make-empty ms 16)
                                        (m5_push_str_bytes ms s
                                          (if (< from 0) (+ slen from) from)
                                          (if (< to 0) (+ slen to) to))
                                        (mut-str-finalize ms out) 0)))
    ((:lit "format")           . (let* ((ms (alloc-bytes 32 8))
                                        (fmt (wf_arg_ptr args 0)))
                                   (seq (mut-str-make-empty ms 32)
                                        (m5_fmt_loop ms fmt 0 (str-len fmt)
                                                     (nl_cons_cdr_ptr args))
                                        (mut-str-finalize ms out) 0)))
    ;; --- M7 file I/O (impls in m7b-fileio.o glue unit) ---
    ((:u8 "wrf")  . (seq (nl_bi_write_file args out) 0))
    ((:u8 "rdf")  . (seq (nl_bi_read_file args out) 0))
    ((:u8 "slen") . (seq (nl_bi_slen args out) 0)))
  "Unified (MATCH . IMPL) builtin dispatch table for `nelisp_apply_function'.
MATCH = (:u8 NAME) for <=8-byte u64-packed names, (:lit NAME) for full-length
`sexp-name-eq' compare (any length).")

;; The first 19 entries (arithmetic / comparison / list) are the ONLY ones the
;; baked-form eval path (`nelisp-standalone--manifest') links -- they touch just
;; the cons/symbol units already in that manifest.  The HT/string/file entries
;; (M4/M5/M7) lower to extern calls (mut-str / str-direct / syscall / nl_alloc_str)
;; that only the reader manifest provides, so the baked applyfn must exclude them.
(defconst nelisp-standalone--applyfn-dispatch-table-baked
  (cl-subseq nelisp-standalone--applyfn-dispatch-table 0 19)
  "Arithmetic/comparison/list-only dispatch subset for the baked-form eval path.")

(defun nelisp-standalone--applyfn-build-dispatch (&optional table)
  "Fold the (MATCH . IMPL) TABLE (default the full dispatch table) into a
nested-if Phase47 dispatch chain, defaulting to rc 1 (unknown builtin)."
  (let ((u #'nelisp-standalone--name-u64)
        (dispatch 1))
    (dolist (entry (reverse (or table nelisp-standalone--applyfn-dispatch-table)))
      (let* ((match (car entry))
             (impl (cdr entry))
             (cond-form (pcase (car match)
                          (:u8  `(wf_name_is name_ptr ,(funcall u (cadr match)) ,(length (cadr match))))
                          (:lit `(= (sexp-name-eq name_ptr ,(cadr match)) 1)))))
        (setq dispatch `(if ,cond-form ,impl ,dispatch))))
    dispatch))

;; Core wf_* helpers (arithmetic / list / write-int-t-nil / name-match).  Shared
;; by BOTH the baked and the reader applyfn -- they reference only cons/symbol
;; units present in every manifest.
(defconst nelisp-standalone--applyfn-core-helpers
  '((defun wf_sym_eq (name_ptr buf len)
      (let* ((tmp_slot (alloc-bytes 32 8)) (res_slot (alloc-bytes 32 8)))
        (seq (nl_alloc_symbol buf len tmp_slot) (nelisp_eq_symbol name_ptr tmp_slot res_slot)
             (= (ptr-read-u64 res_slot 0) 1))))
    (defun wf_name_is (name_ptr u64 len)
      (let* ((buf (alloc-bytes 8 1)))
        (seq (ptr-write-u64 buf 0 u64) (wf_sym_eq name_ptr buf len))))
    (defun wf_arg_ptr (args n)
      (if (= n 0) (nl_cons_car_ptr args) (wf_arg_ptr (nl_cons_cdr_ptr args) (- n 1))))
    (defun wf_argval (args n) (ptr-read-u64 (wf_arg_ptr args n) 8))
    (defun wf_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0)) (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16)) (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun wf_write_int (out v)
      (seq (ptr-write-u64 out 0 2) (ptr-write-u64 out 8 v)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_write_t (out)
      (seq (ptr-write-u64 out 0 1) (ptr-write-u64 out 8 0)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_write_nil (out)
      (seq (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
           (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0) 0))
    (defun wf_sum (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_sum (nl_cons_cdr_ptr list_ptr) (+ acc v))) acc))
    (defun wf_prod (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_prod (nl_cons_cdr_ptr list_ptr) (* acc v))) acc))
    (defun wf_subtail (list_ptr acc)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (v (ptr-read-u64 car_ptr 8)))
            (wf_subtail (nl_cons_cdr_ptr list_ptr) (- acc v))) acc))
    (defun wf_diff (list_ptr)
      (if (= (ptr-read-u64 list_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr list_ptr)) (first (ptr-read-u64 car_ptr 8))
                 (rest (nl_cons_cdr_ptr list_ptr)))
            (if (= (ptr-read-u64 rest 0) 7) (wf_subtail rest first) first)) 0)))
  "Core wf_* dispatch helpers (shared by baked + reader applyfn).")

;; M4 hash-table helpers (cons-alist v1).  Reader-only: wf_key_eq uses symbol-eq
;; / str-eq grammar ops that lower to extern calls present only in the reader.
(defconst nelisp-standalone--applyfn-ht-helpers
  '((defun wf_ht_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0)) (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16)) (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun wf_key_eq (ka kb)
      (let* ((ta (ptr-read-u64 ka 0)) (tb (ptr-read-u64 kb 0)))
        (if (= ta tb)
            (if (= ta 2)
                (if (= (ptr-read-u64 ka 8) (ptr-read-u64 kb 8)) 1 0)
              (if (= ta 4)
                  (symbol-eq ka kb)
                (if (= ta 5)
                    (str-eq ka kb)
                  (if (= ta 0) 1 (if (= ta 1) 1 0)))))
          0)))
    (defun wf_ht_alist_slot (table_ptr) (nl_cons_cdr_ptr table_ptr))
    (defun wf_ht_find (node_ptr key_ptr)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (let* ((entry_ptr (nl_cons_car_ptr node_ptr))
                 (key_slot (nl_cons_car_ptr entry_ptr)))
            (if (= (wf_key_eq key_slot key_ptr) 1)
                entry_ptr
              (wf_ht_find (nl_cons_cdr_ptr node_ptr) key_ptr)))
        0))
    (defun wf_ht_count (node_ptr acc)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (wf_ht_count (nl_cons_cdr_ptr node_ptr) (+ acc 1))
        acc))
    (defun wf_ht_make (out)
      (let* ((marker (alloc-bytes 32 8)) (nil_s (alloc-bytes 32 8)))
        (seq
         (ptr-write-u64 marker 0 2) (ptr-write-u64 marker 8 0)
         (ptr-write-u64 marker 16 0) (ptr-write-u64 marker 24 0)
         (ptr-write-u64 nil_s 0 0) (ptr-write-u64 nil_s 8 0)
         (ptr-write-u64 nil_s 16 0) (ptr-write-u64 nil_s 24 0)
         (nelisp_cons_construct marker nil_s out)
         0)))
    (defun wf_ht_put (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (val_ptr (wf_arg_ptr args 1))
             (table_ptr (wf_arg_ptr args 2))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (entry_ptr (wf_ht_find alist_slot key_ptr)))
        (if (= entry_ptr 0)
            (let* ((pair_s (alloc-bytes 32 8)) (newhead_s (alloc-bytes 32 8)))
              (seq
               (nelisp_cons_construct key_ptr val_ptr pair_s)
               (nelisp_cons_construct pair_s alist_slot newhead_s)
               (wf_ht_copy32 alist_slot newhead_s)
               (wf_ht_copy32 out val_ptr)
               0))
          (let* ((val_slot (nl_cons_cdr_ptr entry_ptr)))
            (seq
             (wf_ht_copy32 val_slot val_ptr)
             (wf_ht_copy32 out val_ptr)
             0)))))
    (defun wf_ht_get (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (entry_ptr (wf_ht_find alist_slot key_ptr)))
        (if (= entry_ptr 0)
            (let* ((rest1 (nl_cons_cdr_ptr args))
                   (rest2 (nl_cons_cdr_ptr rest1)))
              (if (= (ptr-read-u64 rest2 0) 7)
                  (wf_ht_copy32 out (nl_cons_car_ptr rest2))
                (wf_write_nil out)))
          (wf_ht_copy32 out (nl_cons_cdr_ptr entry_ptr)))))
    (defun wf_ht_rem (args out)
      (let* ((key_ptr (wf_arg_ptr args 0)) (table_ptr (wf_arg_ptr args 1))
             (alist_slot (wf_ht_alist_slot table_ptr))
             (rebuilt (alloc-bytes 32 8)))
        (seq
         (wf_ht_rem_walk alist_slot key_ptr rebuilt)
         (wf_ht_copy32 alist_slot rebuilt)
         (wf_write_nil out)
         0)))
    (defun wf_ht_rem_walk (node_ptr key_ptr out_slot)
      (if (= (ptr-read-u64 node_ptr 0) 7)
          (let* ((entry_ptr (nl_cons_car_ptr node_ptr))
                 (key_slot (nl_cons_car_ptr entry_ptr)))
            (if (= (wf_key_eq key_slot key_ptr) 1)
                (wf_ht_copy32 out_slot (nl_cons_cdr_ptr node_ptr))
              (let* ((rest_s (alloc-bytes 32 8)) (entry_s (alloc-bytes 32 8)))
                (seq
                 (wf_ht_rem_walk (nl_cons_cdr_ptr node_ptr) key_ptr rest_s)
                 (wf_ht_copy32 entry_s entry_ptr)
                 (nelisp_cons_construct entry_s rest_s out_slot)
                 0))))
        (wf_write_nil out_slot)))
    (defun wf_ht_maphash (args out) (seq (wf_write_nil out) 0)))
  "M4 hash-table helpers (reader-only).")

;; M5 string + format helpers.  Reader-only: mut-str / str-len / str-byte-at ops
;; lower to extern calls present only in the reader manifest.
(defconst nelisp-standalone--applyfn-m5-helpers
  '((defun m5_push_str_bytes (ms src_ptr i n)
      (if (>= i n) 1
        (seq (mut-str-push-byte ms (str-byte-at src_ptr i))
             (m5_push_str_bytes ms src_ptr (+ i 1) n))))
    (defun m5_push_str (ms src_ptr)
      (m5_push_str_bytes ms src_ptr 0 (str-len src_ptr)))
    (defun m5_push_repeat (ms b count)
      (if (<= count 0) 1
        (seq (mut-str-push-byte ms b)
             (m5_push_repeat ms b (- count 1)))))
    (defun m5_push_udec (ms v)
      (if (< v 10)
          (mut-str-push-byte ms (+ 48 v))
        (seq (m5_push_udec ms (/ v 10))
             (mut-str-push-byte ms (+ 48 (mod v 10))))))
    (defun m5_push_dec (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_udec ms (- 0 v)))
        (m5_push_udec ms v)))
    (defun m5_hex_digit (d)
      (if (< d 10) (+ 48 d) (+ 87 d)))
    (defun m5_push_uhex (ms v)
      (if (< v 16)
          (mut-str-push-byte ms (m5_hex_digit v))
        (seq (m5_push_uhex ms (/ v 16))
             (mut-str-push-byte ms (m5_hex_digit (mod v 16))))))
    (defun m5_push_hex (ms v)
      (if (< v 0)
          (seq (mut-str-push-byte ms 45) (m5_push_uhex ms (- 0 v)))
        (m5_push_uhex ms v)))
    (defun m5_emit_value (ms vptr)
      (let* ((tag (ptr-read-u64 vptr 0)))
        (if (= tag 2) (m5_push_dec ms (ptr-read-u64 vptr 8))
          (if (= tag 5) (m5_push_str ms vptr)
            (if (= tag 4) (m5_push_str ms vptr)
              (if (= tag 6) (m5_push_str ms vptr)
                1))))))
    (defun m5_s2n_mag (s i n acc)
      (if (>= i n) acc
        (let* ((c (str-byte-at s i)))
          (if (>= c 48)
              (if (<= c 57)
                  (m5_s2n_mag s (+ i 1) n (+ (* acc 10) (- c 48)))
                acc)
            acc))))
    (defun m5_s2n (s)
      (let* ((n (str-len s)))
        (if (= n 0) 0
          (let* ((c0 (str-byte-at s 0)))
            (if (= c0 45) (- 0 (m5_s2n_mag s 1 n 0))
              (if (= c0 43) (m5_s2n_mag s 1 n 0)
                (m5_s2n_mag s 0 n 0)))))))
    (defun m5_streq_bytes (a b i n)
      (if (>= i n) 1
        (if (= (str-byte-at a i) (str-byte-at b i))
            (m5_streq_bytes a b (+ i 1) n)
          0)))
    (defun m5_streq (a b)
      (if (= (str-len a) (str-len b))
          (m5_streq_bytes a b 0 (str-len a))
        0))
    (defun m5_list_len (p acc)
      (if (= (ptr-read-u64 p 0) 7)
          (m5_list_len (nl_cons_cdr_ptr p) (+ acc 1))
        acc))
    (defun m5_length (p)
      (let* ((tag (ptr-read-u64 p 0)))
        (if (= tag 5) (str-len p)
          (if (= tag 6) (str-len p)
            (if (= tag 4) (str-len p)
              (if (= tag 7) (m5_list_len p 0)
                0))))))
    (defun m5_concat_walk (ms cur)
      (if (= (ptr-read-u64 cur 0) 7)
          (seq (m5_emit_value ms (nl_cons_car_ptr cur))
               (m5_concat_walk ms (nl_cons_cdr_ptr cur)))
        1))
    (defun m5_fmt_argptr (argp)
      (nl_cons_car_ptr argp))
    (defun m5_fmt_loop (ms fmt i n argp)
      (if (>= i n) 1
        (let* ((c (str-byte-at fmt i)))
          (if (= c 37)
              (if (>= (+ i 1) n)
                  (seq (mut-str-push-byte ms 37) 1)
                (let* ((d (str-byte-at fmt (+ i 1))))
                  (if (= d 37)
                      (seq (mut-str-push-byte ms 37)
                           (m5_fmt_loop ms fmt (+ i 2) n argp))
                    (if (= d 100)
                        (seq (m5_push_dec ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                             (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                      (if (= d 120)
                          (seq (m5_push_hex ms (ptr-read-u64 (m5_fmt_argptr argp) 8))
                               (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                        (if (= d 115)
                            (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                 (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                          (if (= d 83)
                              (seq (m5_emit_value ms (m5_fmt_argptr argp))
                                   (m5_fmt_loop ms fmt (+ i 2) n (nl_cons_cdr_ptr argp)))
                            (seq (mut-str-push-byte ms 37)
                                 (mut-str-push-byte ms d)
                                 (m5_fmt_loop ms fmt (+ i 2) n argp)))))))))
            (seq (mut-str-push-byte ms c)
                 (m5_fmt_loop ms fmt (+ i 1) n argp)))))))
  "M5 string + format helpers (reader-only).")

;; ===================================================================
;; B-foundation breadth helpers (Wave-1 (B)).  Reader-only: they use the
;; vector grammar ops (vector-make/-slot-set/-ref-ptr/-len -> nl_vector_*),
;; symbol-eq / str-eq, nl_alloc_symbol / nl_alloc_str, nelisp_env_lookup_function
;; and nelisp_mirror_is_bound -- all present only in the reader manifest.  They
;; back the new predicate / vector / symbol / setcar/setcdr / equal arms AND the
;; nil-safe car/cdr + tag-aware eq REPLACEMENTS (the two highest-priority fixes:
;; the stock `car'/`cdr' arms deref nl_cons_car_ptr's 0 on a non-cons -> SIGSEGV;
;; the stock `eq' arm compares value@8 -> `(eq 'a 'b)' truthy, breaking
;; assq/memq/plist).
;; ===================================================================
(defconst nelisp-standalone--applyfn-bf-helpers
  '(;; symbol-name str-bytes -> Symbol(tag4).  A Str (tag5) keeps ptr@16/len@24,
    ;; a MutStr (tag6) wraps NlStr* @8 with ptr@8/len@16.  nl_alloc_symbol takes
    ;; (bytes-ptr len result-slot).
    (defun bf_str_ptr (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 8)
        (ptr-read-u64 sx 16)))
    (defun bf_str_len (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 16)
        (ptr-read-u64 sx 24)))
    (defun bf_intern (sx out)
      (nl_alloc_symbol (bf_str_ptr sx) (bf_str_len sx) out))
    ;; make-vector LEN INIT -> Vector(tag8) with every slot = INIT (cloned).
    ;; NB: use the `vector-slot-set' GRAMMAR OP (takes the Sexp ptr, derefs the
    ;; box internally), NOT the raw nl_vector_set_slot (which wants the box ptr).
    (defun bf_mv_fill (vec i n init)
      (if (>= i n) 0
        (seq (vector-slot-set vec i init)
             (bf_mv_fill vec (+ i 1) n init))))
    (defun bf_make_vector (args out)
      (let* ((n (ptr-read-u64 (wf_arg_ptr args 0) 8))
             (init (wf_arg_ptr args 1)))
        (seq (vector-make n out)
             (bf_mv_fill out 0 n init)
             0)))
    ;; vector &rest -> Vector(tag8) from the arg list.
    (defun bf_vec_fill (vec i node)
      (if (= (ptr-read-u64 node 0) 7)
          (seq (vector-slot-set vec i (nl_cons_car_ptr node))
               (bf_vec_fill vec (+ i 1) (nl_cons_cdr_ptr node)))
        0))
    (defun bf_vec_count (node acc)
      (if (= (ptr-read-u64 node 0) 7)
          (bf_vec_count (nl_cons_cdr_ptr node) (+ acc 1))
        acc))
    (defun bf_vector (args out)
      (let* ((n (bf_vec_count args 0)))
        (seq (vector-make n out)
             (bf_vec_fill out 0 args)
             0)))
    ;; aref ARR IDX: vector -> copy slot[idx]; string -> int byte at idx.
    (defun bf_aref (args out)
      (let* ((arr (wf_arg_ptr args 0))
             (idx (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (tg (ptr-read-u64 arr 0)))
        (if (= tg 8)
            (seq (wf_copy32 out (vector-ref-ptr arr idx)) 0)
          (wf_write_int out (str-byte-at arr idx)))))
    ;; aset ARR IDX VAL: vector slot set (string aset not supported -> 0).
    (defun bf_aset (args out)
      (let* ((arr (wf_arg_ptr args 0))
             (idx (ptr-read-u64 (wf_arg_ptr args 1) 8))
             (val (wf_arg_ptr args 2)))
        (if (= (ptr-read-u64 arr 0) 8)
            (seq (vector-slot-set arr idx val)
                 (wf_copy32 out val) 0)
          (seq (wf_copy32 out val) 0))))
    ;; signal/error: NON-CRASHING.  Stash (sym . data) into the catch/throw
    ;; region + set the throw flag, then return rc=1 so the rc!=0 propagation
    ;; unwinds the native stack like an error.
    ;; Stash layout (reserved arena): flag@268435472, TAG@268435480, VAL@268435512.
    ;;
    ;; WAVE-2 (PATCH 2): both bf_signal and bf_error stash a CANONICAL Symbol
    ;; (tag 4) re-allocated from its NAME BYTES via `nl_alloc_symbol' (= the
    ;; bf_intern / nl_install_one path), so the symbol the condition-case handler
    ;; matches on (`nelisp_eq_symbol' in nl_cc_match_and_bind) sees the exact box
    ;; layout nl_alloc_symbol produces.  The OLD raw 32-byte copy of the runtime
    ;; symbol arg carried a box layout that broke clone/symbol-eq, so
    ;; condition-case never matched.  bf_signal re-allocs from the passed symbol's
    ;; name bytes (ptr@16/len@24 of a tag-4 Symbol); bf_error stashes the literal
    ;; `error' symbol (elisp `(error MSG)' signals the `error' condition).
    (defun bf_sig_copy32 (dst src)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
           (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
           (ptr-write-u64 dst 24 (ptr-read-u64 src 24)) 0))
    (defun bf_signal (args out)
      (let* ((sym (wf_arg_ptr args 0)))
        (seq
         (nl_alloc_symbol (ptr-read-u64 sym 16) (ptr-read-u64 sym 24) 268435480)
         (bf_sig_copy32 268435512 (nl_cons_cdr_ptr args))
         (ptr-write-u64 268435472 0 1)
         1)))
    (defun bf_error (args out)
      (let* ((ebuf (alloc-bytes 8 1)))
        (seq
         (ptr-write-u64 ebuf 0 491496043109)              ; "error" packed LE
         (nl_alloc_symbol ebuf 5 268435480)               ; TAG slot <- Symbol "error"
         (bf_sig_copy32 268435512 (nl_cons_cdr_ptr args)) ; VAL slot <- (MSG ...)
         (ptr-write-u64 268435472 0 1)                    ; raise flag
         1)))
    ;; fboundp/boundp: look up in the env mirror.  env+0 = mirror, env+64 = unbound.
    ;; nelisp_env_lookup_function(mirror, unbound, sym, out_slot) returns 0 if found.
    (defun bf_fboundp (args env out)
      (let* ((sym (wf_arg_ptr args 0)) (tmp (alloc-bytes 32 8)) (mirror (+ env 0)) (unbound (+ env 64)))
        (if (= (nelisp_env_lookup_function mirror unbound sym tmp) 0)
            (wf_write_t out) (wf_write_nil out))))
    (defun bf_boundp (args env out)
      (let* ((sym (wf_arg_ptr args 0)) (mirror (+ env 0)))
        (if (= (nelisp_mirror_is_bound mirror sym) 1)
            (wf_write_t out) (wf_write_nil out))))
    ;; length that also handles vectors (tag 8) -> vector-len; else m5_length.
    (defun bf_length (p)
      (if (= (ptr-read-u64 p 0) 8) (vector-len p) (m5_length p)))
    ;; CORRECT eq: tag-aware identity.  The stock applyfn `eq' arm compared
    ;; value@8 which is garbage for Symbol/Str (their identity is the name).
    ;;   Int(2)   -> value@8 equal
    ;;   Symbol(4)-> symbol-eq (name compare; no interning so name == identity)
    ;;   Nil(0)/T(1) -> same tag is enough
    ;;   else (Cons/Vector/Str/...) -> pointer identity (same box@8)
    (defun bf_eq2 (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta tb)
            (if (= ta 2) (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)
              (if (= ta 4) (symbol-eq a b)
                (if (= ta 0) 1
                  (if (= ta 1) 1
                    (if (= (ptr-read-u64 a 8) (ptr-read-u64 b 8)) 1 0)))))
          0)))
    (defun bf_eq (args out)
      (if (= (bf_eq2 (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
          (wf_write_t out) (wf_write_nil out)))
    ;; equal: like eq but Str(5/6) compared by bytes, and one-level structural
    ;; for cons (recurses car+cdr).  Enough for member/assoc on flat data.
    (defun bf_equal2 (a b)
      (let* ((ta (ptr-read-u64 a 0)) (tb (ptr-read-u64 b 0)))
        (if (= ta tb)
            (if (= ta 5) (m5_streq a b)
              (if (= ta 6) (m5_streq a b)
                (if (= ta 7)
                    (if (= (bf_equal2 (nl_cons_car_ptr a) (nl_cons_car_ptr b)) 1)
                        (bf_equal2 (nl_cons_cdr_ptr a) (nl_cons_cdr_ptr b)) 0)
                  (bf_eq2 a b))))
          0)))
    (defun bf_equal (args out)
      (if (= (bf_equal2 (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
          (wf_write_t out) (wf_write_nil out)))
    ;; nil-safe car/cdr.  The stock arms call nl_cons_car_ptr/cdr_ptr which
    ;; return 0 for a non-cons, then wf_copy32 derefs address 0 -> SIGSEGV.
    ;; Real elisp: (car nil)=(cdr nil)=nil.  Guard on tag==7.
    (defun bf_car (args out)
      (let* ((a (wf_arg_ptr args 0)))
        (if (= (ptr-read-u64 a 0) 7) (wf_copy32 out (nl_cons_car_ptr a)) (wf_write_nil out))))
    (defun bf_cdr (args out)
      (let* ((a (wf_arg_ptr args 0)))
        (if (= (ptr-read-u64 a 0) 7) (wf_copy32 out (nl_cons_cdr_ptr a)) (wf_write_nil out)))))
  "B-foundation breadth helpers (Wave-1 (B), reader-only): predicates, vector /
symbol ops, setcar/setcdr, structural equal, vector-aware length, plus the
nil-safe car/cdr + tag-aware eq REPLACEMENTS for the buggy stock arms.")

;; B-foundation breadth dispatch arms (Wave-1 (B)).  APPENDED to the reader
;; dispatch table (see `nelisp-standalone--applyfn-reader-table').  tag values:
;;   0 Nil  1 T  2 Int  3 Float  4 Symbol  5 Str  6 MutStr  7 Cons  8 Vector.
;; All are (:lit ...) because every name is matched by the full-length
;; `sexp-name-eq' op (some are <=8 bytes but :lit is always correct).
(defconst nelisp-standalone--applyfn-bf-arms
  '(;; --- predicates ---
    ((:lit "consp")    . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 7) (wf_write_t out) (wf_write_nil out)))
    ((:lit "atom")     . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 7) (wf_write_nil out) (wf_write_t out)))
    ((:lit "stringp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 5) (wf_write_t out) (if (= tg 6) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "symbolp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           ;; nil and t are also symbols in elisp
                           (if (= tg 4) (wf_write_t out) (if (= tg 0) (wf_write_t out) (if (= tg 1) (wf_write_t out) (wf_write_nil out))))))
    ((:lit "integerp") . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 2) (wf_write_t out) (wf_write_nil out)))
    ((:lit "natnump")  . (let* ((p (wf_arg_ptr args 0)))
                           (if (= (ptr-read-u64 p 0) 2)
                               (if (>= (ptr-read-u64 p 8) 0) (wf_write_t out) (wf_write_nil out))
                             (wf_write_nil out))))
    ((:lit "numberp")  . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 2) (wf_write_t out) (if (= tg 3) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "floatp")   . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 3) (wf_write_t out) (wf_write_nil out)))
    ((:lit "vectorp")  . (if (= (ptr-read-u64 (wf_arg_ptr args 0) 0) 8) (wf_write_t out) (wf_write_nil out)))
    ((:lit "listp")    . (let* ((tg (ptr-read-u64 (wf_arg_ptr args 0) 0)))
                           (if (= tg 7) (wf_write_t out) (if (= tg 0) (wf_write_t out) (wf_write_nil out)))))
    ((:lit "zerop")    . (let* ((p (wf_arg_ptr args 0)))
                           (if (= (ptr-read-u64 p 0) 2)
                               (if (= (ptr-read-u64 p 8) 0) (wf_write_t out) (wf_write_nil out))
                             (wf_write_nil out))))
    ((:lit "fboundp")  . (bf_fboundp args env out))
    ((:lit "boundp")   . (bf_boundp args env out))
    ;; --- symbol ops ---
    ;; symbol-name: a Symbol (tag 4) already has the str layout (ptr@16/len@24);
    ;; produce a Str (tag 5) sharing those bytes via nl_alloc_str.
    ((:lit "symbol-name") . (let* ((s (wf_arg_ptr args 0)))
                              (seq (nl_alloc_str (ptr-read-u64 s 16) (ptr-read-u64 s 24) out) 0)))
    ;; intern / make-symbol: take a Str (tag 5/6), build a Symbol (tag 4).
    ((:lit "intern")      . (seq (bf_intern (wf_arg_ptr args 0) out) 0))
    ((:lit "make-symbol") . (seq (bf_intern (wf_arg_ptr args 0) out) 0))
    ;; --- vector ops ---
    ((:lit "make-vector") . (bf_make_vector args out))
    ((:lit "vector")      . (bf_vector args out))
    ((:lit "aref")        . (bf_aref args out))
    ((:lit "aset")        . (bf_aset args out))
    ;; --- signal / error (non-crashing stub; condition-case trapping deferred) ---
    ((:lit "signal")      . (bf_signal args out))
    ((:lit "error")       . (bf_error args out))
    ;; equal (member/assoc use it).  eq is REPLACED (not appended) in the table.
    ((:lit "equal")       . (bf_equal args out))
    ;; setcar/setcdr: in-place cons mutation (plist-put / nconc need these).
    ((:lit "setcar")      . (let* ((c (wf_arg_ptr args 0)) (v (wf_arg_ptr args 1)))
                              (seq (cons-set-car c v) (wf_copy32 out v) 0)))
    ((:lit "setcdr")      . (let* ((c (wf_arg_ptr args 0)) (v (wf_arg_ptr args 1)))
                              (seq (cons-set-cdr c v) (wf_copy32 out v) 0))))
  "B-foundation breadth dispatch arms (Wave-1 (B)): predicates, symbol / vector
ops, signal/error stubs, structural equal, setcar/setcdr.")

(defconst nelisp-standalone--applyfn-bf-builtins
  '("consp" "atom" "stringp" "symbolp" "integerp" "natnump" "numberp" "floatp"
    "vectorp" "listp" "zerop" "fboundp" "boundp"
    "symbol-name" "intern" "make-symbol"
    "make-vector" "vector" "aref" "aset"
    "signal" "error" "equal" "setcar" "setcdr")
  "Builtin names added by Wave-1 (B) breadth glue; appended to
`nelisp-standalone--reader-builtins'.")

(defun nelisp-standalone--applyfn-reader-table ()
  "Build the reader dispatch table: the base table with the buggy stock
`car'/`cdr'/`eq' arms REPLACED (nil-safe car/cdr + tag-aware eq) and `length'
made vector-aware, then the B-foundation breadth arms APPENDED."
  (append
   (mapcar
    (lambda (entry)
      (cond
       ((equal (car entry) '(:lit "length"))
        '((:lit "length") . (wf_write_int out (bf_length (wf_arg_ptr args 0)))))
       ((equal (car entry) '(:u8 "eq"))
        '((:u8 "eq") . (bf_eq args out)))
       ((equal (car entry) '(:u8 "car"))
        '((:u8 "car") . (bf_car args out)))
       ((equal (car entry) '(:u8 "cdr"))
        '((:u8 "cdr") . (bf_cdr args out)))
       (t entry)))
    nelisp-standalone--applyfn-dispatch-table)
   nelisp-standalone--applyfn-bf-arms))

(defun nelisp-standalone--applyfn-assemble (helper-groups table)
  "Assemble an applyfn `(seq ...)' unit from HELPER-GROUPS (lists of defun forms,
appended in order) and the dispatch TABLE, ending in nelisp_apply_function."
  (let ((dispatch (nelisp-standalone--applyfn-build-dispatch table)))
    (append
     '(seq)
     (apply #'append helper-groups)
     (list `(defun nelisp_apply_function (func_ptr args env out)
              (let* ((name_ptr (nl_cons_car_ptr (nl_cons_cdr_ptr func_ptr))))
                ,dispatch))))))

;; Baked-form eval applyfn: core helpers + arithmetic/list dispatch only.  Self-
;; contained against `nelisp-standalone--manifest' (no mut-str / str-direct / file
;; externs), so `make standalone-eval-test' stays green.
(defconst nelisp-standalone--applyfn-baked-source
  (nelisp-standalone--applyfn-assemble
   (list nelisp-standalone--applyfn-core-helpers)
   nelisp-standalone--applyfn-dispatch-table-baked)
  "Arithmetic/list-only applyfn for the baked-form eval build.")

;; Reader applyfn: core + HT (M4) + string/format (M5) + B-foundation breadth
;; helpers + the reader dispatch table (= the FULL table with nil-safe car/cdr +
;; tag-aware eq + vector-aware length REPLACEMENTS and the breadth arms appended;
;; incl. file-I/O wrf/rdf/slen impls in `nelisp-standalone--fileio-source').
(defconst nelisp-standalone--applyfn-source
  (nelisp-standalone--applyfn-assemble
   (list nelisp-standalone--applyfn-core-helpers
         nelisp-standalone--applyfn-ht-helpers
         nelisp-standalone--applyfn-m5-helpers
         nelisp-standalone--applyfn-bf-helpers)
   (nelisp-standalone--applyfn-reader-table))
  "Full reader-path applyfn: arithmetic + hash tables + strings/format + file I/O
+ B-foundation breadth (predicates / vectors / symbols / equal / setcar-setcdr,
plus the nil-safe car/cdr and tag-aware eq fixes).")

;; ===================================================================
;; M7 file-I/O glue unit — the impls for the wrf/rdf/slen builtins
;; dispatched in `nelisp-standalone--applyfn-source'.  Freestanding (no
;; libc): raw syscalls only (open=2, read=0, write=1, close=3).  Paths
;; from the reader are NOT NUL-terminated, so nl_bi_make_cpath copies the
;; bytes into a fresh arena buffer + a trailing NUL before open(2).
;; A Sexp::Str (tag 5) has ptr@16/len@24; a Sexp::MutStr (tag 6) wraps an
;; NlStr* @8 whose ptr@8/len@16.  Both are handled for robustness.
;; ===================================================================
(defconst nelisp-standalone--fileio-source
  '(seq
    (defun nl_bi_strptr (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 8)
        (ptr-read-u64 sx 16)))
    (defun nl_bi_strlen (sx)
      (if (= (ptr-read-u64 sx 0) 6)
          (ptr-read-u64 (ptr-read-u64 sx 8) 16)
        (ptr-read-u64 sx 24)))
    (defun nl_bi_cpath_loop (src dst i n)
      (if (= i n)
          (nl_seq2 (ptr-write-u8 dst n 0) dst)
        (nl_seq2 (ptr-write-u8 dst i (ptr-read-u8 src i))
                 (nl_bi_cpath_loop src dst (+ i 1) n))))
    (defun nl_bi_make_cpath (sx)
      (nl_bi_cpath_loop (nl_bi_strptr sx) (alloc-bytes (+ (nl_bi_strlen sx) 1) 1)
                        0 (nl_bi_strlen sx)))
    (defun nl_bi_wf_withfd (fd ptr len)
      (if (< fd 0) fd
        (let* ((wr (syscall-direct 1 fd ptr len 0 0 0)))
          (nl_seq2 (syscall-direct 3 fd 0 0 0 0 0) wr))))
    (defun nl_bi_write_file (args out)
      (let* ((path_sx (wf_arg_ptr args 0)) (cont_sx (wf_arg_ptr args 1)))
        (let* ((cpath (nl_bi_make_cpath path_sx))
               (fd (syscall-direct 2 cpath 577 420 0 0 0)))
          (wf_write_int out
            (nl_bi_wf_withfd fd (nl_bi_strptr cont_sx) (nl_bi_strlen cont_sx))))))
    (defun wf_copy32_strnil (out)
      (nl_alloc_str (alloc-bytes 1 1) 0 out))
    (defun nl_bi_rf_withfd (fd buf out)
      (if (< fd 0)
          (wf_copy32_strnil out)
        (let* ((n (syscall-direct 0 fd buf 4194304 0 0 0)))
          (nl_seq2 (syscall-direct 3 fd 0 0 0 0 0)
                   (nl_seq2 (nl_alloc_str buf (if (< n 0) 0 n) out) 0)))))
    (defun nl_bi_read_file (args out)
      (let* ((path_sx (wf_arg_ptr args 0)))
        (let* ((cpath (nl_bi_make_cpath path_sx))
               (buf (alloc-bytes 4194304 1))
               (fd (syscall-direct 2 cpath 0 0 0 0 0)))
          (nl_bi_rf_withfd fd buf out))))
    (defun nl_bi_slen (args out)
      (wf_write_int out (nl_bi_strlen (wf_arg_ptr args 0)))))
  "M7 file-I/O builtin impls (wrf/rdf/slen).  Uses `nl_seq2' (arena unit),
`wf_arg_ptr'/`wf_write_int' (applyfn unit) and `nl_alloc_str' (alloc-str.o).")

;; ===================================================================
;; M6 catch/throw glue unit (nl_sf_catch + nl_sf_throw + helpers).
;;
;; THROW stashes (tag,val) into the reserved arena region and returns rc=1;
;; rc=1 propagates up through every body-walker (progn/if/let/...) exactly like
;; an error -- they all abort on rc!=0 -- naturally unwinding the whole native
;; call stack with NO frame/landing bookkeeping.  CATCH evals BODY via
;; nelisp_eval_call; on rc=1 it inspects the stash:
;;   flag==0          -> genuine (non-throw) error: re-propagate rc=1.
;;   flag==1, tag eq  -> caught: copy stashed val to *out, clear flag, rc=0.
;;   flag==1, tag ne  -> re-propagate rc=1 (stash kept for the outer catch).
;; Every defun has even arity and every extern-call is argument 0 at its call
;; site so rsp == 0 mod 16 at the call (SysV ABI alignment for the variadic
;; nelisp_eval_call path).  Stash region: TAG @268435480, VAL @268435512,
;; flag @268435472 (reserved by `nelisp-standalone--arena-source').
;; ===================================================================
(defconst nelisp-standalone--catch-throw-source
  '(seq
    (defun nl_ct_copy32 (dst src _p2 _p3)
      (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
           (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
           (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
           (ptr-write-u64 dst 24 (ptr-read-u64 src 24))
           0))
    ;;================= THROW =================
    (defun nl_ct_throw_after_val (rc val_slot tag_slot _p3)
      (if (= rc 0)
          (seq
           (nl_ct_copy32 268435480 tag_slot 0 0)
           (nl_ct_copy32 268435512 val_slot 0 0)
           (ptr-write-u64 268435472 0 1)
           1)
        1))
    (defun nl_ct_throw_after_tag (rc val_form env out tag_slot val_slot)
      (if (= rc 0)
          (nl_ct_throw_after_val
           (extern-call nelisp_eval_call val_form env val_slot)
           val_slot tag_slot 0)
        1))
    (defun nl_ct_throw_eval_tag (tag_form val_form env out tag_slot val_slot)
      (nl_ct_throw_after_tag
       (extern-call nelisp_eval_call tag_form env tag_slot)
       val_form env out tag_slot val_slot))
    (defun nl_ct_throw_got_cdr1 (cdr1 tag_form env out tag_slot val_slot)
      (nl_ct_throw_eval_tag
       tag_form
       (extern-call nl_cons_car_ptr cdr1)
       env out tag_slot val_slot))
    (defun nl_ct_throw_got_tag_form (tag_form args env out tag_slot val_slot)
      (nl_ct_throw_got_cdr1
       (extern-call nl_cons_cdr_ptr args)
       tag_form env out tag_slot val_slot))
    (defun nl_sf_throw (args env out _pad)
      (if (= (sexp-tag args) 7)
          (let* ((tag_slot (alloc-bytes 32 8)) (val_slot (alloc-bytes 32 8)))
            (nl_ct_throw_got_tag_form
             (extern-call nl_cons_car_ptr args)
             args env out tag_slot val_slot))
        1))
    ;;================= CATCH =================
    (defun nl_ct_catch_on_match (eqres tag_slot env out _p4 _p5)
      (if (= eqres 1)
          (seq (nl_ct_copy32 out 268435512 0 0)
               (ptr-write-u64 268435472 0 0)
               0)
        1))
    (defun nl_ct_catch_check_tag (tag_slot env out eqres_slot _p4 _p5)
      (nl_ct_catch_on_match
       (seq (nelisp_eq_symbol 268435480 tag_slot eqres_slot)
            (ptr-read-u64 eqres_slot 0))
       tag_slot env out 0 0))
    (defun nl_ct_catch_caught (_rc tag_slot env out eqres_slot _p5)
      (if (= (ptr-read-u64 268435472 0) 1)
          (nl_ct_catch_check_tag tag_slot env out eqres_slot 0 0)
        1))
    (defun nl_ct_catch_body_step (rc body_rest tag_slot env out eqres_slot)
      (if (= rc 0)
          (nl_ct_catch_body body_rest tag_slot env out eqres_slot 0)
        (nl_ct_catch_caught rc tag_slot env out eqres_slot 0)))
    (defun nl_ct_catch_body_eval (car body_rest tag_slot env out eqres_slot)
      (nl_ct_catch_body_step
       (extern-call nelisp_eval_call car env out)
       body_rest tag_slot env out eqres_slot))
    (defun nl_ct_catch_body_cdr (body_rest body tag_slot env out eqres_slot)
      (nl_ct_catch_body_eval
       (extern-call nl_cons_car_ptr body)
       body_rest tag_slot env out eqres_slot))
    (defun nl_ct_catch_body (body tag_slot env out eqres_slot _p5)
      (if (= (sexp-tag body) 0)
          0
        (nl_ct_catch_body_cdr
         (extern-call nl_cons_cdr_ptr body)
         body tag_slot env out eqres_slot)))
    (defun nl_ct_catch_after_tag (rc body tag_slot env out eqres_slot)
      (if (= rc 0)
          (nl_ct_catch_body body tag_slot env out eqres_slot 0)
        1))
    (defun nl_ct_catch_got_body (body tag_form env out tag_slot eqres_slot)
      (nl_ct_catch_after_tag
       (extern-call nelisp_eval_call tag_form env tag_slot)
       body tag_slot env out eqres_slot))
    (defun nl_ct_catch_got_tag_form (tag_form args env out tag_slot eqres_slot)
      (nl_ct_catch_got_body
       (extern-call nl_cons_cdr_ptr args)
       tag_form env out tag_slot eqres_slot))
    (defun nl_sf_catch (args env out _pad)
      (if (= (sexp-tag args) 7)
          (let* ((tag_slot (alloc-bytes 32 8)) (eqres_slot (alloc-bytes 32 8)))
            (nl_ct_catch_got_tag_form
             (extern-call nl_cons_car_ptr args)
             args env out tag_slot eqres_slot))
        1)))
  "M6 catch/throw special-form impls.  nl_sf_catch/nl_sf_throw are dispatched
from the patched combiner-cons (see `nelisp-standalone--patch-combiner-cons').")

;; M6 special-form name predicates injected into the combiner-cons unit (so they
;; can call its in-unit `nl_cons_sym_eq').  u64 packings: "catch"=448345170275,
;; "throw"=512970877044.
(defconst nelisp-standalone--sp-eq-catch
  '(defun nl_sp_eq_catch (name_ptr)
     (let* ((buf (alloc-bytes 8 1)))
       (seq (ptr-write-u64 buf 0 448345170275)
            (nl_cons_sym_eq name_ptr buf 5)))))

(defconst nelisp-standalone--sp-eq-throw
  '(defun nl_sp_eq_throw (name_ptr)
     (let* ((buf (alloc-bytes 8 1)))
       (seq (ptr-write-u64 buf 0 512970877044)
            (nl_cons_sym_eq name_ptr buf 5)))))

(defun nelisp-standalone--patch-apply-special (form)
  "Rewrite the nl_apply_special defun FORM: replace its terminal else `2'
(unknown-special-form) with catch/throw dispatch arms, keeping the existing
if-chain intact."
  (let* ((name (cadr form))
         (arglist (caddr form))
         (body (cdddr form)))
    (cl-labels
        ((rewrite (node)
           (cond
            ((eq node 2)
             '(if (= (nl_sp_eq_catch name_ptr) 1)
                  (nl_sf_catch args_ptr env_ptr out 0)
                (if (= (nl_sp_eq_throw name_ptr) 1)
                    (nl_sf_throw args_ptr env_ptr out 0)
                  2)))
            ((consp node) (cons (rewrite (car node)) (rewrite (cdr node))))
            (t node))))
      (append (list 'defun name arglist) (mapcar #'rewrite body)))))

(defun nelisp-standalone--patch-combiner-cons (src)
  "Return combiner-cons SRC (a `(seq (defun ...) ...)') with the two
nl_sp_eq_catch / nl_sp_eq_throw predicates inserted before nl_apply_special and
that defun's terminal `2' rewritten to dispatch nl_sf_catch / nl_sf_throw.
Keeps lisp/ pristine (mirrors `--patch-combiner-apply')."
  (cons (car src)
        (let (out inserted)
          (dolist (form (cdr src))
            (cond
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_apply_special))
              (unless inserted
                (push nelisp-standalone--sp-eq-catch out)
                (push nelisp-standalone--sp-eq-throw out)
                (setq inserted t))
              (push (nelisp-standalone--patch-apply-special form) out))
             (t (push form out))))
          (nreverse out))))

;; ===================================================================
;; MACRO-EXPANSION IN-PLACE CACHING (Wave-1 (A)).  A second build-time sexp patch
;; on the combiner-cons unit, COMPOSED AFTER the catch/throw patch above (both
;; patch the same source, so chain them: catch/throw first, then macro-cache).
;;
;; It rewrites two defuns:
;;   * nl_cons_macro_apply_eval — gains a leading `box_ptr' parameter (the
;;     original form's NlConsBox pointer = &car @ box+0, &cdr @ box+32).  After
;;     computing the macro expansion it OVERWRITES the original form box's car+cdr
;;     (64 bytes) with the expansion box's car+cdr (8x ptr-write-u64), IN PLACE,
;;     so the form the lambda body holds BECOMES the expansion.  The next time
;;     that body form is evaluated its head is the expansion's head (if/+/progn/
;;     ...) not the macro name -> the macro is never re-expanded.  Caching is
;;     restricted to Cons expansions (cond/when/unless/myif all expand to a Cons);
;;     non-Cons expansions (macro->constant/symbol) fall back to the un-cached
;;     eval (provably safe -- overwriting a 64-byte cons box with a 32-byte
;;     self-eval value would leave a stale cdr / wrong tag).
;;   * nl_eval_inner_cons — its macro arm threads `head_ptr' (= the box pointer)
;;     as that new first argument of nl_cons_macro_apply_eval.
;; ===================================================================
(defconst nelisp-standalone--macro-cache-apply-eval
  '(defun nl_cons_macro_apply_eval (box_ptr func_ptr tail_ptr env out)
     ;; box_ptr: NlConsBox* of the ORIGINAL form (= &car @ box+0, &cdr @ box+32).
     ;; func_ptr: resolved macro value, shape (macro . (CLOSURE . nil)).
     ;; tail_ptr: unevaluated arg list (= original box's cdr value).
     (let* ((func_cdr (nl_cons_cdr_ptr func_ptr)))
       (if (= (ptr-read-u64 func_cdr 0) 7)
           (let* ((macrofn_ptr (nl_cons_car_ptr func_cdr))
                  (exp_slot (alloc-bytes 32 8))
                  (env_ptr env))
             (let* ((rc_mac (nl_apply_function macrofn_ptr tail_ptr env_ptr exp_slot)))
               (if (= rc_mac 0)
                   ;; Expansion computed.  Cache it in place IFF it is a Cons
                   ;; (tag 7): copy the expansion box's car+cdr (64 bytes) over
                   ;; the ORIGINAL form box, then eval the now-rewritten form.
                   (if (= (ptr-read-u64 exp_slot 0) 7)
                       (let* ((exp_box (nl_cons_car_ptr exp_slot)))
                         (seq
                          ;; original.car (@box_ptr+0..32) <- expansion.car
                          (ptr-write-u64 box_ptr 0  (ptr-read-u64 exp_box 0))
                          (ptr-write-u64 box_ptr 8  (ptr-read-u64 exp_box 8))
                          (ptr-write-u64 box_ptr 16 (ptr-read-u64 exp_box 16))
                          (ptr-write-u64 box_ptr 24 (ptr-read-u64 exp_box 24))
                          ;; original.cdr (@box_ptr+32..64) <- expansion.cdr
                          (ptr-write-u64 box_ptr 32 (ptr-read-u64 exp_box 32))
                          (ptr-write-u64 box_ptr 40 (ptr-read-u64 exp_box 40))
                          (ptr-write-u64 box_ptr 48 (ptr-read-u64 exp_box 48))
                          (ptr-write-u64 box_ptr 56 (ptr-read-u64 exp_box 56))
                          ;; Now eval the rewritten form: head = box.car (= exp
                          ;; head), tail = box.cdr.  box_ptr IS &car, box_ptr+32
                          ;; IS &cdr -- exactly nl_eval_inner_cons's (head,tail).
                          (nl_eval_inner_cons box_ptr (+ box_ptr 32) env_ptr out)))
                     ;; Non-Cons expansion: do not cache (the form is a 64-byte
                     ;; cons box; overwriting it with a 32-byte self-eval value
                     ;; would leave a stale cdr / wrong tag).  Eval directly.
                     (nelisp_eval_call exp_slot env_ptr out))
                 1)))
         (nl_cons_stash_void_function env func_ptr))))
  "Macro-caching replacement for nl_cons_macro_apply_eval (gains box_ptr arg).")

(defun nelisp-standalone--macro-cache-patch-eval-inner-cons (form)
  "Rewrite the nl_eval_inner_cons defun FORM: its macro arm
`(nl_cons_macro_apply_eval func_slot tail_ptr env out)' gains the leading
`head_ptr' argument (= the original form's box pointer)."
  (cl-labels
      ((rw (node)
         (cond
          ((and (consp node) (eq (car node) 'nl_cons_macro_apply_eval))
           ;; (nl_cons_macro_apply_eval func_slot tail_ptr env out)
           ;;   -> (nl_cons_macro_apply_eval head_ptr func_slot tail_ptr env out)
           (cons 'nl_cons_macro_apply_eval (cons 'head_ptr (mapcar #'rw (cdr node)))))
          ((consp node) (cons (rw (car node)) (rw (cdr node))))
          (t node))))
    (rw form)))

(defun nelisp-standalone--patch-macro-cache (src)
  "Patch combiner-cons SRC: swap nl_cons_macro_apply_eval for the caching version
and thread head_ptr at its call site in nl_eval_inner_cons.  COMPOSE AFTER
`nelisp-standalone--patch-combiner-cons' (apply catch/throw first, then this)."
  (cons (car src)
        (mapcar
         (lambda (form)
           (cond
            ((and (consp form) (eq (car form) 'defun)
                  (eq (cadr form) 'nl_cons_macro_apply_eval))
             nelisp-standalone--macro-cache-apply-eval)
            ((and (consp form) (eq (car form) 'defun)
                  (eq (cadr form) 'nl_eval_inner_cons))
             (nelisp-standalone--macro-cache-patch-eval-inner-cons form))
            (t form)))
         (cdr src))))

(defun nelisp-standalone--patch-combiner-cons-full (src)
  "Apply BOTH combiner-cons build-time patches in order: the catch/throw dispatch
patch (`--patch-combiner-cons') first, then the macro-expansion in-place caching
patch (`--patch-macro-cache').  Both patch the same combiner-cons source."
  (nelisp-standalone--patch-macro-cache
   (nelisp-standalone--patch-combiner-cons src)))

;; CORRECTED arg-list walk.  Byte-identical to nelisp-cc-evalport-combiner-arglist
;; --source EXCEPT the success tail wraps the constructor in (seq ... 0): the
;; pure-elisp `cons-make' op returns the SLOT pointer (nonzero) in rax, which the
;; real nl_eval_inner_cons rc==0 check would reject.  The per-arg evaluation
;; (REAL nelisp_eval_call on each car) is unchanged.
(defconst nelisp-standalone--arglist-source
  '(seq
    (defun nl_write_nil_slot (slot)
      (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0)
           (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0))
    (defun nl_eval_arg_list_walk (cur_ptr env_ptr acc_slot)
      (if (= (ptr-read-u64 cur_ptr 0) 7)
          (let* ((car_ptr (nl_cons_car_ptr cur_ptr)) (cdr_ptr (nl_cons_cdr_ptr cur_ptr))
                 (eval_slot (alloc-bytes 32 8)) (rest_slot (alloc-bytes 32 8)))
            (let* ((rc_eval (nelisp_eval_call car_ptr env_ptr eval_slot)))
              (if (= rc_eval 0)
                  (let* ((rc_rest (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot)))
                    (if (= rc_rest 0)
                        (seq (nelisp_cons_construct eval_slot rest_slot acc_slot) 0)
                      1))
                1)))
        (nl_write_nil_slot acc_slot)))
    (defun nl_eval_arg_list (args_ptr env out_list_slot)
      (let* ((env_ptr env)) (nl_eval_arg_list_walk args_ptr env_ptr out_list_slot)))))

;; TRAP-STUBS — externs genuinely never executed on the (OP A B) builtin path
;; (special-form bodies, eval-inner var/cell branches, lambda/closure, frame/bind
;; shims, nelisp_aot_builtin_call1).  Each returns a sentinel; probes confirmed
;; none run for a plain (builtin . (+ . nil)) application.
(defconst nelisp-standalone--trap-source
  '(seq
    (defun nl_sf_if (_a _e _o _p) 1) (defun nl_sf_let (_a _e _o _p) 1)
    (defun nl_sf_let_star (_a _e _o _p) 1) (defun nl_sf_setq (_a _e _o _p) 1)
    (defun nl_sf_while (_a _e _o _p) 1) (defun nl_sf_lambda (_a _e _o _p) 1)
    (defun nl_sf_function (_a _e _o _p) 1) (defun nl_sf_condition_case (_a _e _o _p) 1)
    (defun nl_sf_unwind_protect (_a _e _o _p) 1) (defun nl_sf_quote (_a _o) 1)
    (defun nl_sf_progn (_a _e _o _p) 1)
    (defun nl_env_lookup_val (_f _e _o) 1) (defun nl_cell_get_value (_c _o) 1)
    (defun nl_apply_lambda_inner (_cap _f _b _a _e _o) 1)
    (defun nelisp_frame_push (_f _s) 1) (defun nelisp_frame_pop (_f _s) 1)
    (defun nelisp_env_bind_local (_m _f _n _v _vec _flag) 1)
    (defun nelisp_env_shim_op (_op _m _s _u _o _p) 0)
    (defun nelisp_env_shim_set_op (_op _m _s _sc _o _z) 0)
    (defun nl_env_push_captured (_e _a) 1)
    (defun nelisp_aot_builtin_call1 (_a _b _c _d) 1)))

;; ===================================================================
;; _start unit (always first): capture the entry stack pointer in rdi
;; (mov rdi,rsp) BEFORE the call perturbs rsp, call driver(sp); mov edi,eax;
;; exit(60).  At process entry rsp -> argc, [rsp+8]=argv[0], [rsp+16]=argv[1].
;; The dual-mode reader driver reads argv[1] = (ptr-read-u64 sp 16); the
;; baked-form eval `driver' takes no args and simply ignores rdi (ABI-safe).
;; Byte layout (reloc :offset = 4 because of the 3-byte mov prefix):
;;   48 89 e7          mov  rdi, rsp     ; rdi = &argc  (driver arg0)
;;   e8 00 00 00 00    call driver       ; reloc pc32 @ offset 4
;;   89 c7             mov  edi, eax      ; exit code = driver()
;;   b8 3c 00 00 00    mov  eax, 60       ; SYS_exit
;;   0f 05            syscall
;; ===================================================================
(defun nelisp-standalone--start-unit ()
  (nelisp-link-unit-make "start.o"
   (list (cons 'text (unibyte-string
                      #x48 #x89 #xe7           ; mov rdi, rsp
                      #xe8 0 0 0 0              ; call driver  (reloc @4)
                      #x89 #xc7                 ; mov edi, eax
                      #xb8 60 0 0 0             ; mov eax, 60
                      #x0f #x05)))              ; syscall
   (list (nelisp-link-symbol "_start" 0 :section 'text :bind 'global :type 'func))
   (list (list :offset 4 :type 'pc32 :symbol "driver" :addend 0 :section 'text))))

;; ===================================================================
;; Parametrized driver (FULL real path).  Bootstrap mirror + 60 system builtins,
;; install OP via REAL nl_install_one (arithmetic is not in the bootstrap set),
;; build EvalCtx as real nl_eval_ctx_make does, construct (OP A B), eval via the
;; REAL shim -> nl_eval_inner -> ... -> nelisp_apply_function.  Always recompiled.
;; ===================================================================
(defun nelisp-standalone--form-params ()
  "Return (OP-U64 A B EXPECTED) for the embedded form from the environment."
  (let* ((op (or (getenv "NELISP_FORM_OP") "+"))
         (a (string-to-number (or (getenv "NELISP_FORM_A") "1")))
         (b (string-to-number (or (getenv "NELISP_FORM_B") "2")))
         (expected (cond ((string= op "+") (+ a b))
                         ((string= op "-") (- a b))
                         ((string= op "*") (* a b))
                         (t (error "standalone: unsupported OP %S (use + - *)" op)))))
    (list (aref op 0) a b expected)))

(defun nelisp-standalone--driver-source ()
  (pcase-let ((`(,op-u64 ,a ,b ,_exp) (nelisp-standalone--form-params)))
    `(defun driver ()
       (let* ((arena (nl_arena_init))
              (globals (alloc-bytes 32 8)) (frames (alloc-bytes 32 8)) (unbound (alloc-bytes 32 8))
              (ctx (alloc-bytes 120 8))
              (opbuf (alloc-bytes 8 1)) (op_sym (alloc-bytes 32 8))
              (builtin_buf (alloc-bytes 8 1)) (builtin_sym (alloc-bytes 32 8))
              (int1 (alloc-bytes 32 8)) (int2 (alloc-bytes 32 8))
              (nil_s (alloc-bytes 32 8)) (tail_s (alloc-bytes 32 8))
              (args_s (alloc-bytes 32 8)) (form_s (alloc-bytes 32 8)) (out_s (alloc-bytes 32 8)))
         (seq
          (nl_bootstrap_make_mirror globals frames unbound)
          (ptr-write-u64 builtin_buf 0 31078196194145634)
          (nl_alloc_symbol builtin_buf 7 builtin_sym)
          (ptr-write-u64 opbuf 0 ,op-u64)
          (nl_install_one globals unbound opbuf 1 builtin_sym)
          (nl_sexp_clone_into globals (+ ctx 0))
          (nl_sexp_clone_into frames (+ ctx 32))
          (nl_sexp_clone_into unbound (+ ctx 64))
          ;; rec_max 2000: below the ~2500-level native C-stack ceiling (each eval level
;; is several KB of native frames), so deep recursion errors at the guard rather
;; than SIGSEGV.  The toolchain recurses by AST-nesting depth (tens), far under this.
(ptr-write-u64 ctx 96 0) (ptr-write-u64 ctx 104 2000)
          (nl_alloc_symbol opbuf 1 op_sym)
          (ptr-write-u64 int1 0 2) (ptr-write-u64 int1 8 ,a) (ptr-write-u64 int1 16 0) (ptr-write-u64 int1 24 0)
          (ptr-write-u64 int2 0 2) (ptr-write-u64 int2 8 ,b) (ptr-write-u64 int2 16 0) (ptr-write-u64 int2 24 0)
          (ptr-write-u64 nil_s 0 0) (ptr-write-u64 nil_s 8 0) (ptr-write-u64 nil_s 16 0) (ptr-write-u64 nil_s 24 0)
          (nelisp_cons_construct int2 nil_s tail_s)
          (nelisp_cons_construct int1 tail_s args_s)
          (nelisp_cons_construct op_sym args_s form_s)
          (ptr-write-u64 out_s 0 0) (ptr-write-u64 out_s 8 0) (ptr-write-u64 out_s 16 0) (ptr-write-u64 out_s 24 0)
          (nelisp_eval_call form_s ctx out_s)
          (ptr-read-u64 out_s 8))))))

;; ===================================================================
;; Manifest — ORDERED unit list.  Each entry: (NAME KIND SRC).
;;   KIND = a feature symbol  -> (require it); SRC = its --source defvar symbol
;;        = :glue              -> SRC is a local glue defconst symbol
;;        = :start / :driver   -> synthesized specially
;; _start first, arena last (linker convention).
;; ===================================================================
(defconst nelisp-standalone--manifest
  '(("start.o"            :start  nil)
    ("driver.o"           :driver nil)
    ("shim.o"             :glue   nelisp-standalone--shim-source)
    ("applyfn.o"          :glue   nelisp-standalone--applyfn-baked-source)
    ("eval-inner.o"       nelisp-cc-eval-inner                    nelisp-cc-eval-inner--source)
    ("combiner-cons.o"    nelisp-cc-evalport-combiner-cons        nelisp-cc-evalport-combiner-cons--source)
    ("combiner-apply.o"   nelisp-cc-evalport-combiner-apply       nelisp-cc-evalport-combiner-apply--source)
    ("arglist.o"          :glue   nelisp-standalone--arglist-source)
    ("lookup-fn.o"        nelisp-cc-env-lookup-function           nelisp-cc-env-lookup-function--source)
    ("bootstrap.o"        nelisp-cc-evalport-bootstrap            nelisp-cc-evalport-bootstrap--source)
    ("mirror-lookup.o"    nelisp-cc-mirror-lookup-entry           nelisp-cc-mirror-lookup-entry--source)
    ("mirror-alloc.o"     nelisp-cc-mirror-alloc-entry            nelisp-cc-mirror-alloc-entry--source)
    ("mirror-prepend.o"   nelisp-cc-mirror-bucket-prepend         nelisp-cc-mirror-bucket-prepend--source)
    ("mirror-setfn.o"     nelisp-cc-mirror-set-function-or-insert nelisp-cc-mirror-set-function-or-insert--source)
    ("env-install.o"      nelisp-cc-env-install-empty             nelisp-cc-env-install-empty--source)
    ("fnv1a.o"            nelisp-cc-fnv1a                         nelisp-cc-fnv1a--source)
    ("clone.o"            nelisp-cc-sexp-clone-into               nelisp-cc-sexp-clone-into--source)
    ("car-ptr.o"          nelisp-cc-jit-cons-car-ptr              nelisp-cc-jit-cons-car-ptr--source)
    ("cdr-ptr.o"          nelisp-cc-jit-cons-cdr-ptr              nelisp-cc-jit-cons-cdr-ptr--source)
    ("eq-symbol.o"        nelisp-cc-eq-symbol                     nelisp-cc-eq-symbol--source)
    ("cons-ctor.o"        nelisp-cc-cons-construct                nelisp-cc-cons-construct--source)
    ("consbox.o"          nelisp-cc-nlconsbox-alloc               nelisp-cc-nlconsbox-alloc--source)
    ("consbox-clone.o"    nelisp-cc-nlconsbox-clone               nelisp-cc-nlconsbox-clone--source)
    ("consbox-setcar.o"   nelisp-cc-nlconsbox-set-car             nelisp-cc-nlconsbox-set-car--source)
    ("consbox-setcdr.o"   nelisp-cc-nlconsbox-set-cdr             nelisp-cc-nlconsbox-set-cdr--source)
    ("vec-alloc.o"        nelisp-cc-nlvector-alloc                nelisp-cc-nlvector-alloc--source)
    ("vec-set.o"          nelisp-cc-nlvector-set-slot             nelisp-cc-nlvector-set-slot--source)
    ("vec-clone.o"        nelisp-cc-nlvector-clone                nelisp-cc-nlvector-clone--source)
    ("alloc-str.o"        nelisp-cc-nlstr-direct-ops              nelisp-cc-nlstr-direct-ops--alloc-str-source)
    ("str-bytes.o"        nelisp-cc-nlstr-direct-ops              nelisp-cc-nlstr-direct-ops--str-bytes-ptr-source)
    ("str-clone.o"        nelisp-cc-nlstr-clone                   nelisp-cc-nlstr-clone--source)
    ("cell-clone.o"       nelisp-cc-nlcell-clone                  nelisp-cc-nlcell-clone--source)
    ("chartable-clone.o"  nelisp-cc-nlchartable-clone             nelisp-cc-nlchartable-clone--source)
    ("boolvec-clone.o"    nelisp-cc-nlboolvector-clone            nelisp-cc-nlboolvector-clone--source)
    ("record-alloc.o"     nelisp-cc-nlrecord-alloc                nelisp-cc-nlrecord-alloc--source)
    ("record-clone.o"     nelisp-cc-nlrecord-clone                nelisp-cc-nlrecord-clone--source)
    ("record-set.o"       nelisp-cc-nlrecord-set-slot             nelisp-cc-nlrecord-set-slot--source)
    ("trap.o"             :glue   nelisp-standalone--trap-source)
    ("arena.o"            :glue   nelisp-standalone--arena-source))
  "Ordered standalone-eval unit manifest.")

(defun nelisp-standalone--unit-for (entry)
  "Produce the link-unit for a manifest ENTRY."
  (pcase-let ((`(,name ,kind ,src) entry))
    (pcase kind
      (:start (nelisp-standalone--start-unit))
      ;; driver is form-dependent and tiny: always recompiled, never cached.
      (:driver (let ((u (nelisp-standalone--compile-to-unit "driver.o"
                                                            (nelisp-standalone--driver-source))))
                 (push "driver.o" nelisp-standalone--recompiled) u))
      (:glue (nelisp-standalone--cached-unit name (symbol-value src)
                                             nelisp-standalone--this-file))
      (_ ;; feature unit
       (require kind)
       (nelisp-standalone--cached-unit name (symbol-value src)
                                       (locate-library (symbol-name kind)))))))

;;;###autoload
(defun nelisp-standalone-compile-chunk ()
  "Compile this worker's slice of the cacheable units to the cache (NO link).
Reads NELISP_CHUNK_IDX / NELISP_CHUNK_N from the environment; a worker takes the
cacheable manifest positions where (mod POS N) == IDX, skipping :start (synthesized)
and :driver (always recompiled at link time).  Concurrency-safe: each unit writes
its own NAME.unit file, so N processes never contend.  Used by the multi-process
parallel build (tools/build-standalone-parallel.sh).

NOTE: for the current 37-unit set the per-unit compile cost (~0.4s total) is far
below the per-process emacs startup + module-load cost (~4s), so the serial build
is faster.  Parallelism pays off only once per-unit compilation dominates startup
(e.g. many more / much heavier units)."
  (let* ((idx (string-to-number (or (getenv "NELISP_CHUNK_IDX") "0")))
         (n   (max 1 (string-to-number (or (getenv "NELISP_CHUNK_N") "1"))))
         (pos 0))
    (setq nelisp-standalone--recompiled nil)
    (dolist (entry nelisp-standalone--manifest)
      (unless (memq (nth 1 entry) '(:start :driver))
        (when (= (mod pos n) idx)
          (nelisp-standalone--unit-for entry))
        (setq pos (1+ pos))))
    (message "[standalone] chunk %d/%d compiled: %s" idx n
             (if nelisp-standalone--recompiled
                 (string-join (reverse nelisp-standalone--recompiled) " ")
               "none (all cached)"))))

;;;###autoload
(defun nelisp-standalone-build ()
  "Incrementally build the standalone NeLisp eval ELF; return its path."
  (setq nelisp-standalone--recompiled nil)
  (let ((units (mapcar #'nelisp-standalone--unit-for nelisp-standalone--manifest)))
    (nelisp-link-units nelisp-standalone--out units)
    (set-file-modes nelisp-standalone--out #o755)
    (message "[standalone] linked %d units -> %s" (length units) nelisp-standalone--out)
    (message "[standalone] recompiled this build: %s"
             (if nelisp-standalone--recompiled
                 (string-join (reverse nelisp-standalone--recompiled) " ")
               "none (all served from cache)"))
    nelisp-standalone--out))

;;;###autoload
(defun nelisp-standalone-rebuild-one (name)
  "Force-recompile just unit NAME (e.g. \"eq-symbol.o\") then relink."
  (let ((cache (expand-file-name (concat name ".unit") nelisp-standalone--cache-dir)))
    (when (file-exists-p cache) (delete-file cache)))
  (nelisp-standalone-build))

;;;###autoload
(defun nelisp-standalone-clean ()
  "Delete the per-unit object cache (forces a full recompile next build)."
  (when (file-directory-p nelisp-standalone--cache-dir)
    (delete-directory nelisp-standalone--cache-dir t))
  (message "[standalone] cache cleaned: %s" nelisp-standalone--cache-dir))

;;;###autoload
(defun nelisp-standalone-test ()
  "Build then run the standalone binary; assert exit == expected.  Exits 0/1."
  (nelisp-standalone-build)
  (pcase-let ((`(,_op ,_a ,_b ,expected) (nelisp-standalone--form-params)))
    (let ((code (call-process nelisp-standalone--out nil nil nil)))
      (if (= code expected)
          (progn (message "[standalone] PASS: %s -> exit %d (expected %d)"
                          nelisp-standalone--out code expected)
                 (kill-emacs 0))
        (message "[standalone] FAIL: %s -> exit %d (expected %d)"
                 nelisp-standalone--out code expected)
        (kill-emacs 1)))))

;; ===================================================================
;; READER PATH (Doc 137 M1) — text -> AOT reader -> eval, ZERO Rust.
;;
;; Instead of evaluating a HAND-BUILT (OP A B) AST, the binary parses
;; embedded SOURCE TEXT through the pure-elisp AOT reader
;; (nelisp_reader_parse_one, the FIRST non-Rust caller of it) and then
;; evals the parsed form.  The eval flow after parse is byte-identical to
;; the baked-form driver; only the form's origin changes (real reader vs.
;; hand-built slots).  This is the REPL floor of the self-hosting ladder.
;;
;; Source via NELISP_SRC (default "(+ 40 2)" -> 42).  Supported builtins
;; are still + - * (M2 grows these); the embedded form must use them.
;;   make standalone-reader        # build target/nelisp-standalone-reader
;;   make standalone-reader-test   # build, run, assert eval(NELISP_SRC)
;; ===================================================================

(defconst nelisp-standalone--reader-extra-manifest
  '(("reader-lexer.o"     nelisp-cc-reader-lexer                 nelisp-cc-reader-lexer--source)
    ("reader-parser.o"    nelisp-cc-reader-parser                nelisp-cc-reader-parser--source)
    ("str-to-float.o"     nelisp-cc-evalport-str-to-float        nelisp-cc-evalport-str-to-float--source)
    ("mut-str-push.o"     nelisp-cc-evalport-nonenv-mut-str-push nelisp-cc-evalport-nonenv-mut-str-push--source)
    ("alloc-mut-str.o"    nelisp-cc-nlstr-direct-ops             nelisp-cc-nlstr-direct-ops--alloc-mut-str-source)
    ("mut-str-finalize.o" nelisp-cc-nlstr-direct-ops             nelisp-cc-nlstr-direct-ops--mut-str-finalize-source)
    ("ptr-read-u8.o"      nelisp-cc-atomic-raw-mem               nelisp-cc-atomic-raw-mem--read-u8-source)
    ("ptr-write-u8.o"     nelisp-cc-atomic-raw-mem               nelisp-cc-atomic-raw-mem--write-u8-source)
    ("alloc-bytes-fn.o"   nelisp-cc-alloc-dealloc                nelisp-cc-alloc-dealloc--alloc-bytes-source)
    ("dealloc-bytes-fn.o" nelisp-cc-alloc-dealloc                nelisp-cc-alloc-dealloc--dealloc-bytes-source))
  "Extra units the reader path needs beyond `nelisp-standalone--manifest'.
The reader's grammar ops (mut-str-make-empty/-push-byte/-finalize, the
raw-mem u8 ops, alloc/dealloc) lower to runtime extern calls that the
baked-form path never references; these units resolve them.")

;; nl_sexp_write_float: runtime symbol with no elisp `defun' (only the
;; Float-token path calls it via nl_str_to_float).  All arithmetic inputs
;; are integers, so it never executes -- linked as a never-run stub purely
;; so the parser's extern resolves.
(defconst nelisp-standalone--reader-float-stub-source
  '(seq (defun nl_sexp_write_float (slot _val) (seq (ptr-write-u64 slot 0 9) slot))))

(defun nelisp-standalone--reader-src ()
  "Embedded source text for the reader build (NELISP_SRC; default \"(+ 40 2)\")."
  (or (getenv "NELISP_SRC") "(+ 40 2)"))

(defun nelisp-standalone--reader-expected ()
  "Host-Emacs oracle: eval ALL top-level forms of NELISP_SRC; return the last
value (matches the binary's M8 read+eval-loop driver)."
  (let ((forms (car (read-from-string
                     (concat "(" (nelisp-standalone--reader-src) "\n)"))))
        (r nil))
    (dolist (f forms r) (setq r (eval f t)))))

(defconst nelisp-standalone--reader-builtins
  '("+" "-" "*" "/" "mod" "1+" "1-" "=" "<" ">" "<=" ">=" "car" "cdr" "cons" "list" "eq" "null" "not"
    ;; M4 hash tables
    "make-hash-table" "puthash" "gethash" "remhash" "hash-table-count" "maphash"
    ;; M5 strings + format
    "length" "concat" "substring" "make-string" "string="
    "char-to-string" "string-to-char" "number-to-string" "string-to-number" "format"
    ;; M7 file I/O
    "wrf" "rdf" "slen"
    ;; Wave-1 (B) breadth: predicates / symbol+vector ops / equal / setcar-setcdr
    ;; / signal-error (the names back the breadth arms in the reader applyfn).
    "consp" "atom" "stringp" "symbolp" "integerp" "natnump" "numberp" "floatp"
    "vectorp" "listp" "zerop" "fboundp" "boundp"
    "symbol-name" "intern" "make-symbol"
    "make-vector" "vector" "aref" "aset"
    "signal" "error" "equal" "setcar" "setcdr")
  "Builtin names installed into the reader binary's mirror; each is dispatched by
the pure-elisp `nelisp_apply_function' (see `nelisp-standalone--applyfn-source').
Names > 8 bytes (e.g. \"make-hash-table\") require the full-length name-buffer
install in `nelisp-standalone--reader-driver-source' + the `sexp-name-eq' dispatch
arm in `nelisp-standalone--applyfn-dispatch-table'.")

(defconst nelisp-standalone--reader-read-cap 4194304
  "4 MiB read cap for the file-load path (plenty for any single .el).")

(defun nelisp-standalone--name-words (nm)
  "Return the little-endian u64 words packing NM's UTF-8 bytes (ceil(len/8) of
them).  Used to install builtin names of ANY length: the stock single-u64
buffer corrupts names > 8 bytes because nl_alloc_symbol reads `name_len' bytes
from an 8-byte buffer."
  (let* ((bytes (encode-coding-string nm 'utf-8 t))
         (n (length bytes))
         (words nil) (i 0))
    (while (< i n)
      (let ((w 0) (j 0))
        (while (and (< j 8) (< (+ i j) n))
          (setq w (logior w (ash (aref bytes (+ i j)) (* j 8))))
          (setq j (1+ j)))
        (push w words))
      (setq i (+ i 8)))
    (nreverse words)))

(defun nelisp-standalone--reader-install-builtins-forms ()
  "Phase47 forms that install every `nelisp-standalone--reader-builtins' name
with a FULL-LENGTH name buffer (ceil(len/8) u64 words), fixing >8-byte names."
  (mapcar
   (lambda (nm)
     (let* ((words (nelisp-standalone--name-words nm))
            (cap (* (length words) 8))
            (len (length (encode-coding-string nm 'utf-8 t))))
       `(let* ((b (alloc-bytes ,cap 1)))
          (seq
           ,@(let ((w 0) (forms nil))
               (dolist (word words)
                 (push `(ptr-write-u64 b ,(* w 8) ,word) forms)
                 (setq w (1+ w)))
               (nreverse forms))
           (nl_install_one globals unbound b ,len builtin_sym)))))
   nelisp-standalone--reader-builtins))

(defun nelisp-standalone--reader-driver-source ()
  "DUAL-MODE reader driver (M7 file-load + M8 multi-form loop).
Takes the entry stack pointer SP; reads argv[1] = (ptr-read-u64 sp 16):
  argv[1] == 0  (no file arg, e.g. `make standalone-reader-test') -> use the
                EMBEDDED NELISP_SRC via `sexp-write-str-lit';
  argv[1] != 0  -> open+read that file (freestanding raw syscalls open=2/read=0/
                close=3, NOT extern-call -- no libc/PLT) and wrap the bytes via
                `nl_alloc_str'.
Then the SAME multi-form parse+eval loop runs `src'.  argc==1 => argv[1]==NULL==0
(argv is NULL-terminated) so the check is reliable.  Each builtin name installs
through a fresh, full-length arena buffer so `nl_install_one' never aliases a
reused buffer and >8-byte names install correctly."
  `(defun driver (sp)
     (let* ((arena (nl_arena_init))
            (globals (alloc-bytes 32 8)) (frames (alloc-bytes 32 8)) (unbound (alloc-bytes 32 8))
            (ctx (alloc-bytes 120 8))
            (builtin_buf (alloc-bytes 8 1)) (builtin_sym (alloc-bytes 32 8))
            (src (alloc-bytes 32 8)) (cursor (alloc-bytes 32 8))
            (result (alloc-bytes 32 8)) (pool (alloc-bytes 32 8)) (out (alloc-bytes 32 8))
            ;; argv[1] = C-string path pointer at [sp + 16] (0 if argc==1).
            (path (ptr-read-u64 sp 16))
            ;; raw read buffer (bypasses the Rust UTF-8 layer)
            (fbuf (alloc-bytes ,nelisp-standalone--reader-read-cap 1)))
       (seq
        (nl_bootstrap_make_mirror globals frames unbound)
        (ptr-write-u64 builtin_buf 0 31078196194145634)
        (nl_alloc_symbol builtin_buf 7 builtin_sym)
        ,@(nelisp-standalone--reader-install-builtins-forms)
        (nl_sexp_clone_into globals (+ ctx 0))
        (nl_sexp_clone_into frames (+ ctx 32))
        (nl_sexp_clone_into unbound (+ ctx 64))
        ;; rec_max 2000: below the ~2500-level native C-stack ceiling (each eval level
;; is several KB of native frames), so deep recursion errors at the guard rather
;; than SIGSEGV.  The toolchain recurses by AST-nesting depth (tens), far under this.
(ptr-write-u64 ctx 96 0) (ptr-write-u64 ctx 104 2000)
        ;; --- source selection: embedded vs. file (M7 dual mode) ---
        (if (= path 0)
            ;; embedded NELISP_SRC (gate path)
            (sexp-write-str-lit src ,(nelisp-standalone--reader-src))
          ;; file path: open(path,O_RDONLY) -> read -> close -> wrap as Str
          (let* ((fd (syscall-direct 2 path 0 0 0 0 0)))
            (let* ((n (syscall-direct 0 fd fbuf ,nelisp-standalone--reader-read-cap 0 0 0)))
              (seq
               (syscall-direct 3 fd 0 0 0 0 0)
               (nl_alloc_str fbuf (if (< n 0) 0 n) src)))))
        ;; --- reader path (M8): read+eval EVERY top-level form, keep the last
        ;; value.  parse_one advances the shared cursor; it returns 1 per form
        ;; and != 1 (e.g. -1 at EOF) when no more forms remain. ---
        (ptr-write-u64 cursor 0 2) (ptr-write-u64 cursor 8 0)   ; Sexp::Int 0
        (vector-make 256 pool)                                  ; Sexp::Vector(256) slot-pool
        (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
        (let* ((more 1))
          (while (= more 1)
            (seq
             (ptr-write-u64 result 0 0) (ptr-write-u64 result 8 0)
             (let* ((prc (nelisp_reader_parse_one src cursor result pool 0)))
               (if (= prc 1)
                   ;; Reset out to Nil per form: nl_sf_if (no-else, false) and
                   ;; other forms leave out untouched on a nil result, relying on
                   ;; the per-eval fresh-Nil contract.  Only reset when a form
                   ;; actually parsed, so the EOF iteration keeps the last value.
                   (seq (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                        (nelisp_eval_call result ctx out))
                 (setq more 0))))))
        (ptr-read-u64 out 8)))))

;; REAL special-form + env machinery (Doc 137 M2/M3 un-trap).  Replaces the
;; baked path's trap.o stubs so the binary is a GENUINE general interpreter for
;; the 11 special forms (if/let/let*/setq/while/progn/quote/lambda/function/
;; condition-case/unwind-protect) with real lexical environments.  The first 21
;; are the direct trap replacements; the rest are the transitive deps they pull
;; (env/frame/bind/mirror/cell ops) -- frozen here so the build is a single link.
(defconst nelisp-standalone--reader-real-sf-manifest
  '(("sf-if.o"             nelisp-cc-sf-if                        nelisp-cc-sf-if--source)
    ("sf-let.o"            nelisp-cc-sf-let                       nelisp-cc-sf-let--source)
    ("sf-let-star.o"       nelisp-cc-sf-let-star                  nelisp-cc-sf-let-star--source)
    ("sf-setq.o"           nelisp-cc-sf-setq                      nelisp-cc-sf-setq--source)
    ("sf-while.o"          nelisp-cc-sf-while                     nelisp-cc-sf-while--source)
    ("sf-progn.o"          nelisp-cc-sf-progn                     nelisp-cc-sf-progn--source)
    ("sf-quote.o"          nelisp-cc-sf-quote                     nelisp-cc-sf-quote--source)
    ("sf-lambda.o"         nelisp-cc-sf-lambda                    nelisp-cc-sf-lambda--source)
    ("sf-function.o"       nelisp-cc-sf-function                  nelisp-cc-sf-function--source)
    ("sf-cc.o"             nelisp-cc-sf-condition-case            nelisp-cc-sf-condition-case--source)
    ("sf-uwp.o"            nelisp-cc-sf-unwind-protect            nelisp-cc-sf-unwind-protect--source)
    ("env-leaves-simple.o" nelisp-cc-evalport-env-leaves-simple  nelisp-cc-evalport-env-leaves-simple--source)
    ("nlcell-get-value.o"  nelisp-cc-nlcell-get-value            nelisp-cc-nlcell-get-value--source)
    ("apply-lambda-inner.o" nelisp-cc-apply-lambda-inner         nelisp-cc-apply-lambda-inner--source)
    ("frame-push.o"        nelisp-cc-frame-push                   nelisp-cc-frame-push--source)
    ("frame-pop.o"         nelisp-cc-frame-pop                    nelisp-cc-frame-pop--source)
    ("env-bind-local.o"    nelisp-cc-env-bind-local              nelisp-cc-env-bind-local--source)
    ("env-shim-op.o"       nelisp-cc-env-shim-op                  nelisp-cc-env-shim-op--source)
    ("env-shim-set-op.o"   nelisp-cc-env-shim-set-op             nelisp-cc-env-shim-set-op--source)
    ("env-leaves-frame.o"  nelisp-cc-evalport-env-leaves-frame   nelisp-cc-evalport-env-leaves-frame--source)
    ("aot-builtin-call1.o" nelisp-cc-evalport-aot-builtin-call1  nelisp-cc-evalport-aot-builtin-call1--source)
    ("sf-eval-is-truthy.o" nelisp-cc-eval-is-truthy              nelisp-cc-eval-is-truthy--source)
    ("sf-let-setup.o"      nelisp-cc-evalport-env-leaves-logic   nelisp-cc-evalport-env-leaves-logic--source)
    ("sf-env-set-value.o"  nelisp-cc-evalport-env-leaves-bind    nelisp-cc-evalport-env-leaves-bind--source)
    ("sf-cons-prepend-clone.o" nelisp-cc-cons-prepend-clone      nelisp-cc-cons-prepend-clone--source)
    ;; sf-capture-lexical.o intentionally omitted -- replaced by the corrected
    ;; nl_env_capture_lexical in `nelisp-standalone--reader-capture-source'.
    ("sf-symbol-is-lambda.o" nelisp-cc-symbol-is-lambda          nelisp-cc-symbol-is-lambda--source)
    ("sf-env-lookup-value.o" nelisp-cc-env-lookup-value          nelisp-cc-env-lookup-value--source)
    ("sf-frame-ensure-cap.o" nelisp-cc-frame-ensure-capacity     nelisp-cc-frame-ensure-capacity--source)
    ("sf-alloc-cell.o"     nelisp-cc-nlcell-alloc                nelisp-cc-nlcell-alloc--source)
    ("sf-frame-bind.o"     nelisp-cc-frame-bind                   nelisp-cc-frame-bind--source)
    ("sf-mirror-setval.o"  nelisp-cc-mirror-set-value-or-insert  nelisp-cc-mirror-set-value-or-insert--source)
    ("sf-mirror-is-bound.o" nelisp-cc-mirror-is-bound            nelisp-cc-mirror-is-bound--source)
    ("sf-mirror-is-fbound.o" nelisp-cc-mirror-is-fbound          nelisp-cc-mirror-is-fbound--source)
    ("sf-mirror-is-const.o" nelisp-cc-mirror-is-constant         nelisp-cc-mirror-is-constant--source)
    ("sf-mirror-clearval.o" nelisp-cc-mirror-clear-value         nelisp-cc-mirror-clear-value--source)
    ("sf-mirror-clearfn.o" nelisp-cc-mirror-clear-function       nelisp-cc-mirror-clear-function--source)
    ("sf-mirror-setconst.o" nelisp-cc-mirror-set-constant-or-insert nelisp-cc-mirror-set-constant-or-insert--source)
    ("sf-bind-formals.o"   nelisp-cc-bind-formals                nelisp-cc-bind-formals--source)
    ("sf-env-set-value2.o" nelisp-cc-env-set-value               nelisp-cc-env-set-value--source)
    ("sf-frame-stack-find.o" nelisp-cc-frame-stack-find          nelisp-cc-frame-stack-find--source)
    ("sf-bf-args-nth-ptr.o" nelisp-cc-bf-args-nth-ptr            nelisp-cc-bf-args-nth-ptr--source)
    ("sf-bf-formal-tag.o"  nelisp-cc-bf-formal-tag               nelisp-cc-bf-formal-tag--source)
    ("sf-bf-precompute.o"  nelisp-cc-bf-precompute               nelisp-cc-bf-precompute--source)
    ("sf-cell-set-value.o" nelisp-cc-nlcell-set-value            nelisp-cc-nlcell-set-value--source))
  "Real special-form + env units replacing the baked path's trap.o stubs.")

;; nelisp_eval_call_with_err(form, env, out, err_out): the condition-case
;; error-trapping eval variant has no real elisp `defun' (it was a Rust shim).
;;
;; WAVE-2 condition-case trapping (PATCH 1 of 4).  The no-error path stays
;; behaviourally exact (clear err_out, delegate to the real nelisp_eval_call).
;; The NEW error path inspects the M6 signal stash shared with catch/throw:
;;   flag @268435472, stashed TAG @268435480, stashed VAL @268435512.
;; On rc=1 with the flag set, build err_out = (error_sym . data) from the arena
;; stash via `nelisp_cons_construct' (cons-ctor.o); the existing
;; `nl_cc_match_and_bind' (real defun in env-leaves-logic, via sf-let-setup.o)
;; then matches the handler clause against err_out.car.
;;
;; The flag is LEFT SET here (not cleared): a clause MATCH clears it (PATCH 4,
;; `nl_sf_cc_after_match'); on NO match the still-set flag lets an OUTER
;; condition-case re-trap the same signal (nested reraise) by rebuilding the
;; cons from the unchanged arena stash.  rc=1 on a non-signal abort (flag==0)
;; falls through to `nl_cce_clear' + the original rc, so genuine errors still
;; propagate.
(defconst nelisp-standalone--reader-errstub-source
  '(seq
    (defun nl_cce_clear (err_out)
      (seq (ptr-write-u64 err_out 0 0) (ptr-write-u64 (+ err_out 8) 0 0)
           (ptr-write-u64 (+ err_out 16) 0 0) (ptr-write-u64 (+ err_out 24) 0 0) 0))
    (defun nelisp_eval_call_with_err (form env out err_out)
      (let* ((rc (nelisp_eval_call form env out)))
        (if (= rc 0)
            (nl_cce_clear err_out)
          (if (= (ptr-read-u64 268435472 0) 1)
              (seq (nelisp_cons_construct 268435480 268435512 err_out)
                   1)
            (seq (nl_cce_clear err_out) rc))))))
  "WAVE-2 condition-case-trapping nelisp_eval_call_with_err (PATCH 1): on a
signalled abort it builds err_out=(TAG . VAL) from the M6 arena stash so
`nl_cc_match_and_bind' can match the handler; the no-error path is unchanged.")

;; CORRECTED closure-capture (Doc 137 M3).  The shipped AOT
;; `nelisp-cc-evalport-capture-lexical' passes the raw frames-record pointer to
;; `nl_capture_descend_native', which expects a 3-slot in-vec ([0]=stack record,
;; [1]=depth Int, [2]=scratch) -- so it misreads the depth and segfaults on every
;; lambda creation.  (The real Emacs path builds the in-vec in an elisp JIT
;; wrapper; the AOT port never did.)  This override reads the real depth from
;; frames.slot1 and builds the proper in-vec; depth 0 (top level) -> nil capture.
;; Replaces the sf-capture-lexical.o unit in the reader build.
(defconst nelisp-standalone--reader-capture-source
  '(seq
    (defun nl_clx_write_nil (slot)
      (seq (ptr-write-u64 slot 0 0) (ptr-write-u64 (+ slot 8) 0 0)
           (ptr-write-u64 (+ slot 16) 0 0) (ptr-write-u64 (+ slot 24) 0 0) 0))
    (defun nl_env_capture_lexical (env out)
      (let* ((frames_ptr (+ env 32))
             (depth (sexp-int-unwrap (record-slot-ref-ptr frames_ptr 1))))
        (seq
         (nl_clx_write_nil out)
         (if (= depth 0)
             0
           (let* ((invec (alloc-bytes 32 8)) (alist (alloc-bytes 32 8)))
             (seq
              (vector-make 3 invec)
              (nl_sexp_clone_into frames_ptr (vector-ref-ptr invec 0))
              (sexp-int-make (vector-ref-ptr invec 1) depth)
              (nl_clx_write_nil (vector-ref-ptr invec 2))
              (nl_clx_write_nil alist)
              (nl_capture_descend_native invec alist)
              (nl_sexp_clone_into alist out)
              0))))))))

;; ===================================================================
;; TOP-LEVEL FORM-BOUNDARY ARENA RECLAMATION (the safe reclamation point).
;;
;; The bump arena (`nelisp-standalone--arena-source') never frees: every
;; nested function / builtin / macro-expansion call leaks ~4-5 KiB of dead
;; temporaries (frame vectors, arg-list conses, lookup clones, symbol bufs).
;; A single `fib' invocation re-expands the `cond' macro and runs ~10 nested
;; calls -> MEASURED 89_024 bytes/call (constant) -> any fixed arena is
;; exhausted at shallow recursion (fib(20) SIGSEGVs at 1 GiB).
;;
;; PER-CALL region reset (resetting the bump inside `nl_apply_lambda_inner'
;; on an immediate result) is NOT SAFE in this interpreter: a lambda used as
;; a SUBEXPRESSION (e.g. the `(fib (- n 1))' inside `(+ (fib ..) (fib ..))')
;; returns into an ENCLOSING evaluation that is still mid-allocation; reseting
;; while that enclosing eval is pending corrupts its in-progress arg list /
;; result (empirically: `(+ (g 3) 100)' -> 0 instead of 103).  The eval
;; machinery holds live pointers into the per-call region from the caller, so
;; safe per-call reclamation needs escape analysis the build glue cannot do.
;;
;; The DRIVER-LEVEL top-level boundary IS safe: between two top-level forms
;; nothing is pending and the previous form's result has been read out.  So we
;; reclaim there.  The one hazard is that a top-level DEFINITION
;; (`defmacro' / `fset' / `defun') stores closures whose BODIES live in the
;; arena above the reclaim mark — those must survive.  We detect a definition
;; by watching the globals fast-hash-table entry-count (globals.slot0 ->
;; HT.slot2, an Int bumped on every new mirror insert):
;;   * form GREW the mirror  (= a new global was defined)  -> ADVANCE the mark
;;     to the current bump (keep everything this form allocated, incl. closure
;;     bodies); do NOT reset.
;;   * form did NOT grow the mirror AND returned an immediate (Int/Nil/T,
;;     no arena pointer) -> RESET the bump to the mark (free the whole form's
;;     call-tree garbage).
;;   * otherwise (pointer result, or in-place mutation) -> keep (no reset),
;;     and advance the mark to current bump so later resets never free it.
;;
;; This reclaims the per-form garbage of every pure top-level expression while
;; never freeing a definition.  It bounds arena growth for multi-form programs
;; (the self-host compiler processes thousands of top-level forms) to the sum
;; of the LIVE definitions plus ONE form's peak, instead of the sum of ALL
;; forms' peaks.  It does NOT bound a single deeply-recursive form (see the
;; arena-size note); that is the residual blocker.
;;
;; Implemented directly in the reader driver (see
;; `nelisp-standalone--reader-driver-source'); the fixed arena slot 268435472
;; (reserved [16,96) region, unused outside catch/throw which fib does not hit)
;; holds the current reclaim mark.  lisp/ + golden binaries stay untouched.
;; ===================================================================

(defun nelisp-standalone--reader-extra-unit (entry)
  "Compile a (NAME FEATURE SRC-SYM) manifest ENTRY to a cached link-unit."
  (pcase-let ((`(,name ,feat ,src) entry))
    (require feat)
    (nelisp-standalone--cached-unit name (symbol-value src)
                                    (locate-library (symbol-name feat)))))

;; rc-correct nl_apply_do_fset (Doc 137 M3).  The shipped handler in
;; nelisp-cc-evalport-combiner-apply has two rc-plumbing bugs (the non-symbol
;; clone arm returns the dst ptr -- treated as error; the mirror setter returns 1
;; on success -- treated as failure) AND triggers a Phase47 compiler mis-lowering:
;; `(not (= raw-bool 0))' inside a dispatcher-boundary-available defun is routed
;; through nelisp_aot_builtin_call1, which reads 32 bytes from the raw i64 boolean
;; as a *const Sexp -> NULL deref -> SIGSEGV (the real root cause; see the
;; compiler gate at nelisp-phase47-compiler ~L7846/7862, a durable fix TODO).
;; This rewrite avoids `not', yields rc 0 from the clone arm, and treats the
;; setter's 1 as success.  Applied as a build-time sexp patch so lisp/ + golden
;; binaries stay untouched.
(defconst nelisp-standalone--reader-do-fset-fixed
  '(defun nl_apply_do_fset (args_list_ptr env out)
     (let* ((sym_ptr (nl_apply_list_nth args_list_ptr 0))
            (def_ptr (nl_apply_list_nth args_list_ptr 1)))
       (if (= sym_ptr 0) (nl_apply_stash_wta env args_list_ptr)
         (if (= def_ptr 0) (nl_apply_stash_wta env args_list_ptr)
           (let* ((mirror_ptr (+ env 0)) (unbound_ptr (+ env 64))
                  (def_tag (ptr-read-u64 def_ptr 0)))
             (let* ((resolved_slot (alloc-bytes 32 8)))
               (let* ((resolve_rc
                       (if (= def_tag 4)
                           (nelisp_env_lookup_function mirror_ptr unbound_ptr def_ptr resolved_slot)
                         (seq (nl_sexp_clone_into def_ptr resolved_slot) 0))))
                 (if (= resolve_rc 0)
                     (let* ((scratch_slot (alloc-bytes 32 8)))
                       (seq
                        (nl_apply_build_fn_scratch unbound_ptr resolved_slot scratch_slot)
                        (nelisp_mirror_set_function_or_insert mirror_ptr sym_ptr scratch_slot 0)
                        (nl_sexp_clone_into def_ptr out)
                        0))
                   resolve_rc))))))))
  "Rc-correct, `not'-free replacement for the shipped nl_apply_do_fset.")

;; M4 keyword self-eval.  Without this, evaluating a keyword symbol (e.g. the
;; `:test' in `(make-hash-table :test 'equal)') routes through nl_env_lookup_val
;; and aborts as an unbound variable.  Keywords must self-evaluate.  This is a
;; build-time sexp patch of the eval-inner unit (lisp/ stays pristine): it adds
;; the `nl_kw_is_keyword' predicate and prepends a keyword clause to
;; `nl_eval_inner's tag-dispatch cond.  nl_kw_is_keyword(form) = 1 iff form is a
;; Sexp::Symbol(4) whose first name byte is ':' (= 58).
(defconst nelisp-standalone--kw-is-keyword
  '(defun nl_kw_is_keyword (form)
     (if (= (sexp-tag form) 4)
         (if (> (str-len form) 0)
             (if (= (str-byte-at form 0) 58) 1 0)
           0)
       0)))

(defun nelisp-standalone--patch-eval-inner-defun (form)
  "Rewrite the nl_eval_inner defun FORM to prepend a keyword self-eval cond
clause as the FIRST branch (so keyword symbols self-evaluate before the
Symbol(4) variable-lookup branch)."
  ;; form = (defun nl_eval_inner (form env out _pad) (cond CLAUSES...))
  (let* ((name (cadr form))
         (arglist (caddr form))
         (cond-form (cadddr form)))   ; the (cond ...) body
    (if (and (consp cond-form) (eq (car cond-form) 'cond))
        (list 'defun name arglist
              (cons 'cond
                    (cons '((= (nl_kw_is_keyword form) 1)
                            (nl_ei_self_eval_done (extern-call nl_sexp_clone_into form out) 0))
                          (cdr cond-form))))
      form)))

(defun nelisp-standalone--patch-eval-inner (src)
  "Return eval-inner SRC (a `(seq (defun ...) ...)') with the M4 keyword
self-eval clause wired in: insert `nl_kw_is_keyword' and rewrite the
`nl_eval_inner' tag-dispatch cond.  Mirrors `--patch-combiner-apply'."
  (cons (car src)
        (let (out inserted)
          (dolist (form (cdr src))
            (cond
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_eval_inner))
              (unless inserted
                (push nelisp-standalone--kw-is-keyword out)
                (setq inserted t))
              (push (nelisp-standalone--patch-eval-inner-defun form) out))
             (t (push form out))))
          (nreverse out))))

;; WAVE-2 (PATCH 3): un-defer `signal' in combiner-apply.  The AOT combiner's
;; `nl_apply_name_classify' tags `signal' as category 2 ("deferred") and routes
;; it to `nl_apply_stash_wta' (stashes a wrong-type-argument into the env var),
;; BYPASSING `nelisp_apply_function' so bf_signal never runs and the M6 arena
;; signal stash is never set -> condition-case (the PATCH-1 arena-stash errstub)
;; cannot catch it.  Neutralise `nl_apply_deferred_signal' (always return 0) so
;; `signal' is no longer deferred: it falls through to `nelisp_apply_function' ->
;; bf_signal (PATCH 2) -> arena stash -> errstub (PATCH 1) -> nl_cc_match_and_bind.
;; Composed onto the existing do_fset rc-fix below (both patch combiner-apply).
(defun nelisp-standalone--patch-combiner-apply-deferred-signal (src)
  "Return combiner-apply SRC with `nl_apply_deferred_signal' neutralised to
always return 0, so `signal' flows to the builtin applyfn (bf_signal) instead of
`nl_apply_stash_wta'.  WAVE-2 PATCH 3."
  (cons (car src)
        (mapcar (lambda (form)
                  (if (and (consp form) (eq (car form) 'defun)
                           (eq (cadr form) 'nl_apply_deferred_signal))
                      '(defun nl_apply_deferred_signal (name_ptr) 0)
                    form))
                (cdr src))))

(defun nelisp-standalone--patch-combiner-apply (src)
  "Return combiner-apply SRC (a `(seq (defun ...) ...)') with nl_apply_do_fset
swapped for `nelisp-standalone--reader-do-fset-fixed' (M3) AND
`nl_apply_deferred_signal' neutralised (WAVE-2 PATCH 3), so condition-case can
trap `signal'.  Both patches operate on the same combiner-apply source; the
do_fset swap is applied first, then the deferred-signal neutralisation.  Keeps
lisp/ pristine."
  (nelisp-standalone--patch-combiner-apply-deferred-signal
   (cons (car src)
         (mapcar (lambda (form)
                   (if (and (consp form) (eq (car form) 'defun)
                            (eq (cadr form) 'nl_apply_do_fset))
                       nelisp-standalone--reader-do-fset-fixed
                     form))
                 (cdr src)))))

;; WAVE-2 (PATCH 4): condition-case clears the M6 arena signal flag on a clause
;; MATCH.  Pairs with PATCH 1 (the errstub no longer clears flag@268435472), so a
;; clause MATCH must clear it -- else the next signal-free form would observe a
;; stale pending-signal flag.  A NO match leaves the flag set so an outer
;; condition-case re-traps (nested reraise).  `nl_sf_cc_after_match(match-rc, ...)'
;; receives match-rc=0 when `nl_cc_match_and_bind' matched + pushed the handler
;; frame; we clear the flag then run the handler body via `nl_sf_cc_body'.
;; Applied as a build-time sexp patch on the sf-cc.o source (lisp/ stays
;; pristine; the matcher `nl_cc_match_and_bind' itself is reused unchanged via
;; sf-let-setup.o = env-leaves-logic).
(defun nelisp-standalone--patch-sf-cc (src)
  "Return sf-condition-case SRC (a `(seq (defun ...) ...)') with
`nl_sf_cc_after_match' rewritten to clear the M6 arena signal flag (@268435472)
on a clause MATCH before running the handler body.  WAVE-2 PATCH 4."
  (cons (car src)
        (mapcar
         (lambda (form)
           (if (and (consp form) (eq (car form) 'defun)
                    (eq (cadr form) 'nl_sf_cc_after_match))
               '(defun nl_sf_cc_after_match (match-rc env out s1 _p5 _p6)
                  (if (= match-rc 0)
                      (seq (ptr-write-u64 268435472 0 0)
                           (nl_sf_cc_body s1 env out 0))
                    1))
             form))
         (cdr src))))

(defun nelisp-standalone--reader-units ()
  "Build the ORDERED reader-path unit list (start first, arena last).
Links the REAL special-form + env machinery (no trap stubs) so the binary is a
genuine general interpreter for the 11 special forms + installed builtins."
  (let* ((start (nelisp-standalone--start-unit))
         (driver (let ((u (nelisp-standalone--compile-to-unit
                           "driver.o" (nelisp-standalone--reader-driver-source))))
                   (push "driver.o" nelisp-standalone--recompiled) u))
         ;; helpers: shipped manifest minus arena/trap and minus the units we
         ;; build from PATCHED sources below (eval-inner = M4 keyword self-eval,
         ;; combiner-cons = M6 catch/throw, combiner-apply = M3 do_fset fix).
         (helpers (delq nil
                        (mapcar (lambda (entry)
                                  (pcase-let ((`(,name ,kind ,_src) entry))
                                    (unless (or (memq kind '(:start :driver))
                                                (string= name "arena.o")
                                                (string= name "trap.o")
                                                ;; applyfn.o built from the FULL
                                                ;; reader source (HT+str+file)
                                                ;; below, not the baked subset.
                                                (string= name "applyfn.o")
                                                (string= name "eval-inner.o")
                                                (string= name "combiner-cons.o")
                                                (string= name "combiner-apply.o"))
                                      (nelisp-standalone--unit-for entry))))
                                nelisp-standalone--manifest)))
         ;; Full reader applyfn (arithmetic + HT + strings/format + file I/O).
         (applyfn (nelisp-standalone--cached-unit
                   "applyfn-reader.o" nelisp-standalone--applyfn-source
                   nelisp-standalone--this-file))
         ;; M4: keyword self-eval patched eval-inner.
         (eval-inner (progn
                       (require 'nelisp-cc-eval-inner)
                       (nelisp-standalone--cached-unit
                        "eval-inner-kw.o"
                        (nelisp-standalone--patch-eval-inner
                         (symbol-value 'nelisp-cc-eval-inner--source))
                        nelisp-standalone--this-file)))
         ;; M6 catch/throw dispatch + Wave-1 (A) macro-expansion in-place caching,
         ;; both patched onto combiner-cons (catch/throw first, then macro cache).
         (combiner-cons (progn
                          (require 'nelisp-cc-evalport-combiner-cons)
                          (nelisp-standalone--cached-unit
                           "combiner-cons-ct-mc.o"
                           (nelisp-standalone--patch-combiner-cons-full
                            (symbol-value 'nelisp-cc-evalport-combiner-cons--source))
                           nelisp-standalone--this-file)))
         ;; M3: do_fset rc-fix patched combiner-apply.
         (combiner (progn
                     (require 'nelisp-cc-evalport-combiner-apply)
                     (nelisp-standalone--cached-unit
                      "combiner-apply-fix.o"
                      (nelisp-standalone--patch-combiner-apply
                       (symbol-value 'nelisp-cc-evalport-combiner-apply--source))
                      nelisp-standalone--this-file)))
         (extras (mapcar #'nelisp-standalone--reader-extra-unit
                         nelisp-standalone--reader-extra-manifest))
         (float-stub (nelisp-standalone--cached-unit
                      "reader-float-stub.o" nelisp-standalone--reader-float-stub-source
                      nelisp-standalone--this-file))
         ;; real-sf: all real special-form units EXCEPT sf-cc.o, which is built
         ;; from the WAVE-2 PATCH-4 source below (flag-clear on clause match).
         (real-sf (delq nil
                        (mapcar (lambda (entry)
                                  (unless (string= (car entry) "sf-cc.o")
                                    (nelisp-standalone--reader-extra-unit entry)))
                                nelisp-standalone--reader-real-sf-manifest)))
         ;; WAVE-2 PATCH 4: sf-condition-case with nl_sf_cc_after_match rewritten
         ;; to clear the M6 arena signal flag on a clause MATCH.
         (sf-cc (progn
                  (require 'nelisp-cc-sf-condition-case)
                  (nelisp-standalone--cached-unit
                   "sf-cc-flagclear.o"
                   (nelisp-standalone--patch-sf-cc
                    (symbol-value 'nelisp-cc-sf-condition-case--source))
                   nelisp-standalone--this-file)))
         (capture (nelisp-standalone--cached-unit
                   "reader-capture.o" nelisp-standalone--reader-capture-source
                   nelisp-standalone--this-file))
         (errstub (nelisp-standalone--cached-unit
                   "reader-errstub.o" nelisp-standalone--reader-errstub-source
                   nelisp-standalone--this-file))
         ;; M7: file-I/O builtin impls (wrf/rdf/slen) referenced by applyfn.o.
         (fileio (nelisp-standalone--cached-unit
                  "reader-fileio.o" nelisp-standalone--fileio-source
                  nelisp-standalone--this-file))
         ;; M6: catch/throw glue (nl_sf_catch/nl_sf_throw) referenced by the
         ;; patched combiner-cons.
         (catch-throw (nelisp-standalone--cached-unit
                       "reader-catch-throw.o" nelisp-standalone--catch-throw-source
                       nelisp-standalone--this-file))
         (arena (nelisp-standalone--unit-for
                 (assoc "arena.o" nelisp-standalone--manifest))))
    (append (list start driver applyfn) helpers
            (list eval-inner combiner-cons combiner)
            extras (list float-stub) real-sf
            (list sf-cc capture errstub fileio catch-throw arena))))

;;;###autoload
(defun nelisp-standalone-build-reader ()
  "Incrementally build the reader-path standalone ELF; return its path."
  (setq nelisp-standalone--recompiled nil)
  (let ((units (nelisp-standalone--reader-units)))
    (nelisp-link-units nelisp-standalone--reader-out units)
    (set-file-modes nelisp-standalone--reader-out #o755)
    (message "[standalone-reader] linked %d units -> %s (src=%S)"
             (length units) nelisp-standalone--reader-out
             (nelisp-standalone--reader-src))
    nelisp-standalone--reader-out))

;;;###autoload
(defun nelisp-standalone-reader-test ()
  "Build the reader binary, run it, assert exit == eval(NELISP_SRC).  Exits 0/1."
  (nelisp-standalone-build-reader)
  (let ((code (call-process nelisp-standalone--reader-out nil nil nil))
        (expected (nelisp-standalone--reader-expected)))
    (if (= code expected)
        (progn (message "[standalone-reader] PASS: %S -> exit %d (expected %d)"
                        (nelisp-standalone--reader-src) code expected)
               (kill-emacs 0))
      (message "[standalone-reader] FAIL: %S -> exit %d (expected %d)"
               (nelisp-standalone--reader-src) code expected)
      (kill-emacs 1))))

(defconst nelisp-standalone--prelude-file
  (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                    nelisp-standalone--repo-root)
  "Loadable stdlib prelude (defmacro bootstrap + core macros + list/search/hof/
plist/backquote lib).  The binary loads it via file-load before user code; see
its header for the `cat PRELUDE yourfile.el | binary' usage.")

(defun nelisp-standalone--prelude-breadth-test-src ()
  "Breadth test exercising the Wave-1 (B) primitives + the prelude macros.
Returns 42: defun+cond, dolist+setq, nth, plist-get, and a backquote `length'
all combine so a single wrong primitive shifts the result off 42."
  (concat
   "(defun bt-f (x) (cond ((< x 0) 0) (t (* x 2))))\n"
   "(let ((s 0)) (dolist (x (list 10 20 12)) (setq s (+ s x)))\n"
   "  (let ((b 2) (c (list 3 4)))\n"
   "    (+ (if (= (bt-f 21) 42) 0 100)\n"          ; defun+cond -> 42 ? +0 : +100
   "       (if (= s 42) 0 100)\n"                  ; dolist sum 10+20+12 -> 42
   "       (if (= (nth 2 (list 9 8 42 7)) 42) 0 100)\n"   ; nth -> 42
   "       (if (= (plist-get (list 'a 1 'b 42) 'b) 42) 0 100)\n" ; plist-get -> 42
   "       (if (= (length `(1 ,b ,@c 5)) 5) 42 100))))\n")) ; backquote length 5 -> 42

;;;###autoload
(defun nelisp-standalone-reader-prelude-test ()
  "Build the reader binary, then run it on a test file = the stdlib prelude
\(`scripts/nelisp-stdlib-prelude.el') followed by a breadth test, and assert the
binary's exit code == 42.  This proves the prelude LOADS AS-IS on standalone
NeLisp and the Wave-1 (B) breadth primitives back the stdlib (cond/dolist/nth/
plist-get/backquote).  Exits 0/1."
  (nelisp-standalone-build-reader)
  (let* ((tmp (make-temp-file "nelisp-prelude-breadth-" nil ".el"))
         (expected 42))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert-file-contents nelisp-standalone--prelude-file)
            (goto-char (point-max))
            (insert "\n" (nelisp-standalone--prelude-breadth-test-src)))
          (let ((code (call-process nelisp-standalone--reader-out nil nil nil tmp)))
            (if (= code expected)
                (progn
                  (message "[standalone-reader-prelude] PASS: prelude + breadth -> exit %d (expected %d)"
                           code expected)
                  (kill-emacs 0))
              (message "[standalone-reader-prelude] FAIL: prelude + breadth -> exit %d (expected %d)"
                       code expected)
              (kill-emacs 1))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'nelisp-standalone-build)

;;; nelisp-standalone-build.el ends here
