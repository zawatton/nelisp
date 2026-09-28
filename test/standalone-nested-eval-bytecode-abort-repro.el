;;; standalone-nested-eval-bytecode-abort-repro.el --- minimal repro -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; HANDOFF: read-from-string vs load-reader divergence, memory-safety abort
;; ---------------------------------------------------------------------------
;;
;; WHAT THIS IS: a standalone repro of a process abort reported adopting
;; opcode 183 (Bswitch) for lisp/nelisp-eln-leaf-code.el's
;; `nelisp-eln-leaf-code-valid-p'. Extensive bisection (see the S2.5
;; worklog) ruled out Bswitch, Bpushcatch, `throw', `maphash', and
;; `make-closure' individually and in combination (padded stack depth,
;; inflated switch-table targets, hash-table-backed state, a maphash
;; callback that is itself a closure) -- none of those hand-built
;; analogues reproduce the abort. An earlier pass also floated a
;; call-frame-depth theory (installing from inside a nested `defun' vs. at
;; a script's top level); that theory does NOT hold either -- see the
;; FRESH-NAME control below, which installs at genuine top level under a
;; brand-new symbol and still aborts.
;;
;; The one dimension that DOES flip the outcome, on the exact same file
;; content, verified two ways below:
;;   - `load'-ing a file whose own top-level form is
;;     `(fset NAME (make-byte-code ...BYTES...))' installs and calls the
;;     function correctly.
;;   - `read-from-string' of that identical file's text, then `eval' of
;;     the resulting form, aborts the process on the very first call.
;; Same bytes, same target symbol, same process -- the only difference is
;; which reader produced the form handed to `eval'.
;; `nelisp--core-bytecode-reinstall-loaded' (scripts/nelisp-standalone-
;; build.el / scripts/nelisp-prelude-bytecode.el's runtime installation
;; path for core modules) uses `read-from-string', which is why adopting
;; this module hits the abort at all. This looks like `read-from-string'
;; (or how its result differs from the file-load reader's once handed to
;; `eval') mishandling this exact bytecode string's content -- it embeds
;; escaped control-character bytes, a nested `#[...]' closure-prototype
;; literal (the module's `maphash' callback), and a `#s(hash-table ...)'
;; literal (the Bswitch jump table).
;;
;; This is a reader/GC-lifetime issue, not anything in the opcode/
;; allowlist scope the S2.5 lane is authorized to touch. Not fixed here;
;; this file is the handoff artifact. GC/eval/reader internals are
;; deliberately left alone.
;;
;; EXACT STEPS:
;;   cd <repo root>
;;   NELISP_READER_DYNAMIC=1 NELISP_STANDALONE_READER_OUTPUT=./target/nelisp \
;;     make standalone-reader
;;   ./target/nelisp --load test/standalone-nested-eval-bytecode-abort-repro.el --
;;
;; EXPECTED OUTPUT (current binary, at the time this file was written):
;;   CONTROL-VIA-LOAD-BYTECODEP=t
;;   CONTROL-VIA-LOAD-RESULT=nil
;;   then, attempting the read-from-string + eval install:
;;   nelisp: form aborted without signal (rc=1)
;;   (no Lisp condition, no OS-level signal name -- a clean process exit
;;   with status 1; nothing for a `condition-case' around the `eval' call
;;   to catch, since the abort happens below the Lisp-condition machinery)
;;
;; EXIT CODE: 1 for the whole process; the abort is unconditional, not a
;; catchable Lisp error.
;;
;; WHAT WAS RULED OUT (see the S2.5 worklog for the full bisection):
;;   - Bswitch (183) alone, including all 7 symbol keys plus `nil' and the
;;     miss/fallthrough case, against a hash table built the same shape.
;;   - Bswitch immediately followed by a `throw' in the selected branch.
;;   - The above combined with deep local-variable stack padding (34 vs.
;;     23 declared slots) and switch jump-table targets inflated past 800
;;     (vs. the real function's 446-535).
;;   - `maphash' whose callback is itself a `make-closure' closure,
;;     preceding the switch+throw, in isolation and combined with all of
;;     the above.
;;   - Two independently-installed functions plainly calling each other
;;     (no switch/throw at all).
;;   - Installing from inside a nested `defun' call frame vs. at a
;;     script's true top level (the FRESH-NAME control below aborts at
;;     true top level, under a symbol nothing else has ever touched).
;;   - The real production wrapper's own size/allocation pressure (~180KB,
;;     19 modules) preceding the install.
;;   - The specific symbol name `nelisp-eln-leaf-code-valid-p' itself (a
;;     brand-new, never-before-interned name aborts identically).
;;
;; The dimension that consistently flips the outcome is `read-from-string'
;; vs. the file-load reader for this exact bytecode string, which is why
;; this repro keeps the genuine bytes rather than a further-shrunk
;; analogue -- shrinking risks losing whatever about this content the two
;; readers disagree on.

;;; Code:

;; The genuine baked form for `nelisp-eln-leaf-code-valid-p': `prin1'-
;; round-tripped from actually byte-compiling the real function on host
;; GNU Emacs 31.1 (the exact bytes `nelisp-prelude-bytecode-transform'
;; produces and `nelisp--core-bytecode-reinstall-loaded' installs at
;; runtime), confirmed correct on host through both reading paths below.
;; Retargeted here to a symbol nothing else in this process has ever
;; touched, to rule out anything specific to the original name.
(defconst nl183-repro-baked-form-text
  "(prog1 'nl183-repro-fresh-symbol (fset 'nl183-repro-fresh-symbol (make-byte-code '257 (unibyte-string 192 50 79 2 137 59 131 16 0 137 71 193 86 132 21 0 194 192 195 34 136 196 1 33 178 1 137 71 193 195 197 198 199 34 197 198 199 34 3 5 87 131 118 1 200 4 201 4 35 136 3 195 193 202 6 9 6 8 203 35 131 70 0 182 2 204 205 130 18 1 202 6 9 6 8 206 35 131 87 0 182 2 195 207 130 18 1 202 6 9 6 8 208 35 131 171 0 6 7 6 7 90 209 87 131 112 0 194 192 195 34 136 193 137 137 210 87 131 143 0 211 2 212 6 13 6 12 207 92 5 92 72 4 210 95 34 34 178 2 84 130 114 0 136 137 193 85 132 164 0 213 1 205 34 207 85 132 164 0 194 192 195 34 136 182 3 214 209 130 18 1 202 6 9 6 8 215 35 131 188 0 182 2 216 205 130 18 1 202 6 9 6 8 217 35 131 220 0 6 7 6 7 90 218 87 131 213 0 194 192 195 34 136 182 2 219 218 130 18 1 6 8 6 7 72 220 85 131 252 0 6 7 6 7 90 221 87 131 245 0 194 192 195 34 136 182 2 222 221 130 18 1 6 8 6 7 72 223 85 131 13 1 182 2 224 225 130 18 1 194 192 195 34 136 6 6 1 92 178 7 1 224 61 131 43 1 6 6 6 8 85 132 43 1 194 192 195 34 136 1 226 62 131 103 1 227 6 9 3 219 61 131 64 1 4 207 92 130 66 1 4 84 34 6 7 1 92 137 5 86 131 90 1 137 193 89 131 90 1 137 6 10 87 132 95 1 194 192 195 34 136 200 5 2 6 8 35 182 3 2 6 7 3 69 6 6 66 178 6 182 3 130 38 0 2 131 131 1 207 3 64 56 224 61 132 136 1 194 192 195 34 136 228 229 230 4 34 2 34 136 197 198 199 34 200 193 195 67 3 35 136 3 159 137 131 75 2 137 64 137 64 1 65 64 207 3 56 231 3 6 7 34 137 131 69 2 137 64 1 65 3 232 183 130 32 2 201 178 2 130 32 2 182 2 201 195 130 32 2 1 132 212 1 194 192 195 34 136 136 201 130 32 2 137 132 226 1 194 192 195 34 136 231 6 6 6 11 34 2 67 200 2 233 231 5 6 15 34 4 34 6 13 35 182 3 130 32 2 231 6 6 6 11 34 2 2 66 200 2 233 231 5 6 15 34 4 34 6 13 35 182 3 130 32 2 1 132 32 2 194 192 195 34 136 3 234 62 132 67 2 4 6 14 89 131 50 2 194 192 195 34 136 200 5 233 231 6 8 6 13 34 5 5 66 34 6 11 35 136 182 2 182 5 65 130 157 1 182 7 201 48 135) '[invalid 0 throw nil string-as-unibyte make-hash-table :test eql puthash t nelisp-eln-leaf-code--match (72 137 248) move 3 (49 192) 2 (72 184) 10 8 logior ash logand immediate (72 133 192) test (15 132) 6 jz 233 5 jmp 195 ret 1 (jz jmp) nelisp-eln-leaf-code--s32 maphash make-closure #[514 \"\\301\\1\\300\\\"?\\205\\f\\0\\302\\303\\304\\\"\\207\" [V0 gethash throw invalid nil] 5 \"\\n\\n(fn BRANCH TARGET)\"] gethash #s(hash-table test eq data (immediate 446 move 446 nil 452 test 459 jz 473 jmp 508 ret 535)) nelisp-eln-leaf-code--merge-state (jmp ret)] 23 \"Return non-nil iff BYTES is a safe supported GNU x86-64 unary leaf.\\n\\nThis recognizes only the emitter's register move, nil xor, tagged immediate,\\ntest, forward conditional/unconditional branches, and final return.\")))\n"
  "The genuine baked form, verbatim from `prin1'-round-tripping the real
`nelisp-eln-leaf-code-valid-p' after host-compiling it, retargeted to a
symbol nothing else in this process has ever touched.")

(defvar nl183-repro-tempfile (make-temp-file "nl183-repro-" nil ".el"))
(with-temp-file nl183-repro-tempfile
  (insert nl183-repro-baked-form-text))

;; The helper this bytecode calls by name for its decode loop; a plain
;; interpreted stand-in is enough (the abort is unrelated to it, see the
;; worklog's helpers-alone control).
(defun nelisp-eln-leaf-code--match (bytes offset octets)
  (and (<= (+ offset (length octets)) (length bytes))
       (let ((i 0))
         (while (and (< i (length octets))
                     (= (aref bytes (+ offset i)) (nth i octets)))
           (setq i (1+ i)))
         (= i (length octets)))))

;; --- Control: `load' the file directly. `load' is the file-load reader
;; reading and `eval'-ing each top-level form itself; this stays healthy.
(load nl183-repro-tempfile nil t t)
(princ (format "CONTROL-VIA-LOAD-BYTECODEP=%S\n"
              (byte-code-function-p (symbol-function 'nl183-repro-fresh-symbol))))
(princ (format "CONTROL-VIA-LOAD-RESULT=%S\n"
              (nl183-repro-fresh-symbol (unibyte-string 195))))

;; --- Reproduction: read the SAME file's text via `read-from-string',
;; then `eval' the resulting form by hand -- matching exactly how
;; `nelisp--core-bytecode-reinstall-loaded' installs core-module bytecode
;; at runtime. Retarget to a second fresh symbol so this is not just
;; re-fsetting the already-healthy one above.
(let* ((source (with-temp-buffer
                 (insert-file-contents nl183-repro-tempfile)
                 (buffer-string)))
       (form (car (read-from-string source))))
  ;; Rename both occurrences (the `prog1' tag and the `fset' target) to a
  ;; second, still-never-touched symbol.
  (setcar (cdr (nth 1 form)) 'nl183-repro-fresh-symbol-2)
  (setcar (cdr (nth 1 (nth 2 form))) 'nl183-repro-fresh-symbol-2)
  (eval form))
(princ (format "READFROMSTRING-BYTECODEP=%S\n"
              (byte-code-function-p (symbol-function 'nl183-repro-fresh-symbol-2))))
;; Host resolves (nl183-repro-fresh-symbol-2 (unibyte-string 195)) to nil
;; -- a trivial single #xc3 (`ret') byte. This is NOT a wrong-answer bug:
;; the line below is never reached because the process aborts first.
(princ (format "READFROMSTRING-RESULT=%S\n"
              (nl183-repro-fresh-symbol-2 (unibyte-string 195))))
(princ "UNREACHABLE-IF-ABORT-REPRODUCES\n")
(ignore-errors (delete-file nl183-repro-tempfile))

;;; standalone-nested-eval-bytecode-abort-repro.el ends here
