;;; native-real-corpus-exclusions.el --- Reviewed F3 findings -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Each excluded function retains its exact byte code and GNU inputs in the
;; fixture. Use --reproduce NAME to replay that minimal failing witness.
(defconst f3-real-corpus-exclusions
  (append
   (mapcar (lambda (name)
             (cons name "In-house cache compile rejects the genuine object: rooted-cfg verified plan or runtime contract is unavailable; reproduce with --reproduce NAME."))
           '(always cl-copy-list cl-signum ensure-list ensure-proper-list
             event-click-count event-end event-line-count event-start fixnump ignore
             mouse-movement-p posn-point seq--into-list seq--into-string seq--into-vector
             string-chop-newline string-limit string-prefix-p string-suffix-p xor))
   (mapcar (lambda (name)
             (cons name "Outside the selected 50-function corpus to bound aggregate qualification at 600 s, based on measured compile costs; no compiler/runtime failure is claimed. Exact object/inputs remain replayable with --reproduce NAME."))
           '(posn-area ring-index string-equal-ignore-case ring-empty-p ring-minus1
             cl-values-list posn-timestamp oddp evenp cl-nreconc
             minusp subr-primitive-p cl-multiple-value-apply posn-actual-col-row))
   '((posn-object . "Interpreter lacks posn-string: input (nil), GNU nil, standalone void-function (posn-string).")
     (cl-digit-char-p . "Interpreter lacks cl-digit-char-table: input (51), GNU 3, standalone void-variable.")
     (cl-pairlis . "In-house compile rejects the object; interpreter input (17 nil) also gives wrong-type-argument (listp 17), GNU requires (sequencep 17).")
     (make-ring . "Interpreter accepts invalid length: input (bad), GNU wrong-type-argument (wholenump bad), standalone (0 0 . [nil nil nil]).")
     (primitive-function-p . "Interpreter lacks subr-arity: input car's actual subr object (:f3-subr car), GNU t, standalone void-function (subr-arity).")
     (ring-plus1 . "GCC JIT cache compile rejects the object: shared-v2 compile contract refused.")
     (cl-tailp . "Unqualified surplus candidate: initial static two-function batch exceeded 290 s; no isolated-function failure is claimed.")
     (string-truncate-left . "Unqualified surplus candidate: initial static two-function batch exceeded 290 s; no isolated-function failure is claimed.")
     (ring-p . "Unqualified surplus candidate: initial static two-function batch exceeded 290 s; GCC JIT passes, no isolated static failure is claimed.")
     (ring-copy . "Unqualified surplus candidate: static batch exceeded 290 s while compiling this first function; GCC JIT passes."))))
(provide 'native-real-corpus-exclusions)
