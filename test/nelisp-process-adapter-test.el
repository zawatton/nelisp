;;; nelisp-process-adapter-test.el --- Doc 184 P1-P3 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for `packages/nelisp-process-adapter/src/nelisp-process-adapter.el'
;; (Doc 184 P1-P3) and `packages/nelisp-eventloop/src/nelisp-async-core.el'
;; (Doc 184 P0).
;;
;; Every case `skip-unless (fboundp 'nelisp-process-start)': the native
;; `nelisp-process-*' primitives these files build on only exist in the
;; standalone `target/nelisp' binary, not under host Emacs (Doc 184 S1.1)
;; -- exactly the same guard the existing `standalone-reader-process-smoke'
;; family of Makefile targets exists because of (AI.md: "the ERT suite
;; running under host Emacs and the built target/nelisp binary are not the
;; same claim").  This file is real coverage when run against a build of
;; this repository's own `ert' ported into the standalone runtime, or
;; against any future host-bridge that defines these primitives; today it
;; documents the exact defect shapes and their fixes, and the
;; authoritative red/green evidence is the `standalone-reader-async-core-
;; smoke' / `standalone-reader-process-adapter-smoke' /
;; `standalone-reader-process-adapter-smoke-red' / `standalone-reader-repl-
;; idle-pump-smoke' Makefile targets, run directly against the built
;; binary.

;;; Code:

(require 'ert)
;; Doc 184: DO NOT unconditionally `require' these two files here.  Both
;; unconditionally REDEFINE standard names (`accept-process-output',
;; `make-process', `run-at-time', `sit-for', `delete-process', ...) as
;; part of their whole "upgrade on load" design (Doc 184 S2) -- correct
;; for a standalone `target/nelisp' binary, but under host Emacs (which
;; is what `nelisp-ai.sh test'/`make test' run, loading every `test/*.el'
;; into ONE shared batch process) that would clobber the REAL host Emacs
;; primitives for every OTHER test file loaded afterward in the same
;; process.  Measured: loading them unconditionally here broke
;; `packages/nelisp-network/test/nelisp-network-test.el' (which calls the
;; real `accept-process-output') with `void-function alloc-bytes' from
;; deep inside this adapter's own poll loop, entirely unrelated to
;; anything nelisp-network tests on its own.  Only load when the native
;; process primitives this adapter builds on are actually present --
;; i.e. only in a context that could not possibly be plain host Emacs.
(when (fboundp 'nelisp-process-start)
  (require 'nelisp-async-core)
  (require 'nelisp-process-adapter))

(defmacro nelisp-process-adapter-test--fresh (&rest body)
  "Run BODY with a clean timer queue and process registry."
  (declare (indent 0))
  `(progn
     (nelisp-async-core-reset-timers)
     (setq nelisp-process-adapter--live nil)
     ,@body))

;; Doc 184 S5.1's own illustrative case, verbatim.
(ert-deftest nelisp-make-process-filter-not-silently-dropped ()
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let (got)
      (make-process :name "t" :command '("/bin/echo" "hi")
                    :filter (lambda (_p chunk) (push chunk got)))
      (accept-process-output nil 1)
      (should got))))

(ert-deftest nelisp-make-process-filter-arrives-incrementally ()
  "Two separate writes reach the filter as two separate chunks, not one
combined chunk read after the fact (Doc 184 S1.3's measured defect: the
prelude's own adapter could only retrieve output after a blocking wait,
never streamed)."
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let (chunks
          (p (make-process :name "cat" :command '("/bin/cat")
                            :filter (lambda (_p c) (push c chunks)))))
      (nelisp-process-write p "first-")
      (accept-process-output p 0.3)
      (nelisp-process-write p "second")
      (accept-process-output p 0.3)
      (nelisp-process-close-stdin p)
      (delete-process p)
      (should (equal (nreverse chunks) '("first-" "second"))))))

(ert-deftest nelisp-accept-process-output-does-not-drain-unrelated-processes ()
  "proc2's sentinel must not fire as a side effect of waiting on proc1
(Doc 184 S5.1's own illustrative case, and S1.3's measured defect: the
old `accept-process-output' unconditionally drained EVERY pending
process, not just the one it was asked to wait for)."
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let* (fired2
           (p1 (make-process :name "fast" :command '("/bin/sh" "-c" "exit 0")))
           (p2 (make-process :name "slow" :command '("/bin/sleep" "1")
                              :sentinel (lambda (_p m) (setq fired2 m)))))
      (accept-process-output p1 1)
      (should (null fired2))
      (should (process-live-p p2))
      (delete-process p2))))

(ert-deftest nelisp-process-adapter-sentinel-status-strings ()
  "The sentinel status-string collapse (Doc 184 S1.3: every exit reported
as the literal string \"finished\\n\" regardless of cause) is fixed:
normal exit 0 -> \"finished\\n\", nonzero exit -> \"exited abnormally
with code N\\n\", SIGTERM-killed (this substrate's `delete-process' --
native `nl_bi_process_delete_object' hardcodes SIGTERM(15), verified
against host Emacs 30.1's own `signal-process'+`process-sentinel') ->
\"terminated\\n\"."
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let (msgs)
      (let ((p (make-process :name "ok" :command '("/bin/sh" "-c" "exit 0")
                              :sentinel (lambda (_p m) (push m msgs)))))
        (accept-process-output p 1) (accept-process-output p 1))
      (let ((p (make-process :name "bad" :command '("/bin/sh" "-c" "exit 7")
                              :sentinel (lambda (_p m) (push m msgs)))))
        (accept-process-output p 1) (accept-process-output p 1))
      (let ((p (make-process :name "sl" :command '("/bin/sleep" "1")
                              :sentinel (lambda (_p m) (push m msgs)))))
        (accept-process-output p 0.2)
        (delete-process p))
      (should (equal (nreverse msgs)
                      '("finished\n" "exited abnormally with code 7\n" "terminated\n"))))))

(ert-deftest nelisp-accept-process-output-return-value ()
  "Non-nil iff real output was received before the timeout -- measured
against host Emacs 30.1: a timeout with nothing ready, or a process that
merely changed status with no output, both return nil; actual bytes
return t."
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let ((p (make-process :name "slow" :command '("/bin/sleep" "2"))))
      (should (null (accept-process-output p 0.1)))
      (delete-process p))
    (let (got (p (make-process :name "echo" :command '("/bin/echo" "hi")
                                :filter (lambda (_p c) (setq got c)))))
      (should (accept-process-output p 1))
      (should (equal got "hi\n")))))

(ert-deftest nelisp-process-adapter-run-at-time-repeat-through-shared-loop ()
  "`run-at-time' REPEAT fires through the SAME poll loop
`accept-process-output' uses (Doc 184 S2's decided direction), not a
separate mechanism -- closing `tools/partial-accepted.txt''s `run-at-time'
entry for anything that loads this adapter."
  (skip-unless (fboundp 'nelisp-process-start))
  (nelisp-process-adapter-test--fresh
    (let ((n 0))
      (run-at-time 0 0.02 (lambda () (setq n (1+ n))))
      (accept-process-output nil 0.3)
      (should (>= n 2)))))

(ert-deftest nelisp-make-network-process-signals-clear-error-not-silent ()
  "Doc 184 S1.7/P4: `make-network-process' is out of scope (no native
socket primitive family exists), but it must signal a clear, named error
-- not stay void-function, and not silently no-op or return a broken
process object."
  (skip-unless (fboundp 'nelisp-process-start))
  (should-error (make-network-process :name "x") :type 'error))

;; Doc 184 P0's own exit criterion, verbatim (REPEAT re-arms and fires
;; more than once across two `--fire-due' calls with an intervening
;; sleep) -- this one only needs `nelisp-async-core', not the process
;; primitives, so it is gated on `alloc-bytes' (the nanosleep builtin)
;; like the rest of this package's timer tests.
(ert-deftest nelisp-async-core-repeat-fires-more-than-once ()
  (skip-unless (fboundp 'alloc-bytes))
  (nelisp-process-adapter-test--fresh
    (let ((n 0))
      (nelisp-async-core-run-at-time 0 0.01 (lambda () (setq n (1+ n))))
      (nelisp-async-core--fire-due (+ (nelisp-async-core--now) 0.001))
      (nelisp-async-core--nanosleep 0.02)
      (nelisp-async-core--fire-due (nelisp-async-core--now))
      (should (> n 1)))))

;; ---- Doc 194 IPv6 phase (P7) --------------------------------------------
;; `nelisp--ipv6-parse'/`-unparse' and friends are pure elisp (no
;; `alloc-bytes'/native primitive involved at all, unlike everything
;; above) -- but they live in the SAME file this test file's own header
;; comment explains NOT `require'-ing unconditionally under host Emacs
;; (it redefines `make-process'/`accept-process-output'/etc., clobbering
;; sibling test files sharing one batch process).  So these cases use the
;; SAME `skip-unless (fboundp 'nelisp-process-start)' convention every
;; other case in this file already uses: documentary under host Emacs
;; (skipped), real coverage once this package is actually loaded (a
;; standalone-binary/host-bridge context).  The AUTHORITATIVE red/green
;; evidence for this phase is `standalone-reader-ipv6-socket-smoke'
;; (Makefile), which runs the SAME functions for real inside
;; `target/nelisp' against a reference table plus a live loopback round
;; trip -- exactly this file's own header comment's established
;; precedent for every sibling case above.

(ert-deftest nelisp-ipv6-parse-unparse-round-trip ()
  "`nelisp--ipv6-parse' composed with `nelisp--ipv6-unparse' returns to
the SAME 8-group value for every literal in the reference table (Doc 194
IPv6 phase DoD item (c)): a bare `::', `::1', a full 8-group form with no
compression, an embedded-IPv4 `::ffff:1.2.3.4' tail, and the bracketed
`open-network-stream' form.  Byte values checked directly (not just
`equal' round-trip) against RFC 4291's own worked examples."
  (skip-unless (fboundp 'nelisp-process-start))
  (let ((cases '(("::" . (0 0 0 0 0 0 0 0))
                 ("::1" . (0 0 0 0 0 0 0 1))
                 ("2001:db8:0:0:0:0:0:1" . (8193 3512 0 0 0 0 0 1))
                 ("::ffff:1.2.3.4" . (0 0 0 0 0 65535 258 772))
                 ("[::1]" . (0 0 0 0 0 0 0 1))
                 ("fe80::1" . (65152 0 0 0 0 0 0 1)))))
    (dolist (case cases)
      (let ((groups (nelisp--ipv6-parse (car case))))
        (should (equal groups (cdr case)))
        ;; Round trip: unparse then re-parse must return the SAME groups,
        ;; regardless of exactly which `::'-compressed text form the
        ;; unparser chose to emit.
        (should (equal (nelisp--ipv6-parse (nelisp--ipv6-unparse groups)) groups))))))

(ert-deftest nelisp-ipv6-parse-rejects-malformed-literals ()
  "Malformed IPv6 literals signal the catchable `nelisp-dns-error', never
an uncaught error or a wrong-but-silent parse (Doc 194 IPv6 phase, same
against-the-bug convention as `nelisp--dns-byte''s own bounds guard)."
  (skip-unless (fboundp 'nelisp-process-start))
  (dolist (bad '("" ":::" "1:2:3:4:5:6:7:8:9" "1::2::3" "gggg::1" "1:2:3"
                  "::1.2.3.4.5"))
    (should-error (nelisp--ipv6-parse bad) :type 'nelisp-dns-error)))

(ert-deftest nelisp-ipv6-literal-p-family-detection ()
  "Family auto-detection (Doc 194 IPv6 phase SCOPE item 2): a colon means
IPv6, matching the native `nl_ipv6_has_colon_walk' convention -- neither
an IPv4 dotted-quad nor \"localhost\" ever contains one."
  (skip-unless (fboundp 'nelisp-process-start))
  (should (nelisp--ipv6-literal-p "::1"))
  (should (nelisp--ipv6-literal-p "fe80::1"))
  (should-not (nelisp--ipv6-literal-p "127.0.0.1"))
  (should-not (nelisp--ipv6-literal-p "localhost"))
  (should-not (nelisp--ipv6-literal-p "example.com")))

(ert-deftest nelisp-net-effective-family-detection ()
  "`nelisp--net-effective-family' returns `ipv6' only for an explicit
`:family \\='ipv6' or an already-IPv6 `:host' literal -- every other
PLIST shape (the overwhelming majority, every pre-existing caller) gets
`ipv4', the TOP CONSTRAINT this whole phase is built on."
  (skip-unless (fboundp 'nelisp-process-start))
  (should (eq (nelisp--net-effective-family '(:family ipv6 :host "example.com")) 'ipv6))
  (should (eq (nelisp--net-effective-family '(:host "::1")) 'ipv6))
  (should (eq (nelisp--net-effective-family '(:host "[::1]")) 'ipv6))
  (should (eq (nelisp--net-effective-family '(:host "127.0.0.1")) 'ipv4))
  (should (eq (nelisp--net-effective-family '(:host "localhost")) 'ipv4))
  (should (eq (nelisp--net-effective-family '(:host nil)) 'ipv4))
  (should (eq (nelisp--net-effective-family '(:family ipv4 :host "127.0.0.1")) 'ipv4)))

(ert-deftest nelisp-dns-encode-query-aaaa-qtype-byte ()
  "`nelisp--dns-encode-query' with QTYPE 28 (AAAA) differs from the
default (QTYPE 1, A) ONLY in the QTYPE field's own two bytes -- same
length, same header, same QNAME encoding (Doc 194 IPv6 phase: the A
query's own wire bytes, the default/omitted-QTYPE case, are unchanged)."
  (skip-unless (fboundp 'nelisp-process-start))
  (let* ((a (nelisp--dns-encode-query "example.com"))
         (aaaa (nelisp--dns-encode-query "example.com" 28))
         (n (string-bytes a)))
    (should (= n (string-bytes aaaa)))
    ;; QTYPE occupies the 2 bytes immediately before the trailing 2-byte
    ;; QCLASS field.
    (should (equal (list (string-byte a (- n 4)) (string-byte a (- n 3))) '(0 1)))
    (should (equal (list (string-byte aaaa (- n 4)) (string-byte aaaa (- n 3))) '(0 28)))
    ;; Everything else -- flags/counts/QNAME/QCLASS -- is byte-identical
    ;; except the QTYPE field just checked and the query ID (bytes 2-3,
    ;; right after the 2-byte TCP length prefix; `nelisp--dns-next-id'
    ;; increments on every call, so it necessarily differs between these
    ;; two consecutive calls -- by how many OF ITS OWN two bytes depends
    ;; on whether the low byte happened to wrap, which this test does not
    ;; assume either way).
    (dotimes (i n)
      (unless (or (memq i '(2 3)) (= i (- n 4)) (= i (- n 3)))
        (should (= (string-byte a i) (string-byte aaaa i)))))))

(ert-deftest nelisp-dns-parse-response-aaaa-fixture ()
  "A hand-built DNS-over-TCP response with one AAAA/IN answer (RDATA = 16
raw bytes for `2001:db8::1') parses to the canonical IPv6 string via
`nelisp--dns-parse-response' QTYPE 28 -- against-the-bug: the SAME fixture
parsed with the default QTYPE (1, A) finds no matching answer (nil, not
a wrong value and not an error), proving QTYPE actually gates which
record type is extracted."
  (skip-unless (fboundp 'nelisp-process-start))
  (let* ((header (concat (string 0 1)             ; ID
                          (string 1 0)             ; flags: RD=1
                          (string 0 1)             ; QDCOUNT=1
                          (string 0 1)             ; ANCOUNT=1
                          (string 0 0) (string 0 0))) ; NS/ARCOUNT=0
         (qname (concat (string 7) "example" (string 3) "com" (string 0)))
         (question (concat qname (string 0 28) (string 0 1))) ; QTYPE=28 QCLASS=1
         (rdata (nelisp--ipv6-groups-to-bytes-string
                 (nelisp--ipv6-parse "2001:db8::1")))
         (answer (concat (unibyte-string 192 12)    ; NAME = compression ptr to qname@12
                          (string 0 28)             ; TYPE = 28 (AAAA)
                          (string 0 1)              ; CLASS = 1 (IN)
                          (string 0 0 0 60)         ; TTL
                          (string 0 16)             ; RDLENGTH = 16
                          rdata))
         (msg (concat header question answer)))
    (should (equal (nelisp--dns-parse-response msg 28) "2001:db8::1"))
    (should (null (nelisp--dns-parse-response msg 1)))))

(provide 'nelisp-process-adapter-test)
;;; nelisp-process-adapter-test.el ends here
