;;; nelisp-eln-handler-probe-driver.el --- Doc 210 S8.4 normal-path e2e -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The genuine host-compiled `condition-case' probe (test/fixtures/eln-handler/
;; s8-handler-probe.eln) is opened through the ordinary system loader (its
;; `_setjmp' surface and body admitted by declaration), given the S8
;; substrate (`current_thread_reloc' chain and the private `_setjmp' bound to
;; its GOT slot), and its native body is called on the NORMAL (no-error)
;; path.  The body is the unmodified GNU code: it calls `push_handler',
;; `_setjmp' through its own PLT, the guarded `Fsymbol_value'/`Fset', and
;; pops its handler through the three-load path -- all against the shadow
;; thread block.
;;
;; Only the three imports the body makes are test stubs here, written with
;; the NeLisp assembler and installed in a table that stands in for the
;; artifact's `freloc_link_table': `push_handler' (mints a GNU-layout
;; handler in the substrate pool, the S9 port's job in production),
;; `Fsymbol_value' (identity) and `Fset' (returns the value).  No signal is
;; raised in the guarded region (that is S9).
;;
;; Environment: NELISP_ROOT, NELISP_S8_ELN, NELISP_S8_BODY_SHA256.  Prints
;; `S8_<CHECK>=PASS', the probe transcript line `S8_PROBE_RESULT=<value>'
;; and a final PASS line.

;;; Code:

(let ((repo (getenv "NELISP_ROOT")))
  (add-to-list 'load-path (expand-file-name "lisp" repo))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" repo)))
(require 'cl-lib)
(require 'nelisp-eln-handler-substrate)

(defconst s8-probe--body "F73382d68616e646c65722d70726f6265_s8_handler_probe_0")
(defconst s8-probe--ok-word #x0123456789ab01)
(defconst s8-probe--caught-word #x0123456789ab02)
(defconst s8-probe--tag-word #x0123456789ab03)
(defconst s8-probe--argument-word #x0123456789ab04)

(defun s8-probe--pass (name) (princ (format "S8_%s=PASS\n" name)))

(defun s8-probe--check (name condition)
  (unless condition (error "S8 check failed: %s" name))
  (s8-probe--pass name))

(defun s8-probe--emit-push-handler (buf label thread block next-override)
  "push_handler(rdi=tag, rsi=type): mint BLOCK, link it, return it.
NEXT-OVERRIDE non-nil links to that word instead of the old handlerlist
(the negative control that leaves a wrong chain behind)."
  (nelisp-asm-x86_64-define-label buf label)
  (nelisp-asm-x86_64-mov-imm64 buf 'rax thread)
  (if next-override
      (nelisp-asm-x86_64-mov-imm64 buf 'rdx next-override)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'rdx 'rax #x68))
  (nelisp-asm-x86_64-mov-imm64 buf 'rcx block)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rcx 0 'rsi)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rcx 8 'rdi)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rcx #x20 'rdx)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rax #x68 'rcx)
  (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rcx)
  (nelisp-asm-x86_64-ret buf))

(defun s8-probe--stubs (thread block wrong-next)
  "Assemble the import stubs; return a plist with RWX page and entry addresses."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (s8-probe--emit-push-handler buf 'push-handler thread block nil)
    (s8-probe--emit-push-handler buf 'push-handler-wrong thread block wrong-next)
    (nelisp-asm-x86_64-define-label buf 'symbol-value)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rdi)
    (nelisp-asm-x86_64-ret buf)
    (nelisp-asm-x86_64-define-label buf 'set)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)
    (nelisp-asm-x86_64-ret buf)
    (let* ((bytes (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (page (nelisp-native-load-map-anonymous
                  (nelisp-eln-handler-substrate--page-round (length bytes)) t)))
      (nelisp-eln-handler-substrate--write-bytes page bytes)
      (list :push (+ page (cdr (assq 'push-handler labels)))
            :push-wrong (+ page (cdr (assq 'push-handler-wrong labels)))
            :symbol-value (+ page (cdr (assq 'symbol-value labels)))
            :set (+ page (cdr (assq 'set labels)))))))

(defun s8-probe--object (handle name)
  (plist-get (nelisp-eln-system-loader-symbol-info handle name) :address))

(defun s8-probe--install-ports (handle push stubs)
  "Fill a link table (slots 2, 1334, 1335), point `freloc_link_table' at it
and set the body's d_reloc constants (the genuine layout: d_reloc[0] is the
condition-case tag, [1] the landing pad's `caught', [2] the normal `ok')."
  (let* ((table-owner (nl-ffi-memory-allocate (* 8 1400)))
         (table (nl-ffi-memory-address table-owner))
         (d-reloc (s8-probe--object handle "d_reloc")))
    (dotimes (i 1400) (ptr-write-u64 table (* 8 i) 0))
    (ptr-write-u64 table (* 8 2) push)
    (ptr-write-u64 table (* 8 1334) (plist-get stubs :set))
    (ptr-write-u64 table (* 8 1335) (plist-get stubs :symbol-value))
    (ptr-write-u64 (s8-probe--object handle "freloc_link_table") 0 table)
    (ptr-write-u64 d-reloc 0 s8-probe--tag-word)
    (ptr-write-u64 d-reloc 8 s8-probe--caught-word)
    (ptr-write-u64 d-reloc 16 s8-probe--ok-word)
    table-owner))

(defun s8-probe--call (handle)
  (let ((cap (nelisp-eln-system-loader-function-capability
              handle s8-probe--body)))
    (ptr-call (nth 3 cap) s8-probe--argument-word 0 0 0 0 0)))

(defun s8-probe--run ()
  (setq nelisp-eln-registration--setjmp-declared-body-sha256s
        (list (getenv "NELISP_S8_BODY_SHA256")))
  (let* ((handle (nelisp-eln-system-loader-open (getenv "NELISP_S8_ELN")))
         (sub (nelisp-eln-handler-substrate-ensure))
         (thread (plist-get sub :thread))
         (sentinel (plist-get sub :sentinel))
         (block (nelisp-eln-handler-substrate-allocate-block))
         (wrong-block (nelisp-eln-handler-substrate-allocate-block))
         (stubs (s8-probe--stubs thread block wrong-block))
         (cap (nelisp-eln-system-loader-function-capability
               handle s8-probe--body))
         (body-start (nth 3 cap))
         (body-end (+ body-start (nth 6 cap)))
         (ports (s8-probe--install-ports handle (plist-get stubs :push) stubs)))
    (unwind-protect
        (progn
          (nelisp-eln-handler-substrate-bind-chain handle)
          (s8-probe--check "PROBE_CHAIN_AT_SENTINEL_BEFORE"
                           (= (nelisp-eln-handler-substrate-artifact-handlerlist
                               handle)
                              sentinel))
          ;; Control: with the artifact's PLT still bound to glibc `_setjmp'
          ;; the same body runs the normal path, but the jump buffer is not
          ;; in the private layout.
          (let ((result (s8-probe--call handle)))
            (s8-probe--check "PROBE_GLIBC_RUN_RESULT"
                             (= result s8-probe--ok-word))
            (let ((rip (ptr-read-u64 block (+ #x40 56))))
              (s8-probe--check "PROBE_CONTROL_UNBOUND_BUFFER_NOT_PRIVATE"
                               (not (and (>= rip body-start) (< rip body-end))))))
          (s8-probe--check "PROBE_GLIBC_RUN_HANDLERLIST_SENTINEL"
                           (= (nelisp-eln-handler-substrate-artifact-handlerlist
                               handle)
                              sentinel))
          ;; Bind the private pair, clear the block, run the normal path.
          (nelisp-eln-handler-substrate-bind-setjmp handle)
          (dotimes (i (/ nelisp-eln-handler-substrate-handler-size 8))
            (ptr-write-u64 block (* 8 i) 0))
          (let ((result (s8-probe--call handle)))
            (s8-probe--check "PROBE_NORMAL_PATH_RESULT"
                             (= result s8-probe--ok-word))
            (princ (format "S8_PROBE_RESULT=%s\n"
                           (cond ((= result s8-probe--ok-word) "ok")
                                 ((= result s8-probe--caught-word) "caught")
                                 (t (format "unexpected-%x" result))))))
          ;; The body's own bytes pushed a GNU-layout handler and popped it.
          (s8-probe--check "PROBE_HANDLER_BLOCK_GNU_LAYOUT"
                           (and (= (ptr-read-u64 block 0) 1)
                                (= (ptr-read-u64 block 8) s8-probe--tag-word)
                                (= (ptr-read-u64 block #x20) sentinel)))
          (s8-probe--check "PROBE_SETJMP_WAS_THE_PRIVATE_PAIR"
                           (let ((rip (ptr-read-u64 block (+ #x40 56))))
                             (and (>= rip body-start) (< rip body-end))))
          (s8-probe--check "PROBE_HANDLERLIST_AT_SENTINEL_AFTER"
                           (= (nelisp-eln-handler-substrate-artifact-handlerlist
                               handle)
                              sentinel))
          ;; The run is repeatable and leaves the chain where it found it.
          (s8-probe--check "PROBE_SECOND_RUN"
                           (and (= (s8-probe--call handle) s8-probe--ok-word)
                                (= (nelisp-eln-handler-substrate-artifact-handlerlist
                                    handle)
                                   sentinel)))
          ;; Negative: a push_handler that links the wrong `next' leaves
          ;; handlerlist off the sentinel, and the check notices.
          (let ((table (ptr-read-u64 (s8-probe--object handle "freloc_link_table")
                                     0)))
            (ptr-write-u64 table 16 (plist-get stubs :push-wrong)))
          (s8-probe--call handle)
          (s8-probe--check "PROBE_NEGATIVE_WRONG_CHAIN_DETECTED"
                           (/= (nelisp-eln-handler-substrate-artifact-handlerlist
                                handle)
                               sentinel))
          (ptr-write-u64 thread #x68 sentinel)
          (s8-probe--check "PROBE_CHAIN_RESET"
                           (= (nelisp-eln-handler-substrate-artifact-handlerlist
                               handle)
                              sentinel)))
      (nelisp-eln-system-loader-close handle)
      (nelisp-eln-system-loader--release-owner ports)))
  (princ "NELISP-ELN-HANDLER-PROBE-NORMAL-PASS 1\n"))

(s8-probe--run)
