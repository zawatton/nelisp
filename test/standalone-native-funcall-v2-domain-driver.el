;;; standalone-native-funcall-v2-domain-driver.el --- Cold domain refusal controls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(defun f1b-domain-assert (value label) (unless value (error "F1B domain: %s" label)))
(let* ((verified (nelisp-native-compiler-f1-runtime-proof--verify))
       (symbols (plist-get verified :symbols))
       (arena (ptr-read-u64 (plist-get (gethash "nl_arena_base" symbols) :address) 0))
       (env (nelisp--native-env)) (descriptor (+ arena 768))
       (witness (plist-get (gethash "nl_cold_chunk0_domain" symbols) :address))
       (walker (cdr (assoc "nl_gc_conserv_owner_slow" (plist-get verified :addresses))))
       (check (lambda (value)
                (nelisp-native-compiler-f1-runtime-proof--environment-domain-p arena value witness walker)))
       (size (ptr-read-u64 descriptor 8)) (limit (ptr-read-u64 descriptor 32))
       (cold-base (ptr-read-u64 witness 0)) (cold-size (ptr-read-u64 witness 8)))
  (f1b-domain-assert (funcall check env) "genuine domain")
  (f1b-domain-assert (not (funcall check (+ env 8))) "interior pointer refused")
  ;; No collector may run while these deliberate temporary corruptions are
  ;; installed. Every word is restored even if the checker signals.
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (unwind-protect
         (progn
           (ptr-write-u64 witness 0 0) (ptr-write-u64 witness 8 0)
           (ptr-write-u64 descriptor 8 (+ 268435456 65536))
           (ptr-write-u64 descriptor 32 (+ arena 268435456 65536))
           (f1b-domain-assert (condition-case nil (progn (funcall check env) nil) (error t))
                             "larger descriptor without cold authority refused"))
       (ptr-write-u64 descriptor 8 size) (ptr-write-u64 descriptor 32 limit)
       (ptr-write-u64 witness 0 cold-base) (ptr-write-u64 witness 8 cold-size))
     (dolist (pair (list (list (+ arena 8) (+ size 65536))
                        (list arena (+ size 1)) (list arena 17179869184)))
       (unwind-protect
           (progn
             (ptr-write-u64 witness 0 (car pair)) (ptr-write-u64 witness 8 (cadr pair))
             (f1b-domain-assert (condition-case nil (progn (funcall check env) nil) (error t))
                               "forged witness refused"))
         (ptr-write-u64 witness 0 cold-base) (ptr-write-u64 witness 8 cold-size)))
     (let ((header (- env 8)) (word (ptr-read-u64 (- env 8) 0)))
       (dolist (mutant (list (logior (logand word -8) 2) 8 4294967288))
         (unwind-protect
             (progn
               (ptr-write-u64 header 0 mutant)
               (f1b-domain-assert (not (condition-case nil (funcall check env) (error nil)))
                                 "free or malformed target header refused"))
           (ptr-write-u64 header 0 word))))))
  (f1b-domain-assert (funcall check env) "restored domain")
  (princ (format "F1B-DOMAIN-CONTROLS-PASS cold=%S size=%d\n" (> cold-size 0) size)))
