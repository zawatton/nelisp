;;; nelisp-native-template-profile.el --- In-process template phase/callee profile -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Run with PROFILE_FIXTURE (genuine GNU elc), PROFILE_CALLS, and a private
;; NELISP_NATIVE_CACHE. Profiling wraps live owners without reloading sources;
;; its timings include wrapper overhead and never qualify latency acceptance.
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-native-template)
(defvar nelisp-native-template-profile--stack nil)
(let ((records (make-hash-table :test 'eq)) (owners nil) (decode-start nil))
  (setq nelisp-native-template--phase-trace
        (lambda (phase)
          (if (eq phase 'decode-begin) (setq decode-start (float-time))
            (puthash 'bytecode-decode (vector 1 (- (float-time) decode-start)
                                             (- (float-time) decode-start)) records))))
  (dolist (name '(nelisp-native-cache-file nelisp-native-cache-abi-hash
                  nelisp-native-cache--input-hash nelisp-native-template-abi-hash
                  nelisp-native-template-open-library nelisp-native-template-recipe
                  nelisp-native-template-validate nelisp-native-template-frame-states
                  nelisp-native-template-assemble nelisp-native-template-manifest
                  nelisp-native-template--print nelisp-native-template--native-printable-p nelisp-native-template--hash
                  nelisp-native-template-bounded-data-p nelisp-native-template-patch32
                  nelisp-native-cache--publish nelisp-native-cache-load
                  nelisp-native-load--sha256 nelisp-native-load--raw-v2-trusted-decode
                  nelisp-native-load--mmap nelisp-native-load--poke-string
                  nelisp-native-load-raw-v2-artifact-trusted
                  nelisp-native-cache--callable-from-entry nelisp-native-load-box
                  nelisp-native-load-unbox nelisp-native-funcall-v2-initializer))
    (when (fboundp name)
      (let ((owner (symbol-function name)) (name name))
        (push (cons name owner) owners)
        (fset name (lambda (&rest arguments)
                     (let* ((start (float-time)) (frame (list 0.0))
                            (nelisp-native-template-profile--stack
                             (cons frame nelisp-native-template-profile--stack)))
                       (unwind-protect
                           (if (eq name 'nelisp-native-template-manifest)
                               (let ((hash-owner (symbol-function 'secure-hash)))
                                 (unwind-protect
                                     (progn
                                       (fset 'secure-hash
                                             (lambda (&rest args)
                                               (let ((start (float-time)))
                                                 (unwind-protect (apply hash-owner args)
                                                   (let* ((elapsed (- (float-time) start))
                                                          (row (or (gethash 'producer-secure-hash records)
                                                                   (vector 0 0.0 0.0))))
                                                     (aset row 0 (1+ (aref row 0)))
                                                     (aset row 1 (+ (aref row 1) elapsed))
                                                     (aset row 2 (+ (aref row 2) elapsed))
                                                     (puthash 'producer-secure-hash row records)
                                                     (setcar (car nelisp-native-template-profile--stack)
                                                             (+ (car (car nelisp-native-template-profile--stack)) elapsed)))))))
                                       (apply owner arguments))
                                   (fset 'secure-hash hash-owner)))
                             (apply owner arguments))
                         (let* ((elapsed (- (float-time) start))
                                (row (or (gethash name records) (vector 0 0.0 0.0))))
                           (aset row 0 (1+ (aref row 0)))
                           (aset row 1 (+ (aref row 1) elapsed))
                           (aset row 2 (+ (aref row 2) (- elapsed (car frame))))
                           (puthash name row records)
                           (when (cdr nelisp-native-template-profile--stack)
                             (setcar (cadr nelisp-native-template-profile--stack)
                                     (+ (car (cadr nelisp-native-template-profile--stack)) elapsed)))))))))))
  (unwind-protect
      (progn
        ;; Measure the actual frozen poll rather than replacing its GC/quit
        ;; policy. The provider and its function cell are restored below.
        (let* ((name 'nelisp-bytecode-native-rooted-cfg-poll-function)
               (owner (symbol-function name)) (poll (funcall owner)))
          (push (cons name owner) owners)
          (fset name
                (lambda ()
                  (lambda ()
                    (let ((start (float-time)))
                      (unwind-protect (funcall poll)
                        (let* ((elapsed (- (float-time) start))
                               (row (or (gethash 'actual-frozen-poll records)
                                        (vector 0 0.0 0.0))))
                          (aset row 0 (1+ (aref row 0)))
                          (aset row 1 (+ (aref row 1) elapsed))
                          (aset row 2 (+ (aref row 2) elapsed))
                          (puthash 'actual-frozen-poll row records))))))))
      (let* ((nelisp-native-cache-backend 'template)
             (function (cdr (assq 'compiler-r3-cons
                                  (nelisp-bytecode-native-consumer-read-elc-functions
                                   (getenv "PROFILE_FIXTURE")))))
             (start (float-time))
             (file (nelisp-native-cache-compile function))
             (compiled (float-time)) (native (nelisp-native-cache-load function))
             (loaded (float-time)) (result (funcall native 40 2)) (called (float-time))
             (calls (string-to-number (or (getenv "PROFILE_CALLS") "100"))))
        (unless (equal result '(40 . 2)) (error "Profile exact result failed"))
        (dotimes (i calls)
          (unless (equal (funcall native i i) (cons i i)) (error "Profile repeated call failed")))
        (princ (format "TEMPLATE-PROFILE compile=%.6f load=%.6f call=%.6f repeated=%d seconds=%.6f validations=%d file=%s\n"
                       (- compiled start) (- loaded compiled) (- called loaded) calls
                       (- (float-time) called) nelisp-native-template--validation-count file))
        (maphash (lambda (name row)
                   (princ (format "TEMPLATE-CALLEE %s calls=%d inclusive=%.6f exclusive=%.6f\n"
                                  name (aref row 0) (aref row 1) (aref row 2)))) records)))
    (setq nelisp-native-template--phase-trace nil)
    (dolist (owner owners) (fset (car owner) (cdr owner)))))
