;;; standalone-native-funcall-v2-controls-driver.el --- Actual root ABI mutations -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(load "test/standalone-native-funcall-v2-driver.el" nil t)
(defun f1b-control-frame ()
  (let* ((env (nelisp--native-env))
         (addresses (list :begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2")
                          :reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2")
                          :end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2")
                          :slot (nelisp-native-load--symbol-addr "nl_root_pin_slot_v2")))
         (entry (nelisp-native-load--symbol-addr "nl_native_funcall_v2"))
         (ticket (ptr-call (plist-get addresses :begin) env 0 0 0 0 0)))
    (f1-root-assert (> ticket 0) "control begin")
    (unwind-protect
        (progn
          (dotimes (_ 20) (ptr-call (plist-get addresses :reserve) env ticket 0 0 0 0))
          (nelisp--native-pin-copy-v2 env ticket 1 (lambda (x) (signal 'wrong-type-argument (list 'listp x))))
          (nelisp--native-pin-copy-v2 env ticket 2 17)
          (let ((status (ptr-call entry env ticket 1 2 1 12)))
            (f1-root-assert (= status 1037) "signal captured in rooted exit triple")
            (f1-root-assert (equal (condition-case err
                                      (nelisp-native-cache--resume-exit addresses env ticket 13)
                                    (wrong-type-argument err))
                                  '(wrong-type-argument listp 17)) "exact signal data")
            ;; A real rooted exit is corrupted, then the real public resume
            ;; rejects its kind instead of fabricating signal/throw semantics.
            (nelisp--native-pin-copy-v2 env ticket 13 99)
            (f1-root-assert
             (equal (condition-case err
                        (progn (nelisp-native-cache--resume-exit addresses env ticket 13) nil)
                      (error (cadr err))) "Malformed native cache exit kind")
             "exit-kind mutation refused")))
      (f1-root-assert (= (ptr-call (plist-get addresses :end) env ticket 0 0 0 0) 1) "control end"))
    (f1-root-assert (= (ptr-call entry env ticket 1 2 1 12) 2) "stale ticket refused")))
(f1b-control-frame)
(princ "F1B-ROOT-CONTROLS-PASS signal-data exit-kind malformed-ticket stale-ticket\n")
