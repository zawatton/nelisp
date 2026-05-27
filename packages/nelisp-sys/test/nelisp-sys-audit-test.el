;;; nelisp-sys-audit-test.el --- ERT tests for nelisp-sys-audit -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 131.6 gate tests for `nelisp-sys-audit-unsafe' and
;; `nelisp-sys-diagnostic-format'.
;;
;; T1 — two unsafe blocks, one missing :reason
;; T2 — no unsafe blocks
;; T3 — unsafe extern collected
;; T4 — diagnostic-format stable shape

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-audit)

;;; T1 — two unsafe blocks, one missing :reason.

(ert-deftest nelisp-sys-audit-t1-two-unsafe-one-missing ()
  "T1: two unsafe blocks, one without :reason; count=2, missing-reason length=1."
  (let* ((forms '((sys:defun f ((p (ptr u8)) (q (ptr u8))) void (:alloc none)
                    (seq (sys:unsafe (:reason "ok") (sys:store! p 1))
                         (sys:unsafe (sys:store! q 2))))))
         (module (nelisp-sys-frontend-parse-module forms))
         (audit (nelisp-sys-audit-unsafe module)))
    (should (= 2 (plist-get audit :unsafe-count)))
    (should (= 1 (length (plist-get audit :missing-reason))))))

;;; T2 — no unsafe blocks.

(ert-deftest nelisp-sys-audit-t2-no-unsafe ()
  "T2: a defun with no unsafe blocks; count=0, missing-reason=nil."
  (let* ((forms '((sys:defun g ((a i32)) i32 () (+ a 1))))
         (module (nelisp-sys-frontend-parse-module forms))
         (audit (nelisp-sys-audit-unsafe module)))
    (should (= 0 (plist-get audit :unsafe-count)))
    (should (null (plist-get audit :missing-reason)))))

;;; T3 — unsafe extern collected.

(ert-deftest nelisp-sys-audit-t3-unsafe-extern ()
  "T3: an extern declared :unsafe t; its name appears in :unsafe-externs."
  (let* ((forms '((sys:extern memx (:symbol "memx" :abi c :unsafe t)
                    ((d (ptr u8))) (ptr u8) (:alloc none))))
         (module (nelisp-sys-frontend-parse-module forms))
         (audit (nelisp-sys-audit-unsafe module)))
    (should (equal '(memx) (plist-get audit :unsafe-externs)))))

;;; T4 — diagnostic-format stable shape.

(ert-deftest nelisp-sys-audit-t4-diagnostic-format ()
  "T4: nelisp-sys-diagnostic-format returns stable \"CODE: MESSAGE\" string."
  (should (string= "E-SYS-OWN-001: use after move"
                   (nelisp-sys-diagnostic-format
                    '(E-SYS-OWN-001 "use after move" (foo))))))

;;; nelisp-sys-audit-test.el ends here
