;;; nelisp-eln-system-loader-capcache-test.el --- identity-cache fast path -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit coverage for `nelisp-eln-system-loader--ensure-file-unchanged' and
;; `nelisp-eln-system-loader--file-identity': the stat-based fast path that
;; lets per-call capability revalidation skip the full read+SHA-256 re-hash
;; when a cheap `stat(2)'-derived identity record still matches the one
;; captured at open time (or at the last full re-hash).
;;
;; All tests run under host Emacs ERT, where
;; `nelisp--syscall-stat-field' is not `fboundp', so
;; `nelisp-eln-system-loader--file-identity' always exercises its
;; `file-attributes'-based branch; `--state' and `--file-identity' are
;; mocked directly with `cl-letf' so no real file ever needs to exist on
;; disk, following the same style as
;; test/nelisp-eln-system-loader-indirection-test.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-system-loader)

(defvar nelisp-eln-system-loader-capcache-test--state nil
  "The single mutable state plist shared across a test's mocked calls.")

(defmacro nelisp-eln-system-loader-capcache-test--with-mocks
    (identity-fn &rest body)
  "Run BODY with `--state'/`--read-file'/`--file-sha256'/`--file-identity'
mocked.  IDENTITY-FN is called with no arguments and must return the
identity record `--file-identity' should report for the current call.
`read-file-calls' and `hash-calls' (dynamically bound) count calls into
the full, expensive path so tests can assert the cache actually skipped
it."
  (declare (indent 1))
  `(let ((read-file-calls 0)
         (hash-calls 0))
     (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
                (lambda (_handle) nelisp-eln-system-loader-capcache-test--state))
               ((symbol-function 'nelisp-eln-system-loader--file-identity)
                (lambda (_path) (funcall ,identity-fn)))
               ((symbol-function 'nelisp-eln-system-loader--read-file)
                (lambda (_path) (setq read-file-calls (1+ read-file-calls)) "bytes"))
               ((symbol-function 'nelisp-eln-system-loader--file-sha256)
                (lambda (_bytes) (setq hash-calls (1+ hash-calls)) "digest")))
       ,@body)))

(defun nelisp-eln-system-loader-capcache-test--signalled-p (thunk)
  "Return the `nelisp-eln-system-loader-error' data signalled by THUNK, or nil."
  (condition-case err
      (progn (funcall thunk) nil)
    (nelisp-eln-system-loader-error (cadr err))))

(ert-deftest nelisp-eln-system-loader-capcache-repeated-validation-hashes-once ()
  "An unchanging stat identity is hashed at most once across many calls."
  (let ((nelisp-eln-system-loader-capcache-test--state
         (list :path "fixture.eln" :file-sha "digest" :identity nil)))
    (nelisp-eln-system-loader-capcache-test--with-mocks
        (lambda () '(2 3 4 5 6 7 8))
      (dotimes (_ 5)
        (should-not
         (nelisp-eln-system-loader-capcache-test--signalled-p
          (lambda ()
            (nelisp-eln-system-loader--ensure-file-unchanged 'h 'probe)))))
      ;; Only the very first call (no stored identity yet) took the full
      ;; read+hash path; the remaining four hit the cheap stat-only path.
      (should (= read-file-calls 1))
      (should (= hash-calls 1))
      (should (equal (plist-get nelisp-eln-system-loader-capcache-test--state
                                 :identity)
                     '(2 3 4 5 6 7 8))))))

(defmacro nelisp-eln-system-loader-capcache-test--change-case (name field-index)
  "Define a test asserting a change at FIELD-INDEX forces re-hash + detection.
The identity record shape is (DEVICE INODE SIZE MTIME CTIME); FIELD-INDEX
selects which single field differs between the cached and fresh record."
  `(ert-deftest ,name ()
     (let* ((base '(2 3 100 1000 2000))
            (changed (copy-sequence base)))
       (setcar (nthcdr ,field-index changed) (1+ (nth ,field-index base)))
       (let ((nelisp-eln-system-loader-capcache-test--state
              (list :path "fixture.eln" :file-sha "digest" :identity base)))
         (nelisp-eln-system-loader-capcache-test--with-mocks
             (lambda () changed)
           ;; The file's identity moved (this field), and its content
           ;; really did change too (the stubbed hash no longer matches
           ;; the stored digest) -- the mismatch must be detected.
           (cl-letf (((symbol-function 'nelisp-eln-system-loader--file-sha256)
                      (lambda (_bytes) (setq hash-calls (1+ hash-calls))
                              "changed-digest")))
             (should (eq (nelisp-eln-system-loader-capcache-test--signalled-p
                          (lambda ()
                            (nelisp-eln-system-loader--ensure-file-unchanged
                             'h 'probe)))
                         'root-file-changed))
             (should (= read-file-calls 1))
             (should (= hash-calls 1))))))))

(nelisp-eln-system-loader-capcache-test--change-case
 nelisp-eln-system-loader-capcache-size-change-detects-content-change 2)
(nelisp-eln-system-loader-capcache-test--change-case
 nelisp-eln-system-loader-capcache-mtime-change-detects-content-change 3)
(nelisp-eln-system-loader-capcache-test--change-case
 nelisp-eln-system-loader-capcache-inode-change-detects-content-change 1)

(ert-deftest nelisp-eln-system-loader-capcache-negative-control-ignoring-identity-misses-change ()
  "Negative control: if the identity comparison is bypassed (as it would be
by a broken/regressed cache that always trusts the cached record), the
same on-disk content change from the size/mtime/inode tests above goes
undetected.  This proves the positive tests above are actually exercising
the identity gate, not passing for some unrelated reason."
  (let ((nelisp-eln-system-loader-capcache-test--state
         (list :path "fixture.eln" :file-sha "digest" :identity '(2 3 100 1000 2000))))
    (progn
      (nelisp-eln-system-loader-capcache-test--with-mocks
          ;; The broken variant: always report the STORED identity back,
          ;; regardless of what the real on-disk stat now is -- i.e. the
          ;; identity check can never observe a mismatch.
          (lambda () (plist-get nelisp-eln-system-loader-capcache-test--state
                                 :identity))
        (cl-letf (((symbol-function 'nelisp-eln-system-loader--file-sha256)
                   (lambda (_bytes) (setq hash-calls (1+ hash-calls))
                           "changed-digest")))
          ;; With identity comparison disabled this way, the real content
          ;; change (`changed' vs `base', and the digest stub above) is
          ;; never re-hashed and so is never caught: no error, zero calls
          ;; into the full path.  This is the failure mode the identity
          ;; check exists to prevent.
          (should-not
           (nelisp-eln-system-loader-capcache-test--signalled-p
            (lambda ()
              (nelisp-eln-system-loader--ensure-file-unchanged 'h 'probe))))
          (should (= read-file-calls 0))
          (should (= hash-calls 0)))))))

(ert-deftest nelisp-eln-system-loader-capcache-missing-identity-always-full-path ()
  "When the fast path cannot produce an identity record, every call takes
the full read+hash path (still safe, just not cheap)."
  (let ((nelisp-eln-system-loader-capcache-test--state
         (list :path "fixture.eln" :file-sha "digest" :identity nil)))
    (nelisp-eln-system-loader-capcache-test--with-mocks
        (lambda () nil)
      (dotimes (_ 4)
        (should-not
         (nelisp-eln-system-loader-capcache-test--signalled-p
          (lambda ()
            (nelisp-eln-system-loader--ensure-file-unchanged 'h 'probe)))))
      (should (= read-file-calls 4))
      (should (= hash-calls 4))
      ;; No usable identity was ever produced, so nothing was cached.
      (should-not (plist-get nelisp-eln-system-loader-capcache-test--state
                              :identity)))))

(ert-deftest nelisp-eln-system-loader-capcache-file-identity-host-emacs-shape ()
  "On host Emacs, `--file-identity' reads real `file-attributes' fields."
  (should-not (fboundp 'nelisp--syscall-stat-field))
  (let* ((path (make-temp-file "nelisp-eln-system-loader-capcache-"))
         (unwind (unwind-protect
                     (nelisp-eln-system-loader--file-identity path)
                   (delete-file path))))
    (should (consp unwind))
    (should (= (length unwind) 5))
    (should (integerp (nth 0 unwind)))  ; device
    (should (integerp (nth 1 unwind)))  ; inode
    (should (integerp (nth 2 unwind))))) ; size

(ert-deftest nelisp-eln-system-loader-capcache-file-identity-missing-file-is-nil ()
  "A nonexistent path has no identity, so the full path is always taken."
  (should-not
   (nelisp-eln-system-loader--file-identity
    "/nonexistent/nelisp-eln-system-loader-capcache-fixture.eln")))

(provide 'nelisp-eln-system-loader-capcache-test)

;;; nelisp-eln-system-loader-capcache-test.el ends here
