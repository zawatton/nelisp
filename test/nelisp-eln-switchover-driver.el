;;; nelisp-eln-switchover-driver.el --- S7.7 .neln -> .eln switchover e2e -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Run by test/nelisp-eln-switchover-smoke.sh on the NeLisp binary, one
;; process, top-level forms in order (a finalize step must run from a
;; frame that no longer holds the native callable, so every phase is its
;; own top-level form).
;;
;;   old path   -- every `.neln' module-init defun is defined first, as the
;;                 previous (non-native) definitions the switchover replaces
;;   migrate    -- `nelisp-eln-switchover-migrate-neln' on the real,
;;                 host-compiled `.neln' cache
;;   corrupt    -- three emitted artifacts are damaged: truncated and
;;                 wrong-ABI-hash copies whose migration records are
;;                 re-pinned to the damaged bytes (so only the loader can
;;                 reject them), and one flipped byte left unpinned
;;   batch      -- one batch, corrupted artifacts in the middle, plus a
;;                 pinned genuine GNU artifact; the batch must continue
;;   unload     -- retract + finalize: previous definitions restored,
;;                 owners/units/handles/private mappings released
;;   reload     -- the same artifacts route again, with new module ids
;;
;; The deferred-release and quarantine phases run concurrently in
;; test/nelisp-eln-switchover-lifecycle-driver.el (quarantine is
;; process-lifetime, so it needs its own process anyway).
;;
;; Every check prints S77_<NAME>=PASS; any failure prints S77_FAIL and
;; exits 1.

(load (expand-file-name "nelisp-eln-switchover-common.el"
                        (getenv "NELISP_S77_TEST_DIR"))
      nil t)

;;; Phase: old path.

(unless (and s77-neln (file-readable-p s77-neln) s77-eln-dir
             s77-gnu-identity (file-readable-p s77-gnu-identity))
  (s77-fail "env" s77-neln s77-eln-dir s77-gnu-identity))

(defvar s77-defuns (nelisp-eln-switchover--neln-defuns s77-neln))
(dolist (cell s77-defuns) (eval (cdr cell) t))
;; The genuine artifact's symbol has no old definition: its baseline is
;; the source semantics, evaluated only to capture expected results.
(defvar s77-previous
  (cons (cons 'nelisp-gnu-identity nil)
        (mapcar (lambda (cell) (cons (car cell) (symbol-function (car cell))))
                s77-defuns)))
(defvar s77-baseline
  (progn
    (fset 'nelisp-gnu-identity (lambda (value) value))
    (prog1 (s77-results) (fmakunbound 'nelisp-gnu-identity))))
(defvar s77-maps-0 (s77-private-mappings))
(s77-check "OLD_PATH" (and (= (length s77-defuns) 7)
                           (equal (nth 0 s77-baseline) 23))
           s77-defuns s77-baseline)

;;; Phase: migrate the existing .neln cache.

(defvar s77-migration (nelisp-eln-switchover-migrate-neln s77-neln s77-eln-dir))

(defun s77-entry (symbol)
  (let ((found nil))
    (dolist (e (plist-get s77-migration :entries))
      (when (eq (plist-get e :symbol) symbol) (setq found e)))
    found))

(s77-check "MIGRATE"
           (and (equal (mapcar (lambda (e) (list (plist-get e :symbol)
                                                 (plist-get e :route)))
                               (plist-get s77-migration :entries))
                       '((nelisp-s77-const eln) (nelisp-s77-bad-trunc eln)
                         (nelisp-s77-ident eln) (nelisp-s77-bad-abi eln)
                         (nelisp-s77-stale eln) (nelisp-s77-choose eln)
                         (nelisp-s77-add1 fallback)))
                (eq (plist-get (s77-entry 'nelisp-s77-add1) :reason)
                    'emitter-unsupported-shape)
                (equal (plist-get (s77-entry 'nelisp-s77-choose) :kind)
                       'expression)
                (file-readable-p (plist-get s77-migration :record))
                (equal (plist-get (nelisp-eln-switchover-read-migration
                                   (plist-get s77-migration :record))
                                  :neln-sha256)
                       (plist-get s77-migration :neln-sha256)))
           (plist-get s77-migration :entries))

;;; Phase: corrupt three artifacts.

(defun s77-rewrite (path bytes)
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil path nil 'silent)))

(defun s77-bytes (path) (nelisp-eln-system-loader--read-file path))

(defun s77-repin (symbol)
  (let ((entry (s77-entry symbol)))
    (plist-put entry :sha256
               (nelisp-eln-system-loader--file-sha256
                (s77-bytes (plist-get entry :eln))))))

(let* ((path (plist-get (s77-entry 'nelisp-s77-bad-trunc) :eln))
       (bytes (s77-bytes path)))
  (s77-rewrite path (substring bytes 0 (/ (length bytes) 2)))
  (s77-repin 'nelisp-s77-bad-trunc))
(let* ((path (plist-get (s77-entry 'nelisp-s77-bad-abi) :eln))
       (before (nelisp-eln-system-loader--file-sha256 (s77-bytes path))))
  ;; Zero the recorded producer ABI hash string.  GNU sed in the C locale
  ;; edits the binary in place; an in-process `string-search' over the
  ;; artifact costs seconds on the standalone binary.
  (unless (and (eq 0 (call-process "env" nil nil nil "LC_ALL=C" "sed" "-i"
                                   "s/ba35c031/00000000/g" path))
               (not (equal before (nelisp-eln-system-loader--file-sha256
                                   (s77-bytes path)))))
    (s77-fail "CORRUPT_ABI" "ABI hash not rewritten" path))
  (s77-repin 'nelisp-s77-bad-abi))
(let* ((path (plist-get (s77-entry 'nelisp-s77-stale) :eln))
       (bytes (s77-bytes path))
       (mid (/ (length bytes) 2)))
  (aset bytes mid (logxor (aref bytes mid) #xff))
  (s77-rewrite path bytes))
(s77-check "CORRUPT" t)

;;; Phase: one batch, corrupted artifacts in the middle.

;; The genuine pinned GNU artifact rides in the same batch, after the first
;; corrupted item and before the second.
(let ((entries (plist-get s77-migration :entries)))
  (plist-put s77-migration :entries
             (append (list (nth 0 entries) (nth 1 entries)
                           (list :symbol 'nelisp-gnu-identity :route 'eln
                                 :eln s77-gnu-identity
                                 :form '(defun nelisp-gnu-identity (value)
                                          value)))
                     (nthcdr 2 entries))))

(defvar s77-batch (nelisp-eln-switchover-load-migrated s77-migration))

(defconst s77-native '(nelisp-s77-const nelisp-gnu-identity nelisp-s77-ident
                       nelisp-s77-choose))
(defconst s77-expected-routes
  '((nelisp-s77-const eln self-emitted-migrated)
    (nelisp-s77-bad-trunc fallback registration-rejected)
    (nelisp-gnu-identity eln genuine-pinned)
    (nelisp-s77-ident eln self-emitted-migrated)
    (nelisp-s77-bad-abi fallback registration-rejected)
    (nelisp-s77-stale fallback artifact-hash-mismatch)
    (nelisp-s77-choose eln self-emitted-migrated)
    (nelisp-s77-add1 fallback emitter-unsupported-shape)))

(s77-check "BATCH_ROUTES"
           (let ((ok t))
             (dolist (exp s77-expected-routes)
               (let ((rec (s77-route (nth 0 exp))))
                 (unless (and rec (eq (plist-get rec :route) (nth 1 exp))
                              (eq (plist-get rec :reason) (nth 2 exp))
                              (= 1 (length (seq-filter
                                            (lambda (r) (eq (plist-get r :op) 'load))
                                            (nelisp-eln-switchover-log-for
                                             (nth 0 exp)))))
                              (or (eq (nth 1 exp) 'eln)
                                  (eq (plist-get rec :fallback-installed) t)))
                   (setq ok nil))))
             ok)
           (mapcar (lambda (e) (s77-route (car e))) s77-expected-routes))
(s77-check "BATCH_CONTINUED"
           (equal (mapcar #'car s77-batch)
                  '(nelisp-s77-const nelisp-s77-bad-trunc nelisp-gnu-identity
                    nelisp-s77-ident nelisp-s77-bad-abi nelisp-s77-stale
                    nelisp-s77-choose))
           s77-batch)
(s77-check "BATCH_RESULTS" (equal (s77-results) s77-baseline)
           (s77-results) s77-baseline)
(s77-check "BATCH_NATIVE"
           (let ((ok t))
             (dolist (s s77-native)
               (let ((entry (nelisp-eln-switchover-entry s)))
                 (unless (and entry
                              (eq (symbol-function s)
                                  (plist-get entry :published))
                              (nelisp-eln-switchover-function s)
                              (not (eq (symbol-function s)
                                       (cdr (assq s s77-previous))))
                              (>= (nelisp--native-subr-live-count
                                   (plist-get entry :module-id))
                                  1)
                              (> (car (nelisp-eln-switchover-counters s)) 0))
                   (setq ok nil))))
             ok)
           (mapcar #'nelisp-eln-switchover-counters s77-native))
(s77-check "CALL_FALLBACK_RECORDED"
           (let ((ok t))
             (dolist (s '(nelisp-s77-ident nelisp-gnu-identity))
               (let ((recs (seq-filter
                            (lambda (r) (eq (plist-get r :op) 'call-fallback))
                            (nelisp-eln-switchover-log-for s))))
                 (unless (and (= (length recs) 1)
                              (equal (plist-get (car recs) :kind)
                                     '(nelisp-eln-objects-unsupported
                                       unsupported-symbol-state))
                              (> (cdr (nelisp-eln-switchover-counters s)) 0))
                   (setq ok nil))))
             ;; Constants take no argument: never outside argument coverage.
             (and ok (= (cdr (nelisp-eln-switchover-counters 'nelisp-s77-const))
                        0)))
           nelisp-eln-switchover-log)
(s77-check "BATCH_REGISTRY"
           (and (= (s77-report :owners) 4) (= (s77-report :live-units) 4)
                (= (s77-report :open-handles) 4)
                (= (s77-report :pending-cleanups) 0)
                (null (s77-report :inconsistencies))
                (= (s77-private-mappings) (+ s77-maps-0 4)))
           (s77-report-all) s77-maps-0 (s77-private-mappings))
;; Negative control: the restoration checker must see that routed symbols
;; are NOT their previous definitions yet.
(s77-check "NEG_RESTORED_CHECKER" (not (s77-restored-p s77-native)))

;;; Phase: unload.

(defvar s77-first-modules
  (mapcar (lambda (s) (plist-get (nelisp-eln-switchover-entry s) :module-id))
          s77-native))
(defvar s77-first-handles
  (mapcar (lambda (s) (plist-get (nelisp-eln-switchover-entry s) :handle))
          s77-native))
(s77-check "UNLOAD_RETRACT"
           (equal (mapcar #'nelisp-eln-switchover-unload s77-native)
                  '(retracted retracted retracted retracted)))
(s77-check "UNLOAD_RESTORED"
           (and (s77-restored-p s77-native)
                ;; The genuine artifact's symbol had no old definition, so
                ;; its three calls (the last three inputs) are void again.
                (equal (butlast (s77-results) 3) (butlast s77-baseline 3))
                (not (fboundp 'nelisp-gnu-identity))
                (null (nelisp-eln-switchover-function 'nelisp-s77-const)))
           (s77-results))
(defvar s77-fin-1 (nelisp-eln-switchover-finalize-unloads))
(s77-check "UNLOAD_RELEASED"
           (and (equal s77-fin-1 '(:released 4 :deferred 0 :failed 0))
                (= (s77-report :owners) 0) (= (s77-report :live-units) 0)
                (= (s77-report :open-handles) 0)
                (= (s77-report :live-routes) 0)
                (null (s77-report :inconsistencies))
                (let ((ok t))
                  (dolist (h s77-first-handles)
                    (when (gethash h nelisp-eln-system-loader--handles)
                      (setq ok nil)))
                  ok)
                (let ((ok t))
                  (dolist (m s77-first-modules)
                    (unless (= (nelisp--native-subr-live-count m) 0)
                      (setq ok nil)))
                  ok)
                (= (s77-private-mappings) s77-maps-0))
           s77-fin-1 (s77-report-all) (s77-private-mappings))

;;; Phase: reload.

(defvar s77-reload
  (nelisp-eln-switchover-load-batch
   (list (list 'nelisp-s77-const
               (plist-get (s77-entry 'nelisp-s77-const) :eln)
               (s77-entry 'nelisp-s77-const) nil)
         (list 'nelisp-gnu-identity s77-gnu-identity
               '(:form (defun nelisp-gnu-identity (value) value))
               nil))))
(s77-check "RELOAD"
           (and (equal s77-reload '((nelisp-s77-const . eln)
                                    (nelisp-gnu-identity . eln)))
                (equal (s77-results) s77-baseline)
                (not (memq (plist-get (nelisp-eln-switchover-entry
                                       'nelisp-s77-const)
                                      :module-id)
                           s77-first-modules))
                (= (s77-report :owners) 2)
                (= (s77-private-mappings) (+ s77-maps-0 2)))
           s77-reload (s77-report-all))

(dolist (rec (reverse nelisp-eln-switchover-log))
  (princ (format "S77_INFO %S %S %S %s\n" (plist-get rec :op)
                 (plist-get rec :symbol)
                 (or (plist-get rec :route) (plist-get rec :result))
                 (let ((text (format "%S" (or (plist-get rec :reason)
                                              (plist-get rec :kind)))))
                   (concat text " "
                           (let ((d (format "%S" (plist-get rec :detail))))
                             (if (> (length d) 160) (substring d 0 160) d)))))))
(princ (format "NELISP-ELN-SWITCHOVER-SMOKE-PASS decisions=%d\n"
               (length nelisp-eln-switchover-log)))
(kill-emacs 0)

;;; nelisp-eln-switchover-driver.el ends here
