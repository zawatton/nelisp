;;; nelisp-cc-fxir-facts.el --- Conservative FXIR side tables -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Source analysis only: no result authorizes a compiler transformation.
;; Producer CODE is evidence about the supplied vector, not a live-code guard.

;;; Code:
(require 'cl-lib)
(require 'nelisp-cc-fxir-analysis)
(require 'nelisp-cc-fxir-origin)

(defconst nelisp-cc-fxir-facts--any 15)
(defconst nelisp-cc-fxir-facts--fixnum 1)
(defconst nelisp-cc-fxir-facts--nil 2)
(defconst nelisp-cc-fxir-facts--true 4)
(defconst nelisp-cc-fxir-facts--other 8)
(defconst nelisp-cc-fxir-facts--origin-schema nelisp-cc-fxir-origin-schema)
;; The native WORD contract uses 64 bits and two low tag bits.
(defconst nelisp-cc-fxir-facts--word-bits 64)
(defconst nelisp-cc-fxir-facts--tag-bits 2)
(defvar nelisp-cc-fxir-facts--loop-iterations 2
  "Internal fixed-point cap; values below two exercise widening conservatively.")
(defconst nelisp-cc-fxir-facts--min (- (ash 1 (- nelisp-cc-fxir-facts--word-bits nelisp-cc-fxir-facts--tag-bits 1))))
(defconst nelisp-cc-fxir-facts--max (1- (- nelisp-cc-fxir-facts--min)))

;; Keep the historical producer API as aliases to the sole origin authority.
(defalias 'nelisp-cc-fxir-facts--origin-decode #'nelisp-cc-fxir-origin-decode)
(defalias 'nelisp-cc-fxir-facts--origin-make #'nelisp-cc-fxir-origin-make)
(defalias 'nelisp-cc-fxir-facts--origin-get #'nelisp-cc-fxir-origin-get)

(defun nelisp-cc-fxir-facts-conversion-valid-p (type from to &optional boundary)
  "Check a proposed TYPE conversion between representations FROM and TO.
At BOUNDARY only a materialized tagged word is accepted.  Conditions are
internal and have no implicit conversion to or from Lisp values."
  (and (integerp type) (< 0 type) (<= type nelisp-cc-fxir-facts--any)
       (memq from '(:word :payload :condition))
       (memq to '(:word :payload :condition))
       (or (not boundary) (eq to :word))
       (cond ((or (eq from :condition) (eq to :condition))
              (and (not boundary) (eq from :condition) (eq to :condition)))
             ((or (eq from :payload) (eq to :payload))
              (= type nelisp-cc-fxir-facts--fixnum))
             (t t))))

(defun nelisp-cc-fxir-facts-analyze (row &optional code)
  "Analyze ROW and optional producer CODE without changing native generation.
Types are masks: bottom 0, fixnum 1, nil 2, true 4, other 8, any 15.
Versions are structural paths, including explicit join and barrier versions.
Only successful canonical comparisons establish BOOLEAN result facts."
  (let ((analysis (nelisp-cc-fxir-analysis-row row code))
        (facts (make-hash-table :test 'equal))
        (joins (make-hash-table :test 'equal))
        (boundaries (make-hash-table :test 'equal))
        (value-proofs (make-hash-table :test 'equal))
        (truth-proofs (make-hash-table :test 'equal))
        (syntax-hash nil))
    (if (not (and (plist-get analysis :valid) (<= (nth 2 row) 512)))
        (list :valid nil :refusal :invalid-or-resource-bound)
      (catch 'refusal
        (cl-labels
            ((record (table key value) (puthash key value table))
             (items (table)
               (let (xs) (maphash (lambda (k v) (push (list k v) xs)) table)
                    (sort xs (lambda (a b) (string< (prin1-to-string (car a))
                                                    (prin1-to-string (car b)))))))
             (constant (k)
               (if (not code) nelisp-cc-fxir-facts--any
                 (let ((v (aref code k)))
                   (cond ((null v) nelisp-cc-fxir-facts--nil)
                         ((eq v t) nelisp-cc-fxir-facts--true)
                         ((and (integerp v) (<= nelisp-cc-fxir-facts--min v nelisp-cc-fxir-facts--max)) nelisp-cc-fxir-facts--fixnum)
                         (t nelisp-cc-fxir-facts--other)))))
             (boolean-mask-p (mask) (= 0 (logand mask (lognot 6))))
             (public-env (env)
               (mapcar (lambda (entry) (list (car entry) (cadr entry))) (append env nil)))
             (barrier (env path kind)
               (record boundaries (list kind path) :word)
               (dotimes (i (length env))
                 (aset env i (list nelisp-cc-fxir-facts--any (list :barrier path i) nil))))
             (merge-env (a b path)
               (let ((out (copy-sequence a)))
                 (dotimes (i (length out))
                   (let* ((x (aref a i)) (y (aref b i))
                          (ty (logior (car x) (car y)))
                          (origin (and (boolean-mask-p ty) (nth 2 x) (nth 2 y)
                                       (nelisp-cc-fxir-facts--origin-make
                                        :join :path path :slot i :version (list :join path i)
                                        :sources (list (nth 2 x) (nth 2 y)))))
                          (v (if (equal x y) x
                               (list ty (list :join path i) origin))))
                     (aset out i v)
                     (unless (equal x y)
                       (record joins (list path i)
                               (list :inputs (mapcar (lambda (e) (list (car e) (cadr e))) (list x y))
                                     :result (list (car v) (cadr v)))))))
                 out))
             (prove-condition (node path env)
               (remhash path truth-proofs)
               (let* ((mask (walk node path env))
                      (origin (gethash path value-proofs)))
                 (when (and (boolean-mask-p mask) origin)
                   (record truth-proofs path
                           (list :kind :boolean-truth-test :syntax-sha256 syntax-hash
                                 :path path :source origin :result-mask 6
                                 :representation :word :status0 t)))
                 mask))
             (walk (n path env)
               ;; Loop fixed-point visits can revisit a structural path with a
               ;; less precise environment. Replace, never accumulate, facts.
               (remhash path value-proofs)
               (remhash path truth-proofs)
               (let ((kind (car n)) (value nelisp-cc-fxir-facts--any) (proof nil))
                 (pcase kind
                   (:nil (setq value nelisp-cc-fxir-facts--nil))
                   (:const (setq value (constant (nth 1 n))))
                   (:slot
                    (let ((entry (aref env (nth 1 n))))
                      (setq value (car entry))
                      (when (nth 2 entry)
                        (setq proof (nelisp-cc-fxir-facts--origin-make
                                     :slot-use :path path :slot (nth 1 n)
                                     :version (cadr entry) :source (nth 2 entry))))))
                   (:setslot
                    (setq value (walk (nth 2 n) (append path '(2)) env))
                    (aset env (nth 1 n)
                          (list value (list :assignment path (nth 1 n))
                                (and (boolean-mask-p value)
                                     (gethash (append path '(2)) value-proofs)
                                     (nelisp-cc-fxir-facts--origin-make
                                      :assignment :path path :slot (nth 1 n)
                                      :version (list :assignment path (nth 1 n))
                                      :source (gethash (append path '(2)) value-proofs))))))
                   (:seq
                    (setq value nelisp-cc-fxir-facts--nil)
                    (cl-loop for child in (cdr n) for i from 1 do
                             (setq value (walk child (append path (list i)) env))))
                   (:if
                    (prove-condition (nth 1 n) (append path '(1)) env)
                    (let* ((left (copy-sequence env)) (right (copy-sequence env))
                           (a (walk (nth 2 n) (append path '(2)) left))
                           (b (walk (nth 3 n) (append path '(3)) right))
                           (left-origin (gethash (append path '(2)) value-proofs))
                           (right-origin (gethash (append path '(3)) value-proofs))
                           (merged (merge-env left right path)))
                      (dotimes (i (length env)) (aset env i (aref merged i)))
                      (setq value (logior a b)
                            proof (and (boolean-mask-p a) (boolean-mask-p b)
                                       left-origin right-origin
                                       (nelisp-cc-fxir-facts--origin-make
                                        :if-value :path path
                                        :condition-path (append path '(1))
                                        :sources (list left-origin right-origin)
                                        :result-mask value
                                        :representation :word)))))
                   (:while
                    (let ((entry (copy-sequence env)) (head (copy-sequence env)) (stable nil))
                      (dotimes (_ (max 1 (min 2 nelisp-cc-fxir-facts--loop-iterations)))
                        (unless stable
                          (let ((next (copy-sequence head)))
                            (prove-condition (nth 1 n) (append path '(1)) next)
                            (walk (nth 2 n) (append path '(2)) next)
                            (barrier next path :poll)
                            (setq next (merge-env entry next (list :loop path)))
                            (if (equal next head) (setq stable t) (setq head next)))))
                      (unless stable
                        (dotimes (i (length head))
                          (unless (equal (aref head i) (aref entry i))
                            (aset head i (list nelisp-cc-fxir-facts--any (list :widen path i)))))
                        ;; Node records must reflect the widened loop head.
                        (let ((probe (copy-sequence head)))
                          (prove-condition (nth 1 n) (append path '(1)) probe)
                          (walk (nth 2 n) (append path '(2)) probe)
                          (barrier probe path :poll)))
                      ;; The test runs once more on the exit path.
                      (prove-condition (nth 1 n) (append path '(1)) head)
                      (dotimes (i (length env)) (aset env i (aref head i)))
                      (setq value nelisp-cc-fxir-facts--nil)))
                   (:let
                    (let ((saved (make-hash-table :test 'eql)) (bound nil))
                      (dolist (binding (nth 1 n))
                        (unless (eq (car binding) :store)
                          (throw 'refusal (list :valid nil :refusal (list :unsupported-binding (car binding) path))))
                        (let* ((slot (nth 1 binding)) (value-path (append path (list 1 (length bound) 2))))
                          (unless (gethash slot saved) (puthash slot (aref env slot) saved))
                          (push slot bound)
                          (let ((ty (walk (nth 2 binding) value-path env))
                                (origin (gethash value-path value-proofs)))
                            (aset env slot
                                  (list ty (list :assignment (append path (list 1 (1- (length bound)))) slot)
                                        (and (boolean-mask-p ty) origin
                                             (nelisp-cc-fxir-facts--origin-make
                                              :let-store :path value-path :slot slot
                                              :version (list :assignment (append path (list 1 (1- (length bound)))) slot)
                                              :source origin)))))))
                      (setq value (walk (nth 2 n) (append path '(2)) env))
                      (dolist (slot bound)
                        (aset env slot (list nelisp-cc-fxir-facts--any (list :scope-exit path slot) nil)))))
                   ((or :call :inline)
                    (cl-loop for child in (nthcdr 3 n) for i from 3 do
                             (walk child (append path (list i)) env))
                    ;; Inline operations retain the signal/reentry barrier.
                    ;; The successful comparison result is refined only after
                    ;; that invalidation has been applied.
                    (unless (eq kind :call) (barrier env path :signal))
                    (if (and (eq kind :inline)
                             (memq (nth 1 n) '(6 13 14))
                             (= (- (length n) 3) 2))
                        (setq value 6
                              proof (nelisp-cc-fxir-facts--origin-make
                                     :comparison :path path :opcode (nth 1 n)
                                     :arity (- (length n) 3)
                                     :status0 t :representation :word))
                      (when (eq kind :call) (barrier env path :call))
                      (setq value nelisp-cc-fxir-facts--any)))
                   (_ (throw 'refusal (list :valid nil :refusal (list :unsupported kind path)))))
                 (when proof (record value-proofs path proof))
                 (record facts path (list :type value :representation :word
                                          :slots (public-env env)))
                 value)))
          (let ((env (make-vector (nth 2 row) nil)))
            (setq syntax-hash (plist-get analysis :syntax-sha256))
            (dotimes (i (length env)) (aset env i (list nelisp-cc-fxir-facts--any (list :entry i) nil)))
            (let ((result (walk (nth 1 (nth 4 row)) '(1) env)))
              (record boundaries '(:exit) :word)
              (list :valid t :schema 2 :origin-schema nelisp-cc-fxir-facts--origin-schema
                    :analysis-hash syntax-hash
                    :producer-proof (if code :supplied-vector :absent)
                    :result result :facts (items facts) :joins (items joins)
                    :boundaries (items boundaries) :boolean-truth-tests (items truth-proofs)
                    :optimization-authorized nil))))))))

(defun nelisp-cc-fxir-facts--origin-proof-valid-p (origin)
  "Check that ORIGIN has a bounded, canonical schema and valid roots."
  (and (nelisp-cc-fxir-facts--origin-decode origin) t))

(defun nelisp-cc-fxir-facts--truth-proofs-consistent-p (facts)
  "Require each exported truth proof to match its final fact and provenance."
  (let ((syntax-hash (plist-get facts :analysis-hash))
        (entries (plist-get facts :facts))
        (proofs (plist-get facts :boolean-truth-tests)))
    (and (listp proofs)
         (eql (plist-get facts :origin-schema) nelisp-cc-fxir-facts--origin-schema)
         (cl-every
          (lambda (entry)
            (let* ((path (car entry)) (proof (cadr entry))
                   (fact-entry (assoc path entries))
                   (fact (cadr fact-entry))
                   (type (plist-get fact :type)))
              (and fact-entry (listp proof)
                   (equal (plist-get proof :path) path)
                   (equal (plist-get proof :syntax-sha256) syntax-hash)
                   (eq (plist-get proof :kind) :boolean-truth-test)
                   (= (or (plist-get proof :result-mask) 0) 6)
                   (eq (plist-get proof :representation) :word)
                   ;; :status0 is a consumer precondition marker, not proof of
                   ;; native control-flow dominance.
                   (eq (plist-get proof :status0) t)
                   (integerp type) (> type 0) (= 0 (logand type (lognot 6)))
                   (eq (plist-get fact :representation) :word)
                   (nelisp-cc-fxir-facts--origin-proof-valid-p
                    (plist-get proof :source))
                   (equal (nelisp-cc-fxir-facts--origin-get
                           (plist-get proof :source) :path)
                          path))))
          proofs))))

(defun nelisp-cc-fxir-facts-verify (row code candidate)
  "Recompute ROW and CODE facts and reject modified CANDIDATE tables.
This is a consistency check, not an independent semantic or native proof."
  (condition-case nil
      (let ((expected (nelisp-cc-fxir-facts-analyze row code)))
        ;; Compare against the complete finite recomputation before traversing
        ;; caller-owned tables. Malformed, cyclic, stale or noncanonical data
        ;; cannot become an authority through a shape-only validation pass.
        (and (plist-get expected :valid)
             (equal expected candidate)
             (nelisp-cc-fxir-facts--truth-proofs-consistent-p expected)))
    (error nil)))

;; These bind complete executable trees, not a list of asserted properties.
;; They must be re-reviewed when the guarded compiler template changes.
(defconst nelisp-cc-fxir-facts--add-baseline-sha256 "17ac7d32bfc9fccd61ba847c28f8a5582692c421d41fc2fca9d3589d5c2fbb12")
(defconst nelisp-cc-fxir-facts--add-candidate-sha256 "1376d0d9dfaee90c78ed4a18b4d0c387d363ee521d6afc0c827f9788d25c24bd")
(defconst nelisp-cc-fxir-facts--add-site-path '(cdr car cdr car cdr car))
(defconst nelisp-cc-fxir-facts--add-id-paths
  '((cdr cdr car cdr cdr car cdr car cdr cdr cdr cdr car cdr cdr car)))

(defun nelisp-cc-fxir-facts--canonical-add-region (row site form)
  "Normalize only hygienic prefix, verified SITE and encoded ROW id in FORM.
Refuse any unexpected shape.  Do not mutate FORM or retain live pointers."
  (unless (and (plist-get (nelisp-cc-fxir-analysis-row row) :valid)
               (= (nth 2 row) 2) (null (nth 3 row))
               (equal (nth 5 row) '(2 2 -1))
               (equal (nth 4 row) '(:fn (:inline 4 11 (:inline 4 7 (:slot 0) (:slot 1)) (:const 9))))
               (integerp (nth 1 row)) (<= 0 (nth 1 row) 1073741823)
               (eq (car-safe form) 'let*) (proper-list-p (nth 1 form))
               (= (length (nth 1 form)) 6)
               (equal (cadr (nth 1 (nth 1 form))) site)
               (symbolp (caar (nth 1 form))))
    (error "Unsupported addition region"))
  (let* ((name (symbol-name (caar (nth 1 form))))
         (prefix (and (string-match "\\`\\(ni_\\(?:[0-9]+_\\)+\\)1_env\\'" name)
                      (match-string 1 name)))
         (id-word (1+ (* 4 (- -16 (nth 1 row)))))
         (left 100000) (found 0) (site-found nil))
    (unless prefix (error "Invalid hygienic region"))
    (cl-labels ((walk (x path depth)
                  (when (or (< (cl-decf left) 0) (> depth 500)) (error "Region budget"))
                  (cond ((equal path nelisp-cc-fxir-facts--add-site-path)
                         (unless (equal x site) (error "Site mismatch"))
                         (setq site-found t) :verified-site)
                        ((member path nelisp-cc-fxir-facts--add-id-paths)
                         (unless (eql x id-word) (error "Native id mismatch"))
                         (cl-incf found) :verified-row-id)
                        ((consp x) (cons (walk (car x) (append path '(car)) (1+ depth))
                                         (walk (cdr x) (append path '(cdr)) (1+ depth))))
                        ((and (symbolp x) (string-prefix-p prefix (symbol-name x)))
                         (unless (eq x (intern-soft (symbol-name x))) (error "Uninterned local"))
                         (intern (concat "ni_0_" (substring (symbol-name x) (length prefix)))))
                        ((and (symbolp x) (string-prefix-p "ni_" (symbol-name x)))
                         (error "Foreign hygienic prefix"))
                        (t x))))
      (let ((result (walk form nil 0)))
        (unless (and site-found (= found (length nelisp-cc-fxir-facts--add-id-paths)))
          (error "Missing canonical positions: site %S, ids %S" site-found found))
        result))))

(defun nelisp-cc-fxir-facts--add-region-hash (row site form)
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-quoted t) (print-gensym t))
    (secure-hash 'sha256 (prin1-to-string (nelisp-cc-fxir-facts--canonical-add-region row site form)))))

(defun nelisp-cc-fxir-facts-verify-add-region (row site baseline candidate)
  "Check exact audited BASELINE and retag-only CANDIDATE for ROW and SITE.
Accept only the reviewed executable trees, including guards and fallback.
This proves structural identity with audited templates, not live runtime
identity, source freshness, emitted machine code, or native execution.
Consume the same unmodified CANDIDATE immediately on success."
  (condition-case nil
      (and (equal (nelisp-cc-fxir-facts--add-region-hash row site baseline)
                  nelisp-cc-fxir-facts--add-baseline-sha256)
           (equal (nelisp-cc-fxir-facts--add-region-hash row site candidate)
                  nelisp-cc-fxir-facts--add-candidate-sha256))
    (error nil)))

(provide 'nelisp-cc-fxir-facts)
;;; nelisp-cc-fxir-facts.el ends here
