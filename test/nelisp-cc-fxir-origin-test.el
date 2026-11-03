;;; nelisp-cc-fxir-origin-test.el --- Bounded origin codec tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-cc-fxir-origin)

(defun nfo-comparison (&optional path)
  (nelisp-cc-fxir-origin-make :comparison :path (or path '(1 2))
                              :opcode 13 :arity 2 :status0 t :representation :word))

(ert-deftest nfo-all-origin-variants-roundtrip-and-copy-fields ()
  (let* ((comparison (nfo-comparison))
         (assignment (nelisp-cc-fxir-origin-make
                      :assignment :path '(2 1) :slot 0 :version '(:assignment (2 1) 0)
                      :source comparison))
         (let-store (nelisp-cc-fxir-origin-make
                     :let-store :path '(2 2 1 0 2) :slot 1
                     :version '(:assignment (2 2 1 0) 1) :source comparison))
         (slot-use (nelisp-cc-fxir-origin-make
                    :slot-use :path '(2 3) :slot 0 :version '(:assignment (2 1) 0)
                    :source assignment))
         (join (nelisp-cc-fxir-origin-make
                :join :path '(2 4) :slot 0 :version '(:join (2 4) 0)
                :sources (list assignment let-store)))
         (origins (list comparison assignment let-store slot-use join)))
    (should (= nelisp-cc-fxir-origin-schema 1))
    (dolist (origin origins)
      (should origin)
      (should (equal origin (nelisp-cc-fxir-origin-decode origin))))
    (should (= 13 (nelisp-cc-fxir-origin-get comparison :opcode)))
    (should (= 0 (nelisp-cc-fxir-origin-get slot-use :slot)))
    ;; The returned tree owns its path and nested source plist.
    (let ((decoded (nelisp-cc-fxir-origin-decode assignment)))
      (setcar (plist-get decoded :path) 99)
      (should (equal '(2 1) (nelisp-cc-fxir-origin-get assignment :path))))))

(ert-deftest nfo-nested-copy-join-and-shared-dag-roundtrip ()
  (let* ((comparison (nfo-comparison '(1 1)))
         (copy (nelisp-cc-fxir-origin-make
                :assignment :path '(1 2) :slot 1 :version '(:assignment (1 2) 1)
                :source comparison))
         (join (nelisp-cc-fxir-origin-make
                :join :path '(1 3) :slot 1 :version '(:join (1 3) 1)
                :sources (list copy copy)))
         (use (nelisp-cc-fxir-origin-make
               :slot-use :path '(1 4) :slot 1 :version '(:join (1 3) 1)
               :source join))
         (decoded (nelisp-cc-fxir-origin-decode use)))
    (should decoded)
    (should (equal use decoded))
    (should (eq :slot-use (nelisp-cc-fxir-origin-get decoded :kind)))
    (should (eq :join (nelisp-cc-fxir-origin-get
                       (nelisp-cc-fxir-origin-get decoded :source) :kind)))
    (should (= 200 (let ((i 0))
                     (dotimes (_ 200) (when (nelisp-cc-fxir-origin-decode use) (setq i (1+ i))))
                     i)))))

(ert-deftest nfo-rejects-malformed-plists-and-fields ()
  (let* ((valid (nfo-comparison))
         (malformed
          (list '(:kind :comparison :kind :comparison :path (1) :opcode 13 :arity 2
                  :status0 t :representation :word)
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 t)
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 t
                  :representation :word :extra nil)
                '(:kind :future :path (1))
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 t
                  :representation :word . :tail)
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 t
                  :representation)
                '(:kind :comparison :path (1 . 2) :opcode 13 :arity 2 :status0 t
                  :representation :word)
                '(:kind :comparison :path (1 -1) :opcode 13 :arity 2 :status0 t
                  :representation :word)
                '(:kind :comparison :path (1) :opcode 7 :arity 2 :status0 t
                  :representation :word)
                '(:kind :comparison :path (1) :opcode 13 :arity 3 :status0 t
                  :representation :word)
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 nil
                  :representation :word)
                '(:kind :comparison :path (1) :opcode 13 :arity 2 :status0 t
                  :representation :payload)
                (butlast valid))))
    (dolist (origin malformed)
      (should-not (nelisp-cc-fxir-origin-decode origin)))
    (should-not (nelisp-cc-fxir-origin-make
                 :slot-use :path '(1) :slot 512 :version '(:entry 1) :source valid))
    (should-not (nelisp-cc-fxir-origin-make
                 :slot-use :path '(1) :slot 0 :version nil :source valid))
    (should-not (nelisp-cc-fxir-origin-make
                 :slot-use :path '(1) :slot 0 :version "unsupported" :source valid))
    (should-not (nelisp-cc-fxir-origin-make
                 :join :path '(1) :slot 0 :version '(:join (1) 0)
                 :sources (list valid)))))

(ert-deftest nfo-rejects-cycles-and-depth-or-work-exhaustion ()
  (let* ((comparison (nfo-comparison))
         (plist-cycle (copy-tree comparison))
         (source-cycle (list :kind :assignment :path '(1) :slot 0
                             :version '(:entry 0) :source nil))
         (version (list :entry 0))
         (version-cycle nil)
         (deep comparison))
    (setcdr (last plist-cycle) plist-cycle)
    (setf (plist-get source-cycle :source) source-cycle)
    (setcdr (last version) version)
    (setq version-cycle (list :kind :assignment :path '(1) :slot 0
                              :version version :source comparison))
    (dotimes (i 30)
      (setq deep (list :kind :assignment :path (list i) :slot 0
                       :version (list :assignment (list i) 0) :source deep)))
    (dolist (bad (list plist-cycle source-cycle version-cycle deep))
      (should-not (nelisp-cc-fxir-origin-decode bad)))
    ;; A broad, shallow binary origin tree must exhaust the shared budget.
    (let ((level (cl-loop for i below 16 collect (nfo-comparison (list i)))))
      (dotimes (depth 4)
        (let ((next nil))
          (while level
            (let ((left (pop level)) (right (pop level)))
              (push (list :kind :join :path (list depth 0) :slot 0
                          :version (list :join depth 0) :sources (list left right)) next)))
          (setq level (nreverse next))))
      (should-not (nelisp-cc-fxir-origin-decode (car level))))
    ;; Rejected inputs do not poison a fresh decode budget.
    (should (nelisp-cc-fxir-origin-decode comparison))))

(ert-deftest nfo-comparison-opcodes-and-canonical-key-order ()
  (dolist (opcode '(6 13 14))
    (let* ((origin (nelisp-cc-fxir-origin-make
                    :comparison :representation :word :status0 t :arity 2
                    :opcode opcode :path nil))
           (decoded (nelisp-cc-fxir-origin-decode origin)))
      (should decoded)
      (should (equal decoded (list :kind :comparison :path nil :opcode opcode
                                   :arity 2 :status0 t :representation :word))))))

(ert-deftest nfo-rejects-forged-and-legacy-origin-fields ()
  (let ((comparison (nfo-comparison)))
    (dolist (bad '((:slot -1) (:slot bad) (:version nil) (:version [1])
                   (:path bad) (:path (1 . 2)) (:source nil)
                   (:source (:comparison (1) :opcode 13))))
      (let ((origin (list :kind :assignment :path '(1) :slot 0
                          :version 'entry :source comparison)))
        (setf (plist-get origin (car bad)) (cadr bad))
        (should-not (nelisp-cc-fxir-origin-decode origin))
        (should-not (nelisp-cc-fxir-origin-get origin :kind))))
    (should-not (nelisp-cc-fxir-origin-decode
                 '(:comparison (1) :opcode 13 :arity 2 :status0 t
                   :representation :word)))
    (dolist (sources (list nil (list comparison comparison comparison)
                          (cons comparison comparison)))
      (should-not (nelisp-cc-fxir-origin-make
                   :join :path '(1) :slot 0 :version 'entry :sources sources)))))

(ert-deftest nfo-path-boundary-and-cyclic-path-or-source-list ()
  (should (nfo-comparison (make-list 64 0)))
  (should-not (nfo-comparison (make-list 65 0)))
  (let ((path (list 0))
        (sources (list (nfo-comparison) (nfo-comparison))))
    (setcdr path path)
    (setcdr (last sources) sources)
    (should-not (nfo-comparison path))
    (should-not (nelisp-cc-fxir-origin-make
                 :join :path '(1) :slot 0 :version 'entry :sources sources))))

(ert-deftest nfo-exact-work-boundary-and-fresh-budget ()
  ;; One comparison with a one-cell path costs exactly 18 work units:
  ;; 2 events + 12 plist cells + 2 path visits + 2 copied cells.
  (let ((origin '(:kind :comparison :path (1) :opcode 13 :arity 2
                  :status0 t :representation :word)))
    (let ((nelisp-cc-fxir-origin--work-limit 18))
      (should (nelisp-cc-fxir-origin-decode origin)))
    (let ((nelisp-cc-fxir-origin--work-limit 17))
      (should-not (nelisp-cc-fxir-origin-decode origin)))
    (should (nelisp-cc-fxir-origin-decode origin))
    (should (= nelisp-cc-fxir-origin--work-limit 512))))

(ert-deftest nfo-owned-nested-source-and-version ()
  (let* ((origin (nelisp-cc-fxir-origin-make
                  :assignment :path '(1) :slot 0 :version '(:entry 0)
                  :source (nfo-comparison)))
         (decoded (nelisp-cc-fxir-origin-decode origin)))
    (should decoded)
    (setcar (plist-get decoded :version) :changed)
    (setcar (plist-get (plist-get decoded :source) :path) 99)
    (should (equal (plist-get origin :version) '(:entry 0)))
    (should (equal (plist-get (plist-get origin :source) :path) '(1 2)))))

(ert-deftest nfo-if-value-requires-ordered-arm-roots-and-exact-condition-path ()
  (let* ((then (nfo-comparison '(1 2)))
         (else (nfo-comparison '(1 3)))
         (valid (nelisp-cc-fxir-origin-make
                 :if-value :path '(1) :condition-path '(1 1)
                 :sources (list then else) :result-mask 6 :representation :word))
         (swapped (copy-tree valid))
         (bad-condition (copy-tree valid))
         (bad-mask (copy-tree valid))
         (bad-representation (copy-tree valid))
         (bad-root (copy-tree valid))
         (missing (copy-tree valid))
         (duplicate (append (copy-tree valid) '(:result-mask 6))))
    (should valid)
    (should (equal valid (nelisp-cc-fxir-origin-decode valid)))
    (setf (plist-get swapped :sources) (list else then))
    (setf (plist-get bad-condition :condition-path) '(1 2))
    (setf (plist-get bad-mask :result-mask) 15)
    (setf (plist-get bad-representation :representation) :payload)
    (setf (plist-get bad-root :sources) (list (nfo-comparison '(1 4)) else))
    (setq missing (plist-put missing :condition-path nil))
    (dolist (bad (list swapped bad-condition bad-mask bad-representation bad-root
                       (plist-put (copy-tree valid) :result-mask 0)
                       (plist-put (copy-tree valid) :representation nil)
                       (plist-put (copy-tree valid) :sources (list then))
                       duplicate missing))
      (should-not (nelisp-cc-fxir-origin-decode bad)))))

(ert-deftest nfo-if-value-rejects-cyclic-source-and-owns-both-paths ()
  (let* ((then (nfo-comparison '(1 2)))
         (else (nfo-comparison '(1 3)))
         (origin (nelisp-cc-fxir-origin-make
                  :if-value :path '(1) :condition-path '(1 1)
                  :sources (list then else) :result-mask 2 :representation :word))
         (decoded (nelisp-cc-fxir-origin-decode origin))
         (cyclic (copy-tree origin)))
    (should decoded)
    (setcar (plist-get decoded :condition-path) 9)
    (setcar (plist-get (nth 0 (plist-get decoded :sources)) :path) 8)
    (should (equal '(1 1) (nelisp-cc-fxir-origin-get origin :condition-path)))
    (should (equal '(1 2) (nelisp-cc-fxir-origin-get
                            (nth 0 (nelisp-cc-fxir-origin-get origin :sources)) :path)))
    (setf (plist-get cyclic :sources) (list cyclic else))
    (should-not (nelisp-cc-fxir-origin-decode cyclic))))

(ert-deftest nfo-if-value-rejects-overlong-child-before-raw-path-lookup ()
  (let* ((child (append (apply #'append (make-list 10000 '(:noise 1)))
                        '(:path (1 2))))
         (else (nfo-comparison '(1 3)))
         (origin (list :kind :if-value :path '(1) :condition-path '(1 1)
                       :sources (list child else) :result-mask 6
                       :representation :word))
         (original (symbol-function 'plist-get))
         (raw-pairs 0)
         (observer
          (lambda (plist property)
            (when (eq plist child)
              (let ((tail plist))
                (while (and tail (not (eq (car tail) property)))
                  (setq raw-pairs (1+ raw-pairs) tail (cddr tail)))))
            (funcall original plist property))))
    ;; Observe the real lookup, preserving its result and the shared budget.
    ;; Refusing the parent after scanning a raw child is already too late.
    (unwind-protect
        (progn
          (fset 'plist-get observer)
          (should-not (nelisp-cc-fxir-origin-decode origin)))
      (fset 'plist-get original))
    (should (= raw-pairs 0))
    (should (= nelisp-cc-fxir-origin--work-limit 512))))

(provide 'nelisp-cc-fxir-origin-test)
;;; nelisp-cc-fxir-origin-test.el ends here
