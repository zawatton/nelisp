;;; nelisp-cc-fxir-origin.el --- Bounded FXIR fact-origin codec -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'cl-lib)

(defconst nelisp-cc-fxir-origin-schema 1)
;; One budget covers ENTER/LEAVE events, plist cells, path/version traversals,
;; and the reserved owned-copy work. It is never reset for an origin child.
(defconst nelisp-cc-fxir-origin--work-limit 512)
(defconst nelisp-cc-fxir-origin--depth-limit 24)
(defconst nelisp-cc-fxir-origin--path-limit 64)

(defconst nelisp-cc-fxir-origin--field-specs
  '((:comparison :kind :path :opcode :arity :status0 :representation)
    (:assignment :kind :path :slot :version :source)
    (:let-store :kind :path :slot :version :source)
    (:slot-use :kind :path :slot :version :source)
    (:join :kind :path :slot :version :sources)
    (:if-value :kind :path :condition-path :sources :result-mask :representation)))

(defun nelisp-cc-fxir-origin--charge (budget amount)
  "Charge AMOUNT work units from mutable BUDGET, returning non-nil if allowed."
  (let ((left (- (aref budget 0) amount)))
    (aset budget 0 left)
    (>= left 0)))

(defun nelisp-cc-fxir-origin--spec (kind)
  "Return the closed schema field list for KIND, or nil."
  (cdr (assq kind nelisp-cc-fxir-origin--field-specs)))

(defun nelisp-cc-fxir-origin--plist-read (value budget)
  "Read a bounded proper keyword plist VALUE into fields, or return nil."
  (let ((tail value) (seen-cells (make-hash-table :test 'eq))
        (seen-keys (make-hash-table :test 'eq)) (fields nil) (count 0) (ok t))
    (while (and tail ok)
      (setq count (1+ count))
      (if (or (> count 6) (not (nelisp-cc-fxir-origin--charge budget 2))
              (not (consp tail)) (gethash tail seen-cells)
              (not (keywordp (car tail))) (not (consp (cdr tail)))
              (gethash (car tail) seen-keys))
          (setq ok nil)
        (puthash tail t seen-cells)
        (puthash (car tail) t seen-keys)
        (push (cons (car tail) (cadr tail)) fields)
        (setq tail (cddr tail))))
    (and ok (null tail) (nreverse fields))))

(defun nelisp-cc-fxir-origin--path-valid-p (path budget)
  "Validate proper nonnegative integer PATH, charging BUDGET at each cell."
  (let ((tail path) (seen (make-hash-table :test 'eq)) (count 0) (ok t))
    (while (and tail ok)
      (setq count (1+ count))
      (if (or (> count nelisp-cc-fxir-origin--path-limit)
              (not (nelisp-cc-fxir-origin--charge budget 1))
              (not (consp tail)) (gethash tail seen)
              (not (integerp (car tail))) (< (car tail) 0))
          (setq ok nil)
        (puthash tail t seen)
        (setq tail (cdr tail))))
    (and ok (null tail) count)))

(defun nelisp-cc-fxir-origin--version-valid-p (version depth budget)
  "Validate nonempty symbol/integer/list VERSION iteratively under BUDGET."
  (let ((stack (list (vector :enter version depth)))
        (active (make-hash-table :test 'eq)) (ok t) (nodes 0))
    (while (and stack ok)
      (let* ((event (pop stack)) (operation (aref event 0))
             (value (aref event 1)) (level (aref event 2)))
        (unless (nelisp-cc-fxir-origin--charge budget 1) (setq ok nil))
        (when ok
          (if (eq operation :leave)
              (remhash value active)
            (setq nodes (1+ nodes))
            (if (> level nelisp-cc-fxir-origin--depth-limit)
                (setq ok nil)
              (cond
               ((or (symbolp value) (integerp value)) nil)
               ((consp value)
                (if (gethash value active)
                    (setq ok nil)
                  (puthash value t active)
                  (push (vector :leave value level) stack)
                  (push (vector :enter (cdr value) (1+ level)) stack)
                  (push (vector :enter (car value) (1+ level)) stack)))
               (t (setq ok nil))))))))
    (and ok version nodes)))

(defun nelisp-cc-fxir-origin--source-list-read (value budget)
  "Read exactly two proper source origins from VALUE under BUDGET."
  (let ((tail value) (seen (make-hash-table :test 'eq)) (sources nil) (ok t))
    (while (and tail ok)
      (if (or (>= (length sources) 2)
              (not (nelisp-cc-fxir-origin--charge budget 1))
              (not (consp tail)) (gethash tail seen))
          (setq ok nil)
        (puthash tail t seen)
        (push (car tail) sources)
        (setq tail (cdr tail))))
    (and ok (null tail) (= (length sources) 2) (nreverse sources))))

(defun nelisp-cc-fxir-origin--fields-valid-p (kind fields budget depth)
  "Validate the data-driven fields of KIND and return normalized source list."
  (let ((spec (nelisp-cc-fxir-origin--spec kind))
        (actual (mapcar #'car fields)))
    (when (and spec (= (length spec) (length actual))
               (cl-every (lambda (key) (memq key actual)) spec))
      (let ((path (cdr (assq :path fields))))
        (when (nelisp-cc-fxir-origin--path-valid-p path budget)
          (pcase kind
            (:comparison
             (and (memq (cdr (assq :opcode fields)) '(6 13 14))
                  (eql (cdr (assq :arity fields)) 2)
                  (eq (cdr (assq :status0 fields)) t)
                  (eq (cdr (assq :representation fields)) :word)
                  :comparison))
            ((or :assignment :let-store :slot-use)
             (let ((slot (cdr (assq :slot fields)))
                   (version (cdr (assq :version fields))))
               (and (integerp slot) (<= 0 slot 511)
                    (nelisp-cc-fxir-origin--version-valid-p version (1+ depth) budget)
                    version :source)))
            (:join
             (let ((slot (cdr (assq :slot fields)))
                   (version (cdr (assq :version fields))))
               (and (integerp slot) (<= 0 slot 511)
                    (nelisp-cc-fxir-origin--version-valid-p version (1+ depth) budget)
                    version (nelisp-cc-fxir-origin--source-list-read
                             (cdr (assq :sources fields)) budget))))
            (:if-value
             (let* ((condition-path (cdr (assq :condition-path fields)))
                    (sources (nelisp-cc-fxir-origin--source-list-read
                              (cdr (assq :sources fields)) budget))
                    (result-mask (cdr (assq :result-mask fields))))
               (and (nelisp-cc-fxir-origin--path-valid-p condition-path budget)
                    (equal condition-path (append path '(1)))
                    (integerp result-mask) (> result-mask 0)
                    (= 0 (logand result-mask (lognot 6)))
                    (eq (cdr (assq :representation fields)) :word)
                    sources)))))))))

(defun nelisp-cc-fxir-origin--canonical (kind fields budget)
  "Build owned canonical fields from validated FIELDS under BUDGET."
  (let ((out nil) (spec (nelisp-cc-fxir-origin--spec kind)) (ok t))
    (dolist (key (cdr spec))
      (let ((value (cdr (assq key fields))))
        (when (memq key '(:path :version :condition-path))
          (let ((cost (cond
                       ((memq key '(:path :condition-path))
                        (* 2 (or (nelisp-cc-fxir-origin--path-valid-p value budget) 0)))
                       (t (let ((nodes (nelisp-cc-fxir-origin--version-valid-p value 0 budget)))
                            (and nodes (* 2 nodes)))))))
            ;; The validation passes above charge their own work. This charge
            ;; reserves the bounded cost of the owned copy before it runs.
            (unless (and cost (nelisp-cc-fxir-origin--charge budget cost))
              (setq ok nil)))
          (when ok (setq value (copy-tree value))))
        (when (eq key :source)
          (setq value (gethash (cdr (assq :source fields)) (aref budget 1))))
        (when (eq key :sources)
          (setq value (mapcar (lambda (item) (gethash item (aref budget 1))) value))
          (unless (and value (cl-every #'identity value)) (setq ok nil))
          ;; At parent LEAVE, these are owned canonical children decoded with
          ;; this same budget. Never inspect raw child plists on ENTER.
          (when (and ok (eq kind :if-value))
            (let ((path (cdr (assq :path fields))))
              (unless (and (equal (plist-get (nth 0 value) :path) (append path '(2)))
                           (equal (plist-get (nth 1 value) :path) (append path '(3))))
                (setq ok nil)))))
        (push (cons key value) out)))
    (and ok (apply #'append (mapcar (lambda (pair) (list (car pair) (cdr pair)))
                                    (nreverse out))))))

(defun nelisp-cc-fxir-origin-decode (origin)
  "Decode untrusted ORIGIN to a canonical owned plist, or nil on any refusal."
  (let* ((decoded (make-hash-table :test 'eq))
         (budget (vector nelisp-cc-fxir-origin--work-limit decoded))
         (active (make-hash-table :test 'eq))
        (stack (list (vector :enter origin 0))) (ok t))
    (condition-case nil
        (progn
          (while (and stack ok)
            (let* ((event (pop stack)) (op (aref event 0))
                   (value (aref event 1)) (depth (aref event 2)))
              (unless (nelisp-cc-fxir-origin--charge budget 1) (setq ok nil))
              (when ok
                (if (eq op :leave)
                    (let* ((kind (aref event 3)) (fields (aref event 4))
                           (canonical (nelisp-cc-fxir-origin--canonical kind fields budget)))
                      (if canonical
                          (progn (puthash value (cons :kind (cons kind canonical)) decoded)
                                 (remhash value active))
                        (setq ok nil)))
                  (cond
                   ((not (consp value)) (setq ok nil))
                   ((gethash value decoded) nil)
                   ((gethash value active) (setq ok nil))
                   ((> depth nelisp-cc-fxir-origin--depth-limit) (setq ok nil))
                   (t
                    (let* ((fields (nelisp-cc-fxir-origin--plist-read value budget))
                           (kind (cdr (assq :kind fields)))
                           (child-kind (and fields
                                            (nelisp-cc-fxir-origin--fields-valid-p
                                             kind fields budget depth)))
                           (children (pcase child-kind
                                       (:source (list (cdr (assq :source fields))))
                                       ((pred listp) child-kind)
                                       (_ nil))))
                      (if (not child-kind)
                          (setq ok nil)
                        (puthash value t active)
                        (push (vector :leave value depth kind fields) stack)
                        (dolist (child (reverse children))
                          (push (vector :enter child (1+ depth)) stack))))))))))
          (and ok (null stack) (gethash origin decoded)))
      (error nil))))

(defun nelisp-cc-fxir-origin-make (kind &rest fields)
  "Construct KIND with keyword FIELDS, validating it through the decoder."
  (nelisp-cc-fxir-origin-decode (cons :kind (cons kind fields))))

(defun nelisp-cc-fxir-origin-get (origin key)
  "Return KEY from validated ORIGIN, otherwise nil."
  (let ((decoded (nelisp-cc-fxir-origin-decode origin)))
    (and decoded (plist-get decoded key))))

(provide 'nelisp-cc-fxir-origin)
;;; nelisp-cc-fxir-origin.el ends here
