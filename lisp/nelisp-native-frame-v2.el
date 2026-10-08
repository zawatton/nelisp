;;; nelisp-native-frame-v2.el --- Rooted dynamic frame bridge -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-lexframe)
(require 'nelisp-env)
(require 'nelisp-native-funcall-v2)
(require 'nelisp-bytecode-cleanup)
(defconst nelisp-native-frame-v2-version "nelisp-native-frame-v2-3")
(let ((descriptor
       '(:version "nelisp-native-frame-v2-3" :name "nl_native_frame_v2"
         :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64
         :root-limit 256 :scratch-count 4 :exit-offset 1 :exit-base 1024
         :ownership reauthenticate :unwind activation-watermark :state-count 7 :identity ticket
         :actions ((0 enter 2) (1 specbind 2) (2 unbind-n 1) (3 leave 0)
                   (4 register-cleanup 1) (5 save-buffer 0)
                   (6 save-excursion 0) (7 save-restriction 0)
                   (8 push-handler 2) (9 pop-handler 0) (10 select-handler 2)
                   (11 land-handler 3) (12 copy-bank-cell 1)))))
  (defun nelisp-native-frame-v2-descriptor ()
    "Return a defensive copy of the source-owned action and operand schemas."
    (copy-tree descriptor)))
(defun nelisp-native-frame-v2-source ()
  "Lisp policy over the evaluator's actual mirror, frame records and cells.
The adapter installs/pops the prepared records after the callback has returned;
otherwise the evaluator would pop a new binding instead of its callback frame.
No symbol value is saved or set: removing a frame reveals the original void,
alias or dynamic cell without copying it to a parallel value store."
  '(let ((value-reader (symbol-function 'symbol-value))
         (save-state (symbol-function 'nelisp--bytecode-save-state))
         (run-cleanup (symbol-function 'nelisp-bytecode-cleanup-run)))
     (lambda (self state action operands mirror depth cell &optional frames)
     (cond
      ((= action 0)
       (when (and (vectorp state) (aref state 1)) (error "Native frame already active"))
       (vector self t depth nil (car operands) (cadr operands) cell))
      (t
       (unless (and (vectorp state) (= (length state) 7) (aref state 1)
                    (= depth (+ (aref state 2)
                                (let ((bindings 0))
                                  (dolist (event (aref state 3))
                                    (unless (consp event) (setq bindings (1+ bindings))))
                                  bindings))))
         (error "Native frame ownership mismatch"))
       (cond
        ((= action 1)
         (let ((name (car operands)) (seen nil) entry target)
           (unless (symbolp name) (signal 'wrong-type-argument (list 'symbolp name)))
           ;; The mirror's alias cell is authoritative, including void targets.
           (while (progn
                    (setq entry (nelisp--fast-hash-get
                                 (nelisp--record-ref mirror 0)
                                 (nelisp-lexframe--key name) nil (symbolp (nelisp-lexframe--key name)))
                          target (and entry (> (length entry) 5)
                                      (nelisp--record-ref entry 4)))
                    target)
             (when (memq name seen) (signal 'cyclic-variable-indirection (list name)))
             (push name seen)
             (setq name target))
           (when (or (memq name '(nil t)) (keywordp name))
             (signal 'setting-constant (list name)))
           ;; Buffer-local redirects share the evaluator mirror. Binding the
           ;; selected cell symbol preserves both the default and other buffers;
           ;; leaving this activation reveals the previous local/void cell.
           (when (and entry (> (length entry) 6))
             (let ((redirect (nelisp--record-ref entry 5)))
               (when (and (vectorp redirect) (= (length redirect) 2))
                 (let ((local (assq (funcall value-reader (aref redirect 0)) (aref redirect 1))))
                   (when local (setq name (cdr local)))))))
           (when frames
             (nelisp-lexframe-stack--ensure-capacity frames (1+ (nelisp-lexframe-stack-depth frames))))
           (let ((frame (nelisp-lexframe-make)))
             (nelisp-lexframe-bind frame name cell t)
             (aset state 3 (cons frame (aref state 3)))
             frame)))
        ((or (= action 2) (= action 3))
         (let* ((events (aref state 3))
                (count (if (= action 3) (length events) (car operands))))
           (unless (and (integerp count) (<= 0 count) (<= count (length events)))
             (error "Native frame unbind underflow"))
           ;; The adapter pops ONE entry before each callback, after this
           ;; callback's own evaluator frames have gone. Never detach the
           ;; remaining chain before a cleanup has run.
           count))
        ((= action 8)
         ;; Slots 4/5 were unused entry-layout hints. Keep the seven-slot ABI;
         ;; slot 4 becomes the activation's rooted, typed handler chain.
         (let* ((chain (if (listp (aref state 4)) (aref state 4) nil))
                (selector (car operands)) (meta (cadr operands))
                (nesting (length chain)) (pair (+ (aref meta 3) (* 2 nesting))))
           (unless (and (vectorp meta) (= (length meta) 6)
                        (< nesting (aref meta 4)))
             (error "Native handler nesting overflow"))
           (let ((entry (vector selector (aref meta 0) (aref meta 1)
                                (aref meta 2) (aref state 3) pair (1+ pair)
                                (aref meta 5))))
             (aset state 4 (cons entry chain))
             entry)))
        ((= action 9)
         (unless (consp (aref state 4)) (error "Native handler underflow"))
         (let ((entry (car (aref state 4))))
           (aset state 4 (cdr (aref state 4))) entry))
        ((= action 10)
         (let* ((kind (car operands)) (exit (cadr operands))
                (tag (car exit)) (conditions (and (= kind 1) (get tag 'error-conditions)))
                (chain (if (listp (aref state 4)) (aref state 4) nil))
                (selected nil))
           (while (and chain (not selected))
             (let* ((entry (car chain)) (selector (aref entry 0)))
               (when (and (= kind (aref entry 1))
                          (if (= kind 2) (and tag (eq tag selector))
                            (or (eq selector t)
                                (if (listp selector)
                                    (cl-some (lambda (pattern) (memq pattern conditions)) selector)
                                  (or (eq selector tag) (memq selector conditions))))))
                 (setq selected entry)))
             (setq chain (cdr chain)))
           selected))
        ((and (<= 4 action) (<= action 7))
         (let ((payload (if (= action 4)
                            (let ((cleanup (car operands)))
                              (lambda () (funcall run-cleanup cleanup)))
                          (funcall save-state (cond ((= action 5) 114)
                                                    ((= action 6) 138) (t 140))))))
           (aset state 3 (cons (list 1 payload) (aref state 3)))
           nil))
        (t (error "Unknown native frame action"))))))))
(let* ((source (nelisp-native-frame-v2-source))
       (hash (secure-hash 'sha256
                          (prin1-to-string (list (nelisp-native-frame-v2-descriptor)
                                                source (nelisp-bytecode-cleanup-source)))))
       (provider nil))
  ;; Bind the hash to the SAME source value used to build the private callback.
  ;; Public source queries and defensive descriptor copies cannot alter it.
  (defun nelisp-native-frame-v2-hash ()
    "Return the immutable bridge schema and policy identity for this load."
    (copy-sequence hash))
  ;; Bind SELF to a private function value, never a mutable public function cell.
  (setq provider (eval source t))
  (defun nelisp-native-frame-v2-initializer ()
    "Return the frozen policy callback used by each fresh native activation."
    provider))
(defun nelisp-native-frame-v2-bank-copy-emit (plan inputs roots body)
  "Copy authenticated operand cells through the active frame's raw adapter."
  (let ((result body) (index (length inputs)))
    (while (> index 0)
      (setq index (1- index))
      (unless (= (nth index inputs) (nth index roots))
        (let ((status (intern (format "bank_copy_%d" index))))
          (setq result
                `(let ((,status (extern-call nl_native_frame_v2 env ticket
                                             ,(plist-get plan :frame-state-root) 12
                                             ,(nth index inputs) ,(nth index roots))))
                   (if (= ,status 0) ,result ,status))))))
    result))

(defun nelisp-native-frame-v2-copy-emit (plan action inputs continuation)
  "Stage ACTION operands and check its status before CONTINUATION."
  (let ((status (intern (format "frame_status_%d" action))))
    (funcall (if (and (plist-get plan :handler-bank) (/= action 0))
                 (lambda (sources destinations body)
                   (nelisp-native-frame-v2-bank-copy-emit plan sources destinations body))
               #'nelisp-native-funcall-v2-copy-form)
     inputs (cl-subseq (plist-get plan :frame-staging-roots) 0 (length inputs))
     `(let ((,status (extern-call nl_native_frame_v2 env ticket ,(plist-get plan :frame-state-root)
                                 ,action ,(car (plist-get plan :frame-staging-roots))
                                 ,(plist-get plan :frame-result-root))))
        (if (= ,status 0) ,continuation
          (if (= ,status ,(+ 1024 (1+ (plist-get plan :frame-result-root))))
              ,(funcall (if (plist-get plan :handler-bank)
                            (lambda (sources destinations body)
                              (nelisp-native-frame-v2-bank-copy-emit plan sources destinations body))
                          #'nelisp-native-funcall-v2-copy-form)
                (number-sequence (1+ (plist-get plan :frame-result-root))
                                 (+ (plist-get plan :frame-result-root) 3))
                (number-sequence (plist-get plan :exit-root-base)
                                 (+ (plist-get plan :exit-root-base) 2))
                (+ 1024 (plist-get plan :exit-root-base)))
            ,status))))))
(provide 'nelisp-native-frame-v2)
