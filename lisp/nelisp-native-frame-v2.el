;;; nelisp-native-frame-v2.el --- Rooted dynamic frame bridge -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(require 'nelisp-lexframe)
(require 'nelisp-env)
(require 'nelisp-native-funcall-v2)
(defconst nelisp-native-frame-v2-version "nelisp-native-frame-v2-1")
(let ((descriptor
       '(:version "nelisp-native-frame-v2-1" :name "nl_native_frame_v2"
         :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64
         :root-limit 256 :scratch-count 4 :exit-offset 1 :exit-base 1024
         :ownership reauthenticate :unwind activation-watermark :state-count 7 :identity ticket
         :actions ((0 enter 2) (1 specbind 2) (2 unbind-n 1) (3 leave 0)))))
  (defun nelisp-native-frame-v2-descriptor ()
    "Return a defensive copy of the source-owned action and operand schemas."
    (copy-tree descriptor)))
(defun nelisp-native-frame-v2-hash ()
  "Bind the bridge schema and Lisp policy to the artifact contract."
  (secure-hash 'sha256 (prin1-to-string (list (nelisp-native-frame-v2-descriptor)
                                            (nelisp-native-frame-v2-source)))))
(defun nelisp-native-frame-v2-source ()
  "Lisp policy over the evaluator's actual mirror, frame records and cells.
The adapter installs/pops the prepared records after the callback has returned;
otherwise the evaluator would pop a new binding instead of its callback frame.
No symbol value is saved or set: removing a frame reveals the original void,
alias or dynamic cell without copying it to a parallel value store."
  '(let ((value-reader (symbol-function 'symbol-value)))
     (lambda (self state action operands mirror depth cell &optional frames)
     (cond
      ((= action 0)
       (when (and (vectorp state) (aref state 1)) (error "Native frame already active"))
       (vector self t depth nil (car operands) (cadr operands) cell))
      (t
       (unless (and (vectorp state) (= (length state) 7) (aref state 1)
                    (= depth (+ (aref state 2) (length (aref state 3)))))
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
           (aset state 3 (nthcdr count events))
           (when (= action 3) (aset state 1 nil))
           count))
        (t (error "Unknown native frame action"))))))))
(let ((provider nil))
  ;; Bind SELF to a private function value, never a mutable public function cell.
  (setq provider (eval (nelisp-native-frame-v2-source) t))
  (defun nelisp-native-frame-v2-initializer ()
    "Return the frozen policy callback used by each fresh native activation."
    provider))
(defun nelisp-native-frame-v2-copy-emit (plan action inputs continuation)
  "Stage ACTION operands and check its status before CONTINUATION."
  (let ((status (intern (format "frame_status_%d" action))))
    (nelisp-native-funcall-v2-copy-form
     inputs (cl-subseq (plist-get plan :frame-staging-roots) 0 (length inputs))
     `(let ((,status (extern-call nl_native_frame_v2 env ticket ,(plist-get plan :frame-state-root)
                                 ,action ,(car (plist-get plan :frame-staging-roots))
                                 ,(plist-get plan :frame-result-root))))
        (if (= ,status 0) ,continuation
          (if (= ,status ,(+ 1024 (1+ (plist-get plan :frame-result-root))))
              ,(nelisp-native-funcall-v2-copy-form
                (number-sequence (1+ (plist-get plan :frame-result-root))
                                 (+ (plist-get plan :frame-result-root) 3))
                (number-sequence (plist-get plan :exit-root-base)
                                 (+ (plist-get plan :exit-root-base) 2))
                (+ 1024 (plist-get plan :exit-root-base)))
            ,status))))))
(provide 'nelisp-native-frame-v2)
