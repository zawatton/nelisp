;;; nelisp-native-call-v2.el --- Artifact-local rooted CALL provider -*- lexical-binding: t; -*-

(require 'cl-lib)

;; The owner-sealed accessors are defined together inside a lexical scope.
(declare-function nelisp-native-call-v2-source "nelisp-native-call-v2" ())
(declare-function nelisp-native-call-v2-descriptor "nelisp-native-call-v2" ())
(declare-function nelisp-native-call-v2-runtime-imports "nelisp-native-call-v2" ())
(declare-function nelisp-native-call-v2--snapshot "nelisp-native-call-v2" (value))
(declare-function nelisp-native-call-v2--copy-node "nelisp-native-call-v2"
                  (item ancestors depth budget))

(defconst nelisp-native-call-v2--source
  '(seq
    (defun nl_native_call_v2_copy (to from)
      (seq (ptr-write-u64 to 0 (ptr-read-u64 from 0))
           (ptr-write-u64 to 8 (ptr-read-u64 from 8))
           (ptr-write-u64 to 16 (ptr-read-u64 from 16))
           (ptr-write-u64 to 24 (ptr-read-u64 from 24)) 0))
    (defun nl_native_call_v2_exit (kind tag value outer-kind outer-tag outer-value)
      (let* ((base (ptr-read-u64 (data-addr nl_arena_base) 0))
             (flag (ptr-read-u64 base 16)))
        (if (or (= flag 1) (= flag 2))
            (seq
             (nl_native_call_v2_copy tag (+ base 24))
             (nl_native_call_v2_copy value (+ base 56))
             (ptr-write-u64 kind 0 2)
             (ptr-write-u64 kind 8 flag)
             (ptr-write-u64 kind 16 0)
             (ptr-write-u64 kind 24 0)
             (nl_native_call_v2_copy outer-kind kind)
             (nl_native_call_v2_copy outer-tag tag)
             (nl_native_call_v2_copy outer-value value)
             ;; The stash remains owned until all three outer roots contain it.
             (ptr-write-u64 base 16 0) 1)
          2)))
    (defun nl_native_call_v2_inner
        (env nested marker ticket function argc output kind tag value)
      (let* ((first (nl_root_pin_reserve_v2 env nested))
             (i 1) (valid (if (= first 0) 0 1)))
        ;; Reserve function, arguments, nil, result, inspection and exit triple.
        (seq
         (while (and (= valid 1) (< i (+ argc 7)))
           (let* ((slot (nl_root_pin_reserve_v2 env nested)))
             (if (and (/= slot 0) (= slot (+ first (* i 32))))
                 (setq i (+ i 1))
               (setq valid 0))))
         (if (= valid 0) 2
           (let* ((nil-slot (+ first (* (+ argc 1) 32)))
                  (result (+ first (* (+ argc 2) 32)))
                  (exit-kind (+ first (* (+ argc 4) 32)))
                  (exit-tag (+ first (* (+ argc 5) 32)))
                  (exit-value (+ first (* (+ argc 6) 32))))
             (seq
              (setq i 0)
              (while (<= i argc)
                (seq (nl_native_call_v2_copy (+ first (* i 32))
                                            (+ function (* i 32)))
                     (setq i (+ i 1))))
              (let* ((rc (wf_bytecode_call_gateway env first first 1 argc result))
                     (link (- first 32)))
                (seq
                 ;; Recheck every nested slot before touching the arena stash.
                 (setq i 0)
                 (while (and (= valid 1) (< i (+ argc 7)))
                   (if (= (nl_root_pin_slot_v2 env nested i) (+ first (* i 32)))
                       (setq i (+ i 1))
                     (setq valid 0)))
                 (if (or (= valid 0)
                         (/= (ptr-read-u64 link 0) 2)
                         (/= (ptr-read-u64 link 8) marker)
                         (/= (ptr-read-u64 link 16) ticket)
                         (/= (ptr-read-u64 link 24) first))
                     2
                   (if (= rc 0)
                       (seq (nl_native_call_v2_copy output result)
                            (nl_native_call_v2_copy kind nil-slot)
                            (nl_native_call_v2_copy tag nil-slot)
                            (nl_native_call_v2_copy value nil-slot) 0)
                     (if (= rc 1)
                         (nl_native_call_v2_exit exit-kind exit-tag exit-value
                                                kind tag value)
                       2)))))))))))
    (defun nl_native_call_v2 (env ticket call-base argc result-index exit-base)
      ;; Bound words before sums: oversized unsigned words cannot wrap a window.
      (if (or (<= call-base 0) (>= call-base 256)
              (< argc 0) (> argc 5)
              (<= result-index 0) (>= result-index 256)
              (<= exit-base 0) (> exit-base 253))
          2
        (if (or (>= (+ call-base argc) 256)
                (and (>= result-index call-base) (<= result-index (+ call-base argc)))
                (and (>= result-index exit-base) (< result-index (+ exit-base 3)))
                (and (<= call-base (+ exit-base 2)) (>= (+ call-base argc) exit-base)))
            2
          (let* ((marker (nl_root_pin_slot_v2 env ticket 0))
                 (function (nl_root_pin_slot_v2 env ticket call-base))
                 (output (nl_root_pin_slot_v2 env ticket result-index))
                 (kind (nl_root_pin_slot_v2 env ticket exit-base))
                 (tag (nl_root_pin_slot_v2 env ticket (+ exit-base 1)))
                 (value (nl_root_pin_slot_v2 env ticket (+ exit-base 2)))
                 (i 0) (valid 1))
            (if (or (= marker 0) (= function 0) (= output 0)
                    (= kind 0) (= tag 0) (= value 0))
                2
              (seq
               (while (and (= valid 1) (<= i argc))
                 (if (= (nl_root_pin_slot_v2 env ticket (+ call-base i))
                        (+ function (* i 32)))
                     (setq i (+ i 1))
                   (setq valid 0)))
               ;; Nil and t have their own tags; tag 16 is an uninterned symbol.
               (if (or (= valid 0) (/= (ptr-read-u64 function 0) 4))
                   2
                 (let* ((nested (nl_root_pin_begin_v2 env)))
                   (if (= nested 0) 2
                     (let* ((status (nl_native_call_v2_inner
                                    env nested marker ticket function argc
                                    output kind tag value))
                            (ended (nl_root_pin_end_v2 env nested)))
                       (seq
                        (setq i 0)
                        (while (and (= valid 1) (<= i argc))
                          (if (= (nl_root_pin_slot_v2 env ticket (+ call-base i))
                                 (+ function (* i 32)))
                              (setq i (+ i 1))
                            (setq valid 0)))
                        (if (and (= ended 1) (= valid 1)
                                 (= (nl_root_pin_slot_v2 env ticket 0) marker)
                                 (= (nl_root_pin_slot_v2 env ticket result-index) output)
                                 (= (nl_root_pin_slot_v2 env ticket exit-base) kind)
                                 (= (nl_root_pin_slot_v2 env ticket (+ exit-base 1)) tag)
                                 (= (nl_root_pin_slot_v2 env ticket (+ exit-base 2)) value))
                            status 2)))))))))))))
  "Source-owned six-word provider; never a reader builtin or CALL permission.
Status 0 writes the result and clears the exit triple; 1 captures an exit;
2 refuses the request. Post-execution failure must never cause a retry.")

;; Keep canonical data and original owners inaccessible to public rebinding.
(let ((canonical nil) (owners nil) (owner-checker nil)
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr))
      (reject (symbol-function 'error)))
  (defun nelisp-native-call-v2-owner-valid-p ()
    "Check original owners before executing snapshot helpers."
    (let ((remaining owners))
      (while remaining
        (let ((entry (funcall head remaining)))
          (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
              nil (funcall reject "native-call: source owner changed")))
        (setq remaining (funcall tail remaining)))
      t))
  (defun nelisp-native-call-v2--copy-node (item ancestors depth budget)
    "Copy bounded, acyclic source data without macro expansion."
    (setcar budget (1- (car budget)))
    (if (or (< (car budget) 0) (> depth 64))
        (error "native-call: source bound exceeded"))
    (cond
     ((consp item)
      (if (memq item ancestors) (error "native-call: cyclic source"))
      (let ((next (cons item ancestors)))
        (cons (nelisp-native-call-v2--copy-node (car item) next (1+ depth) budget)
              (nelisp-native-call-v2--copy-node (cdr item) next (1+ depth) budget))))
     ((stringp item)
      (if (or (> (length item) 256) (text-properties-at 0 item)
              (< (or (next-property-change 0 item) (length item)) (length item)))
          (error "native-call: noncanonical source string"))
      (copy-sequence item))
     ((or (symbolp item) (integerp item)) item)
     (t (error "native-call: malformed source atom"))))
  (defun nelisp-native-call-v2--snapshot (value)
    "Return an independent bounded snapshot of VALUE."
    (nelisp-native-call-v2--copy-node value nil 0 (list 8192)))
  (defun nelisp-native-call-v2-source ()
    "Return canonical compilation data without granting native admission."
    (funcall owner-checker)
    (nelisp-native-call-v2--snapshot canonical))
  (defun nelisp-native-call-v2-descriptor ()
    "Return the exact slice-1 six-word SysV descriptor."
    (funcall owner-checker)
    (nelisp-native-call-v2--snapshot
     '(:name "nl_native_call_v2" :kind func :arity 6
       :params (u64 u64 u64 u64 u64 u64) :return u64
       :calling-convention sysv-amd64 :max-argc 5 :operation call
       :status-success 0 :status-exit 1 :status-refused 2 :exit-root-count 3
       :signal-kind 1 :throw-kind 2
       :exit-kind-layout (:bytes 32 :tag 2 :tag-offset 0 :payload-offset 8
                         :zero-offsets (16 24)))))
  (defun nelisp-native-call-v2-runtime-imports ()
    "Return exactly the six runtime imports allowed by Doc 212 section 3."
    (funcall owner-checker)
    (nelisp-native-call-v2--snapshot
     '((:name "nl_arena_base" :kind data :size 8)
       (:name "nl_root_pin_begin_v2" :kind func :arity 1 :params (u64) :return u64)
       (:name "nl_root_pin_end_v2" :kind func :arity 2 :params (u64 u64) :return u64)
       (:name "nl_root_pin_reserve_v2" :kind func :arity 2 :params (u64 u64) :return u64)
       (:name "nl_root_pin_slot_v2" :kind func :arity 3 :params (u64 u64 u64) :return u64)
       (:name "wf_bytecode_call_gateway" :kind func :arity 6
        :params (u64 u64 u64 u64 u64 u64) :return u64))))
  (defun nelisp-native-call-v2-dependency-context ()
    "Return original process-local owners and independent canonical data."
    (funcall owner-checker)
    (vector (mapcar (lambda (entry) (cdr entry)) owners)
            (nelisp-native-call-v2-source)
            (nelisp-native-call-v2-descriptor)
            (nelisp-native-call-v2-runtime-imports)))
  (setq canonical (nelisp-native-call-v2--snapshot nelisp-native-call-v2--source)
        owner-checker (funcall lookup 'nelisp-native-call-v2-owner-valid-p)
        owners (mapcar
                (lambda (name) (cons name (funcall lookup name)))
                '(nelisp-native-call-v2-owner-valid-p nelisp-native-call-v2-source
                  nelisp-native-call-v2-descriptor nelisp-native-call-v2-runtime-imports
                  nelisp-native-call-v2-dependency-context nelisp-native-call-v2--snapshot
                  nelisp-native-call-v2--copy-node car cdr cons list setcar memq
                  copy-sequence text-properties-at next-property-change 1- < >
                  consp 1+ stringp length symbolp integerp error vector mapcar
                  symbol-function eq and or cond))))

(defun nelisp-native-call-v2--reference-apply (function args tags)
  "Apply FUNCTION to ARGS, capturing the host's known throw TAGS.
The native stash captures arbitrary throws. Host Emacs needs explicit catch
tags; otherwise an unmatched throw becomes its ordinary no-catch signal."
  (if tags
      (let ((normal nil) (answer nil) (tag (car tags)))
        (setq answer
              (catch tag
                (prog1 (nelisp-native-call-v2--reference-apply function args (cdr tags))
                  (setq normal t))))
        (if normal answer (list 1 2 tag answer)))
    (condition-case condition
        (list 0 (apply function args))
      ((error quit) (list 1 1 (car condition) (cdr condition))))))

(defun nelisp-native-call-v2-reference (env ticket call-base argc result-index exit-base)
  "Model the six-word ABI using ENV's ticket and vector of reserved slots.
ENV is a plist with :ticket, :slots, optional :catch-tags and failure controls
:begin-fails, :reserve-limit, :gateway-refuses, :post-fails and :end-fails.
This host oracle models objects, not machine addresses or the collector."
  (let ((slots (plist-get env :slots)))
    (if (not (and (integerp ticket) (> ticket 0) (eql ticket (plist-get env :ticket))
                  (vectorp slots)
                  (integerp call-base) (< 0 call-base) (< call-base 256)
                  (integerp argc) (<= 0 argc) (<= argc 5)
                  (integerp result-index) (< 0 result-index) (< result-index 256)
                  (integerp exit-base) (< 0 exit-base) (<= exit-base 253)
                  (< (+ call-base argc) (min 256 (length slots)))
                  (< result-index (length slots)) (< (+ exit-base 2) (length slots))
                  (not (<= call-base result-index (+ call-base argc)))
                  (not (<= exit-base result-index (+ exit-base 2)))
                  (or (< (+ call-base argc) exit-base) (< (+ exit-base 2) call-base))
                  (let ((callee (aref slots call-base)))
                    (and (symbolp callee) callee (not (eq callee t))
                         (eq (intern-soft (symbol-name callee)) callee)))
                  (not (plist-get env :begin-fails))
                  (>= (or (plist-get env :reserve-limit) (+ argc 7)) (+ argc 7))))
        2
      (let ((answer
             (if (plist-get env :gateway-refuses) '(2)
               (nelisp-native-call-v2--reference-apply
                (aref slots call-base)
                (cl-loop for index from (1+ call-base) to (+ call-base argc)
                         collect (aref slots index))
                (plist-get env :catch-tags)))))
        (unless (plist-get env :post-fails)
          (cond ((= (car answer) 0)
                 (aset slots result-index (cadr answer))
                 (dotimes (i 3) (aset slots (+ exit-base i) nil)))
                ((= (car answer) 1)
                 (dotimes (i 3) (aset slots (+ exit-base i) (nth (1+ i) answer))))))
        (if (or (plist-get env :post-fails) (plist-get env :end-fails))
            2 (car answer))))))

(provide 'nelisp-native-call-v2)
;;; nelisp-native-call-v2.el ends here
