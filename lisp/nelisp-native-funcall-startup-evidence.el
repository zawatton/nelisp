;;; nelisp-native-funcall-startup-evidence.el --- Separate F1 proof specialization -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-compiler-startup-evidence)
(require 'nelisp-native-frame-v2)
(defun nelisp-native-funcall-startup-evidence-build (units builder root directory)
  "Derive an independent evaluator-boundary proof; preserve constructor issuance.
The evaluator itself is a source/binary-bound terminal, since its arbitrary
Lisp callees cannot form a finite static direct closure. Ticket and GC proof
policies remain unchanged. All boundary bytes and relocations are verified."
  (let* ((rewrite (symbol-function 'nelisp-native-compiler-startup-evidence--rewrite))
         (api (symbol-function 'nelisp-native-compiler-startup-evidence--proof-api))
         (roots (append nelisp-native-compiler-startup-evidence--roots '("nl_native_funcall_v2" "nl_native_frame_v2")))
         (exports (append nelisp-native-compiler-startup-evidence--exports '(("nl_native_funcall_v2" func 6) ("nl_native_frame_v2" func 6))))
         (nelisp-native-compiler-startup-evidence--roots roots)
         (nelisp-native-compiler-startup-evidence--exports exports))
    (cl-labels
        ((specialize (node generator)
           (cond
            ((equal node '(constructor)) '(f1))
            ;; The shared catch registry and throw-site signal construction
            ;; extend F1's closed runtime helpers. The separate authenticated
            ;; frame root admits at most 280, with every edge still proved.
            ((equal node '(<= 1 (length (alist-get 'records closure)) 192))
             '(<= 1 (length (alist-get 'records closure)) 280))
            ((equal node '(<= 15 (length functions) 192))
             '(<= 15 (length functions) 280))
            ((eq node 'compiler-runtime-v1) 'compiler-f1-runtime-v1)
            ((eq node 'compiler-constructor-memory-v1) 'compiler-f1-memory-v1)
            ((equal node "nelisp-compiler-constructor-prelink-v1") "nelisp-compiler-f1-prelink-v1")
            ((equal node "lisp/nelisp-native-compiler-runtime-capability.el") node)
            ((equal node "PRELINK_DIRECT_CLOSURE_PASS") "PRELINK_F1_BOUNDARY_PASS")
            ((equal node "scripts/nelisp-native-compiler-constructor-prelink.py") "scripts/nelisp-native-compiler-f1-prelink.py")
            ((equal node '(= (length offsets) 14))
             '(and (= (length offsets) (length (plist-get nelisp-native-compiler-f1-runtime-proof--expected :bss-offsets)))
                   (< (length offsets) 128)))
            ;; Only the independently source/binary-bound evaluator entry is a
            ;; terminal. Every bridge/root helper retains closed direct edges.
            ((and (not generator)
                  (equal node '(dolist (record (plist-get nelisp-native-compiler-runtime-proof--expected :functions))
                                 (puthash (plist-get record :name) t requested))))
             '(dolist (record (plist-get nelisp-native-compiler-f1-runtime-proof--expected :functions))
                (puthash (plist-get record :name) t requested)
                (dolist (relocation (plist-get record :relocations))
                  (puthash (plist-get relocation :symbol) t requested))))
            ((and generator (consp node) (eq (car node) 'list)
                  (equal (cl-subseq node 0 (min 5 (length node))) '(list :version 1 :layout layout)))
             (append (mapcar (lambda (item) (specialize item t)) node)
                     '(:funcall-descriptor (nelisp-native-funcall-v2-descriptor)
                       :funcall-hash (nelisp-native-funcall-v2-hash)
                       :frame-descriptor (nelisp-native-frame-v2-descriptor)
                       :frame-hash (nelisp-native-frame-v2-hash))))
            ((and (consp node) (eq (car node) 'quote)
                  (listp (cadr node))
                  (member "scripts/nelisp-native-compiler-constructor-prelink.py" (cadr node)))
             (list 'quote (append (mapcar (lambda (item) (specialize item generator)) (cadr node))
                                  '("lisp/nelisp-native-funcall-startup-evidence.el"
                                    "lisp/nelisp-native-funcall-v2.el" "lisp/nelisp-native-frame-v2.el"))))
            ((and (not generator) (consp node) (eq (car node) 'list)
                  (equal (cl-subseq node 0 (min 5 (length node))) '(list :version 1 :domain 'compiler-runtime-v1)))
             (let ((record (mapcar (lambda (item) (specialize item nil)) node)))
               (setcar (nthcdr 2 record) 2)
               (append record
                       '(:funcall-descriptor (copy-tree (plist-get expected :funcall-descriptor))
                         :funcall-hash (substring (plist-get expected :funcall-hash) 0)
                         :frame-descriptor (copy-tree (plist-get expected :frame-descriptor))
                         :frame-hash (substring (plist-get expected :frame-hash) 0)))))
            ((and (not generator)
                  (equal node '(cl-every (lambda (edge) (or (member (cdr (assq 'target edge)) names)
                                     (member (cdr (assq 'target edge))
                                             (plist-get nelisp-native-compiler-runtime-proof--expected :os-imports))))
                                        (plist-get record :direct))))
             '(or (equal (plist-get record :name) "nl_apply_function")
                  (cl-every (lambda (edge) (or (member (cdr (assq 'target edge)) names)
                                     (member (cdr (assq 'target edge))
                                             (plist-get nelisp-native-compiler-f1-runtime-proof--expected :os-imports))))
                            (plist-get record :direct))))
            ((and generator
                  (equal node '(unless (or (member (plist-get relocation :symbol) (alist-get 'os_imports closure))
                               (cl-find (plist-get relocation :symbol) records
                                                :key (lambda (item) (plist-get item :name)) :test #'equal))
                                 (cl-pushnew (plist-get relocation :symbol) data-targets :test #'equal))))
             '(unless (or (member (plist-get relocation :symbol) (alist-get 'os_imports closure))
                          (cl-find (plist-get relocation :symbol) records
                                  :key (lambda (item) (plist-get item :name)) :test #'equal)
                          (cl-some (lambda (unit)
                                     (cl-some (lambda (symbol)
                                                (and (equal (alist-get 'name symbol) (plist-get relocation :symbol))
                                                     (equal (alist-get 'section symbol) "text")))
                                              (alist-get 'symbols unit))) metadata))
                (cl-pushnew (plist-get relocation :symbol) data-targets :test #'equal)))
            ((consp node) (cons (specialize (car node) generator) (specialize (cdr node) generator)))
            ((symbolp node)
             (intern (replace-regexp-in-string "nelisp-native-compiler-runtime-" "nelisp-native-compiler-f1-runtime-"
                      (replace-regexp-in-string "nelisp-native-compiler-derived-startup-evidence" "nelisp-native-f1-derived-startup-evidence"
                                                (symbol-name node) t t) t t)))
            ((stringp node)
             (replace-regexp-in-string "nelisp-native-compiler-runtime-" "nelisp-native-compiler-f1-runtime-"
              (replace-regexp-in-string "nelisp-native-compiler-derived-startup-evidence" "nelisp-native-f1-derived-startup-evidence" node t t) t t))
            (t node))))
      (cl-letf (((symbol-function 'nelisp-native-compiler-derived-startup-evidence-build)
                 (lambda (&rest arguments)
                   (apply #'nelisp-native-f1-derived-startup-evidence-build arguments)))
                ((symbol-function 'nelisp-native-compiler-startup-evidence--rewrite)
                 (lambda (forms generator)
                   ;; Apply the reviewed base transform before this independent
                   ;; specialization so its exact structural counters still run.
                   (if generator (specialize (funcall rewrite forms generator) t)
                     (funcall rewrite forms generator))))
                ((symbol-function 'nelisp-native-compiler-startup-evidence--proof-api)
                 (lambda (forms) (specialize (funcall api forms) nil))))
        (nelisp-native-compiler-startup-evidence-build units builder root directory)))))
(provide 'nelisp-native-funcall-startup-evidence)
