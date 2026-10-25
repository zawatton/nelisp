;;; argframe-owner-audit.el --- Bounded production owner-IR evidence -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(if (getenv "ARGFRAME_OWNER_BASELINE")
    (with-temp-buffer
      (insert-file-contents "target/argframe/pre-argframe.el")
      (let ((load-file-name (expand-file-name "scripts/nelisp-standalone-build.el")))
        (eval-buffer)))
  (require 'nelisp-standalone-build))
(setq max-lisp-eval-depth 40000 max-specpdl-size 80000)

(let* ((selected '(nelisp_apply_function_w nl_apply_builtin_frame_w
                    nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w
                    nl_eic_builtin_call_w nl_eic_argframe_call_w m5_fx_call_list))
       (selected (if (getenv "ARGFRAME_OWNER_BASELINE")
                     (cl-remove-if (lambda (name) (memq name '(nl_apply_builtin_frame_w nl_eic_argframe_call_w))) selected)
                   selected))
       (records nil)
       (output (or (getenv "ARGFRAME_OWNER_OUTPUT") "target/argframe/owner-ir.json")))
  (cl-labels
      ((measure (unit source)
         (let ((nelisp-standalone--inline-import-sources
                (nelisp-standalone--unit-inline-imports unit))
               (nelisp-aot-compiler--x86-direct-only-defuns
                (if (equal unit "applyfn-reader.o")
                    nelisp-standalone--direct-only-reader-defuns
                  (nelisp-standalone--direct-only-for unit))))
           (cl-letf
               (((symbol-function 'nelisp-aot-compiler--emit-defun)
                 (lambda (ir _buffer)
                   (let ((name (nelisp-aot-compiler--ir-get ir :name)))
                     (when (memq name selected)
                       (let ((nodes 0) (calls nil))
                         (nelisp-aot-compiler--walk-ir
                          (nelisp-aot-compiler--ir-get ir :body)
                          (lambda (node)
                            (cl-incf nodes)
                            (when (memq (nelisp-aot-compiler--ir-kind node) '(call extern-call))
                              (push (nelisp-aot-compiler--ir-get node :name) calls))))
                         (push (list :name (symbol-name name) :unit unit
                                     :params (length (nelisp-aot-compiler--ir-get ir :params))
                                     :rt-slots (nelisp-aot-compiler--ir-get ir :rt-slot-count)
                                     :nodes nodes
                                     :zero-selected
                                     :json-false
                                     :calls (vconcat (delete-dups calls))) records)))))))
             ;; The production preprocessing/owner-context/inlining pipeline runs;
             ;; machine emission is deliberately omitted by this source audit.
             (nelisp-standalone--compile-to-unit unit source)))))
    (measure "applyfn-reader.o" (nelisp-standalone--patch-autoload-lookups
                                  (nelisp-standalone--applyfn-source)))
    (require 'nelisp-cc-evalport-combiner-cons)
    (measure "combiner-cons-mxcache.o"
             (nelisp-standalone--patch-combiner-cons-word
              (nelisp-standalone--patch-n4-cons
               (nelisp-standalone--patch-autoload-lookups
                (nelisp-standalone--patch-combiner-cons-mxcache
                 (nelisp-standalone--patch-combiner-cons-full
                  nelisp-cc-evalport-combiner-cons--source)))))))
  (dolist (record records)
    (princ (format "OWNER %s params=%s rt=%s nodes=%s zero=%S\n"
                   (plist-get record :name) (plist-get record :params)
                   (plist-get record :rt-slots) (plist-get record :nodes)
                   (plist-get record :zero-selected))))
  (unless (= (length records) (length selected))
    (error "Missing owner IR: saw %d of %d" (length records) (length selected)))
  (let ((fx (cl-find "m5_fx_call_list" records :key (lambda (r) (plist-get r :name)) :test #'equal))
        (builtin (cl-find "nl_eic_builtin_call_w" records :key (lambda (r) (plist-get r :name)) :test #'equal)))
    (when (eq (plist-get fx :zero-selected) t) (error "Nine-argument FX helper cannot enter <=6-GP initializer"))
    (unless (or (getenv "ARGFRAME_OWNER_BASELINE")
                (member 'nl_eic_argframe_call_w (append (plist-get builtin :calls) nil)))
      (error "Rooted argv helper boundary disappeared in actual owner IR")))
  (with-temp-file output (insert (json-encode (vconcat (nreverse records))))))
