;;; nelisp-subr-object-test.el --- Immutable Subr object in the function cell -*- lexical-binding: t; -*-

;; w2subr.  A directcall primitive's function cell holds a Subr object (header
;; kind 20, packed descriptor in header bits 8-32, the legacy (builtin NAME) list
;; in its one child slot).  `nl_directcall_id' recognises it with a header
;; compare and a descriptor load; every other reader gets the legacy list.

(require 'nelisp-directcall-test)

(defun w2s--src ()
  (with-temp-buffer
    (insert-file-contents (locate-library "nelisp-standalone-build.el"))
    (buffer-string)))

(defun w2s--count (re text)
  (let ((n 0) (pos 0))
    (while (string-match re text pos) (setq n (1+ n) pos (match-end 0)))
    n))

(defun w2s--has (x form)
  (or (equal x form) (and (consp form) (or (w2s--has x (car form)) (w2s--has x (cdr form))))))

(ert-deftest w2s-descriptor-packs-id-arity-and-leaf-flag ()
  (let ((car-d (nelisp-standalone--subr-desc "car"))
        (list-d (nelisp-standalone--subr-desc "list")))
    (should (= (logand car-d 255) (+ 1 17)))
    (should (= (logand (ash car-d -8) 255) 1))
    (should (= (logand (ash car-d -16) 255) 1))
    (should (= (ash car-d -24) 1))                    ; leaf
    (should (= (logand list-d 255) (+ 114 17)))
    (should (= (logand (ash list-d -16) 255) 127))    ; many
    (should (= (ash list-d -24) 0))                   ; allocating
    (should-not (nelisp-standalone--subr-desc "eq"))))

(ert-deftest w2s-id-of-a-subr-object-is-a-header-compare-and-one-load ()
  (ndc--with-definitions
   (lambda (_)
     (let ((mem (make-hash-table :test #'eql)) (loads 0) (names 0))
       (puthash 40000 (+ 20 (ash (nelisp-standalone--subr-desc "car") 8)) mem)
       (puthash 41000 (+ 20 (ash (nelisp-standalone--subr-desc "list") 8)) mem)
       (puthash 42000 (+ 21 (ash (nelisp-standalone--subr-desc "car") 8)) mem)
       (cl-letf (((symbol-function 'ptr-read-u64)
                  (lambda (p i) (cl-incf loads) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_sym_sid) (lambda (&rest _) (cl-incf names) 0))
                 ((symbol-function 'sexp-name-eq) (lambda (&rest _) (cl-incf names) 1)))
         ;; car: id 1, exactly one argument; the symbol spelling is irrelevant.
         (setq loads 0)
         (should (= (nl_directcall_id 40006 12345 1) 1))
         (should (= loads 1))
         (should (= names 0))
         (should (= (nl_directcall_id 40006 12345 0) 0))
         (should (= (nl_directcall_id 40006 12345 2) 0))
         ;; list: any count up to the 255 cap.
         (should (= (nl_directcall_id 41006 1 0) 114))
         (should (= (nl_directcall_id 41006 1 255) 114))
         (should (= (nl_directcall_id 41006 1 256) 0))
         ;; another object kind declines.
         (should (= (nl_directcall_id 42006 1 1) 0))
         ;; the descriptor is never read for a negative count.
         (should (= (nl_directcall_id 40006 1 -1) 0)))))))

(ert-deftest w2s-installer-wraps-exactly-the-directcall-primitives ()
  (let* ((forms (nelisp-standalone--reader-install-builtins-forms))
         (subr (cl-remove-if-not (lambda (f) (ndc--contains 'nl_install_subr f)) forms))
         (one (cl-remove-if-not (lambda (f) (ndc--contains 'nl_install_one f)) forms)))
    (should (= (+ (length subr) (length one)) (length forms)))
    (should (>= (length subr) 21))
    (dolist (f subr)
      (let ((spec (cl-find-if (lambda (sp) (w2s--has
                                            (car (nelisp-standalone--name-words (car sp))) f))
                              nelisp-standalone--directcall-specs)))
        (should spec)
        (should (w2s--has (nelisp-standalone--subr-desc (car spec)) f)))
      (should-not (ndc--contains 'nl_install_one f)))
    (should (ndc--find 'nl_install_subr (nelisp-standalone--directcall-defuns)))
    (should (ndc--find 'nelisp_env_lookup_function_raw_w (nelisp-standalone--directcall-defuns)))))

(ert-deftest w2s-walkers-trace-the-subr-child-like-a-cell ()
  ;; Mark, image mark, compaction rewrite and the image dump each give kind 20
  ;; the one-slot cell layout, so the wrapped list survives and is relocated.
  (let ((src (w2s--src)))
    (should (>= (w2s--count "(or (= kind 11) (= kind 20))" src) 3))
    (should (>= (w2s--count "(or (= tag 11) (= tag 20))" src) 1))
    (should (string-match-p "(if (= kind 11) 0 (if (= kind 20) 0 1))" src))))

(ert-deftest w2s-bytecode-call-and-walkers-resolve-the-raw-cell ()
  (let ((f (ndc--find 'wf_bytecode_call nelisp-standalone--applyfn-bytecode-helpers)))
    (should f)
    (should (ndc--contains 'nelisp_env_lookup_function_raw_w f))
    (should-not (ndc--contains 'nelisp_env_lookup_function_w f)))
  (let ((forms (nelisp-standalone--directcall-defuns)))
    (dolist (name '(nl_directcall_list_words nl_directcall_apply_words))
      (let ((f (ndc--find name forms)))
        (should f)
        (should (ndc--contains 'nelisp_env_lookup_function_raw_w f))
        (should-not (ndc--contains 'nelisp_env_lookup_function_w f))))))

(ert-deftest w2s-other-readers-get-the-legacy-list ()
  (dolist (file '("nelisp-cc-env-lookup-function.el" "nelisp-cc-fx-call-primitives.el"))
    (with-temp-buffer
      (insert-file-contents (locate-library file))
      (should (>= (w2s--count "(= (logand (heap-read-u8 (- w\\(?:0\\)? 6) 0) 63) 20)"
                              (buffer-string))
                  1)))))

(ert-deftest w2s-windows-forms-still-carry-no-data-blob ()
  (dolist (target '(windows-x86_64 macos-aarch64 linux-aarch64))
    (let* ((nelisp-standalone--target target)
           (forms (nelisp-standalone--directcall-defuns)))
      (should-not (cl-find-if (lambda (f) (eq (car f) 'data-blob)) forms))
      (should-not (ndc--contains 'data-addr (ndc--find 'nl_directcall_id forms)))
      (should (ndc--find 'nl_install_subr forms)))))


(ert-deftest w2s-interpreter-symbol-call-reads-the-wrapped-list ()
  ;; The interpreter's fast path reads the cell itself; a Subr must not send it
  ;; down the generic lookup (r5-sum-base-toplevel regression).
  (let ((src (w2s--src)))
    (should (string-match-p
             "(ptr-read-u64 (- c 6) 0) 63) 20)\n *(ptr-read-u64 (- c 6) 8) c))" src))))

(provide 'nelisp-subr-object-test)
