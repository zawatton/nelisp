;;; nelisp-fx-ordinary-vector-test.el --- Ordinary-vector FX controls -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'nelisp-cc-fx-call-primitives)

(defconst nelisp-fx-vector--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-fx-vector--hash (file)
  (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally file)
                    (secure-hash 'sha256 (current-buffer))))

;; The old and new sources are read from git at pinned commits (content hash
;; verified), so the test is reproducible in any clone that has the history.
(defconst nelisp-fx-vector--seed-commit "a14b1a2e4dc81b975135e73c8763821cb3484354"
  "Commit holding the pre-vector-scope source (the fx-vector lane base).")
(defconst nelisp-fx-vector--seed-sha256
  "13c6c67527a98e4e9401049c2a5861db793499b33ab682d889812ce293cf68a6"
  "SHA-256 of lisp/nelisp-cc-fx-call-primitives.el at the seed commit.")
(defconst nelisp-fx-vector--landed-commit "bfc3e644270911ab2384837895429add39c274a1"
  "Commit that integrated the vector scope (aref/aset/length kinds).")
(defconst nelisp-fx-vector--landed-sha256 "2f76d6b46f0f4051dbf722d9b8e8e55a0ffff0258ea0f53b2b3d5f4905412056"
  "SHA-256 of lisp/nelisp-cc-fx-call-primitives.el at the landed commit.")
(defconst nelisp-fx-vector--later-changes '(m5_fx_cached_function)
  "Definitions legitimately changed after the vector scope landed (v12 w2subr:
a Subr object answers its legacy list); every other definition must still equal
the landed one.")

(defun nelisp-fx-vector--source-at (commit sha256)
  "Read the production source at COMMIT from git, checking its SHA-256."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((default-directory nelisp-fx-vector--root))
      (unless (eq 0 (call-process "git" nil t nil "show"
                                  (concat commit ":lisp/nelisp-cc-fx-call-primitives.el")))
        (error "Commit %s is not available in this clone" commit)))
    (unless (equal (secure-hash 'sha256 (current-buffer)) sha256)
      (error "Source at %s differs from the pinned hash" commit))
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (let (form answer)
      (condition-case nil
          (while t (setq form (read (current-buffer)))
                 (when (eq (car-safe form) 'defconst) (setq answer (cadr (nth 2 form)))))
        (end-of-file nil))
      answer)))

(defun nelisp-fx-vector--seed ()
  (nelisp-fx-vector--source-at nelisp-fx-vector--seed-commit nelisp-fx-vector--seed-sha256))

(defun nelisp-fx-vector--landed ()
  (nelisp-fx-vector--source-at nelisp-fx-vector--landed-commit nelisp-fx-vector--landed-sha256))

(defun nelisp-fx-vector--def (source name)
  (or (cl-find name (cdr source) :key #'cadr) (error "Missing production definition %s" name)))

(defun nelisp-fx-vector--nodes (form)
  (cond ((vectorp form) (+ 1 (cl-reduce #'+ (mapcar #'nelisp-fx-vector--nodes (append form nil)) :initial-value 0)))
        ((consp form) (+ (nelisp-fx-vector--nodes (car form)) (nelisp-fx-vector--nodes (cdr form))))
        (t 0)))

(ert-deftest nelisp-fx-vector-scope-and-old-kinds ()
  (let* ((seed (nelisp-fx-vector--seed)) (landed (nelisp-fx-vector--landed))
         (new nelisp-cc-fx-call-primitives--source)
         (names '(m5_fx_primitive_id m5_fx_primitive_run)))
    (should (= (length seed) (length landed)))
    (should (= (length seed) (length new)))
    ;; Scope at landing: only the two named definitions differ from the base.
    (dolist (form (cdr seed))
      (unless (memq (cadr form) names)
        (should (equal form (nelisp-fx-vector--def landed (cadr form))))))
    ;; Scope now: nothing but the named and the documented later changes moved.
    (dolist (form (cdr landed))
      (unless (or (memq (cadr form) names) (memq (cadr form) nelisp-fx-vector--later-changes))
        (should (equal form (nelisp-fx-vector--def new (cadr form))))))
    (cl-labels ((lower (x) (cond ((atom x) x)
                                ((eq (car x) 'sexp-name-eq) `(if (equal ,(cadr x) ,(nth 2 x)) 1 0))
                                ((memq (car x) '(= >)) `(if (,(car x) ,@(mapcar #'lower (cdr x))) 1 0))
                                ((eq (car x) 'if) `(if (/= ,(lower (cadr x)) 0) ,(lower (nth 2 x)) ,(lower (nth 3 x))))
                                ((eq (car x) 'or) `(if (or ,@(mapcar (lambda (a) `(/= ,(lower a) 0)) (cdr x))) 1 0))
                                ((eq (car x) 'cond) `(cond ,@(mapcar (lambda (a) (cons (if (eq (car a) t) t `(/= ,(lower (car a)) 0)) (mapcar #'lower (cdr a)))) (cdr x))))
                                (t (mapcar #'lower x)))))
      (let ((old-id (eval `(lambda (name argc) ,(lower (nth 3 (nelisp-fx-vector--def seed 'm5_fx_primitive_id)))) t))
            (new-id (eval `(lambda (name argc) ,(lower (nth 3 (nelisp-fx-vector--def new 'm5_fx_primitive_id)))) t)))
        (dolist (name '("car" "cdr" "null" "not" "consp" "setcdr" "setcar" "eq" "<" "string<" "nth" "nthcdr" "funcall" "symbol-value" "set" "unknown"))
          (dotimes (argc 8) (should (= (funcall old-id name argc) (funcall new-id name argc)))))
      (dolist (entry '(("aref" 2 -14) ("aset" 3 -15) ("length" 1 -16)))
        (dotimes (argc 8)
          (let ((body (lower (nth 3 (nelisp-fx-vector--def new 'm5_fx_primitive_id)))))
            (should (= (funcall (eval `(lambda (name argc) ,body) t) (car entry) argc)
                       (if (= argc (cadr entry)) (nth 2 entry) 0))))))))))

(ert-deftest nelisp-fx-vector-signed-cache-namespace ()
  (dolist (id '(-14 -15 -16))
    (should (= id (ash (1+ (ash id 2)) -2)))
    (should-not (memq id '(0 13 21 22 -3 -1))))
  (let* ((run (nelisp-fx-vector--def nelisp-cc-fx-call-primitives--source 'm5_fx_primitive_run))
         (branch (nth 2 (nth 3 run)))
         (text (prin1-to-string branch)))
    (should (string-match-p "(/= n 7)" text))
    (should (string-match-p "(logand a 7) 6" text))
    (should (string-match-p "(logand (ptr-read-u64 (- a 6) 0) 63) 8" text))
    ;; corecall: the record arm (header kind 12) adds the second epoch bump; the
    ;; vector arm keeps its own single `wf_dirty' and `vector-slot-set'.
    (should (string-match-p "(logand (ptr-read-u64 (- a 6) 0) 63) 12" text))
    (should (= 2 (cl-count 'wf_dirty (flatten-tree branch))))
    (should (= 1 (cl-count 'vector-slot-set (flatten-tree branch))))))

(defun nelisp-fx-vector--extract (file names)
  "Read exact quoted production definitions, without rewriting their bodies."
  (let (found)
    (cl-labels ((walk (x)
                  (when (consp x)
                    (if (and (eq (car x) 'defun) (memq (cadr x) names))
                        (push x found)
                      (walk (car x)) (walk (cdr x))))))
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case nil (while t (walk (read (current-buffer)))) (end-of-file nil))))
    (dolist (name names)
      (unless (= 1 (cl-count name found :key #'cadr)) (error "Nonunique/missing %s" name)))
    (nreverse found)))

(ert-deftest nelisp-fx-vector-actual-consumer-guards ()
  (require 'nelisp-standalone-build)
  (let* ((defs (nelisp-fx-vector--extract
                (expand-file-name "scripts/nelisp-standalone-build.el" nelisp-fx-vector--root)
                '(m5_fx_callout m5_fx_callout_kind m5_fx_site_kind)))
         (site (nelisp-fx-vector--def (cons 'seq defs) 'm5_fx_site_kind))
         (eic (nelisp-fx-vector--def (cons 'seq (nelisp-standalone--eval-call-defuns)) 'nl_eic_primitive_w))
         (callout (nelisp-fx-vector--def (cons 'seq defs) 'm5_fx_callout)))
    (cl-labels ((has (tree needle)
                  (or (equal tree needle) (and (consp tree) (or (has (car tree) needle) (has (cdr tree) needle))))))
      (should (has site '(sar (nl_vector_slot_word cache 2) 2)))
      (should (has site '(+ (shl kind 2) 1)))
      (should (has site '(nl_sb_worker_p env)))
      (should (has callout '(> kind 20)))
      (should (has callout '(= kind 0)))
      (should (has callout '(= kind 13)))
      (should (has eic '(= id 0)))
      (should (has eic '(= id 13)))
      ;; This active EIC version uses the separate positive descriptor table;
      ;; the older quoted five-arg version is removed during assembly.
      (should (has eic '(nl_subr_prim desc argc)))
      (should-not (memq 'm5_fx_primitive_id (flatten-tree eic))))))

(defun nelisp-fx-vector-emit ()
  "Emit a small real production unit; unresolved dependencies are trapped natively."
  (require 'nelisp-standalone-build)
  (let* ((root nelisp-fx-vector--root)
         (out (expand-file-name "target/fx-vector/" root))
         (runtime "scripts/nelisp-standalone-build.el")
         (kernels (nelisp-fx-vector--extract
                   (expand-file-name runtime root)
                   '(nl_val_tag nl_word_tag64 nl_word_payload nl_word_data nl_word_length nl_word_canon
                     nl_vector_slot_word nl_record_slot_word nl_vector_length nl_cons_car_word nl_cons_cdr_word
                     wf_dirty m5_fx_word_cons_p m5_fx_callout_kind m5_fx_site_kind nl_sb_worker_p)))
         (stores (append
                  (nelisp-fx-vector--extract (expand-file-name "lisp/nelisp-cc-nlvector-set-slot.el" root) '(nl_vector_set_slot))
                  (nelisp-fx-vector--extract (expand-file-name "lisp/nelisp-cc-val-load.el" root) '(nl_val_clone_into nl_vl_prog2))))
         (files '("scripts/nelisp-standalone-build.el" "lisp/nelisp-aot-compiler.el"
                  "lisp/nelisp-heap-accessors.el" "lisp/nelisp-cc-val-load.el"
                  "lisp/nelisp-cc-nlvector-set-slot.el" "lisp/nelisp-cc-fx-call-primitives.el"
                  "test/nelisp-fx-ordinary-vector-test.el" "test/nelisp-fx-ordinary-vector-native.py"))
         (pins (mapcar (lambda (f) (cons f (nelisp-fx-vector--hash (expand-file-name f root)))) files))
         reports)
    (make-directory out t)
    (dolist (variant '(seed candidate unsigned-cache))
      (let* ((fx (if (eq variant 'seed) (nelisp-fx-vector--seed) nelisp-cc-fx-call-primitives--source))
             (extra (copy-tree kernels))
             frames
             (original (symbol-function 'nelisp-aot-compiler--emit-defun)))
        (when (eq variant 'unsigned-cache)
          (let ((site (cl-find 'm5_fx_site_kind extra :key #'cadr)))
            (cl-labels ((mutate (x)
                          (when (consp x)
                            (if (equal x '(sar (nl_vector_slot_word cache 2) 2)) (setcar x 'shr)
                              (mapc #'mutate x)))))
              (mutate site))))
        (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-defun)
                   (lambda (ir buf)
                     (push `((name . ,(symbol-name (nelisp-aot-compiler--ir-get ir :name)))
                             (rt_slots . ,(nelisp-aot-compiler--ir-get ir :rt-slot-count))
                             (nodes . ,(nelisp-fx-vector--nodes ir))) frames)
                     (funcall original ir buf))))
          (let* ((source `(seq ,@(cl-remove-if-not
                                  (lambda (d) (memq (cadr d) '(m5_fx_primitive_id m5_fx_primitive_run m5_fx_builtin_name m5_fx_primitive_named)))
                                  (cdr fx)) ,@extra ,@stores))
                 (nelisp-standalone--inline-import-sources nil)
                 (unit (nelisp-standalone--compile-to-unit "fx-vector-probe.o" source 'sysv))
                 (text (cdr (assq 'text (plist-get unit :sections))))
                 (bin (format "%s.bin" variant)))
            (with-temp-buffer (set-buffer-multibyte nil) (insert (apply #'unibyte-string (append text nil)))
                              (let ((coding-system-for-write 'no-conversion)) (write-region (point-min) (point-max) (expand-file-name bin out) nil 'silent)))
            (push `((variant . ,variant) (binary . ,bin) (sha256 . ,(nelisp-fx-vector--hash (expand-file-name bin out)))
                    (frames . ,(vconcat (nreverse frames)))
                    (symbols . ,(vconcat (mapcar (lambda (s) `((name . ,(plist-get s :name)) (offset . ,(plist-get s :value)))) (plist-get unit :symbols))))
                    (relocs . ,(vconcat (mapcar (lambda (r) `((offset . ,(plist-get r :offset)) (type . ,(plist-get r :type))
                                                             (symbol . ,(plist-get r :symbol)) (addend . ,(plist-get r :addend)))) (plist-get unit :relocs))))) reports)))))
    (dolist (pin pins) (unless (equal (cdr pin) (nelisp-fx-vector--hash (expand-file-name (car pin) root))) (error "Source drift %s" (car pin))))
    (with-temp-file (expand-file-name "emission.json" out)
      (insert (json-encode `((status . "EMITTED_ONLY") (files . ,pins) (units . ,(vconcat (nreverse reports)))))))))

(provide 'nelisp-fx-ordinary-vector-test)
