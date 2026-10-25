;;; nelisp-gc-safepoint-guard-test.el --- GC safepoint invariant guard (smap2)  -*- lexical-binding: t; -*-

;; The map-only collector scan (stage 2 of the liveness stack maps) is sound only
;; while collections happen at known safepoints, i.e. at call sites that have a stack
;; map.  This is the static layer of the guard of the design (target/smap2 DESIGN 1.1):
;; (a) the DSL functions that call a collector entry are exactly the allow-list below;
;; (b) the allocators never call a collector entry (allocation only subtracts from the
;; budget; the NEXT poll collects).  A new collector entry or an allocator that polls
;; fails this test and must be reviewed against the stack-map assumptions.

(require 'ert)
(require 'cl-lib)

(defconst gcsg--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name default-directory)))))

(defconst gcsg--entries '("nl_gc_collect" "nl_gc_collect_from_recorded_roots" "nl_gc_vm_poll"))

(defconst gcsg--allow
  '(("nl_gc_collect" "nl_gc_collect_form_boundary")
    ("nl_gc_collect_from_recorded_roots"
     "bf_garbage_collect_w" "nl_conserv_selftest_hold" "nl_float_selftest_hold"
     "nl_gc_midform_collect_now" "nl_gc_stats_run" "nl_symcells_selftest_survive")
    ("nl_gc_vm_poll" "wf_bytecode_poll"))
  "Allowed direct callers of each collector entry in the generator's DSL source.")

(defconst gcsg--allocators '("nl_alloc_bytes" "nl_alloc_bytes_uncheck" "nl_os_alloc_chunk" "nl_gc_free_block"))

(defun gcsg--defuns (text)
  "Return an alist (NAME . BODY-WITHOUT-COMMENTS) for every `(defun NAME' in TEXT."
  (let ((starts nil) (pos 0))
    (while (string-match "^[ \t]*(defun \\([^ \t\n()]+\\)" text pos)
      (push (cons (match-string 1 text) (match-beginning 0)) starts)
      (setq pos (match-end 0)))
    (setq starts (nreverse starts))
    (cl-loop for (cell . rest) on starts
             collect (cons (car cell)
                           (replace-regexp-in-string
                            ";[^\n]*" ""
                            (substring text (cdr cell) (if rest (cdr (car rest)) (length text))))))))

(defun gcsg--calls-p (body name)
  (string-match-p (concat "(" (regexp-quote name) "[ \t\n)]") body))

(defun gcsg--callers (defuns entry)
  (sort (delete-dups
         (cl-loop for (n . b) in defuns
                  when (and (not (equal n entry)) (gcsg--calls-p b entry)) collect n))
        #'string<))

(defun gcsg--source (&optional text)
  (or text (with-temp-buffer
             (insert-file-contents (expand-file-name "scripts/nelisp-standalone-build.el" gcsg--root))
             (buffer-string))))

(defun gcsg--violations (text)
  "Return a list of human-readable guard violations for the generator TEXT."
  (let ((defuns (gcsg--defuns text)) (bad nil))
    (dolist (e gcsg--entries)
      (let ((got (gcsg--callers defuns e))
            (want (sort (copy-sequence (cdr (assoc e gcsg--allow))) #'string<)))
        (unless (equal got want)
          (push (format "callers of %s: got %S, allowed %S" e got want) bad))))
    (dolist (a gcsg--allocators)
      (dolist (d defuns)
        (when (equal (car d) a)
          (dolist (e gcsg--entries)
            (when (gcsg--calls-p (cdr d) e)
              (push (format "allocator %s calls collector entry %s" a e) bad))))))
    (nreverse bad)))

(ert-deftest gcsg-collector-entries-have-only-allowed-callers ()
  (should (null (gcsg--violations (gcsg--source)))))

(ert-deftest gcsg-guard-sees-every-defined-entry-and-allocator ()
  (let ((names (mapcar #'car (gcsg--defuns (gcsg--source)))))
    (dolist (n (append gcsg--entries gcsg--allocators))
      (should (member n names)))))

(ert-deftest gcsg-negative-new-collector-caller-is-detected ()
  ;; Control: a source with an extra caller of a collector entry, and one with a polling
  ;; allocator, must each produce a violation (the guard can fail).
  (let* ((text (gcsg--source))
         (extra (concat text "\n    (defun nl_smap2_guard_probe (x) (nl_gc_vm_poll))\n"))
         (poll (replace-regexp-in-string
                "(defun nl_alloc_bytes (\\([^)]*\\))"
                "(defun nl_alloc_bytes (\\1) (nl_gc_collect_from_recorded_roots 0)" text)))
    (should (cl-some (lambda (v) (string-match-p "nl_smap2_guard_probe" v)) (gcsg--violations extra)))
    (should (not (equal poll text)))
    (should (cl-some (lambda (v) (string-match-p "allocator nl_alloc_bytes calls" v)) (gcsg--violations poll)))))

(provide 'nelisp-gc-safepoint-guard-test)
;;; nelisp-gc-safepoint-guard-test.el ends here
