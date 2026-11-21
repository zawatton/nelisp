;;; nelisp-restore-alloc-test.el --- runtime slots must not alias chunk-0 data  -*- lexical-binding: t; -*-

;;; Commentary:

;; Chunk-0 object data starts at arena base + 1024.  A control-page literal
;; of 268436480 (base + 1024) or more therefore aliases the first heap blocks.
;; The hash-table name cache (base + 1072 / + 1080) and the TTY slots used to
;; live there: after a `--cold-load-from' restore the first `make-hash-table'
;; wrote a pointer over a block header, every header walk (`nl_boot_build',
;; `nl_gc_index_prepare') then failed, no collection ever completed, and the
;; mid-form collector retried on every loop poll (300k `make-vector' 64 took
;; 26 s instead of 0.26 s).  These tests read the build script with the real
;; reader and check the slots now live in the `nl_ctl_slots' BSS block.
;; NELISP_RESTORE_ALLOC_BUILD overrides the build script path.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-restore-alloc-test--lo 268436480
  "First control-page literal (base + 1024) that aliases chunk-0 data.")
(defconst nelisp-restore-alloc-test--hi 268439552
  "End of the literal window checked (base + 4096).")

(defconst nelisp-restore-alloc-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defun nelisp-restore-alloc-test--file ()
  (or (getenv "NELISP_RESTORE_ALLOC_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-restore-alloc-test--dir)))

(defvar nelisp-restore-alloc-test--forms nil)

(defun nelisp-restore-alloc-test--all-forms ()
  (or nelisp-restore-alloc-test--forms
      (setq nelisp-restore-alloc-test--forms
            (let ((out nil))
              (with-temp-buffer
                (insert-file-contents (nelisp-restore-alloc-test--file))
                (goto-char (point-min))
                (condition-case nil
                    (while t (push (read (current-buffer)) out))
                  (end-of-file nil)))
              (nreverse out)))))

(defun nelisp-restore-alloc-test--find (pred)
  "Return every cons in the build script whose car/cdr structure satisfies PRED."
  (let ((hits nil))
    (cl-labels ((walk (form)
                  (when (consp form)
                    (when (and (proper-list-p form) (funcall pred form))
                      (push form hits))
                    (while (consp form)
                      (walk (car form))
                      (setq form (cdr form))))))
      (mapc #'walk (nelisp-restore-alloc-test--all-forms)))
    (nreverse hits)))

(defun nelisp-restore-alloc-test--defun (name)
  (car (nelisp-restore-alloc-test--find
        (lambda (f) (and (eq (car f) 'defun) (eq (cadr f) name))))))

(defun nelisp-restore-alloc-test--atoms (form)
  (cond ((consp form) (append (nelisp-restore-alloc-test--atoms (car form))
                              (nelisp-restore-alloc-test--atoms (cdr form))))
        (form (list form))))

(defun nelisp-restore-alloc-test--aliasing-literals (name)
  (let ((d (nelisp-restore-alloc-test--defun name)))
    (should d)
    (cl-remove-if-not
     (lambda (a) (and (integerp a)
                      (<= nelisp-restore-alloc-test--lo a)
                      (< a nelisp-restore-alloc-test--hi)))
     (nelisp-restore-alloc-test--atoms d))))

(ert-deftest nelisp-restore-alloc-name-cache-not-in-chunk0 ()
  (dolist (name '(nl_ht_marker_sym_w nl_ht_eql_sym_w nl_ht_sym_cache_ok
                  nl_ht_sym_cache_note))
    (should (null (nelisp-restore-alloc-test--aliasing-literals name)))))

(ert-deftest nelisp-restore-alloc-name-cache-slot-in-bss ()
  (let ((d (nelisp-restore-alloc-test--defun 'nl_ht_name_cache_slot)))
    (should d)
    (should (cl-some (lambda (f) (equal f '(data-addr nl_ctl_slots)))
                     (nelisp-restore-alloc-test--find
                      (lambda (f) (equal f '(data-addr nl_ctl_slots))))))
    (should (memq 'nl_ctl_slots (nelisp-restore-alloc-test--atoms d)))
    (dolist (name '(nl_ht_marker_sym_w nl_ht_eql_sym_w))
      (should (memq 'nl_ht_name_cache_slot
                    (nelisp-restore-alloc-test--atoms
                     (nelisp-restore-alloc-test--defun name)))))))

(ert-deftest nelisp-restore-alloc-tty-slots-not-in-chunk0 ()
  (dolist (name '(nl_tty_slot_fd nl_tty_slot_saved_termios
                  nl_tty_slot_scratch_termios nl_tty_slot_pollfd
                  nl_tty_slot_byte nl_tty_slot_dev_tty_path))
    (should (null (nelisp-restore-alloc-test--aliasing-literals name)))))

(ert-deftest nelisp-restore-alloc-ctl-slots-linked-and-sized ()
  (let* ((link (car (nelisp-restore-alloc-test--find
                     (lambda (f) (and (eq (car f) 'nelisp-link-symbol)
                                      (equal (cadr f) "nl_ctl_slots"))))))
         (region (car (nelisp-restore-alloc-test--find
                       (lambda (f) (and (eq (car f) 'nelisp-link-symbol)
                                        (equal (cadr f) "nl_region_table"))))))
         (size (nelisp-restore-alloc-test--defun
                'nelisp-standalone--driver-bss-size)))
    (should link)
    (should region)
    ;; Placed right after nl_region_table (80 -> 2072 bytes of that table).
    (should (equal (caddr link) (append (caddr region) '(2072))))
    ;; The BSS size grows by the 64-byte block, ahead of nl_cold_chunk0_domain.
    ;; Doc 238 A0: between the 64 bytes and the 16 sits the census table's
    ;; (if (nelisp-standalone--census-p) 512 0), zero unless NELISP_REPR_CENSUS=1.
    (should (equal (cl-subseq (memq 2072 (nelisp-restore-alloc-test--atoms size)) 0 9)
                   '(2072 64 if nelisp-standalone--census-p 512 0 8 16 8)))))

(provide 'nelisp-restore-alloc-test)
;;; nelisp-restore-alloc-test.el ends here
