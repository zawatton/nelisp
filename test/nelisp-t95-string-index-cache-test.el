;;; nelisp-t95-string-index-cache-test.el --- T95 cursor safety -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)

(let* ((test-file (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory test-file)))))
  (add-to-list 'load-path (expand-file-name "scripts" root))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "src" root)))

(require 'nelisp-standalone-build)

(defun nelisp-t95-test--defun (name forms)
  (cl-find-if (lambda (form)
                (and (consp form) (eq (car form) 'defun)
                     (eq (cadr form) name)))
              (if (eq (car-safe forms) 'seq) (cdr forms) forms)))

(defun nelisp-t95-test--member (needle tree)
  (cond
   ((equal needle tree) t)
   ((consp tree)
    (or (nelisp-t95-test--member needle (car tree))
        (nelisp-t95-test--member needle (cdr tree))))))

(ert-deftest nelisp-t95-string-index-cache-separates-element-and-endpoint-bounds ()
  (let ((aref-off
         (nelisp-t95-test--defun
          'nl_str_aref_byte_off nelisp-standalone--applyfn-m5-helpers))
        (sub-off
         (nelisp-t95-test--defun
          'nl_str_sub_byte_off nelisp-standalone--applyfn-m5-helpers)))
    (should aref-off)
    (should sub-off)
    (should (nelisp-t95-test--member
             '(if (= target c-char) (if (< c-byte nb) c-byte -1)
                (if (> target c-char)
                    (nl_str_aref_commit bufptr nb target
                      (nl_u8_cidx_byte_bounded sx c-byte nb c-char target))
                  (if (<= (- c-char target) target)
                      (nl_str_aref_commit bufptr nb target
                        (nl_u8_walk_back sx c-byte (- c-char target)))
                    (nl_str_aref_commit bufptr nb target
                      (nl_u8_cidx_byte_bounded sx 0 nb 0 target)))))
             aref-off))
    (should (nelisp-t95-test--member
             '(if (= target c-char) c-byte
                (if (> target c-char)
                    (nl_str_aref_commit bufptr nb target
                      (nl_u8_cidx_byte_bounded_incl
                       sx c-byte nb c-char target))
                  (if (<= (- c-char target) target)
                      (nl_str_aref_commit bufptr nb target
                        (nl_u8_walk_back sx c-byte (- c-char target)))
                    (nl_str_aref_commit bufptr nb target
                      (nl_u8_cidx_byte_bounded_incl sx 0 nb 0 target)))))
             sub-off))))

(ert-deftest nelisp-t95-string-index-cache-bypasses-shared-rows-in-parallel ()
  (dolist (name '(nl_aref_cache_lookup nl_aref_cache_store))
    (let ((form (nelisp-t95-test--defun
                 name nelisp-standalone--applyfn-m5-helpers)))
      (should form)
      (should
       (nelisp-t95-test--member
        '(ptr-read-u64 (data-addr nl_thread_parallel_ctx) 16)
        form)))))

(ert-deftest nelisp-t95-string-index-cache-bypasses-unpackable-byte-offsets ()
  "Neither side of the shared row may pack offsets from a >2GiB string."
  (dolist (name '(nl_aref_cache_lookup nl_aref_cache_store))
    (let ((form (nelisp-t95-test--defun
                 name nelisp-standalone--applyfn-m5-helpers)))
      (should form)
      (should (nelisp-t95-test--member '(> nb 2147483647) form)))))

(ert-deftest nelisp-t95-string-index-cache-clears-at-every-reuse-boundary ()
  (dolist (spec `((nl_gc_collect_recorded_mark_sweep
                   ,nelisp-standalone--gc-source)
                  (nl_gc_collect_parked_mark_sweep
                   ,nelisp-standalone--gc-source)
                  (nl_gc_collect ,nelisp-standalone--gc-source)
                  (nl_boundary_reclaim
                   ,nelisp-standalone--reader-boundary-source)))
    (let ((form (nelisp-t95-test--defun (car spec) (cadr spec))))
      (should form)
      (should (nelisp-t95-test--member '(nl_aref_cache_clear) form)))))

(provide 'nelisp-t95-string-index-cache-test)

;;; nelisp-t95-string-index-cache-test.el ends here
