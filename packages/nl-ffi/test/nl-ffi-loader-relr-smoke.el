;;; nl-ffi-loader-relr-smoke.el --- bounded ELF64 RELR smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(load "packages/nl-ffi/src/nl-ffi.el")
(load "packages/nl-ffi/src/nl-ffi-loader.el" nil nil t)

(defun nl-ffi-relr-test-assert (condition label)
  (unless condition (error "RELR assertion failed: %s" label)))

(defun nl-ffi-relr-test-u64 (ptr offset value)
  (ptr-write-u32 ptr offset (logand value #xffffffff))
  (ptr-write-u32 ptr (+ offset 4) (logand (ash value -32) #xffffffff)))

(defmacro nl-ffi-relr-test-error (form reason)
  `(let ((err (condition-case caught (progn ,form nil) (error caught))))
     (nl-ffi-relr-test-assert err (format "expected error %S" ,reason))
     (nl-ffi-relr-test-assert
      (and (eq (car err) 'nl-ffi-loader-unsupported)
           (eq (car (cdr err)) ,reason))
      (format "expected reason %S, got %S" ,reason err))
     err))

(defun nl-ffi-relr-test-dynamic (tags size)
  (let ((buf (alloc-bytes 128 8)) (i 0))
    (dolist (pair tags)
      (nl-ffi-relr-test-u64 buf (* i 16) (car pair))
      (nl-ffi-relr-test-u64 buf (+ (* i 16) 8) (cdr pair))
      (setq i (1+ i)))
    (nl-ffi-relr-test-u64 buf (* i 16) 0)
    (nl-ffi-relr-test-u64 buf (+ (* i 16) 8) 0)
    (nl-ffi-loader--parse-dynamic 0 buf size)))

(defun nl-ffi-relr-test-parser-and-decode ()
  (let ((dyn (nl-ffi-relr-test-dynamic
              '((36 . 4660) (35 . 0) (37 . 8)) 64)))
    (nl-ffi-relr-test-assert (= (plist-get dyn :relr) 4660) "DT_RELR address")
    (nl-ffi-relr-test-assert (= (plist-get dyn :relrsz) 0) "empty RELR table")
    (nl-ffi-relr-test-assert (= (plist-get dyn :relrent) 8) "DT_RELRENT"))
  (nl-ffi-relr-test-error
   (nl-ffi-relr-test-dynamic '((36 . 4096)) 32) :relr-tag-set)
  (nl-ffi-relr-test-error
   (nl-ffi-relr-test-dynamic '((36 . 4096) (35 . 8) (37 . 4)) 64)
   :relr-entry-size)
  (nl-ffi-relr-test-error
   (nl-ffi-relr-test-dynamic '((36 . 4096) (35 . 1) (37 . 8)) 64)
   :relr-size)
  (nl-ffi-relr-test-error
   (nl-ffi-relr-test-dynamic '((36 . 4096)) 16) :dynamic-missing-null)
  (nl-ffi-relr-test-error
   (nl-ffi-relr-test-dynamic '((#x6fffe035 . 4096)) 32) :legacy-relr-tags)
  (let* ((arena (alloc-bytes 4096 4096))
         (bias (- arena 4096))
         (table-vaddr #x1100)
         (table (+ bias table-vaddr))
         (loads (list (list #x1000 0 4096 4096 6)))
         (dyn (list :relr table :relrsz 48 :relrent 8))
         (words '(#x1202 #x80000003 #x8000000000000001 1 3 #x1000))
         (i 0))
    (dolist (word words)
      (nl-ffi-relr-test-u64 table (* i 8) word)
      (setq i (1+ i)))
    (let ((targets (nl-ffi-loader--relr-targets "unit.so" bias dyn loads)))
      (nl-ffi-relr-test-assert
       (equal (mapcar #'car targets)
              '(#x1202 #x120a #x12fa #x15f2 #x17f2 #x1000))
       (format "direct, bit 63, consecutive/empty bitmap, unaligned and backward direct: %S"
               (mapcar #'car targets))))
    ;; Force a low-half carry while relocating an even, non-8-byte-aligned target.
    (let* ((ptr (+ bias #x1202))
           (addend (+ (ash 7 32) #xffffffff))
           (want (+ bias addend)))
      (nl-ffi-relr-test-u64 ptr 0 addend)
      (let ((before-lo (ptr-read-u32 ptr 0))
            (before-hi (ptr-read-u32 ptr 4)))
      (nl-ffi-loader--apply-relr "unit.so" bias
                                 (list :relr table :relrsz 48 :relrent 8) loads)
      (nl-ffi-relr-test-assert
       (and (= (ptr-read-u32 ptr 0) (logand want #xffffffff))
            (= (ptr-read-u32 ptr 4) (logand (ash want -32) #xffffffff)))
       (format "u64 addend plus bias with carry bias=%x ptr=%x before=%x:%x got=%x:%x want=%x:%x"
               bias ptr before-hi before-lo
               (ptr-read-u32 ptr 4) (ptr-read-u32 ptr 0)
               (logand (ash want -32) #xffffffff)
               (logand want #xffffffff)))))
    ;; A later invalid target is detected before the earlier valid target is written.
    (nl-ffi-relr-test-u64 table 0 #x1200)
    (nl-ffi-relr-test-u64 table 8 #x1ffe)
    (let ((good (+ bias #x1200)))
      (nl-ffi-relr-test-u64 good 0 123)
      (nl-ffi-relr-test-u64 good 4 456)
      (nl-ffi-relr-test-error
       (nl-ffi-loader--apply-relr "unit.so" bias
                                  (list :relr table :relrsz 16 :relrent 8) loads)
       :relr-target-outside-writable-load)
      (nl-ffi-relr-test-assert
       (and (= (ptr-read-u32 good 0) 123) (= (ptr-read-u32 good 4) 456))
       "preflight leaves earlier target untouched"))
    ;; The range check rejects a table crossing the PT_LOAD end before reading it.
    (nl-ffi-relr-test-error
     (nl-ffi-loader--relr-targets
      "unit.so" bias (list :relr (+ bias #x1ff8) :relrsz 16 :relrent 8) loads)
     :relr-table-outside-load)
    ;; A relocation target cannot alias bytes that are still being decoded.
    (nl-ffi-relr-test-u64 table 0 table-vaddr)
    (nl-ffi-relr-test-error
     (nl-ffi-loader--relr-targets
      "unit.so" bias (list :relr table :relrsz 8 :relrent 8) loads)
     :relr-target-overlaps-table)
    ;; PF_R-only loads are not legal RELR destinations.
    (nl-ffi-relr-test-u64 table 0 #x1200)
    (nl-ffi-relr-test-error
     (nl-ffi-loader--relr-targets
      "unit.so" bias (list :relr table :relrsz 8 :relrent 8)
      (list (list #x1000 0 4096 4096 4)))
     :relr-target-outside-writable-load)))

(defun nl-ffi-relr-test-failure-cleanup (bad-fixture)
  (let ((initializer-calls 0)
        (cleanup-calls 0)
        (run-initializers (symbol-function 'nl-ffi-loader--run-initializers-for-node))
        (unmap-reservations (symbol-function 'nl-ffi-loader--unmap-reservations)))
    (unwind-protect
        (progn
          (fset 'nl-ffi-loader--run-initializers-for-node
                (lambda (node)
                  (setq initializer-calls (1+ initializer-calls))
                  (funcall run-initializers node)))
          (fset 'nl-ffi-loader--unmap-reservations
                (lambda ()
                  (setq cleanup-calls (1+ cleanup-calls))
                  (funcall unmap-reservations)))
          (nl-ffi-relr-test-error (nl-ffi-loader-open bad-fixture)
                                  :relr-address-overflow)
          (nl-ffi-relr-test-assert (= initializer-calls 0)
                                   "malformed RELR prevents initializers")
          (nl-ffi-relr-test-assert (= cleanup-calls 1)
                                   "malformed RELR unmaps reservations"))
      (fset 'nl-ffi-loader--run-initializers-for-node run-initializers)
      (fset 'nl-ffi-loader--unmap-reservations unmap-reservations))))

(defun nl-ffi-relr-test-main ()
  (let ((mode (getenv "ELF_RELR_EXPECT"))
        (fixture (getenv "ELF_RELR_FIXTURE"))
        (bad-fixture (getenv "ELF_RELR_BAD_FIXTURE")))
    (unless (and fixture bad-fixture)
      (error "ELF_RELR_FIXTURE and ELF_RELR_BAD_FIXTURE are required"))
    (if (equal mode "baseline")
        (let ((err (condition-case caught
                       (progn (nl-ffi-loader-open fixture) nil)
                     (error caught))))
          (nl-ffi-relr-test-assert err "baseline refuses RELR fixture")
          (nl-ffi-relr-test-assert
           (eq (car (cdr err)) :relr-relocations)
           (format "baseline reason is RELR refusal: %S" err)))
      (nl-ffi-relr-test-parser-and-decode)
      (nl-ffi-relr-test-failure-cleanup bad-fixture)
      (let* ((handle (nl-ffi-loader-open fixture))
             (probe (nl-ffi-loader-symbol handle
                                          "nl_ffi_loader_fixture_relr_probe")))
        (nl-ffi-relr-test-assert (integerp probe) "fixture probe symbol resolves")
        (nl-ffi-relr-test-assert (= (ptr-call probe 0 0 0 0 0 0) 1)
                                 "RELR pointer array and initializer")))
    (princ (format "PASS nl-ffi RELR mode=%s\n" mode))))

(condition-case err
    (nl-ffi-relr-test-main)
  (error
   (princ (format "FAIL nl-ffi RELR: %S\n" err))
   (kill-emacs 1)))
