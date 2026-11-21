;;; standalone-windows-chunk-commit-check.el -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Host-GNU behavioural fixture for the Windows on-demand page commit of the
;; standalone arena.  It evaluates the EXACT production generated DSL
;; definitions -- `nl_windows_chunk_commit_to' and `nl_chunk_try_alloc' from
;; `nelisp-standalone--windows-alloc-chunk-form' /
;; `nelisp-standalone--windows-chunk-try-alloc-form', plus the production
;; `nl_align_up', `nl_chunk_cursor_addr' and `nl_chunk_init_descriptor' -- on a
;; host model of memory.  Only the OS/atomic/heap primitives are stubbed.
;;
;; The model tracks the set of ACTUALLY committed pages independently from the
;; published descriptor watermark (desc+56).  An allocator header write to a
;; page that is not actually committed is rejected and recorded.
;;
;; Run:   emacs --batch -l test/standalone-windows-chunk-commit-check.el
;; Env:   NDTW_COMMIT_MUTATION = (unset) | premature | per-object
;;   unset      production helper; every check must pass; prints
;;              COMMIT-CACHE-PASS and exits 0.
;;   premature  same allocator/model/inputs, helper publishes the watermark
;;              BEFORE its OS commit; must exit nonzero with COMMIT-CACHE-FAIL.
;;   per-object same allocator/model/inputs, helper commits on every bump
;;              (the old behaviour); must exit nonzero with COMMIT-CACHE-FAIL.
;;
;; Not covered (host model only): arena init / cold image / runtime
;; integration, real VirtualAlloc semantics, GC, the chunk-0 cursor special
;; case of `nl_chunk_cursor_addr' (the stub chunk-0 slot never matches), real
;; thread preemption (interleavings are injected deterministically at the OS
;; commit and at the watermark CAS).

(require 'cl-lib)
(require 'subr-x)

;; Suppress host bytecode cache generation BEFORE loading the builder.
(setenv "NELISP_STANDALONE_TOOLCHAIN_ELC" "0")

(defconst ndtw-commit--root
  (let ((f (or load-file-name buffer-file-name)))
    (unless f (error "cannot resolve fixture location"))
    (file-name-directory
     (directory-file-name (file-name-directory (expand-file-name f))))))
(add-to-list 'load-path (expand-file-name "lisp" ndtw-commit--root))
(add-to-list 'load-path (expand-file-name "src" ndtw-commit--root))
(add-to-list 'load-path (expand-file-name "scripts" ndtw-commit--root))
(require 'nelisp-standalone-build)

;;;; Production source extraction and renaming

(defun ndtw-commit--find-defun (tree name)
  "Return the first (defun NAME ...) form found anywhere in TREE."
  (cond ((not (consp tree)) nil)
        ((and (eq (car tree) 'defun) (eq (cadr tree) name)) tree)
        (t (let ((found nil) (rest tree))
             (while (and (consp rest) (not found))
               (setq found (ndtw-commit--find-defun (car rest) name)
                     rest (cdr rest)))
             found))))

(defun ndtw-commit--production (tree name)
  (or (ndtw-commit--find-defun tree name)
      (error "COMMIT-CACHE-SETUP production defun %s not found" name)))

(defconst ndtw-commit--prims
  '(heap-read-u64 ptr-write-u64 atomic-fetch-add atomic-compare-exchange))

(defun ndtw-commit--rename (form)
  "Map DSL FORM onto host-evaluable, namespaced symbols."
  (cond ((consp form)
         (cons (ndtw-commit--rename (car form)) (ndtw-commit--rename (cdr form))))
        ((and form (symbolp form))
         (let ((n (symbol-name form)))
           (cond ((eq form 'seq) 'progn)
                 ((memq form ndtw-commit--prims) (intern (concat "ndtw--m-" n)))
                 ((string-prefix-p "nl_" n) (intern (concat "ndtw--m-" n)))
                 (t form))))
        (t form)))

;;;; Host model

(defconst ndtw-commit--base #x100000000)
(defvar ndtw-commit--mem (make-hash-table :test 'eql))
(defvar ndtw-commit--pages (make-hash-table :test 'eql)) ; page index -> t
(defvar ndtw-commit--os-calls nil)    ; list of (OLD NEW), newest first
(defvar ndtw-commit--violations nil)  ; header writes to uncommitted pages
(defvar ndtw-commit--commit-hook nil) ; one-shot, runs inside the OS commit
(defvar ndtw-commit--fail-next nil)   ; make the next OS commit fail
(defvar ndtw-commit--cas-pre nil)     ; one-shot, runs before a watermark CAS
(defvar ndtw-commit--cas-failures 0)
(defvar ndtw-commit--wm-addr nil)

(defun ndtw-commit--page-committed-p (addr len)
  (let ((rel (- addr ndtw-commit--base)))
    (and (>= rel 0)
         (gethash (/ rel 4096) ndtw-commit--pages)
         (gethash (/ (+ rel len -1) 4096) ndtw-commit--pages))))

(defun ndtw-commit--mark (old new)
  (let ((p (/ old 4096)))
    (while (< (* p 4096) new)
      (puthash p t ndtw-commit--pages)
      (setq p (1+ p)))))

(defun ndtw--m-heap-read-u64 (addr off)
  (gethash (+ addr off) ndtw-commit--mem 0))

(defun ndtw--m-ptr-write-u64 (addr off val)
  (let ((a (+ addr off)))
    (if (ndtw-commit--page-committed-p a 8)
        (puthash a val ndtw-commit--mem)
      (push (list :addr (- a ndtw-commit--base) :val val)
            ndtw-commit--violations)
      0)))

(defun ndtw--m-atomic-fetch-add (addr delta)
  (let ((old (gethash addr ndtw-commit--mem 0)))
    (puthash addr (+ old delta) ndtw-commit--mem)
    old))

(defun ndtw--m-atomic-compare-exchange (addr old new)
  (when (and ndtw-commit--cas-pre (eql addr ndtw-commit--wm-addr))
    (let ((f ndtw-commit--cas-pre))
      (setq ndtw-commit--cas-pre nil)
      (funcall f)))
  (if (eql (gethash addr ndtw-commit--mem 0) old)
      (progn (puthash addr new ndtw-commit--mem) 1)
    (when (eql addr ndtw-commit--wm-addr)
      (cl-incf ndtw-commit--cas-failures))
    0))

(defun ndtw--m-nl_os_commit_range (base old new)
  "Stub of the OS commit: 1 on success, 0 on failure."
  (unless (eql base ndtw-commit--base)
    (error "commit_range called with a foreign base %s" base))
  (push (list old new) ndtw-commit--os-calls)
  (if ndtw-commit--fail-next
      (progn (setq ndtw-commit--fail-next nil) 0)
    ;; The pages are NOT committed yet while a hook runs: this is where a
    ;; second thread can interleave.
    (let ((h ndtw-commit--commit-hook))
      (when h
        (setq ndtw-commit--commit-hook nil)
        (funcall h)))
    (ndtw-commit--mark old new)
    1))

;;;; Install the production definitions (and the optional mutation)

(defconst ndtw-commit--mutation (getenv "NDTW_COMMIT_MUTATION"))

(defconst ndtw-commit--premature-helper
  ;; Production body with ONLY the publication timing changed: the CAS-max
  ;; watermark publication runs before the OS commit.
  '(defun nl_windows_chunk_commit_to (chunk new)
     (let* ((wm_addr (+ chunk 56))
            (wm (atomic-fetch-add wm_addr 0))
            (required (nl_align_up new 4096)))
       (if (>= wm required)
           1
         (let* ((size_end (nl_align_up (heap-read-u64 (+ chunk 8) 0) 4096))
                (want_end (nl_align_up new 65536))
                (end (if (< want_end size_end) want_end size_end))
                (done 0))
           (seq
            (while (= done 0)
              (let ((cur (atomic-fetch-add wm_addr 0)))
                (if (>= cur end)
                    (setq done 1)
                  (if (= (atomic-compare-exchange wm_addr cur end) 1)
                      (setq done 1)
                    0))))
            (if (= (nl_os_commit_range (heap-read-u64 chunk 0) wm end) 0)
                0
              1)))))))

(defconst ndtw-commit--per-object-helper
  ;; Old behaviour: commit the object's pages on every bump, no watermark.
  '(defun nl_windows_chunk_commit_to (chunk new)
     (if (= (nl_os_commit_range (heap-read-u64 chunk 0) 0 (nl_align_up new 4096)) 0)
         0
       1)))

(defun ndtw-commit--install ()
  (let* ((arena nelisp-standalone--arena-source)
         (win-os (nelisp-standalone--windows-alloc-chunk-form))
         (prod (list
                (ndtw-commit--production arena 'nl_align_up)
                (ndtw-commit--production arena 'nl_chunk_cursor_addr)
                (ndtw-commit--production arena 'nl_chunk_init_descriptor)
                (ndtw-commit--production win-os 'nl_windows_chunk_commit_to)
                (nelisp-standalone--windows-chunk-try-alloc-form))))
    (dolist (f prod) (eval (ndtw-commit--rename f) t))
    (pcase ndtw-commit--mutation
      ((or 'nil "") nil)
      ("premature" (eval (ndtw-commit--rename ndtw-commit--premature-helper) t))
      ("per-object" (eval (ndtw-commit--rename ndtw-commit--per-object-helper) t))
      (other (error "COMMIT-CACHE-SETUP unknown NDTW_COMMIT_MUTATION=%S" other)))))

;;;; Check harness

(defvar ndtw-commit--total 0)
(defvar ndtw-commit--failed 0)

(defun ndtw-commit--check (name ok &optional detail)
  (cl-incf ndtw-commit--total)
  (if ok
      (message "ok   %s" name)
    (cl-incf ndtw-commit--failed)
    (message "COMMIT-CACHE-FAIL mutation=%s check=%s %s"
             (or ndtw-commit--mutation "none") name
             (truncate-string-to-width (or detail "") 160 nil nil "..."))))

(defun ndtw-commit--reset (size)
  "Fresh chunk of SIZE bytes via the production descriptor init; return chunk."
  (clrhash ndtw-commit--mem)
  (clrhash ndtw-commit--pages)
  (setq ndtw-commit--os-calls nil ndtw-commit--violations nil
        ndtw-commit--commit-hook nil ndtw-commit--fail-next nil
        ndtw-commit--cas-pre nil ndtw-commit--cas-failures 0)
  (ndtw-commit--mark 0 4096)            ; nl_os_reserve_chunk commits page 0
  (let ((chunk (ndtw--m-nl_chunk_init_descriptor ndtw-commit--base size)))
    (setq ndtw-commit--wm-addr (+ chunk 56))
    chunk))

(defun ndtw-commit--cursor (chunk) (ndtw--m-heap-read-u64 (+ chunk 16) 0))
(defun ndtw-commit--wm (chunk) (ndtw--m-heap-read-u64 (+ chunk 56) 0))
(defun ndtw-commit--ncalls () (length ndtw-commit--os-calls))

(defun ndtw-commit--bump (chunk want)
  "Run production `nl_chunk_try_alloc'; return (OBJ . OLD-CURSOR)."
  (let ((old (ndtw-commit--cursor chunk)))
    (cons (ndtw--m-nl_chunk_try_alloc chunk want) old)))

(defun ndtw-commit--bump-ok (chunk want)
  "Bump and verify address, header and cursor; return nil if fine else a string."
  (let* ((r (ndtw-commit--bump chunk want)) (obj (car r)) (old (cdr r)))
    (cond ((/= obj (+ ndtw-commit--base old 8))
           (format "obj=%s expected=%s" obj (+ ndtw-commit--base old 8)))
          ((/= (ndtw-commit--cursor chunk) (+ old want))
           (format "cursor=%s expected=%s" (ndtw-commit--cursor chunk) (+ old want)))
          ((/= (gethash (+ ndtw-commit--base old) ndtw-commit--mem -1) want)
           (format "header at %s missing" old)))))

(defun ndtw-commit--bumps (chunk want n)
  "N verified bumps; return the first problem string or nil."
  (let ((bad nil) (i 0))
    (while (and (< i n) (not bad))
      (setq bad (ndtw-commit--bump-ok chunk want) i (1+ i)))
    bad))

(defun ndtw-commit--last-call () (car ndtw-commit--os-calls))

;;;; Scenarios

(defun ndtw-commit--test-batching ()
  (let ((c (ndtw-commit--reset 262144)))
    (ndtw-commit--check "initial-descriptor"
                        (and (= (ndtw-commit--cursor c) 1024)
                             (= (ndtw-commit--wm c) 4096)
                             (= (ndtw--m-heap-read-u64 c 0) ndtw-commit--base)
                             (= (ndtw--m-heap-read-u64 c 8) 262144)))
    (ndtw-commit--check "same-page-bumps-correct"
                        (null (ndtw-commit--bumps c 64 48)))
    (ndtw-commit--check "same-page-bumps-no-os-commit"
                        (= (ndtw-commit--ncalls) 0)
                        (format "os-calls=%S" ndtw-commit--os-calls))
    (ndtw-commit--check "same-page-watermark-unchanged"
                        (= (ndtw-commit--wm c) 4096))
    ;; cursor is exactly 4096; the next bump crosses into page 1.
    (ndtw-commit--check "page-cross-bump-correct"
                        (null (ndtw-commit--bump-ok c 64)))
    (ndtw-commit--check "page-cross-commits-64k-batch"
                        (and (= (ndtw-commit--ncalls) 1)
                             (equal (ndtw-commit--last-call) '(4096 65536))
                             (= (ndtw-commit--wm c) 65536))
                        (format "calls=%S wm=%s" ndtw-commit--os-calls (ndtw-commit--wm c)))
    ;; 959 more bumps end exactly on the 64 KiB boundary: no further commit.
    (ndtw-commit--check "batched-bumps-correct"
                        (null (ndtw-commit--bumps c 64 959)))
    (ndtw-commit--check "batched-bumps-no-os-commit"
                        (and (= (ndtw-commit--ncalls) 1)
                             (= (ndtw-commit--cursor c) 65536))
                        (format "calls=%d cursor=%s" (ndtw-commit--ncalls)
                                (ndtw-commit--cursor c)))
    ;; Crossing the 64 KiB boundary commits the next batch.
    (ndtw-commit--check "boundary-cross-bump-correct"
                        (null (ndtw-commit--bump-ok c 64)))
    (ndtw-commit--check "boundary-cross-commits-next-batch"
                        (and (= (ndtw-commit--ncalls) 2)
                             (equal (ndtw-commit--last-call) '(65536 131072))
                             (= (ndtw-commit--wm c) 131072)
                             (= (ndtw-commit--cursor c) 65600))
                        (format "calls=%S wm=%s" ndtw-commit--os-calls (ndtw-commit--wm c)))
    (ndtw-commit--check "no-header-violations"
                        (null ndtw-commit--violations)
                        (format "%S" ndtw-commit--violations))))

(defun ndtw-commit--test-partial-chunk ()
  (let ((c (ndtw-commit--reset 150000)))
    (ndtw-commit--check "partial-big-bump-correct"
                        (null (ndtw-commit--bump-ok c 100000)))
    (ndtw-commit--check "partial-big-bump-commit"
                        (and (= (ndtw-commit--ncalls) 1)
                             (equal (ndtw-commit--last-call) '(4096 131072))
                             (= (ndtw-commit--wm c) 131072))
                        (format "calls=%S" ndtw-commit--os-calls))
    (ndtw-commit--check "partial-final-bump-correct"
                        (null (ndtw-commit--bump-ok c 40000)))
    (ndtw-commit--check "partial-final-chunk-clamped-to-size"
                        (and (= (ndtw-commit--ncalls) 2)
                             (equal (ndtw-commit--last-call) '(131072 151552))
                             (= (ndtw-commit--wm c) 151552)
                             (= (ndtw-commit--cursor c) 141024))
                        (format "calls=%S wm=%s" ndtw-commit--os-calls (ndtw-commit--wm c)))
    (let ((r (ndtw-commit--bump c 10000)))
      (ndtw-commit--check "over-size-bump-fails-without-side-effects"
                          (and (= (car r) 0)
                               (= (ndtw-commit--cursor c) 141024)
                               (= (ndtw-commit--ncalls) 2))))
    (ndtw-commit--check "partial-tail-bump-correct-no-commit"
                        (and (null (ndtw-commit--bump-ok c 8000))
                             (= (ndtw-commit--ncalls) 2)
                             (= (ndtw-commit--cursor c) 149024)))
    (ndtw-commit--check "partial-no-header-violations"
                        (null ndtw-commit--violations)
                        (format "%S" ndtw-commit--violations))))

(defun ndtw-commit--test-failure ()
  (let ((c (ndtw-commit--reset 262144)))
    (ndtw-commit--bumps c 64 48)
    (setq ndtw-commit--fail-next t)
    (let ((r (ndtw-commit--bump c 64)))
      (ndtw-commit--check "commit-failure-returns-allocation-failure"
                          (= (car r) 0) (format "obj=%s" (car r)))
      (ndtw-commit--check "commit-failure-publishes-nothing"
                          (and (= (ndtw-commit--wm c) 4096)
                               (not (gethash 1 ndtw-commit--pages)))
                          (format "wm=%s page1=%s" (ndtw-commit--wm c)
                                  (gethash 1 ndtw-commit--pages))))
    ;; Retry: the cursor already moved past the failed reservation, but the
    ;; helper must still commit before the next reservation is returned.
    (let ((calls-before (ndtw-commit--ncalls)))
      (ndtw-commit--check "retry-after-failure-correct"
                          (null (ndtw-commit--bump-ok c 64)))
      (ndtw-commit--check "retry-after-failure-commits-before-return"
                          (and (= (ndtw-commit--ncalls) (1+ calls-before))
                               (= (ndtw-commit--wm c) 65536)
                               (gethash 1 ndtw-commit--pages))
                          (format "calls=%S wm=%s" ndtw-commit--os-calls
                                  (ndtw-commit--wm c))))
    (ndtw-commit--check "failure-no-header-violations"
                        (null ndtw-commit--violations)
                        (format "%S" ndtw-commit--violations))))

(defun ndtw-commit--test-interleave-header ()
  "A is suspended inside its OS commit; B reserves in A's new page."
  (let ((c (ndtw-commit--reset 262144)) (b nil) (b-old nil))
    (ndtw-commit--bumps c 64 48)        ; cursor = 4096
    (setq ndtw-commit--commit-hook
          (lambda ()
            ;; A holds the reservation [4096,4160) and the cursor is 4160, but
            ;; page 1 is not committed yet.
            (setq b-old (ndtw-commit--cursor c)
                  b (ndtw-commit--bump c 64))))
    (let ((a (ndtw-commit--bump c 64)))
      (ndtw-commit--check "interleave-b-ran-inside-a-commit"
                          (and b (= b-old 4160)))
      (ndtw-commit--check "interleave-b-obj-address"
                          (and b (= (car b) (+ ndtw-commit--base 4160 8)))
                          (format "b=%S" b))
      (ndtw-commit--check "interleave-b-header-in-committed-page"
                          (null ndtw-commit--violations)
                          (format "header written to uncommitted page: %S"
                                  ndtw-commit--violations))
      (ndtw-commit--check "interleave-b-committed-itself"
                          (>= (ndtw-commit--ncalls) 2)
                          (format "calls=%S" ndtw-commit--os-calls))
      (ndtw-commit--check "interleave-b-header-value"
                          (= (gethash (+ ndtw-commit--base 4160) ndtw-commit--mem -1) 64))
      (ndtw-commit--check "interleave-a-correct"
                          (and (= (car a) (+ ndtw-commit--base 4096 8))
                               (= (gethash (+ ndtw-commit--base 4096) ndtw-commit--mem -1) 64)))
      (ndtw-commit--check "interleave-final-state"
                          (and (= (ndtw-commit--cursor c) 4224)
                               (= (ndtw-commit--wm c) 65536))
                          (format "cursor=%s wm=%s" (ndtw-commit--cursor c)
                                  (ndtw-commit--wm c))))))

(defun ndtw-commit--test-interleave-farther-batch ()
  "B commits a farther batch while A is suspended; A must not lower it."
  (let ((c (ndtw-commit--reset 262144)) (b nil))
    (ndtw-commit--bumps c 64 48)
    (setq ndtw-commit--commit-hook
          (lambda () (setq b (ndtw-commit--bump c 70000))))
    (let ((a (ndtw-commit--bump c 64)))
      (ndtw-commit--check "farther-b-obj-address"
                          (and b (= (car b) (+ ndtw-commit--base 4160 8)))
                          (format "b=%S" b))
      (ndtw-commit--check "farther-a-obj-address"
                          (= (car a) (+ ndtw-commit--base 4096 8)))
      (ndtw-commit--check "farther-watermark-not-lowered"
                          (= (ndtw-commit--wm c) 131072)
                          (format "wm=%s calls=%S" (ndtw-commit--wm c)
                                  ndtw-commit--os-calls))
      (ndtw-commit--check "farther-no-header-violations"
                          (null ndtw-commit--violations)
                          (format "%S" ndtw-commit--violations)))))

(defun ndtw-commit--test-cas-retry ()
  "Force the watermark CAS to fail once, in both retry outcomes."
  ;; (a) another thread published a SMALLER watermark: the loop retries and
  ;;     the second CAS publishes END.
  (let ((c (ndtw-commit--reset 262144)))
    (ndtw-commit--bumps c 64 48)
    (setq ndtw-commit--cas-pre
          (lambda ()
            (puthash (+ c 56) 8192 ndtw-commit--mem)
            (ndtw-commit--mark 0 8192)))
    (ndtw-commit--check "cas-retry-bump-correct"
                        (null (ndtw-commit--bump-ok c 64)))
    (ndtw-commit--check "cas-retry-republishes-end"
                        (and (= ndtw-commit--cas-failures 1)
                             (= (ndtw-commit--wm c) 65536))
                        (format "failures=%d wm=%s" ndtw-commit--cas-failures
                                (ndtw-commit--wm c))))
  ;; (b) another thread published a FARTHER watermark: the loop must observe
  ;;     CUR >= END and leave it alone.
  (let ((c (ndtw-commit--reset 262144)))
    (ndtw-commit--bumps c 64 48)
    (setq ndtw-commit--cas-pre
          (lambda ()
            (puthash (+ c 56) 131072 ndtw-commit--mem)
            (ndtw-commit--mark 0 131072)))
    (ndtw-commit--check "cas-farther-bump-correct"
                        (null (ndtw-commit--bump-ok c 64)))
    (ndtw-commit--check "cas-farther-not-lowered"
                        (and (= ndtw-commit--cas-failures 1)
                             (= (ndtw-commit--wm c) 131072))
                        (format "failures=%d wm=%s" ndtw-commit--cas-failures
                                (ndtw-commit--wm c)))))

;;;; Main

(defun ndtw-commit--main ()
  (ndtw-commit--install)
  (ndtw-commit--test-batching)
  (ndtw-commit--test-partial-chunk)
  (ndtw-commit--test-failure)
  (ndtw-commit--test-interleave-header)
  (ndtw-commit--test-interleave-farther-batch)
  (ndtw-commit--test-cas-retry)
  (cond ((= ndtw-commit--total 0)
         (message "COMMIT-CACHE-FAIL mutation=%s no checks ran"
                  (or ndtw-commit--mutation "none"))
         (kill-emacs 2))
        ((> ndtw-commit--failed 0)
         (message "COMMIT-CACHE-FAIL mutation=%s failed=%d of tests=%d"
                  (or ndtw-commit--mutation "none")
                  ndtw-commit--failed ndtw-commit--total)
         (kill-emacs 1))
        (t
         (message "COMMIT-CACHE-PASS tests=%d mutation=%s"
                  ndtw-commit--total (or ndtw-commit--mutation "none"))
         (kill-emacs 0))))

(when noninteractive
  (condition-case err
      (ndtw-commit--main)
    (error
     (message "COMMIT-CACHE-FAIL mutation=%s error=%S"
              (or ndtw-commit--mutation "none") err)
     (kill-emacs 3))))

;;; standalone-windows-chunk-commit-check.el ends here
