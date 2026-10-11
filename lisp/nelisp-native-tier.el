;;; nelisp-native-tier.el --- Owner-thread asynchronous native tier-up -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Install explicitly; qualification runs never enable this module implicitly.
;; Entry counting does no semantic validation. The evaluator owner services
;; `nelisp-native-tier-poll' at event-loop safe points. Compilation runs only
;; in a child of the same executable, optionally using another cold image.
;; Old callables retain their mappings; publication changes one complete cell.
;;; Code:
(require 'cl-lib)
(require 'nelisp-native-cache)
(define-error 'nelisp-native-tier-permanent "Permanent native tier refusal")
(defvar read-eval)
(declare-function nelisp-process-start "ext:reader" (&rest command))
(declare-function nelisp-process-close-stdin "ext:reader" (process))
(declare-function nelisp-process-status "ext:reader" (process))
(declare-function nelisp-process-exit-status "ext:reader" (process))
(declare-function nelisp-process-delete "ext:reader" (process))
(declare-function nl-ffi--dlopen "nl-ffi" (file))
(defvar nelisp-native-tier-threshold 1000)
(defvar nelisp-native-tier-backend 'in-house)
(defvar nelisp-native-tier-worker-binary nil)
(defvar nelisp-native-tier-worker-image nil)
(defvar nelisp-native-tier-timeout 290)
(defvar nelisp-native-tier-cooldown 60)
(defvar nelisp-native-tier--entries nil)
(defvar nelisp-native-tier--queue nil)
(defvar nelisp-native-tier--pending nil)
(defvar nelisp-native-tier--refusals (make-hash-table :test 'equal))
(defvar nelisp-native-tier--tier0 (make-hash-table :test 'equal))
(defvar nelisp-native-tier--accepted (make-hash-table :test 'equal))
(defvar nelisp-native-tier--serial 0)
(defvar nelisp-native-tier--reaped 0)
(defvar nelisp-native-tier--publishing nil)
(defconst nelisp-native-tier--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(defun nelisp-native-tier--read (file)
  "Read exactly one non-evaluating protocol record from FILE."
  (let* ((read-eval nil)
         (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (record (read-from-string text)))
    (unless (string-match-p "\\`[ \t\r\n]*\\'" (substring text (cdr record)))
      (error "Trailing tier protocol data"))
    (car record)))
(defun nelisp-native-tier--write (file record)
  (write-region (concat (nelisp-native-cache--print record) "\n") nil file nil 'silent))
(defun nelisp-native-tier--function (recipe)
  "Reconstruct immutable compiler input, refusing live switch tables."
  (when (plist-get recipe :live-hash-constants) (signal 'nelisp-native-tier-permanent '(live-switch-input)))
  (apply #'make-byte-code
         (append (list (plist-get recipe :descriptor) (plist-get recipe :code)
                       (plist-get recipe :constants) (plist-get recipe :stack-depth))
                 (pcase (plist-get recipe :function-length)
                   (4 nil) (5 (list (plist-get recipe :metadata)))
                   (6 (list (plist-get recipe :metadata) (plist-get recipe :interactive)))
                   (_ (signal 'nelisp-native-tier-permanent '(input-shape)))))))
(defun nelisp-native-tier--identity (function backend)
  "Independently derive BACKEND's identity without template memo cells."
  (let ((nelisp-native-cache-backend backend))
    (let ((abi (nelisp-native-cache-abi-hash)))
      (unless abi (error "Tier optimizer ABI unavailable"))
      (list :abi abi :input (nelisp-native-cache--input-hash function) :backend backend))))
(defun nelisp-native-tier--refuse (entry reason &optional permanent)
  "Retain REASON by exact content key, with cooldown for transient failures."
  (let ((record (list :reason reason :permanent permanent
                      :retry-after (+ (float-time) nelisp-native-tier-cooldown))))
    (puthash (plist-get entry :key) record nelisp-native-tier--refusals)
    (setf (plist-get entry :refusal) record)
    record))
(defun nelisp-native-tier--native-process-p ()
  (and (fboundp 'nelisp-process-start) (fboundp 'nelisp-process-status)
       (fboundp 'nelisp-process-delete) (fboundp 'nelisp-process-close-stdin)))
(defun nelisp-native-tier--binary ()
  (or nelisp-native-tier-worker-binary
      (and (fboundp 'nelisp--syscall-readlink) (nelisp--syscall-readlink "/proc/self/exe"))
      (expand-file-name invocation-name invocation-directory)))
(defun nelisp-native-tier--check-worker (binary backend)
  "Reject foreign executables and unavailable optimizer capabilities."
  ;; Windows async process supervision is not implemented. Refuse before
  ;; reading a worker PE: its whole-file hash is not the running build stamp,
  ;; and accepting a caller-selected stamp would weaken the identity fence.
  (when (nelisp-native-load--windows-p)
    (signal 'nelisp-native-tier-permanent '(windows-worker-unsupported)))
  ;; Hash complete raw bytes, as the running-executable fence does. The
  ;; scratch buffer API may encode high bytes on a standalone reader.
  (let* ((external (nelisp-native-load--sha256-file-external binary))
         (bytes (unless external
                  (if (or (fboundp 'nelisp--syscall-read-file) (fboundp 'rdf))
                      (nelisp-native-load--read-file binary)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally binary) (buffer-string)))))
         (digest (or external
                     (and (nelisp-native-load--complete-file-bytes-p binary bytes)
                          (> (string-bytes bytes) 0) (secure-hash 'sha256 bytes)))))
    (unless (and digest (equal digest (nelisp-native-load-running-binary-sha256)))
      (signal 'nelisp-native-tier-permanent '(different-executable))))
  (unless (memq backend '(in-house gccjit)) (signal 'nelisp-native-tier-permanent '(invalid-backend)))
  (when (eq backend 'gccjit)
    (require 'nl-ffi)
    ;; A static reader advertises nl-ffi-call but its dlopen signals refusal.
    (condition-case err
        (unless (nl-ffi--dlopen "libgccjit.so.0")
          (signal 'nelisp-native-tier-permanent '(gccjit-dlopen)))
      (error (signal 'nelisp-native-tier-permanent (list 'gccjit-capability err))))))
;;;###autoload
(defun nelisp-native-tier-install (symbol function &optional tier0-backend)
  "Install a stable dispatcher for FUNCTION in SYMBOL.
Return its state. Tier-0 refusal leaves the current definition untouched."
  (unless (and (symbolp symbol) (byte-code-function-p function))
    (error "Tier installation needs a symbol and materialized byte code"))
  (let* ((backend nelisp-native-tier-backend)
         (identity (nelisp-native-tier--identity function backend))
         (key (nelisp-native-cache--hash identity))
         (entry (list :symbol symbol :function function :backend backend :identity identity
                      :key key :cell (list nil) :count 0 :tier 0 :refusal nil
                      :job nil :dispatcher nil))
         (nelisp-native-cache-backend (or tier0-backend 'in-house))
         (tier0-key (nelisp-native-cache--hash
                     (nelisp-native-tier--identity function nelisp-native-cache-backend)))
         (factory (gethash tier0-key nelisp-native-tier--tier0))
         (nelisp-native-cache--unit-observer
          (lambda (unit) (puthash tier0-key unit nelisp-native-tier--tier0)))
         (callable (if factory (funcall factory (nelisp-native-cache--constants function))
                     (or (nelisp-native-cache-load function)
                         (progn (nelisp-native-cache-compile function)
                                (nelisp-native-cache-load function))))))
    (unless callable (error "Tier 0 callable unavailable"))
    (setcar (plist-get entry :cell) callable)
    (let ((dispatcher
           (lambda (&rest arguments)
             ;; Snapshot before requesting a job or entering a callback.
             (let ((generation (car (plist-get entry :cell))))
               (when (= (plist-get entry :tier) 0)
                 (when (< (plist-get entry :count) most-positive-fixnum)
                   (setf (plist-get entry :count) (1+ (plist-get entry :count))))
                 (when (and nelisp-native-tier-threshold
                            (>= (plist-get entry :count) nelisp-native-tier-threshold)
                            (not (plist-get entry :job)))
                   (nelisp-native-tier-request entry)))
               (apply generation arguments)))))
      (setf (plist-get entry :dispatcher) dispatcher)
      (fset symbol dispatcher))
    (push entry nelisp-native-tier--entries)
    entry))
(defun nelisp-native-tier--adopt (entry factory)
  "Bind an already accepted owner to ENTRY's own live constants."
  (let* ((cell (plist-get entry :cell)) (generation (car cell))
         (candidate (funcall factory (nelisp-native-cache--constants (plist-get entry :function))))
         (identity (nelisp-native-tier--identity (plist-get entry :function) (plist-get entry :backend))))
    (when (and (equal identity (plist-get entry :identity))
               (eq cell (plist-get entry :cell)) (eq generation (car cell))
               (not (plist-get entry :job))
               (eq (symbol-function (plist-get entry :symbol)) (plist-get entry :dispatcher)))
      (setcar cell candidate)
      (setf (plist-get entry :tier) 1))))
(defun nelisp-native-tier-request (entry &optional retry)
  "Queue ENTRY once; RETRY clears only transient cooldown. Never compile here."
  (let* ((key (plist-get entry :key)) (refusal (gethash key nelisp-native-tier--refusals)))
    (when refusal (setf (plist-get entry :refusal) refusal))
    (when (and (= 0 (plist-get entry :tier)) (gethash key nelisp-native-tier--accepted)
               (not (and refusal (or (plist-get refusal :permanent)
                                    (and (not retry) (< (float-time) (plist-get refusal :retry-after)))))))
      (condition-case err
          (nelisp-native-tier--adopt entry (gethash key nelisp-native-tier--accepted))
        (error (nelisp-native-tier--refuse entry err
                                         (memq (car err) '(nelisp-native-tier-permanent nelisp-native-budget-exhausted))))))
    (unless (or (plist-get entry :job) (= 1 (plist-get entry :tier))
                (gethash key nelisp-native-tier--accepted)
                (and refusal (or (plist-get refusal :permanent)
                                 (and (not retry) (< (float-time) (plist-get refusal :retry-after))))))
      (if (cl-find key (append nelisp-native-tier--queue (list nelisp-native-tier--pending))
                   :key (lambda (job) (plist-get job :key)) :test #'equal)
          (nelisp-native-tier--refuse entry 'duplicate-pending)
        (let ((job (list :key key :entry entry :serial (setq nelisp-native-tier--serial
                                                           (1+ nelisp-native-tier--serial))
                         :process nil :directory nil :started nil :cache nil
                         :cell (plist-get entry :cell) :generation (car (plist-get entry :cell)) :released nil)))
          (setf (plist-get entry :job) job)
          (setq nelisp-native-tier--queue (append nelisp-native-tier--queue (list job)))
          (nelisp-native-tier--launch-next))))))
(defun nelisp-native-tier--launch-next ()
  (when (and (not nelisp-native-tier--pending) nelisp-native-tier--queue)
    (let* ((job (pop nelisp-native-tier--queue)) (entry (plist-get job :entry))
           (binary (nelisp-native-tier--binary)) (backend (plist-get entry :backend)))
      (condition-case err
          (progn
            (unless (nelisp-native-tier--native-process-p)
              (signal 'nelisp-native-tier-permanent '(async-process-capability)))
            (nelisp-native-budget-check (if (eq backend 'gccjit) 4096 8192))
            (nelisp-native-tier--check-worker binary backend)
            (let* ((nelisp-native-cache-backend backend)
                   (recipe (nelisp-native-cache--recipe (plist-get entry :function)))
                   (_input (nelisp-native-tier--function recipe))
                   (root (nelisp-native-cache--private-directory (nelisp-native-cache--root)))
                   (cache (nelisp-native-cache--private-directory (expand-file-name "tier1" root)))
                   (directory (make-temp-file (expand-file-name "tier-" root) t))
                   (request (expand-file-name "request" directory))
                   (reply (expand-file-name "reply" directory))
                   (record (list :version 1 :identity (plist-get entry :identity)
                                 :binary (nelisp-native-load-running-binary-sha256)
                                 :serial (plist-get job :serial) :recipe recipe :cache cache
                                 :image-required (and nelisp-native-tier-worker-image t)))
                   (command (append (list binary)
                                    (and nelisp-native-tier-worker-image
                                         (list "--cold-load-from" nelisp-native-tier-worker-image))
                                    (list "-L" (expand-file-name "lisp" nelisp-native-tier--root)
                                          "-L" (expand-file-name "src" nelisp-native-tier--root)
                                          "-L" (expand-file-name "scripts" nelisp-native-tier--root)
                                          "-L" (expand-file-name "packages/nl-ffi/src" nelisp-native-tier--root)
                                          "-L" (expand-file-name "packages/nl-prelude/src" nelisp-native-tier--root)
                                          "--eval" (format "(progn (require 'nelisp-native-tier) (nelisp-native-tier-worker %S %S))"
                                                           request reply)))))
              (set-file-modes directory #o700)
              (setf (plist-get job :directory) directory
                    (plist-get job :cache) cache)
              (nelisp-native-tier--write request record)
              (setf (plist-get job :process) (apply #'nelisp-process-start command)
                    (plist-get job :started) (float-time))
              (unless (plist-get job :process) (error "Tier worker spawn failed"))
              (nelisp-process-close-stdin (plist-get job :process))
              (setq nelisp-native-tier--pending job)))
        (error
         (when (plist-get job :process)
           (nelisp-process-delete (plist-get job :process)))
         (when (plist-get job :directory) (delete-directory (plist-get job :directory) t))
         (setf (plist-get entry :job) nil)
         (nelisp-native-tier--refuse entry err
                                    (memq (car err) '(nelisp-native-tier-permanent nelisp-native-budget-exhausted)))
         (nelisp-native-tier--launch-next))))))
(defun nelisp-native-tier-worker (request reply)
  "Child-only optimizing compilation with an independently computed key."
  (let* ((record (nelisp-native-tier--read request))
         (identity (plist-get record :identity))
         (backend (plist-get identity :backend))
         (nelisp-native-cache-backend backend)
         (result
          (condition-case err
              (progn
                (unless (= 1 (plist-get record :version))
                  (signal 'nelisp-native-tier-permanent '(request-version)))
                (when (and (plist-get record :image-required)
                           (not nelisp-native-cache--cold-source-check))
                  (signal 'nelisp-native-tier-permanent '(optimizer-image-not-loaded)))
                (nelisp-native-tier--check-worker (nelisp-native-tier--binary) backend)
                (unless (equal (plist-get record :binary) (nelisp-native-load-running-binary-sha256))
                  (signal 'nelisp-native-tier-permanent '(worker-binary-mismatch)))
                (setenv "NELISP_NATIVE_CACHE" (plist-get record :cache))
                (let ((function (nelisp-native-tier--function (plist-get record :recipe))))
                  (unless (equal identity (nelisp-native-tier--identity function backend))
                    (signal 'nelisp-native-tier-permanent '(worker-input-abi-mismatch)))
                  (nelisp-native-cache-compile function)
                  (list :status 'ok :identity identity :serial (plist-get record :serial)
                        :compiler-cold (and nelisp-native-cache--cold-source-check t))))
            (error (list :status 'refused :reason err :identity identity
                         :permanent (memq (car err) '(nelisp-native-tier-permanent nelisp-native-budget-exhausted))
                         :serial (plist-get record :serial))))))
    (nelisp-native-tier--write reply result)))
(defun nelisp-native-tier--current-p (job)
  (let* ((entry (plist-get job :entry))
         ;; Identity can allocate or invoke Lisp: perform it before final guards.
         (identity (nelisp-native-tier--identity (plist-get entry :function)
                                                (plist-get entry :backend))))
    (and (equal (plist-get entry :identity) identity)
         (eq (plist-get entry :job) job)
         (eq (plist-get entry :cell) (plist-get job :cell))
         (eq (car (plist-get entry :cell)) (plist-get job :generation))
         (eq (symbol-function (plist-get entry :symbol)) (plist-get entry :dispatcher)))))
(defun nelisp-native-tier--finish (job)
  (let* ((entry (plist-get job :entry))
         (reply (nelisp-native-tier--read (expand-file-name "reply" (plist-get job :directory)))))
    (unless (and (eq (plist-get reply :status) 'ok)
                 (equal (plist-get reply :identity) (plist-get entry :identity))
                 (eql (plist-get reply :serial) (plist-get job :serial)))
      (nelisp-native-tier--refuse entry (or (plist-get reply :reason) 'invalid-reply)
                                  (plist-get reply :permanent))
      (error "Worker refused tier promotion"))
    (unless (nelisp-native-tier--current-p job) (error "Stale tier job"))
    (when (gethash (plist-get entry :key) nelisp-native-tier--accepted)
      (error "Tier generation already accepted"))
    (let* ((nelisp-native-cache-backend (plist-get entry :backend))
           (cache-before (getenv "NELISP_NATIVE_CACHE"))
           (process-environment (copy-sequence process-environment))
           (factory nil)
           (nelisp-native-cache--unit-observer (lambda (unit) (setq factory unit)))
           (candidate
            (unwind-protect
                (progn
                  (setenv "NELISP_NATIVE_CACHE" (plist-get job :cache))
                  (nelisp-native-cache-load (plist-get entry :function)))
              ;; Standalone setenv also changes the real OS environment;
              ;; dynamic process-environment binding alone cannot restore it.
              (setenv "NELISP_NATIVE_CACHE" cache-before))))
      (unless (and candidate factory) (error "Tier artifact or owner missing"))
      ;; Mapping/authentication may invoke Lisp. Recheck EVERYTHING afterwards.
      (unless (nelisp-native-tier--current-p job) (error "Stale tier job after load"))
      ;; No callback or allocation in this single owner-thread cell store.
      (setcar (plist-get entry :cell) candidate)
      (setf (plist-get entry :tier) 1)
      (puthash (plist-get entry :key) factory nelisp-native-tier--accepted))))
(defun nelisp-native-tier--release (job)
  "Idempotently release JOB without clearing a newer job's ownership."
  (unless (plist-get job :released)
    (setf (plist-get job :released) t)
    (when (plist-get job :process)
      (nelisp-process-delete (plist-get job :process))
      (setq nelisp-native-tier--reaped (1+ nelisp-native-tier--reaped)))
    (when (and (plist-get job :directory) (file-exists-p (plist-get job :directory)))
      (delete-directory (plist-get job :directory) t))
    (when (eq (plist-get (plist-get job :entry) :job) job)
      (setf (plist-get (plist-get job :entry) :job) nil))
    (when (eq nelisp-native-tier--pending job) (setq nelisp-native-tier--pending nil))))
;;;###autoload
(defun nelisp-native-tier-poll ()
  "Service a nonblocking completion at an evaluator-owner safe point.
Do not call from another Lisp thread. Active callables retain their owners."
  (unless nelisp-native-tier--publishing
    (let ((nelisp-native-tier--publishing t) (job nelisp-native-tier--pending))
      (when job
        (let* ((process (plist-get job :process))
               (status (nelisp-process-status process))
               (expired (> (- (float-time) (plist-get job :started)) nelisp-native-tier-timeout)))
          (unless (and (= status 0) (not expired))
            (unwind-protect
                (condition-case err
                    (cond (expired (error "Tier worker timeout"))
                          ((/= 0 (nelisp-process-exit-status process)) (error "Tier worker crashed"))
                          (t (nelisp-native-tier--finish job)))
                  (error (unless (plist-get (plist-get (plist-get job :entry) :refusal) :permanent)
                           (nelisp-native-tier--refuse (plist-get job :entry) err
                                                      (eq (car err) 'nelisp-native-budget-exhausted)))))
              ;; status has waitpid(WNOHANG)-reaped a finished native child;
              ;; delete closes both descriptors, or terminates/reaps a timeout.
              (nelisp-native-tier--release job)))))
      (nelisp-native-tier--launch-next))))
(defun nelisp-native-tier-cancel (entry)
  "Cancel ENTRY's queued or running child, retaining a transient refusal."
  (let ((job (plist-get entry :job)))
    (when job
      (setq nelisp-native-tier--queue (delq job nelisp-native-tier--queue))
      (nelisp-native-tier--release job)
      (nelisp-native-tier--refuse entry 'cancelled)
      (nelisp-native-tier--launch-next))))
(provide 'nelisp-native-tier)
;;; nelisp-native-tier.el ends here
