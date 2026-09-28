;;; nelisp-eln-registration-objects-smoke.el --- GNU registration view probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-eln-registration-objects)

(defun nelisp-eln-registration-objects-smoke--check (condition message)
  (unless condition (error "Registration object smoke failed: %s" message)))

(defun nelisp-eln-registration-objects-smoke--condition (thunk)
  (condition-case error-data
      (progn (funcall thunk) nil)
    (error (car error-data))))

(defun nelisp-eln-registration-objects-smoke--word (address offset)
  (nelisp-eln-abi-read-word address offset))

(defvar nelisp-eln-registration-objects-smoke--handle nil)
(defvar nelisp-eln-registration-objects-smoke--other-handle nil)
(defvar nelisp-eln-registration-objects-smoke--unit nil)
(defvar nelisp-eln-registration-objects-smoke--other-unit nil)
(defvar nelisp-eln-registration-objects-smoke--activation nil)
(defvar nelisp-eln-registration-objects-smoke--other-activation nil)
(defvar nelisp-eln-registration-objects-smoke--native nil)
(defvar nelisp-eln-registration-objects-smoke--module-id nil)
(defvar nelisp-eln-registration-objects-smoke--subr-word nil)
(defvar nelisp-eln-registration-objects-smoke--path nil)
(defvar nelisp-eln-registration-objects-smoke--name nil)
(defvar nelisp-eln-registration-objects-smoke--c-name nil)
(defvar nelisp-eln-registration-objects-smoke--view nil)
(defvar nelisp-eln-registration-objects-smoke--capability nil)
(defvar nelisp-eln-registration-objects-smoke--unit-address nil)
(defvar nelisp-eln-registration-objects-smoke--subr-address nil)

(defun nelisp-eln-registration-objects-smoke-run ()
  "Check measured headers, owner checks, canonical decode, and activation GC."
  (setq nelisp-eln-registration-objects-smoke--path
        (getenv "NELISP_TEST_ELN_PATH")
        nelisp-eln-registration-objects-smoke--name
        (getenv "NELISP_TEST_ELN_LEAF")
        nelisp-eln-registration-objects-smoke--c-name
        (getenv "NELISP_TEST_ELN_C_NAME"))
  (nelisp-eln-registration-objects-smoke--check
   nelisp-eln-registration-objects-smoke--path "fixture path")
  (nelisp-eln-registration-objects-smoke--check
   nelisp-eln-registration-objects-smoke--name "fixture symbol")
  (nelisp-eln-registration-objects-smoke--check
   nelisp-eln-registration-objects-smoke--c-name "fixture native C symbol")
  (setq nelisp-eln-registration-objects-smoke--handle
        (nelisp-eln-system-loader-open
         nelisp-eln-registration-objects-smoke--path))
  (setq nelisp-eln-registration-objects-smoke--unit
        (nelisp-eln-registration-objects-create-unit
         nelisp-eln-registration-objects-smoke--handle))
  (setq nelisp-eln-registration-objects-smoke--unit-address
        (- (nelisp-eln-registration-objects-unit-word
            nelisp-eln-registration-objects-smoke--unit) 5))
  (setq nelisp-eln-registration-objects-smoke--activation
        (nelisp-eln-registration-objects-begin-activation
         nelisp-eln-registration-objects-smoke--unit))
  (setq nelisp-eln-registration-objects-smoke--view
        (nelisp-eln-registration-objects-subr-view
         nelisp-eln-registration-objects-smoke--activation
         nelisp-eln-registration-objects-smoke--name
         nelisp-eln-registration-objects-smoke--c-name nil nil 0 nil))
  (setq nelisp-eln-registration-objects-smoke--subr-word
        (plist-get nelisp-eln-registration-objects-smoke--view :word))
  (setq nelisp-eln-registration-objects-smoke--subr-address
        (- nelisp-eln-registration-objects-smoke--subr-word 5))
  (setq nelisp-eln-registration-objects-smoke--native
        (plist-get nelisp-eln-registration-objects-smoke--view :callable))
  (setq nelisp-eln-registration-objects-smoke--capability
        (nelisp-eln-system-loader-function-capability
         nelisp-eln-registration-objects-smoke--handle
         nelisp-eln-registration-objects-smoke--c-name))
  (setq nelisp-eln-registration-objects-smoke--module-id
        (nelisp-eln-system-loader-module-id
         nelisp-eln-registration-objects-smoke--handle))
  (let ((unit-header
         (nelisp-eln-registration-objects-smoke--word
          nelisp-eln-registration-objects-smoke--unit-address 0))
        (subr-header
         (nelisp-eln-registration-objects-smoke--word
          nelisp-eln-registration-objects-smoke--subr-address 0)))
    (nelisp-eln-registration-objects-smoke--check
     (and (= (logand unit-header #xfff) 6)
          (= (logand (ash unit-header -12) #xfff) 3)
          (= (logand (ash unit-header -24) #x3f) 26))
     "unit pseudovector header lisp=6 rest=3 tag=26")
    (nelisp-eln-registration-objects-smoke--check
     (and (= (logand subr-header #xfff) 0)
          (= (logand (ash subr-header -12) #xfff) 10)
          (= (logand (ash subr-header -24) #x3f) 18))
     "subr pseudovector header lisp=0 rest=10 tag=18"))
  (nelisp-eln-registration-objects-smoke--check
   (= (nelisp-eln-registration-objects-smoke--word
       nelisp-eln-registration-objects-smoke--subr-address 8)
      (nth 3 nelisp-eln-registration-objects-smoke--capability))
   "native entry address")
  (nelisp-eln-registration-objects-smoke--check
   (eq nelisp-eln-registration-objects-smoke--native
       (nelisp-eln-registration-objects-decode
        nelisp-eln-registration-objects-smoke--activation
        nelisp-eln-registration-objects-smoke--subr-word))
   "activation reverse mapping preserves callable identity")
  (let ((again (nelisp-eln-registration-objects-subr-view
                nelisp-eln-registration-objects-smoke--activation
                nelisp-eln-registration-objects-smoke--name
                nelisp-eln-registration-objects-smoke--c-name nil nil 0 nil)))
    (nelisp-eln-registration-objects-smoke--check
     (and (= (plist-get again :word)
             nelisp-eln-registration-objects-smoke--subr-word)
     (eq (plist-get again :callable)
              nelisp-eln-registration-objects-smoke--native))
     "same capability and metadata reuse one temporary GNU view"))
  (let ((release (symbol-function 'nl-ffi-memory-release))
        failed-owner failure)
    (cl-letf (((symbol-function
                'nelisp-eln-registration-objects--write-short)
               (lambda (_address _offset _value)
                 (signal 'nelisp-eln-registration-objects-error
                         '(injected-constructor-failure))))
              ((symbol-function 'nl-ffi-memory-release)
               (lambda (owner)
                 (if (and (null failed-owner) (vectorp owner)
                          (= (aref owner 3) 88))
                     (progn
                       (setq failed-owner owner)
                       (error "injected cleanup failure"))
                   (funcall release owner)))))
      (setq failure
            (condition-case err
                (nelisp-eln-registration-objects-subr-view
                 nelisp-eln-registration-objects-smoke--activation
                 nelisp-eln-registration-objects-smoke--name
                 nelisp-eln-registration-objects-smoke--c-name nil nil 1 nil)
              (error err))))
    (nelisp-eln-registration-objects-smoke--check
     (and (eq (car-safe failure)
              'nelisp-eln-registration-objects-error)
          (equal (cdr-safe failure) '(injected-constructor-failure)))
     "failed view preserves the primary constructor condition")
    (nelisp-eln-registration-objects-smoke--check
     (and failed-owner
          (memq failed-owner
                (mapcar #'cdr
                        nelisp-eln-registration-objects--pending-cleanups)))
     "failed memory cleanup retains its owner for retry")
    (nelisp-eln-registration-objects-retry-pending-cleanup)
    (nelisp-eln-registration-objects-smoke--check
     (not (memq failed-owner
                (mapcar #'cdr
                        nelisp-eln-registration-objects--pending-cleanups)))
     "pending memory cleanup retries successfully"))
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-decode
           nelisp-eln-registration-objects-smoke--activation 0)))
       'nelisp-eln-registration-objects-error)
   "wrong raw word rejected")
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-release-unit
           nelisp-eln-registration-objects-smoke--unit)))
       'nelisp-eln-registration-objects-error)
   "active activation blocks unit release")
  (setq nelisp-eln-registration-objects-smoke--other-handle
        (nelisp-eln-system-loader-open
         nelisp-eln-registration-objects-smoke--path))
  (setq nelisp-eln-registration-objects-smoke--other-unit
        (nelisp-eln-registration-objects-create-unit
         nelisp-eln-registration-objects-smoke--other-handle))
  (setq nelisp-eln-registration-objects-smoke--other-activation
        (nelisp-eln-registration-objects-begin-activation
         nelisp-eln-registration-objects-smoke--other-unit))
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-decode
           nelisp-eln-registration-objects-smoke--other-activation
           nelisp-eln-registration-objects-smoke--subr-word)))
       'nelisp-eln-registration-objects-error)
   "foreign activation cannot decode another owner's subr")
  (nelisp-eln-registration-objects-retire-activation
   nelisp-eln-registration-objects-smoke--other-activation)
  (nelisp-eln-registration-objects-release-unit
   nelisp-eln-registration-objects-smoke--other-unit)
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-unit-word
           nelisp-eln-registration-objects-smoke--other-unit)))
       'nelisp-eln-registration-objects-error)
   "released owner invalidates unit word access")
  (garbage-collect)
  (nelisp-eln-registration-objects-smoke--check
   (eq nelisp-eln-registration-objects-smoke--native
       (nelisp-eln-registration-objects-decode
        nelisp-eln-registration-objects-smoke--activation
        nelisp-eln-registration-objects-smoke--subr-word))
   "activation roots callable across GC"))

(defun nelisp-eln-registration-objects-smoke-retire-run ()
  "Retire transient views while a published callable still owns the module."
  (nelisp-eln-registration-objects-retire-activation
   nelisp-eln-registration-objects-smoke--activation)
  (setq nelisp-eln-registration-objects-smoke--activation nil)
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-decode
           (vector 'nelisp-eln-registration-activation
                   nelisp-eln-registration-objects-smoke--unit 'closed nil nil)
           nelisp-eln-registration-objects-smoke--subr-word)))
       'nelisp-eln-registration-objects-error)
   "retired activation rejects stale subr view")
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-release-unit
           nelisp-eln-registration-objects-smoke--unit)))
       'nelisp-eln-registration-objects-error)
   "live callable blocks unit release after activation")
  (nelisp-eln-registration-objects-smoke--check
   (= (funcall nelisp-eln-registration-objects-smoke--native) 17)
   "callable remains executable after transient view retirement"))

(defun nelisp-eln-registration-objects-smoke-release-run ()
  "Release unit and loader only after callable collection."
  (setq nelisp-eln-registration-objects-smoke--native nil
        nelisp-eln-registration-objects-smoke--subr-word nil
        nelisp-eln-registration-objects-smoke--view nil)
  (garbage-collect)
  (nelisp-eln-registration-objects-smoke--check
   (= (nelisp--native-subr-live-count
       nelisp-eln-registration-objects-smoke--module-id) 0)
   "weak callable lease disappears after references are dropped")
  (nelisp-eln-registration-objects-release-unit
   nelisp-eln-registration-objects-smoke--unit)
  (nelisp-eln-registration-objects-smoke--check
   (eq (nelisp-eln-registration-objects-smoke--condition
        (lambda ()
          (nelisp-eln-registration-objects-unit-word
           nelisp-eln-registration-objects-smoke--unit)))
       'nelisp-eln-registration-objects-error)
   "released owner rejects use")
  t)

(nelisp-eln-registration-objects-smoke-run)
(nelisp-eln-registration-objects-smoke-retire-run)
(nelisp-eln-registration-objects-smoke-release-run)
(princ "NELISP-ELN-REGISTRATION-OBJECTS-PASS\n")
nil

;;; nelisp-eln-registration-objects-smoke.el ends here
