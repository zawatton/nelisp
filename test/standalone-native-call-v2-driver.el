;;; standalone-native-call-v2-driver.el --- Static provider diagnostic -*- lexical-binding: t; -*-

;; Addresses come only from the source-pinned static link, never a forged raw
;; loader contract or a substituted runtime proof/capability owner.
(defvar nelisp-call-diag-addresses nil)
(load (getenv "NELISP_CALL_ADDRESS_FILE") nil t)
(load (getenv "NELISP_CALL_CALLEES") nil t)

(defun nelisp-call-diag--address (name)
  (or (cdr (assoc name nelisp-call-diag-addresses))
      (error "Missing static diagnostic symbol %s" name)))

(defun nelisp-call-diag--assert (value label)
  (unless value (error "native-call-v2 assertion: %s" label)))

(defun nelisp-call-diag--pin (env ticket index object)
  (nelisp-call-diag--assert
   (> (nelisp--native-pin-copy-v2 env ticket index object) 0) "checked root copy"))

(defun nelisp-call-diag--slot (env ticket index)
  (ptr-call (nelisp-call-diag--address "nl_root_pin_slot_v2") env ticket index 0 0 0))

(defun nelisp-call-diag--value (env ticket index)
  (let ((slot (nelisp-call-diag--slot env ticket index)))
    (cond ((= (ptr-read-u64 slot 0) 0) nil)
          ((= (ptr-read-u64 slot 0) 2) (ptr-read-u64 slot 8))
          (t (nelisp--native-unbox-reference slot env (nelisp-call-diag--slot env ticket 0))))))

(defun nelisp-call-diag--same (env ticket left right)
  (nelisp--native-pin-eq-slots (nelisp-call-diag--slot env ticket left)
                              (nelisp-call-diag--slot env ticket right)))

(defun nelisp-call-diag--words (env ticket index)
  (let ((slot (nelisp-call-diag--slot env ticket index)))
    (list (ptr-read-u64 slot 0) (ptr-read-u64 slot 8)
          (ptr-read-u64 slot 16) (ptr-read-u64 slot 24))))

(defun nelisp-call-diag--case (function args action)
  "Run ACTION under one caller ticket, then prove stale/next-ticket recovery."
  (let* ((env (nelisp--native-env))
         (begin (nelisp-call-diag--address "nl_root_pin_begin_v2"))
         (reserve (nelisp-call-diag--address "nl_root_pin_reserve_v2"))
         (end (nelisp-call-diag--address "nl_root_pin_end_v2"))
         (ticket (ptr-call begin env 0 0 0 0 0))
         (marker 0) (answer nil))
    (nelisp-call-diag--assert (> ticket 0) "caller ticket begin")
    (unwind-protect
        (progn
          (dotimes (i 16)
            (let ((slot (ptr-call reserve env ticket 0 0 0 0)))
              (nelisp-call-diag--assert (> slot 0) "caller slot reserve")
              (when (= i 0) (setq marker slot))
              (nelisp-call-diag--assert (= slot (+ marker (* i 32))) "caller contiguity")))
          (nelisp-call-diag--pin env ticket 1 function)
          (let ((i 2))
            (dolist (arg args)
              (nelisp-call-diag--pin env ticket i arg) (setq i (1+ i))))
          (dolist (i '(7 8 9 10)) (nelisp-call-diag--pin env ticket i (list 'unchanged i)))
          (setq answer (funcall action env ticket))
          (nelisp-call-diag--assert (= (nelisp-call-diag--slot env ticket 0) marker)
                                   "caller ticket restored"))
      (nelisp-call-diag--assert (= (ptr-call end env ticket 0 0 0 0) 1) "caller ticket end"))
    (nelisp-call-diag--assert (= (nelisp-call-diag--slot env ticket 0) 0) "ticket stale after end")
    (let ((next (ptr-call begin env 0 0 0 0 0)))
      (nelisp-call-diag--assert (and (> next 0) (/= next ticket)) "fresh ticket generation")
      (nelisp-call-diag--assert (= (ptr-call reserve env next 0 0 0 0) marker) "root top restored")
      (nelisp-call-diag--assert (= (ptr-call end env next 0 0 0 0) 1) "fresh ticket end"))
    answer))

(defun nelisp-call-diag--call (env ticket base argc result exit)
  ;; The static witness returns 99 if either root domain leaks across the call.
  (ptr-call (nelisp-call-diag--address "nl_call_diag_entry") env ticket base argc result exit))

(defun nelisp-call-diag--gc-identity ()
  (let ((object (list (make-symbol "opaque") (list 'payload))))
    (nelisp-call-diag--case
     'nelisp-call-fixture-gc (list object)
     (lambda (env ticket)
       (nelisp-call-diag--assert (= (nelisp-call-diag--call env ticket 1 1 7 8) 0) "GC call status")
       (garbage-collect)
       (nelisp-call-diag--assert (nelisp-call-diag--same env ticket 2 7) "GC argument identity")
       (nelisp-call-diag--assert (eq (nelisp-call-diag--value env ticket 7) object) "GC result identity")))))

(defun nelisp-call-diag--exit (function args kind tag value)
  (nelisp-call-diag--case
   function args
   (lambda (env ticket)
     (nelisp-call-diag--pin env ticket 11 tag)
     (nelisp-call-diag--pin env ticket 12 value)
     (let ((before (nelisp-call-diag--words env ticket 7)))
       (nelisp-call-diag--assert (= (nelisp-call-diag--call env ticket 1 (length args) 7 8) 1)
                                "captured exit status")
       (nelisp-call-diag--assert (equal (nelisp-call-diag--words env ticket 7) before)
                                "exit preserves result")
       (garbage-collect)
       (nelisp-call-diag--assert (equal (nelisp-call-diag--words env ticket 8) (list 2 kind 0 0))
                                "tagged exit kind")
       (nelisp-call-diag--assert (nelisp-call-diag--same env ticket 9 11) "exit tag identity")
       (nelisp-call-diag--assert (nelisp-call-diag--same env ticket 10 12) "exit value identity")
       (let ((arena (ptr-read-u64 (nelisp-call-diag--address "nl_arena_base") 0)))
         (nelisp-call-diag--assert (= (ptr-read-u64 arena 16) 0) "exit stash transferred"))))))

(defun nelisp-call-diag--refusals ()
  (dolist (request '((2 1 0 7 8) (1 1 0 7 8) (0 16 0 7 8) (0 1 6 7 8)
                     (0 1 1 2 8) (0 1 0 8 8) (0 8 0 7 8)
                     (0 0 0 7 8) (0 255 5 7 8) (0 1 0 7 254)))
    (nelisp-call-diag--case
     'list nil
     (lambda (env ticket)
       (let ((before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10)))
             (foreign (car request)))
         (nelisp-call-diag--assert
          (= (nelisp-call-diag--call (if (= foreign 1) (1+ env) env)
                                    (if (= foreign 2) (1- ticket) ticket)
                                    (nth 1 request) (nth 2 request) (nth 3 request) (nth 4 request)) 2)
          "structural refusal status")
         (nelisp-call-diag--assert
          (equal before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10)))
          "refusal preserves output and triple")))))
  (dolist (callee (list nil t 7 (make-symbol "list") '(lambda () nil)))
    (nelisp-call-diag--case
     callee nil
     (lambda (env ticket)
       (let ((before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10))))
         (nelisp-call-diag--assert (= (nelisp-call-diag--call env ticket 1 0 7 8) 2) "callee tag refusal")
         (nelisp-call-diag--assert
          (equal before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10)))
          "callee refusal preserves output and triple")))))
  ;; Exhaust the region without modifying runtime control data. This leaves
  ;; enough room to begin a nested ticket but not to reserve its complete frame.
  (nelisp-call-diag--case
   'list nil
   (lambda (env ticket)
     (let ((reserve (nelisp-call-diag--address "nl_root_pin_reserve_v2"))
           (before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10))))
       (dotimes (_ 16364)
         (nelisp-call-diag--assert (> (ptr-call reserve env ticket 0 0 0 0) 0) "exhaustion reserve"))
       (nelisp-call-diag--assert (= (nelisp-call-diag--call env ticket 1 0 7 8) 2) "root exhaustion refusal")
       (nelisp-call-diag--assert
        (equal before (mapcar (lambda (i) (nelisp-call-diag--words env ticket i)) '(7 8 9 10)))
        "root exhaustion preserves output and triple")))))

(defun nelisp-call-diag-run ()
  (let* ((arena (ptr-read-u64 (nelisp-call-diag--address "nl_arena_base") 0))
         (variant (or (getenv "NELISP_CALL_VARIANT") "good")))
    ;; Refuse the unqualified domain; do not alter collector or worker state.
    (nelisp-call-diag--assert (= (ptr-read-u64 arena 152) 0) "nonmoving collector domain")
    (nelisp-call-diag--assert (= (ptr-read-u64 (nelisp-call-diag--address "nl_thread_registry") 0) 0)
                             "single-thread domain")
    (dolist (name '(nelisp-call-fixture-vm0 nelisp-call-fixture-vm1 nelisp-call-fixture-vm2
                    nelisp-call-fixture-vm3 nelisp-call-fixture-vm4 nelisp-call-fixture-vm5
                    nelisp-call-fixture-gc nelisp-call-fixture-signal nelisp-call-fixture-throw
                    nelisp-call-fixture-quit))
      (nelisp-call-diag--assert (byte-code-function-p (symbol-function name)) "source-free VM callee"))
    (cond
     ((equal variant "no-staging") (nelisp-call-diag--gc-identity))
     ((equal variant "no-exit-copy")
      (let ((tag (make-symbol "exit-tag")) (value (list 'exit-value)))
        (nelisp-call-diag--exit 'throw (list tag value) 2 tag value)))
     (t
      (dotimes (argc 6)
        (let ((args nil))
          (dotimes (i argc) (setq args (append args (list (list 'argument i)))))
          (dolist (callee (list 'list (intern (format "nelisp-call-fixture-vm%d" argc))))
            (nelisp-call-diag--case
             callee args
             (lambda (env ticket)
               (nelisp-call-diag--assert (= (nelisp-call-diag--call env ticket 1 argc 7 8) 0) "normal status")
               (garbage-collect)
               (let ((result (nelisp-call-diag--value env ticket 7)) (expected args))
                 (while expected
                   (nelisp-call-diag--assert (eq (car result) (car expected)) "argument order and identity")
                   (setq result (cdr result) expected (cdr expected)))
                 (nelisp-call-diag--assert (null result) "proper result and nil"))
               (dolist (i '(8 9 10))
                 (nelisp-call-diag--assert (equal (nelisp-call-diag--words env ticket i) '(0 0 0 0))
                                          "normal clears triple")))))))
      (nelisp-call-diag--gc-identity)
      (let ((tag (make-symbol "exit-tag")) (value (list (list 'exit-payload))))
        (nelisp-call-diag--exit 'signal (list 'error value) 1 'error value)
        (nelisp-call-diag--exit 'nelisp-call-fixture-signal (list value) 1 'error value)
        (nelisp-call-diag--exit 'throw (list tag value) 2 tag value)
        (nelisp-call-diag--exit 'nelisp-call-fixture-throw (list tag value) 2 tag value)
        (nelisp-call-diag--exit 'signal '(quit nil) 1 'quit nil)
        (nelisp-call-diag--exit 'nelisp-call-fixture-quit nil 1 'quit nil))
      (nelisp-call-diag--refusals)
      (princ "native-call-v2: PASS (builtin/VM 0..5, GC identity, signal/throw/quit, nil, refusals, exhaustion, ticket/root recovery)\n")))
    t))

(nelisp-call-diag-run)
;;; standalone-native-call-v2-driver.el ends here
