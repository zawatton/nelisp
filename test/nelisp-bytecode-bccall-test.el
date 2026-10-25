;;; nelisp-bytecode-bccall-test.el --- Call-arm cost cuts -*- lexical-binding: t; -*-

;; The in-loop call arm (opcodes 32..37) of wf_bytecode_w_fast_run.  These
;; tests check the exact defects that bccall removes at the lowering level;
;; native behaviour is covered by the differential corpus on the built binary.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-stackset-fast-test)

(defun bcc--call-arm ()
  (sst--find (sst--function 'wf_bytecode_w_fast_run)
             (lambda (form) (equal (car-safe form) 'let*)
               (and (eq (car-safe form) 'let*)
                    (equal (car-safe (car-safe (cadr form))) 'argc)
                    (equal (cadr (car (cadr form))) '(- raw 32))))))

(defun bcc--count (tree symbol)
  (cond ((eq tree symbol) 1)
        ((consp tree) (+ (bcc--count (car tree) symbol)
                         (bcc--count (cdr tree) symbol)))
        (t 0)))

(defun bcc--count-guard-calls (tree bases)
  "Count nl_string_read64 calls in TREE whose base operand is in BASES."
  (cond ((and (consp tree) (eq (car tree) 'nl_string_read64)
              (memq (cadr tree) bases)) 1)
        ((consp tree) (+ (bcc--count-guard-calls (car tree) bases)
                         (bcc--count-guard-calls (cdr tree) bases)))
        (t 0)))

(ert-deftest bcc-arm-found ()
  (should (bcc--call-arm)))

(ert-deftest bcc-no-string-guard-call-on-raw-frame-slots ()
  "Argument copy reads raw root-frame slots; no String-WORD guard call there."
  (let ((lowered (nelisp-heap-accessors-lower (bcc--call-arm))))
    (should (= 0 (bcc--count-guard-calls lowered '(src dst))))
    ;; The cold string-code branch (cw is not a String WORD) keeps its guard.
    (should (= 3 (bcc--count-guard-calls lowered '(cw))))))

(ert-deftest bcc-poll-is-a-statement-not-a-dead-binding ()
  (let ((arm (bcc--call-arm)))
    (should (= 1 (bcc--count arm 'wf_bytecode_poll)))
    (should (= 0 (bcc--count arm 'polled)))
    (should (= 0 (bcc--count arm 'pcret)))))

(ert-deftest bcc-protocol-footer-writes-unchanged ()
  "Six footer fields are still written in order at offsets 0,8,16,24,32,40."
  (let* ((arm (bcc--call-arm)) (offs nil))
    (sst--find arm
               (lambda (f)
                 (when (and (eq (car-safe f) 'ptr-write-u64) (eq (nth 1 f) 'cfoot))
                   (push (nth 2 f) offs))
                 nil))
    (should (equal (nreverse offs) '(0 8 16 24 32 40)))))

(provide 'nelisp-bytecode-bccall-test)
