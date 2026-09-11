;;; nelisp-native-unit-test.el --- stable native unit gate contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-unit)

(ert-deftest nelisp-native-unit/gate-uses-32-byte-stride-and-disp32 ()
  (let ((bytes (nelisp-native-unit--gate-bytes #x123456789abcdef0 63)))
    (should (= (length bytes) 32))
    ;; The four-byte little-endian displacement begins after the two-byte
    ;; indirect-jump opcode (offset 15); trailing bytes are gate padding.
    (should (equal (cl-subseq bytes 15 19) '(#x00 #x02 #x00 #x00)))))

(ert-deftest nelisp-native-unit/gate-address-is-unavailable-before-publish ()
  (let ((nelisp-native-unit--units nil))
    (should-error (nelisp-native-unit-address "u" "f"))))

(ert-deftest nelisp-native-unit/call-checks-arity-before-pointer-call ()
  (let ((nelisp-native-unit--units
         '(("u" :manifest (:native (:exports ((:name "f" :arity 2))))
                 :contract (("f" . 2)) :exports (("f" . 99))
                 :gates (("f" . 99))))))
    (should-error (nelisp-native-unit-call "u" "f" '(1)))))

(ert-deftest nelisp-native-unit/publisher-returns-cas-shape ()
  (let ((bytes (nelisp-native-unit--publisher-bytes)))
    (should (equal (list (nth 0 bytes) (nth 1 bytes) (nth 2 bytes)
                         (nth 3 bytes) (nth 4 bytes) (nth 5 bytes)
                         (nth 6 bytes) (nth 7 bytes))
                   '(#x48 #x89 #xf0 #xf0 #x48 #x0f #xb1 #x17)))
    (should (= (length bytes) 15))))

(defun nelisp-native-unit-test--unit (table)
  (list :unit-id "u" :control 100 :publisher 200
        :contract '(("f" . 1)) :gates '(("f" . 300))
        :binary-sha256 "bin" :table table))

(ert-deftest nelisp-native-unit/publish-rejects-captured-stale-table-and-consumes ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 20))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 10 :generation 2
            :table 400 :artifact "a" :artifact-hash "h"
            :source "s" :source-hash "h" :binary-sha256 "b"
            :created 1000)))
        (calls nil))
    (cl-letf (((symbol-function 'nelisp-native-unit--hash) (lambda (_) "h"))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'float-time) (lambda () 1001))
              ((symbol-function 'ptr-read-u64) (lambda (_ _) 20))
              ((symbol-function 'ptr-call)
               (lambda (publisher control expected next &rest _)
                 (setq calls (list publisher control expected next))
                 ;; Simulate the controller's current table being 20: the
                 ;; captured expected pointer 10 must fail the CAS.
                 (if (= expected 20) 1 0))))
      (let ((result (nelisp-native-unit-publish "c")))
        (should (eq (plist-get result :status) 'rejected))
        (should (equal calls '(200 100 10 400)))
        (should (= (ptr-read-u64 100 0) 20))
        (should-not (assoc "c" nelisp-native-unit--candidates))))))

(ert-deftest nelisp-native-unit/publish-rejects-tampered-input-before-cas ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 0))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 0 :generation 1
            :table 400 :artifact "a" :artifact-hash "expected"
            :source "s" :source-hash "expected" :binary-sha256 "b"
            :created 1000)))
        (calls 0))
    (cl-letf (((symbol-function 'nelisp-native-unit--hash)
               (lambda (path) (if (equal path "a") "expected" "tampered")))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'float-time) (lambda () 1001))
              ((symbol-function 'ptr-call) (lambda (&rest _) (setq calls (1+ calls)) 1)))
      (let ((result (nelisp-native-unit-publish "c")))
        (should (eq (plist-get result :status) 'rejected))
        (should (= calls 0))
        (should-not (assoc "c" nelisp-native-unit--candidates))))))

(ert-deftest nelisp-native-unit/expired-candidate-cannot-be-reused ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 0))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 0 :generation 1
            :table 400 :artifact "a" :artifact-hash "h"
            :source "s" :source-hash "h" :binary-sha256 "b"
            :created 0))))
    (cl-letf (((symbol-function 'float-time) (lambda () 1000)))
      (should (eq (plist-get (nelisp-native-unit-publish "c") :status)
                  'rejected))
      (should (eq (plist-get (nelisp-native-unit-publish "c") :status)
                  'rejected)))))

(ert-deftest nelisp-native-unit/status-is-a-copy-of-export-report ()
  (let ((nelisp-native-unit--units
         (list (cons "u" (nelisp-native-unit-test--unit 500)))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (_ _) 500)))
      (let ((report (nelisp-native-unit-status "u")))
        (setcdr (car (plist-get report :exports)) 99)
        (should (= (cdr (assoc "f"
                               (plist-get (cdr (assoc "u" nelisp-native-unit--units))
                                          :contract)))
                   1))))))

(ert-deftest nelisp-native-unit/stable-address-is-reused-for-a-unit ()
  (let ((nelisp-native-unit--units
         '(("u" :control 10 :contract (("f" . 1))
                 :gates (("f" . 99)) :generation 2))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 4096)))
      (should (= (nelisp-native-unit-address "u" "f")
                 (nelisp-native-unit-address "u" "f"))))))

(provide 'nelisp-native-unit-test)
