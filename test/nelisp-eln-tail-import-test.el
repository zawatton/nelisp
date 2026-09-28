;;; nelisp-eln-tail-import-test.el --- GNU unary import admission tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defun nelisp-eln-tail-import-test--artifact-code (path symbol-fragment)
  "Read the function bytes named by SYMBOL-FRAGMENT from ELF PATH."
  (let* ((bytes (nelisp-eln-system-loader--read-file path))
         (elf (nelisp-eln-system-loader--file-symbols path bytes))
         (entry (catch 'found
                  (maphash (lambda (name value)
                             (when (string-match-p symbol-fragment name)
                               (throw 'found value)))
                           (plist-get elf :symbols))
                  nil))
         (value (plist-get entry :value))
         (load (catch 'found
                 (dolist (row (plist-get elf :loads))
                   (when (and (/= 0 (logand (nth 4 row) 1))
                              (>= value (nth 0 row))
                              (< (- value (nth 0 row)) (nth 2 row)))
                     (throw 'found row)))))
         (offset (+ (nth 1 load) (- value (nth 0 load))))
         (code (substring bytes offset (+ offset (plist-get entry :size)))))
    (list code value elf)))

(defun nelisp-eln-tail-import-test--analyze-artifact (code vaddr abi)
  "Run authenticated tail analysis over CODE at VADDR for ABI."
  (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
             (lambda (_) '(:bias 1048576)))
            ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
             (lambda (&rest _) 12345)))
    (nelisp-eln-native-subr--tail-import-analysis
     'handle (list nil nil nil (+ 1048576 vaddr)) code abi)))

(ert-deftest nelisp-eln-tail-import-admits-genuine-increment-artifact ()
  (let ((path (expand-file-name
               "~/.cache/tmp/eln-gnu-arithmetic-probe/overlay/eln/31.1-ba35c031/gnu-increment-77ce8652-cfc975a9.eln")))
    (skip-unless (file-exists-p path))
    (pcase-let* ((`(,code ,value ,_) (nelisp-eln-tail-import-test--artifact-code
                                      path "nelisp_gnu_increment_0"))
                 (analysis (nelisp-eln-tail-import-test--analyze-artifact
                            code value "ba35c031")))
      (should (equal (plist-get (car (plist-get analysis :imports)) :slot) 1301))
      (should (equal (nth 2 (plist-get analysis :descriptor)) '1+)))))

(ert-deftest nelisp-eln-tail-import-admits-genuine-decrement-artifact ()
  (let ((path (expand-file-name
               "~/.cache/tmp/eln-gnu-decrement-sonnet/overlay/eln/31.1-ba35c031/gnu-decrement-65bb3f2f-636770ba.eln")))
    (skip-unless (file-exists-p path))
    (pcase-let* ((`(,code ,value ,_) (nelisp-eln-tail-import-test--artifact-code
                                      path "nelisp_gnu_decrement_0"))
                 (analysis (nelisp-eln-tail-import-test--analyze-artifact
                            code value "ba35c031")))
      (should (equal (plist-get (car (plist-get analysis :imports)) :slot) 1300))
      (should (equal (nth 2 (plist-get analysis :descriptor)) '1-))
      (should (nelisp-eln-native-subr--tail-constants-match-p
               code (plist-get analysis :descriptor)))
      ;; Calibrate the negative control: the existing CFG verifier alone admits
      ;; this code when allowed slots include 1300; only descriptor constants
      ;; bind it to 1- rather than a mismatched 1+ descriptor.
      (let ((verified-copy (copy-sequence code)))
        (aset verified-copy 33 2)
        (aset verified-copy 34 0)
        (aset verified-copy 35 0)
        (aset verified-copy 36 0)
      (should (eq (plist-get (nelisp-eln-tail-code-analyze
                              verified-copy value '(1300))
                            :safe) t))
      (let ((plus (nelisp-eln-native-subr--tail-descriptor "ba35c031" 1301)))
        (should-not (nelisp-eln-native-subr--tail-constants-match-p code plus))
        ;; Even with the authorized decrement slot, changing its bound word
        ;; must fail the descriptor-bound admission check.
        (let ((unknown-constant (copy-sequence code)))
          (aset unknown-constant 9 (logxor (aref unknown-constant 9) 1))
          (should-not
           (nelisp-eln-native-subr--tail-constants-match-p
            unknown-constant (plist-get analysis :descriptor))))
        (should-not (nelisp-eln-native-subr--tail-descriptor "unknown" 1300)))))))

(ert-deftest nelisp-eln-tail-import-compares-signed-descriptor-bound-as-words ()
  "Match NeLisp's signed descriptor representation to x86's unsigned word."
  (let* ((path (expand-file-name
                "~/.cache/tmp/eln-gnu-decrement-sonnet/overlay/eln/31.1-ba35c031/gnu-decrement-65bb3f2f-636770ba.eln")))
    (skip-unless (file-exists-p path))
    (pcase-let* ((`(,code ,_ ,_) (nelisp-eln-tail-import-test--artifact-code
                                  path "nelisp_gnu_decrement_0"))
                 (signed-descriptor (copy-sequence
                                     (nelisp-eln-native-subr--tail-descriptor
                                      "ba35c031" 1300))))
      (setcar (nthcdr 4 signed-descriptor) -2305843009213693952)
      (should (nelisp-eln-native-subr--tail-constants-match-p
               code signed-descriptor)))))

(ert-deftest nelisp-eln-tail-import-rejects-unknown-slot-and-abi ()
  (let* ((code (apply #'unibyte-string
                      '(#x8d #x47 #xfe #xa8 #x03 #x75 #x29
                        #x48 #xba #xff #xff #xff #xff #xff #xff #xff #x1f
                        #x48 #x89 #xf8 #x48 #xc1 #xf8 #x02 #x48 #x39 #xd0
                        #x74 #x13 #x48 #x8d #x04 #x85 #x06 #x00 #x00 #x00 #xc3
                        #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
                        #x48 #x8b #x05 #xa1 #x2e #x00 #x00 #x48 #x8b #x00
                        #xff #xa0 #xa8 #x28 #x00 #x00)))
         (increment (apply #'unibyte-string
                           '(#x8d #x47 #xfe #xa8 #x03 #x75 #x29
                             #x48 #xba #xff #xff #xff #xff #xff #xff #xff #x1f
                             #x48 #x89 #xf8 #x48 #xc1 #xf8 #x02 #x48 #x39 #xd0
                             #x74 #x13 #x48 #x8d #x04 #x85 #x06 #x00 #x00 #x00 #xc3
                             #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
                             #x48 #x8b #x05 #xa1 #x2e #x00 #x00 #x48 #x8b #x00
                             #xff #xa0 #xa8 #x28 #x00 #x00)))
         (wrong-slot (copy-sequence increment)))
    (aset wrong-slot 60 #xb0)
    ;; Slot 1302 is admitted by CFG only when explicitly added, proving this
    ;; rejection comes from the descriptor table rather than malformed bytes.
    (should (eq (plist-get (nelisp-eln-tail-code-analyze
                            wrong-slot #x1100 '(1302)) :safe) t))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
               (lambda (_) '(:bias 0)))
              ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
               (lambda (&rest _) 12345)))
      (should-not (nelisp-eln-native-subr--tail-import-analysis
                   'handle (list nil nil nil #x1100) wrong-slot "ba35c031"))
      (should-not (nelisp-eln-native-subr--tail-import-analysis
                   'handle (list nil nil nil #x1100) increment "unknown")))))

(ert-deftest nelisp-eln-tail-import-rejects-unverified-and-forged-got ()
  (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
             (lambda (_handle) '(:bias 4096)))
            ((symbol-function 'nelisp-eln-tail-code-analyze)
             (lambda (_code _vaddr slots)
               (list :safe t :imports
                     (list (list :slot (if (equal slots '(1301)) 1301 999)
                                 :got-vaddr 16344)))))
            ((symbol-function 'nelisp-eln-system-loader-validate-root-indirection)
             (lambda (&rest _args) nil)))
    (should-not
     (nelisp-eln-native-subr--tail-import-analysis
      'handle (list nil nil nil 4352) "code"))))

(ert-deftest nelisp-eln-tail-import-captures-canonical-descriptors ()
  (let ((primitive '(builtin 1+)))
    (cl-letf (((symbol-function 'symbol-function)
               (lambda (symbol)
                 (if (eq symbol '1+) primitive nil))))
      (should (equal (nelisp-eln-native-subr--canonical-builtin
                      '("ba35c031" 1301 1+ 1 #x1fffffffffffffff 6)) primitive))
      (setq primitive (list 'builtin '1+ 'extra))
      (should-error (nelisp-eln-native-subr--canonical-builtin
                     '("ba35c031" 1301 1+ 1 #x1fffffffffffffff 6))
                    :type 'nelisp-eln-native-subr-error)
      (setq primitive (cons 'builtin nil))
      (setcdr primitive primitive)
      (should-error (nelisp-eln-native-subr--canonical-builtin
                     '("ba35c031" 1301 1+ 1 #x1fffffffffffffff 6))
                    :type 'nelisp-eln-native-subr-error))))

(ert-deftest nelisp-eln-registration-import-table-retains-both-callback-slots ()
  (let ((writes nil) (base-writes nil))
    (cl-letf (((symbol-function 'ptr-write-u64)
               (lambda (address offset value)
                 (push (list address offset value) writes)))
              ((symbol-function 'nelisp-eln-abi-write-word)
               (lambda (address offset value)
                 (push (list address offset value) base-writes)))
              ((symbol-function 'nelisp-eln-callable-import-entry-address)
               (lambda () 777)))
      (should
       (nelisp-eln-registration--install-import-table
        '(:link-table-address 9000 :link-address 8000 :link-table-slots 1302
          :tail-imports (:safe t :imports ((:slot 1301 :got-vaddr 16344))))
        666))
      (should (member (list 9000 (* 8 1030) 666) writes))
      (should (member (list 9000 (* 8 1301) 777) writes))
      (should (equal (car base-writes) (list 8000 0 9000))))))

(ert-deftest nelisp-eln-registration-import-table-keeps-old-width-for-self-emitter ()
  (let ((writes 0))
    (cl-letf (((symbol-function 'ptr-write-u64)
               (lambda (&rest _args) (setq writes (1+ writes))))
              ((symbol-function 'nelisp-eln-abi-write-word) (lambda (&rest _) t)))
      (should
       (nelisp-eln-registration--install-import-table
        '(:link-table-address 9000 :link-address 8000 :link-table-slots 1031)
        666))
      (should (= writes 1032)))))

(ert-deftest nelisp-eln-tail-import-requires-live-retained-table-lease ()
  (let* ((handle 'handle)
         (cap (list 'cap handle "gnu_increment" 5000 "digest" nil 8 'token))
         (table-owner 'table)
         (owner (make-vector nelisp-eln-registration--owner-size nil))
         (lease nil)
         (nelisp-eln-registration--owners nil)
         (nelisp-eln-registration--active-owner nil))
    (aset owner 1 (vector nil handle))
    (aset owner 7 '(:abi-hash "ba35c031"))
    (aset owner 8 cap)
    (aset owner 12 table-owner)
    (setq lease (vector nelisp-eln-native-subr--tail-lease-marker handle owner
                        table-owner 9000 8000 777
               '(:safe t :proof :forward-cfg
                 :imports ((:slot 1300 :got-vaddr 16344)))))
    (aset owner 17 lease)
    (setq nelisp-eln-registration--owners (list owner)
          nelisp-eln-registration--active-owner owner)
    (cl-letf (((symbol-function 'nl-ffi-memory-address) (lambda (_) 9000))
              ((symbol-function 'ptr-read-u64)
               (lambda (address offset)
                 (if (= address 8000) 9000
                   (if (= offset (* 8 1300)) 777 0))))
              ((symbol-function 'nelisp-eln-callable-import-entry-address)
               (lambda () 777)))
      (should (nelisp-eln-native-subr--tail-lease-valid-p
               lease handle cap t))
      (aset owner 7 '(:abi-hash "other"))
      (should-not (nelisp-eln-native-subr--tail-lease-valid-p
                   lease handle cap))
      (aset owner 7 '(:abi-hash "ba35c031"))
      (aset owner 17 nil)
      (should-not (nelisp-eln-native-subr--tail-lease-valid-p
                   lease handle cap)))))

(ert-deftest nelisp-eln-native-subr-wires-tail-bridge-after-proof ()
  (let ((captured nil)
        (cap (list 'cap 'handle "gnu_increment" 5000 "digest" nil 8 'token))
        (implementation (lambda (x) (+ x 1))))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (&rest _) cap))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
              (lambda (&rest _) "12345678"))
              ((symbol-function 'nelisp-eln-native-subr--tail-import-analysis)
               (lambda (&rest _) '(:safe t)))
              ((symbol-function 'nelisp-eln-native-subr--canonical-builtin)
               (lambda (_) implementation))
              ((symbol-function 'nelisp-eln-native-subr--tail-lease-valid-p)
               (lambda (lease &rest _) lease))
              ((symbol-function 'nelisp-eln-native-subr--unary-bridge)
               (lambda (&rest _) :wrong-bridge))
              ((symbol-function 'nelisp-eln-system-loader-module-id)
               (lambda (_handle) 'module))
              ((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
               (lambda (_capability) t))
              ((symbol-function 'nelisp--native-subr-create)
               (lambda (&rest args) (setq captured args) args))
              ((symbol-function 'nelisp-eln-callable-import--call-unary)
               (lambda (capability primitive argument)
                 (list :import capability primitive argument))))
      (should-error (nelisp-eln-native-subr-create 'handle "gnu_increment")
                    :type 'nelisp-eln-native-subr-error)
      (should-not captured)
      (let* ((nelisp-eln-native-subr--tail-import-context 'mock-lease)
             (result (nelisp-eln-native-subr-create 'handle "gnu_increment")))
        (should (= (nth 4 captured) 1))
        (should (equal (funcall (nth 3 captured) 17)
                       (list :import cap implementation 17)))
        (should (eq (car result) cap))))))

(provide 'nelisp-eln-tail-import-test)

;;; nelisp-eln-tail-import-test.el ends here
