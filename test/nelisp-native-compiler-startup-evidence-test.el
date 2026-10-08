;;; nelisp-native-compiler-startup-evidence-test.el --- Constructor derivation controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-compiler-startup-evidence)
(defconst nelisp-native-compiler-startup-evidence-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-native-compiler-startup-evidence-test--derive (relative generator)
  (nelisp-native-compiler-startup-evidence--rewrite
   (nelisp-native-compiler-startup-evidence--rename
    (nelisp-native-compiler-startup-evidence--forms
     (expand-file-name relative nelisp-native-compiler-startup-evidence-test--root))) generator))

(ert-deftest nelisp-native-compiler-startup-evidence-separate-registry ()
  (let* ((forms (nelisp-native-compiler-startup-evidence--proof-api
                 (nelisp-native-compiler-startup-evidence-test--derive
                  "templates/nelisp-native-rooted-abi-proof.el.in" nil)))
         (text (prin1-to-string forms)))
    (should (string-match-p "nelisp-native-compiler-runtime-proof-metadata" text))
    (should (string-match-p "compiler-constructor-memory-v1" text))
    (should (string-match-p "compiler-runtime-v1" text))
    (should (string-match-p "nl_native_cons_v2" text))
    (should-not (string-match-p "nelisp-native-rooted-abi-proof" text))
    (should-not (string-match-p "ticket-gc-memory-v1" text))
    (should-not (string-match-p "(ticket gc)" text))))

(ert-deftest nelisp-native-compiler-startup-evidence-generator-source-pins ()
  (let ((text (prin1-to-string
               (nelisp-native-compiler-startup-evidence-test--derive
                "lisp/nelisp-native-rooted-startup-evidence.el" t))))
    (should (string-match-p "nelisp-compiler-constructor-prelink-v1" text))
    (should (string-match-p ":derivation-sources" text))
    (should (string-match-p ":active-manifest-sha256" text))
    (should (string-match-p ":compiler-exports" text))))

(ert-deftest nelisp-native-compiler-startup-evidence-startup-seal-does-not-issue ()
  (let ((forms (nelisp-native-compiler-startup-evidence--proof-api
                (nelisp-native-compiler-startup-evidence-test--derive
                 "templates/nelisp-native-rooted-abi-proof.el.in" nil))) found)
    (cl-labels ((find-definition (node)
                  (when (consp node)
                    (if (and (eq (car node) 'defun)
                             (eq (cadr node) 'nelisp-native-compiler-runtime-proof-owners-valid-p))
                        (setq found node)
                      (find-definition (car node)) (find-definition (cdr node))))))
      (find-definition forms))
    (should found)
    (let ((text (prin1-to-string found)))
      (should (string-match-p "captured-eligibility" text))
      (should (string-match-p "--protocol-p" text))
      (should-not (string-match-p "proof-create\\|--verify\\|--runtime\\|binary-sha256" text)))))

(ert-deftest nelisp-native-compiler-startup-evidence-template-drift-refused ()
  (let ((forms (nelisp-native-compiler-startup-evidence--forms
                (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in"
                                  nelisp-native-compiler-startup-evidence-test--root))))
    (cl-labels ((mutate (value)
                  (cond ((eq value 'ticket-gc-memory-v1) 'counterfeit-domain)
                        ((consp value) (cons (mutate (car value)) (mutate (cdr value))))
                        (t value))))
      (should-error (nelisp-native-compiler-startup-evidence--rewrite
                     (nelisp-native-compiler-startup-evidence--rename (mutate forms)) nil)))))

(ert-deftest nelisp-native-compiler-startup-evidence-root-drift-refused ()
  (should-error (nelisp-native-compiler-startup-evidence--rewrite
                 '((defun counterfeit () '("nl_native_cons_v2"))) nil)))

(ert-deftest nelisp-native-compiler-startup-evidence-registry-shape-refused ()
  (should-error (nelisp-native-compiler-startup-evidence--proof-api
                 '((defun counterfeit () t)))))

(ert-deftest nelisp-native-compiler-startup-evidence-canonical-loader-three-gates ()
  (should (nelisp-native-compiler-startup-evidence--loader-template-valid
           nelisp-native-compiler-startup-evidence-test--root))
  (should (equal (nelisp-native-compiler-startup-evidence--source-path
                  'nelisp-native-compiler-constructor-loader)
                 "templates/nelisp-native-load-constructor-startup.el.in")))

(defun nelisp-native-compiler-startup-evidence-test--original-serializer ()
  "Install the independent, pre-repair printer reference."
  (eval '
(defun nelisp-serializer-test-original (value)
  "Serialize bounded acyclic evidence without graph labels or truncation."
  (let ((work (list (cons value 0))) (seen (make-hash-table :test #'eq))
        (nodes 0) (bytes 0))
    (while work
      (let* ((item (pop work)) (object (car item)) (depth (cdr item)))
        (setq nodes (+ nodes 1))
        (unless (and (<= nodes 65536) (<= depth 64))
          (error "Root evidence node/depth bound exceeded"))
        (cond
         ((consp object)
          (when (gethash object seen) (error "Cyclic/shared root evidence rejected"))
          (puthash object t seen)
          (push (cons (car object) (+ depth 1)) work)
          (push (cons (cdr object) depth) work))
         ((stringp object)
          (setq bytes (+ bytes (string-bytes object)))
          (unless (and (<= (string-bytes object) 4096) (<= bytes 1048576))
            (error "Root evidence string bound exceeded")))
         ((or (symbolp object) (integerp object)) nil)
         (t (error "Invalid root evidence type")))))
    (let ((print-circle nil) (print-length nil) (print-level nil)
          (print-escape-newlines t) (print-escape-control-characters t)
          (print-escape-nonascii nil) (print-escape-multibyte nil)
          (print-quoted nil) (print-gensym nil))
      (prin1-to-string value))))
 t))

(defun nelisp-native-compiler-startup-evidence-test--serializers ()
  (nelisp-native-compiler-startup-evidence-test--original-serializer)
  (eval (nelisp-native-compiler-startup-evidence-serializer-form
         'nelisp-serializer-test-fast) t))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-bytes ()
  (nelisp-native-compiler-startup-evidence-test--serializers)
  (dolist (value (list nil t 42 -7 'symbol "quote\"\nλ\001"
                       '(a (b . c) 3) (list (list 'same) (list 'same))
                       (list "λ" (unibyte-string 255))
                       (list (unibyte-string 255) "λ")))
    (let ((old (nelisp-serializer-test-original value))
          (new (nelisp-serializer-test-fast value)))
      (should (equal old new))
      (should (equal (secure-hash 'sha256 old) (secure-hash 'sha256 new))))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-refusals ()
  (nelisp-native-compiler-startup-evidence-test--serializers)
  (let* ((shared (list 'x)) (cycle (list 'x))
         (deep 'leaf) (long (make-list 32768 nil)))
    (setcdr cycle cycle)
    (dotimes (_ 65) (setq deep (list deep)))
    (dolist (value (list (list shared shared) cycle deep long
                         (make-string 4097 ?x)
                         (make-list 257 (make-string 4096 ?x)) [1] 1.5))
      (should (equal (should-error (nelisp-serializer-test-original value))
                     (should-error (nelisp-serializer-test-fast value)))))
  (should (equal (nelisp-serializer-test-original (make-list 32767 nil))
                 (nelisp-serializer-test-fast (make-list 32767 nil))))))

(defvar nelisp-serializer-test-ascii-calls 0)

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-call-local-ascii ()
  (let ((form (nelisp-native-compiler-startup-evidence-serializer-form
               'nelisp-serializer-test-ascii)))
    (cl-labels ((count-checks (node)
                  (cond ((or (equal node '(funcall ascii-safe object nil))
                             (equal node '(funcall ascii-safe text nil)))
                         (list 'progn
                               '(setq nelisp-serializer-test-ascii-calls
                                      (1+ nelisp-serializer-test-ascii-calls))
                               node))
                        ((consp node) (cons (count-checks (car node))
                                           (count-checks (cdr node))))
                        (t node))))
      (eval (count-checks form) t)))
  (nelisp-native-compiler-startup-evidence-test--original-serializer)
  (let ((value (list "repeat" (copy-sequence "repeat")))
        (nelisp-serializer-test-ascii-calls 0))
    (dotimes (index 2)
      (should (equal (nelisp-serializer-test-original value)
                     (nelisp-serializer-test-ascii value)))
      (should (= nelisp-serializer-test-ascii-calls (1+ index))))
    ;; Classification from the prior invocation cannot survive byte mutation.
    (aset (car value) 0 ?\n)
    (should (equal (nelisp-serializer-test-original value)
                   (nelisp-serializer-test-ascii value)))
    (should (= nelisp-serializer-test-ascii-calls 3))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-owner-refusal ()
  (nelisp-native-compiler-startup-evidence-test--serializers)
  (dolist (symbol '(sxhash-eq memq make-vector aref aset logand nth consp car cdr
                   apply concat setcar setcdr))
    (let ((owner (symbol-function symbol)) refused)
      (unwind-protect
          (progn (fset symbol (lambda (&rest _) nil))
                 (condition-case nil (nelisp-serializer-test-fast nil)
                   (error (setq refused t))))
        (fset symbol owner))
      (should refused)))
  (let ((owner (symbol-function 'sxhash-eq)))
    (unwind-protect
        (progn (fset 'sxhash-eq (lambda (_) 0))
               (should-error
                (eval (nelisp-native-compiler-startup-evidence-serializer-form
                       'nelisp-serializer-test-counterfeit) t)))
      (fset 'sxhash-eq owner))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-preserves-input ()
  (nelisp-native-compiler-startup-evidence-test--serializers)
  (let* ((left (list 'a "text")) (right (cons 'b 'c))
         (value (list left right)) (snapshot (copy-tree value))
         (left-tail (cdr left)) (value-tail (cdr value)))
    (should (equal (nelisp-serializer-test-original value)
                   (nelisp-serializer-test-fast value)))
    (should (equal value snapshot))
    (should (eq (car value) left))
    (should (eq (cdr left) left-tail))
    (should (eq (cdr value) value-tail))
    (should (eq (cadr value) right))
    (should-error (nelisp-serializer-test-fast (list left left)))
    (should (equal value snapshot)))
  (let ((cycle (list 'a)))
    (setcdr cycle cycle)
    (should-error (nelisp-serializer-test-fast cycle))
    (should (eq (cdr cycle) cycle))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-collisions ()
  (let ((form (nelisp-native-compiler-startup-evidence-serializer-form
               'nelisp-serializer-test-collision)))
    ;; Force a single bucket without replacing the genuine native identity owner.
    (cl-labels ((mutate (node)
                  (cond ((equal node 4095) 0)
                        ((consp node) (cons (mutate (car node)) (mutate (cdr node))))
                        (t node))))
      (eval (mutate form) t)))
  (nelisp-native-compiler-startup-evidence-test--original-serializer)
  (let ((value (list (list 'same) (list 'same))))
    (should (equal (nelisp-serializer-test-original value)
                   (nelisp-serializer-test-collision value))))
  (let ((shared (list 'same)))
    (should-error (nelisp-serializer-test-collision (list shared shared)))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-derived-publication ()
  (let ((forms (nelisp-native-compiler-startup-evidence-test--derive
                "templates/nelisp-native-rooted-abi-proof.el.in" nil)) sealed)
    (cl-labels ((visit (node)
                  (when (consp node)
                    (if (and (eq (car node) 'let*)
                             (equal (car (cadr node)) '(lookup (symbol-function 'symbol-function))))
                        (push node sealed)
                      (visit (car node)) (visit (cdr node))))))
      (visit forms))
    (should (= (length sealed) 1))
    (should (equal (car sealed)
                   (nelisp-native-compiler-startup-evidence-serializer-form
                    'nelisp-native-compiler-runtime-proof--data-bytes)))
    ;; Execute the exact derived publication under a diagnostic symbol only.
    (let ((publication (copy-tree (car sealed))))
      (cl-labels ((rename (node)
                    (cond ((eq node 'nelisp-native-compiler-runtime-proof--data-bytes)
                           'nelisp-serializer-test-derived)
                          ((consp node) (cons (rename (car node)) (rename (cdr node))))
                          (t node))))
        (eval (rename publication) t)))
    (should (equal (nelisp-serializer-test-derived '(a (b . c))) "(a (b . c))"))))

(ert-deftest nelisp-native-compiler-startup-evidence-serializer-transform-count ()
  (let* ((forms (nelisp-native-compiler-startup-evidence--rename
                 (nelisp-native-compiler-startup-evidence--forms
                  (expand-file-name "templates/nelisp-native-rooted-abi-proof.el.in"
                                    nelisp-native-compiler-startup-evidence-test--root))))
         (target (cl-find-if
                  (lambda (form) (and (eq (car-safe form) 'let*)
                                      (equal (car (cadr form)) '(lookup (symbol-function 'symbol-function)))))
                  forms)))
    (should target)
    (should-error (nelisp-native-compiler-startup-evidence--rewrite
                   (delq target (copy-sequence forms)) nil))
    (should-error (nelisp-native-compiler-startup-evidence--rewrite
                   (cons target forms) nil))
    (let ((generator (nelisp-native-compiler-startup-evidence--rename
                      (nelisp-native-compiler-startup-evidence--forms
                       (expand-file-name "lisp/nelisp-native-rooted-startup-evidence.el"
                                         nelisp-native-compiler-startup-evidence-test--root)))))
      (should-error (nelisp-native-compiler-startup-evidence--rewrite
                     (cons target generator) t)))))

(ert-deftest nelisp-native-compiler-startup-evidence-repr-boundary ()
  (let ((had (fboundp 'nelisp--repr)) (saved (and (fboundp 'nelisp--repr)
                                                (symbol-function 'nelisp--repr)))
        (taken-symbol 'nelisp-serializer-test-taken))
    (unwind-protect
        (progn
          ;; A host native stand-in tests selection only. The real native driver
          ;; separately qualifies the genuine runtime formatter's byte semantics.
          (fset 'nelisp--repr (symbol-function 'prin1-to-string))
          (let ((form (nelisp-native-compiler-startup-evidence-serializer-form
                       'nelisp-serializer-test-repr)))
            (cl-labels ((instrument (node)
                          (cond ((equal node '(funcall native-repr value))
                                 '(progn (set 'nelisp-serializer-test-taken t)
                                         (funcall native-repr value)))
                                ((consp node) (cons (instrument (car node))
                                                    (instrument (cdr node))))
                                (t node))))
              (eval (instrument form) t)))
          (nelisp-native-compiler-startup-evidence-test--original-serializer)
          (dolist (value (list "" "quote\" and slash\\" 'identifier :keyword
                               (make-symbol "uninterned") 0 9223372036854775807
                               '(a . b)))
            (set taken-symbol nil)
            (should (equal (nelisp-serializer-test-original value)
                           (nelisp-serializer-test-repr value)))
            (should (symbol-value taken-symbol)))
          (dolist (value (list "\n" "\001" "\177" "λ" (unibyte-string 255)
                               (intern "") (intern "123") (intern ".")
                               (intern "a b") (intern "a\\b") (intern "ſ")
                               -1 -9223372036854775808 9223372036854775808
                               '(builtin x) '(closure nil x)
                               '(outer (closure nil x)) '(quote (x . y))
                               '(function identity) '(backquote (a b)) '(comma x)
                               '(comma-at x) '(outer (quote x))
                               (list (intern "`") 'x) (list (intern ",") 'x)
                               (list (intern ",@") 'x)))
            (set taken-symbol nil)
            (should (equal (nelisp-serializer-test-original value)
                           (nelisp-serializer-test-repr value)))
            (should-not (symbol-value taken-symbol)))
          (fset 'nelisp--repr (symbol-function 'identity))
          (should-error (nelisp-serializer-test-repr '(a))))
      (if had (fset 'nelisp--repr saved) (fmakunbound 'nelisp--repr)))))

(ert-deftest nelisp-native-compiler-startup-evidence-byte-helper-seals ()
  (let ((saved (and (fboundp 'nelisp--string-search)
                    (symbol-function 'nelisp--string-search))))
    (unwind-protect
        (progn
          (fset 'nelisp--string-search (symbol-function 'string-search))
          (eval (nelisp-native-compiler-startup-evidence-serializer-form
                 'nelisp-serializer-test-byte-owners) t)
          (dolist (symbol '(string-as-multibyte string-as-unibyte multibyte-string-p
                           nelisp--string-search unibyte-string))
            (let ((owner (symbol-function symbol)) refused)
              (unwind-protect
                  (progn (fset symbol (symbol-function 'identity))
                         (condition-case nil (nelisp-serializer-test-byte-owners nil)
                           (error (setq refused t))))
                (fset symbol owner))
              (should refused)))
          (let ((form (nelisp-native-compiler-startup-evidence-serializer-form
                       'nelisp-serializer-test-byte-table)))
            (setcar (cdr (assq 'controls (cadr form))) '(list "corrupted"))
            (eval form t)
            (should-error (nelisp-serializer-test-byte-table nil)))
          ;; Boot absence is permanent; later availability cannot select helpers.
          (fmakunbound 'nelisp--string-search)
          (eval (nelisp-native-compiler-startup-evidence-serializer-form
                 'nelisp-serializer-test-byte-absent) t)
          (fset 'nelisp--string-search (symbol-function 'identity))
          (should (equal (nelisp-serializer-test-byte-absent '(a)) "(a)")))
      (if saved (fset 'nelisp--string-search saved)
        (fmakunbound 'nelisp--string-search)))))

(defvar nelisp-proof-test-hashes 0)
(defvar nelisp-proof-test-mutate nil)
(defvar nelisp-native-compiler-runtime-proof--expected)

(defun nelisp-native-compiler-startup-evidence-test--find (forms head name)
  "Find one generated node without evaluating the native boot program."
  (let (found)
    (cl-labels ((walk (node)
                 (when (consp node)
                   (if (and (eq (car node) head)
                            (if (eq head 'let) (equal (car (cadr node)) (car name))
                              (equal (cadr node) name)))
                       (push node found)
                     (walk (car node)) (walk (cdr node))))))
      (walk forms))
    (unless (= (length found) 1) (error "Ambiguous fixture node: %S" name))
    (car found)))

(defun nelisp-native-compiler-startup-evidence-test--transaction (callback &optional forms)
  "Execute generated registry logic with host-only memory transport fixtures."
  (let* ((forms (or forms (nelisp-native-compiler-startup-evidence--proof-api
                          (nelisp-native-compiler-startup-evidence-test--derive
                           "templates/nelisp-native-rooted-abi-proof.el.in" nil))))
         (evidence (list :layout (list :size 8) :domain 'compiler-runtime-v1
                         :operation-eligibility '(constructor)
                         :abi-sha256 (make-string 64 ?a)
                         :active-manifest-sha256 (make-string 64 ?b)
                         :compiler-exports '(("entry" func 3))
                         :functions '((:name "entry" :size 8))))
         (nelisp-native-compiler-runtime-proof--expected evidence)
         (pin (secure-hash 'sha256 (prin1-to-string evidence)))
         (nelisp-proof-test-hashes 0) (nelisp-proof-test-mutate nil)
         (names '(nelisp-native-compiler-runtime-proof--eligible-p
                  nelisp-native-compiler-runtime-proof-dependency-context
                  nelisp-native-compiler-runtime-proof-create
                  nelisp-native-compiler-runtime-proof-valid-p
                  nelisp-native-compiler-runtime-proof-metadata
                  nelisp-native-compiler-runtime-proof-owners-valid-p)))
    (cl-labels ((pins (node)
                  (cond ((equal node "NELISP_BUILD_EVIDENCE_PIN") pin)
                        ((consp node) (cons (pins (car node)) (pins (cdr node))))
                        (t node))))
      (setq forms (pins forms)))
    (let ((saved (mapcar (lambda (name) (cons name (and (fboundp name) (symbol-function name)))) names)))
      (unwind-protect
          (cl-letf (((symbol-function 'nelisp-native-compiler-runtime-evidence) (lambda () evidence))
                    ((symbol-function 'nelisp-native-compiler-runtime-proof--data-hash)
                     (lambda (value)
                       (when (eq value evidence) (cl-incf nelisp-proof-test-hashes))
                       (secure-hash 'sha256 (prin1-to-string value))))
                    ((symbol-function 'nelisp-native-compiler-runtime-proof--verify)
                     (lambda (&optional _record)
                       (when nelisp-proof-test-mutate
                         (setcar (plist-get evidence :layout) :changed))
                       (let ((symbols (make-hash-table :test #'equal)))
                         (puthash "entry" '(:address 4096) symbols)
                         (list :symbols symbols))))
                    ((symbol-function 'nelisp-native-compiler-runtime-proof--runtime) (lambda (_) 42))
                    ((symbol-function 'nelisp--native-env) (lambda () 42))
                    ((symbol-function 'nelisp-bytecode-compiler-input-dialect)
                     (lambda () '(:runtime-evidence standalone-build-verified)))
                    ((symbol-function 'nelisp-bytecode-compiler-input-native-package-runtime-context) (lambda () nil))
                    ((symbol-function 'nelisp-native-raw-file-dependency-context) (lambda () nil))
                    ((symbol-function 'nelisp-native-load-sha256-dependency-context) (lambda () nil))
                    ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () (make-string 64 ?c))))
            (eval `(let ((captured-all-owners
                          (list (cons 'nelisp-native-compiler-runtime-proof--data-hash
                                      (symbol-function 'nelisp-native-compiler-runtime-proof--data-hash))))
                         (captured-module-owners nil) (captured-primitive-owners nil)
                         (captured-eligibility nil) (captured-checked-context nil)
                         (captured-evidence ',evidence) (captured-evidence-hash ,pin)
                         (captured-evidence-owner (symbol-function 'nelisp-native-compiler-runtime-evidence))
                         (captured-eq (symbol-function 'eq)) (captured-car (symbol-function 'car))
                         (captured-cdr (symbol-function 'cdr))
                         (captured-symbol-function (symbol-function 'symbol-function)))
                     ,(nelisp-native-compiler-startup-evidence-test--find
                       forms 'defun 'nelisp-native-compiler-runtime-proof--eligible-p)
                     ,(nelisp-native-compiler-startup-evidence-test--find
                       forms 'defun 'nelisp-native-compiler-runtime-proof-dependency-context)
                     ,(nelisp-native-compiler-startup-evidence-test--find
                       forms 'let '((issued-records (make-hash-table :test #'eq))))
                     (setq captured-eligibility
                           (symbol-function 'nelisp-native-compiler-runtime-proof--eligible-p))
                     (setq captured-all-owners
                           (append captured-all-owners
                                   (mapcar (lambda (name) (cons name (symbol-function name)))
                                           '(nelisp-native-compiler-runtime-proof--eligible-p
                                             nelisp-native-compiler-runtime-proof-dependency-context))))
                     ,@(let (binding)
                         (cl-labels ((walk (node)
                                       (when (consp node)
                                         (if (and (eq (car node) 'setq)
                                                  (eq (cadr node) 'captured-checked-context))
                                             (push node binding)
                                           (walk (car node)) (walk (cdr node))))))
                           (walk forms)) binding)) t)
            (funcall callback evidence))
        (dolist (entry saved)
          (if (cdr entry) (fset (car entry) (cdr entry)) (fmakunbound (car entry))))))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-hash-count ()
  (nelisp-native-compiler-startup-evidence-test--transaction
   (lambda (_)
     (let ((proof (nelisp-native-compiler-runtime-proof-create)))
       (should (= nelisp-proof-test-hashes 2))
       (setq nelisp-proof-test-hashes 0)
       (should (nelisp-native-compiler-runtime-proof-valid-p proof))
       (should (= nelisp-proof-test-hashes 1))))))

(ert-deftest nelisp-native-compiler-startup-evidence-atomic-metadata ()
  (nelisp-native-compiler-startup-evidence-test--transaction
   (lambda (evidence)
     (let ((proof (nelisp-native-compiler-runtime-proof-create)))
       (setq nelisp-proof-test-hashes 0)
       (let ((record (nelisp-native-compiler-runtime-proof-valid-p proof nil :metadata)))
         (should (= nelisp-proof-test-hashes 1))
         (should (eq (plist-get record :domain) 'compiler-runtime-v1))
         (aset (plist-get record :abi-sha256) 0 ?z)
         (should (equal (plist-get (nelisp-native-compiler-runtime-proof-metadata proof) :abi-sha256)
                        (plist-get evidence :abi-sha256))))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p (make-symbol (symbol-name proof)) nil :metadata))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p proof '(:size 9) :metadata))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p proof nil :unrecognized))
       (aset (plist-get evidence :abi-sha256) 0 ?z)
       (should-not (nelisp-native-compiler-runtime-proof-valid-p proof nil :metadata))))))

(ert-deftest nelisp-native-compiler-startup-evidence-atomic-capability-provider-controls ()
  (require 'nelisp-native-compiler-runtime-capability)
  (let ((record
         (list :version 1 :domain 'compiler-runtime-v1 :operation-eligibility '(constructor)
               :abi-sha256 (make-string 64 ?a) :binary-sha256 (make-string 64 ?b)
               :active-manifest-sha256 (make-string 64 ?c)
               :exports
               (mapcar (lambda (spec)
                         (append (list :name (car spec) :kind (cadr spec) :address 4096)
                                 (if (eq (cadr spec) 'data) (list :size (nth 2 spec))
                                   (list :size 32 :arity (nth 2 spec)))))
                       nelisp-native-compiler-runtime-capability--constructor-exports))))
    (dolist (result (list nil t record))
      (let ((valid-calls 0) (metadata-calls 0))
        (cl-letf (((symbol-function 'nelisp-native-compiler-runtime-proof-create) (lambda () 'proof))
                  ((symbol-function 'nelisp-native-compiler-runtime-proof-valid-p)
                   (lambda (_proof &optional _layout mode)
                     (setq valid-calls (1+ valid-calls))
                     (if (eq mode :metadata) result (and result t))))
                  ((symbol-function 'nelisp-native-compiler-runtime-proof-metadata)
                   (lambda (_) (setq metadata-calls (1+ metadata-calls)) record))
                  ((symbol-function 'nelisp-native-compiler-runtime-proof-owners-valid-p) (lambda () t))
                  ((symbol-function 'nelisp-runtime-reload-contract-hash) (lambda () (make-string 64 ?a)))
                  ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () (make-string 64 ?b)))
                  ((symbol-function 'nelisp-native-compiler-runtime-capability-p)
                   (symbol-function 'nelisp-native-compiler-runtime-capability-p))
                  ((symbol-function 'nelisp-native-compiler-runtime-capability-owner-p)
                   (symbol-function 'nelisp-native-compiler-runtime-capability-owner-p)))
          (load (locate-library "nelisp-native-compiler-runtime-capability.el") nil t)
          (should (eq (not (null (nelisp-native-compiler-runtime-capability-p '(constructor))))
                      (eq result record)))
          (should (= valid-calls 1))
          (should (= metadata-calls 0))
          (should-not (nelisp-native-compiler-runtime-capability-p '(numeric)))
          (cl-letf (((symbol-function 'nelisp-native-compiler-runtime-proof-valid-p) (lambda (&rest _) record)))
            (should-not (nelisp-native-compiler-runtime-capability-p '(constructor)))))))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-interior-mutations ()
  (dolist (kind '(list string))
    (nelisp-native-compiler-startup-evidence-test--transaction
     (lambda (evidence)
       (let ((proof (nelisp-native-compiler-runtime-proof-create)))
         (should (nelisp-native-compiler-runtime-proof-valid-p proof))
         (if (eq kind 'list) (setcar (plist-get evidence :layout) :changed)
           (aset (plist-get evidence :abi-sha256) 0 ?z))
         (should-not (nelisp-native-compiler-runtime-proof-valid-p proof))
         (should-error (nelisp-native-compiler-runtime-proof-metadata proof)))))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-hostile-tokens ()
  (nelisp-native-compiler-startup-evidence-test--transaction
   (lambda (evidence)
     (let* ((proof (nelisp-native-compiler-runtime-proof-create))
            (record (nelisp-native-compiler-runtime-proof-metadata proof)))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p (make-symbol (symbol-name proof))))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p 'forged))
       (should-not (nelisp-native-compiler-runtime-proof-valid-p proof '(:size 9)))
       (aset (plist-get record :abi-sha256) 0 ?z)
       (aset (plist-get (car (plist-get record :exports)) :name) 0 ?z)
       (should (equal (plist-get (nelisp-native-compiler-runtime-proof-metadata proof) :abi-sha256)
                      (plist-get evidence :abi-sha256)))))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-owner-refusal ()
  (nelisp-native-compiler-startup-evidence-test--transaction
   (lambda (_)
     (let ((proof (nelisp-native-compiler-runtime-proof-create)))
       (cl-letf (((symbol-function 'nelisp-native-compiler-runtime-proof--data-hash) (lambda (_) "forged")))
         (should-not (nelisp-native-compiler-runtime-proof-valid-p proof))
         (should-error (nelisp-native-compiler-runtime-proof-create)))
       (cl-letf (((symbol-function 'nelisp-native-compiler-runtime-proof--eligible-p) (lambda () t)))
         (should-not (nelisp-native-compiler-runtime-proof-valid-p proof)))))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-issuance-change ()
  (nelisp-native-compiler-startup-evidence-test--transaction
   (lambda (_)
     (setq nelisp-proof-test-mutate t)
     (should-error (nelisp-native-compiler-runtime-proof-create)))))

(ert-deftest nelisp-native-compiler-startup-evidence-transaction-full-definition-drift ()
  (let ((original (symbol-function 'nelisp-native-compiler-startup-evidence--checked-context)))
   (cl-letf (((symbol-function 'nelisp-native-compiler-startup-evidence--checked-context)
             (symbol-function 'identity)))
    (let ((forms (nelisp-native-compiler-startup-evidence--proof-api
                  (nelisp-native-compiler-startup-evidence-test--derive
                   "templates/nelisp-native-rooted-abi-proof.el.in" nil))))
      (dolist (name '(nelisp-native-compiler-runtime-proof--eligible-p
                      nelisp-native-compiler-runtime-proof-dependency-context
                      nelisp-native-compiler-runtime-proof-create
                      nelisp-native-compiler-runtime-proof-valid-p))
        (let* ((copy (copy-tree forms))
               (definition (nelisp-native-compiler-startup-evidence-test--find copy 'defun name)))
          (setcdr (nthcdr 3 definition) '(t))
          (should-error (funcall original copy))))))))

(ert-deftest nelisp-native-compiler-startup-evidence-real-render-pins ()
  (unless (getenv "NELISP_PRELINK_FIXTURE")
    (ert-skip "NELISP_PRELINK_FIXTURE must identify a captured prelink unit directory"))
  (let* ((root nelisp-native-compiler-startup-evidence-test--root)
         (fixture (expand-file-name (getenv "NELISP_PRELINK_FIXTURE")))
         (directory (make-temp-file "nelisp-render-test-" t))
         (closure (expand-file-name "render-closure.json" directory))
         (template (expand-file-name "render-template.el.in" directory))
         (nelisp-native-compiler-startup-evidence--source-root root)
         (forms (nelisp-native-compiler-startup-evidence--proof-api
                 (nelisp-native-compiler-startup-evidence-test--derive
                  "templates/nelisp-native-rooted-abi-proof.el.in" nil)))
         (capture (list :manifest (expand-file-name "active-build.json" fixture)
                        :metadata (expand-file-name "active-unit-metadata.json" fixture)
                        :generated-data (expand-file-name "generated-data-owner.json" fixture))))
    (unwind-protect
      (progn
    ;; Reuse actual captured units; renderer authenticates their manifest/data hashes.
    (should (= 0 (call-process
                  "python3" nil nil nil "-c"
                  (concat "import importlib.util,pathlib,json,hashlib,sys; r,f,o=map(pathlib.Path,sys.argv[1:]); "
                          "s=importlib.util.spec_from_file_location('p',r/'scripts/nelisp-native-compiler-constructor-prelink.py'); "
                          "m=importlib.util.module_from_spec(s); s.loader.exec_module(m); "
                          "v=m.prelink.prove(f/'active-unit-metadata.json',f,m.ROOTS,f/'generated-data-owner.json',max_functions=192); "
                          "v.update(domain='nelisp-compiler-constructor-prelink-v1',active_manifest_sha256=hashlib.sha256((f/'active-build.json').read_bytes()).hexdigest()); "
                          "o.write_text(json.dumps(v))") root fixture closure)))
    (dolist (form (nelisp-native-compiler-startup-evidence-test--derive
                   "lisp/nelisp-native-rooted-startup-evidence.el" t)) (eval form t))
    (cl-labels ((parse (source)
                 (with-temp-buffer
                   (insert source) (emacs-lisp-mode) (check-parens)
                   (goto-char (point-min)) (forward-comment (point-max))
                   (let (value)
                     (while (< (point) (point-max))
                       (push (read (current-buffer)) value)
                       (forward-comment (point-max)))
                     (nreverse value))))
               (write-template (value)
                 (with-temp-file template
                   (let ((print-length nil) (print-level nil))
                     (dolist (form value) (prin1 form (current-buffer)) (insert "\n")))))
               (render ()
                 (nelisp-native-compiler-derived-startup-evidence-render
                  capture closure (nelisp-native-rooted-build-evidence-source-hash closure 4194304)
                  template (car (nelisp-native-load-rooted-production-contract))))
               (pins (node)
                 (let (found)
                   (cl-labels ((walk (item)
                                (when (consp item)
                                  (when (and (eq (car item) 'equal)
                                             (eq (cadr item) 'captured-evidence-hash)
                                             (stringp (nth 2 item))) (push item found))
                                  (walk (car item)) (walk (cdr item)))))
                     (walk node)) found)))
      (write-template forms)
      (let* ((result (render))
             (source (plist-get result :startup-source))
             (emitted (parse source))
             (actual (pins emitted)))
        (should-error (parse (concat source "\n(defun truncated (")))
        (should (= (length actual) 2))
        (dolist (pin actual) (should (equal (nth 2 pin) (plist-get result :evidence-sha256)))))
      (dolist (mode '(missing third tampered))
        (let* ((copy (copy-tree forms)) (pin (car (pins copy))))
          (pcase mode
            ('missing (setcar (cddr pin) 'missing-pin))
            ('third (push '(equal captured-evidence-hash "NELISP_BUILD_EVIDENCE_PIN") copy))
            ('tampered (setcar (cdr pin) 'counterfeit-evidence-hash)))
          (write-template copy)
          (should-error (render))))))
      (delete-directory directory t))))

(provide 'nelisp-native-compiler-startup-evidence-test)

(defvar nelisp-serializer-test-symbol-checks 0)

(ert-deftest nelisp-native-compiler-startup-evidence-symbol-memo ()
  (let ((saved (and (fboundp 'nelisp--repr) (symbol-function 'nelisp--repr))))
    (unwind-protect
        (progn
          (fset 'nelisp--repr (symbol-function 'prin1-to-string))
          (let ((form (nelisp-native-compiler-startup-evidence-serializer-form
                       'nelisp-serializer-test-memo)))
            (cl-labels ((instrument (node)
                          (cond ((equal node '(funcall ascii-safe (symbol-name object) t))
                                 '(progn (when (memq object (list 'memo-safe (intern "memo unsafe")))
                                           (setq nelisp-serializer-test-symbol-checks
                                                 (1+ nelisp-serializer-test-symbol-checks)))
                                         (funcall ascii-safe (symbol-name object) t)))
                                ((consp node) (cons (instrument (car node)) (instrument (cdr node))))
                                (t node))))
              (eval (instrument form) t)))
          (nelisp-native-compiler-startup-evidence-test--original-serializer)
          (dolist (case (list (cons '(memo-safe memo-safe) 1)
                             (cons (list (intern "memo unsafe") (intern "memo unsafe")) 2)
                             (cons (list 'memo-safe (intern "memo unsafe") 'memo-safe) 2)
                             (cons (list (intern "memo unsafe") 'memo-safe 'memo-safe) 2)))
            ;; The same operation repeated starts with an empty admission list.
            (dotimes (_ 2)
              (setq nelisp-serializer-test-symbol-checks 0)
              (should (equal (nelisp-serializer-test-memo (car case))
                             (nelisp-serializer-test-original (car case))))
              (should (= nelisp-serializer-test-symbol-checks (cdr case))))))
      (if saved (fset 'nelisp--repr saved) (fmakunbound 'nelisp--repr)))))

(ert-deftest nelisp-native-compiler-startup-evidence-template-shared-serializer ()
  (let* ((forms (nelisp-native-compiler-startup-evidence--forms
                 nelisp-native-compiler-startup-evidence--serializer-source))
         (publication (cl-find-if
                       (lambda (form) (and (eq (car-safe form) 'let*)
                                           (equal (car (cadr form))
                                                  '(lookup (symbol-function 'symbol-function)))))
                       forms)))
    (should (equal publication (nelisp-native-compiler-startup-evidence-serializer-form
                               'nelisp-native-rooted-abi-proof--data-bytes)))
    (should (equal (nelisp-native-compiler-startup-evidence--rename publication)
                   (nelisp-native-compiler-startup-evidence-serializer-form
                    'nelisp-native-compiler-runtime-proof--data-bytes)))))

(ert-deftest nelisp-native-compiler-startup-evidence-template-serializer-shape-refused ()
  (let* ((forms (nelisp-native-compiler-startup-evidence--forms
                 nelisp-native-compiler-startup-evidence--serializer-source))
         (publication (cl-find-if
                       (lambda (form) (and (eq (car-safe form) 'let*)
                                           (equal (car (cadr form))
                                                  '(lookup (symbol-function 'symbol-function)))))
                       forms))
         (path (make-temp-file "serializer-shape-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file path (prin1 (delq publication (copy-sequence forms)) (current-buffer)))
          (let ((nelisp-native-compiler-startup-evidence--serializer-source path))
            (should-error (nelisp-native-compiler-startup-evidence-serializer-form 'missing)))
          (let ((unknown (copy-tree publication)))
            (setcar (last unknown) '(setq self nil))
            (should-error (nelisp-native-compiler-startup-evidence--rewrite
                           (nelisp-native-compiler-startup-evidence--rename
                            (cons unknown (delq publication (copy-sequence forms)))) nil))
            (with-temp-file path (prin1 (list unknown) (current-buffer)))
            (let ((nelisp-native-compiler-startup-evidence--serializer-source path))
              (should-error (nelisp-native-compiler-startup-evidence-serializer-form 'unknown)))))
      (delete-file path))))

(defvar nelisp-elf-test-evidence nil)
(defvar nelisp-elf-test-search-taken nil)

(defun nelisp-native-compiler-startup-evidence-test--elf-form (name &optional wrong)
  "Extract the exact template ELF publication with diagnostic dependencies."
  (let* ((forms (nelisp-native-compiler-startup-evidence--forms
                 nelisp-native-compiler-startup-evidence--serializer-source))
         (form (cl-find-if (lambda (node) (and (eq (car-safe node) 'let*)
                                              (eq (caar (cadr node)) 'elf-lookup))) forms)))
    (unless form (error "ELF publication absent"))
    (cl-labels ((rename (node)
                  (cond ((eq node 'nelisp-native-rooted-abi-proof--elf) name)
                        ((eq node 'nelisp-native-rooted-abi-proof--read) 'nelisp-elf-test-read)
                        ((eq node 'nelisp-native-rooted-abi-proof--u) 'nelisp-elf-test-u)
                        ((eq node 'nelisp-native-rooted-abi-proof--expected) 'nelisp-elf-test-evidence)
                        ((equal node '(funcall elf-search elf-zero strings name-offset))
                         `(progn (setq nelisp-elf-test-search-taken t)
                                 ,(if wrong
                                      '(let ((end (funcall elf-search elf-zero strings name-offset)))
                                         (and end (+ end 1)))
                                    node)))
                        ((consp node) (cons (rename (car node)) (rename (cdr node))))
                        (t node))))
      (rename form))))

(defun nelisp-native-compiler-startup-evidence-test--elf-fixture (strings &optional offsets)
  "Return exact bounded ELF read windows for synthetic symbol names."
  (let ((header (string-as-unibyte (make-string 64 0)))
        (sections (string-as-unibyte (make-string 192 0)))
        (entries (string-as-unibyte (make-string (* 24 (length (or offsets '(1 3)))) 0))))
    (cl-labels ((write-u (bytes offset value width)
                  (dotimes (i width) (aset bytes (+ offset i) (logand (lsh value (* -8 i)) 255)))))
      (dotimes (i 6) (aset header i (aref (unibyte-string 127 69 76 70 2 1) i)))
      (dolist (field '((16 2 2) (18 62 2) (40 64 8) (58 64 2) (60 3 2)))
        (apply #'write-u header field))
      (dolist (field `((68 2 4) (88 256 8) (96 ,(length entries) 8) (104 2 4) (120 24 8)
                      (132 3 4) (152 512 8) (160 ,(length strings) 8)))
        (apply #'write-u sections field))
      (cl-loop for offset in (or offsets '(1 3)) for index from 0 do
               (write-u entries (* index 24) offset 4)
               (write-u entries (+ (* index 24) 6) 1 2)
               (write-u entries (+ (* index 24) 8) (+ 4096 index) 8)))
    (list (cons 0 header) (cons 64 sections) (cons 256 entries) (cons 512 strings))))

(defun nelisp-native-compiler-startup-evidence-test--elf-install ()
  (let ((forms (nelisp-native-compiler-startup-evidence--forms
                nelisp-native-compiler-startup-evidence--serializer-source)))
    (eval (cl-subst 'nelisp-elf-test-u 'nelisp-native-rooted-abi-proof--u
                 (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun)
                                               (eq (cadr form) 'nelisp-native-rooted-abi-proof--u))) forms)) t))
  (fmakunbound 'nelisp--string-search)
  (eval (nelisp-native-compiler-startup-evidence-test--elf-form 'nelisp-elf-test-fallback) t)
  (fset 'nelisp--string-search (symbol-function 'string-search))
  (eval (nelisp-native-compiler-startup-evidence-test--elf-form 'nelisp-elf-test-native) t)
  (eval (nelisp-native-compiler-startup-evidence-test--elf-form 'nelisp-elf-test-wrong t) t))

(ert-deftest nelisp-native-compiler-startup-evidence-elf-search-paths ()
  (let ((saved (and (fboundp 'nelisp--string-search) (symbol-function 'nelisp--string-search)))
        (nelisp-elf-test-evidence '(:functions ((:name "a") (:name "b")))))
    (unwind-protect
        (progn
          (nelisp-native-compiler-startup-evidence-test--elf-install)
          (dolist (case (list (list (unibyte-string 0 97 0 98 0) '(1 3) nil)
                             (list (unibyte-string 0 97) '(1) "Unbounded ELF name")
                             (list (concat (unibyte-string 0) (make-string 257 ?x) (unibyte-string 0))
                                   '(1) "Unbounded ELF name")
                             (list (unibyte-string 0 97 0) '(16777216) "Invalid ELF symbol name")
                             (list (unibyte-string 0 97 0) '(1 1) "Ambiguous ELF symbol")))
            (let ((windows (nelisp-native-compiler-startup-evidence-test--elf-fixture
                            (nth 0 case) (nth 1 case))))
              (cl-letf (((symbol-function 'nelisp-elf-test-read)
                         (lambda (offset size)
                           (let ((bytes (cdr (assq offset windows))))
                             (unless (= (length bytes) size) (error "Bad fixture window")) bytes))))
                (let ((nelisp-elf-test-search-taken nil) fallback native)
                  (if (nth 2 case)
                      (progn
                        (setq fallback (should-error (nelisp-elf-test-fallback)))
                        (should-not nelisp-elf-test-search-taken)
                        (setq native (should-error (nelisp-elf-test-native)))
                        (should (equal fallback native))
                        (should (equal fallback (list 'error (nth 2 case)))))
                    (setq fallback (nelisp-elf-test-fallback))
                    (should-not nelisp-elf-test-search-taken)
                    (setq native (nelisp-elf-test-native))
                    (should (equal (car fallback) (car native)))
                    (should (= (hash-table-count (cadr native)) 2))
                    (maphash (lambda (key value) (should (equal value (gethash key (cadr native)))))
                             (cadr fallback))
                    (maphash (lambda (key value) (should (equal value (gethash key (cadr fallback)))))
                             (cadr native))
                    (let ((wrong (nelisp-elf-test-wrong)))
                      (should (equal (car native) (car wrong)))
                      (should-not (equal (gethash "a" (cadr native))
                                         (gethash "a" (cadr wrong))))))
                  (unless (equal (nth 2 case) "Invalid ELF symbol name")
                    (should nelisp-elf-test-search-taken)))))))
      (if saved (fset 'nelisp--string-search saved) (fmakunbound 'nelisp--string-search)))))

(ert-deftest nelisp-native-compiler-startup-evidence-elf-search-owner-refusal ()
  (let ((saved (and (fboundp 'nelisp--string-search) (symbol-function 'nelisp--string-search))))
    (unwind-protect
        (progn
          (nelisp-native-compiler-startup-evidence-test--elf-install)
          (fset 'nelisp--string-search (symbol-function 'identity))
          (should (equal (should-error (nelisp-elf-test-native))
                         '(error "Root proof ELF byte owner changed: nelisp--string-search"))))
      (if saved (fset 'nelisp--string-search saved) (fmakunbound 'nelisp--string-search)))))
