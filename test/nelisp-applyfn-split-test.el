;;; nelisp-applyfn-split-test.el --- applyfn2: the reader applyfn's argv-entry units -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun nelisp-applyfn-split-test--source (keys)
  "A small reader applyfn shape: helpers, a frame leaf per key in KEYS (each with
one specialized helper, the last two sharing one), and the two routers."
  (let* ((leaf (lambda (k) (intern (format "nl_apply_builtin_frame_leaf_%d_w" k))))
         (routes (mapcar (lambda (k) (cons k `(,(funcall leaf k) nl_dk func_ptr args env name_ptr argc))) keys))
         (tree (nelisp-standalone--key-tree 'nl_dk routes 2))
         (shared 'h_shared__af1))
    `(seq
      (defun wf_helper (x) (+ x 1))
      ,@(cl-loop for k in keys for i from 0
                 append
                 (list `(defun ,(funcall leaf k) (nl_dk func_ptr args env name_ptr argc)
                          (,(if (>= i (- (length keys) 2)) shared (intern (format "h_%d__af%d" k i)))
                           (wf_helper argc)))
                       `(defun ,(if (>= i (- (length keys) 2)) (if (= i (1- (length keys))) 'h_unused__af9 shared)
                                  (intern (format "h_%d__af%d" k i)))
                            (n) (wf_helper n))))
      (defun nl_apply_builtin_frame_w (func_ptr args env argc)
        (let* ((name_ptr (nl_cons_car_word func_ptr)) (_pad (+ argc 1)))
          (let* ((nl_dk (nl_apply_builtin_name_key name_ptr))) ,tree)))
      (defun nl_apply_builtin_frame_sid_w (nl_dk func_ptr args env argc)
        (let* ((name_ptr (nl_cons_car_word func_ptr))) ,tree))
      (defun nelisp_apply_function_w (func_ptr args env _pad) (nl_apply_builtin_frame_w func_ptr args env 0)))))

(ert-deftest nelisp-applyfn-split-partitions-every-form-once ()
  (let* ((source (nelisp-applyfn-split-test--source '(20 31 47 52 66 70)))
         (split (nelisp-standalone--applyfn-split-units source 2))
         (names (lambda (u) (mapcar #'cadr (cdr u))))
         (all (apply #'append (mapcar names split))))
    (should (= (length split) 3))
    ;; Every logical form once, plus the second part's router.
    (should (equal (sort (mapcar #'symbol-name all) #'string<)
                   (sort (mapcar #'symbol-name (cons 'nl_apply_builtin_frame_part_1_w (mapcar #'cadr (cdr source))))
                         #'string<)))
    ;; The main unit keeps the helpers and the list entry, no argv entry.
    (should (equal (funcall names (car split)) '(wf_helper nelisp_apply_function_w)))
    ;; Part 0 starts with the routers; part 1 with its own router.
    (should (equal (cl-subseq (funcall names (nth 1 split)) 0 2)
                   '(nl_apply_builtin_frame_w nl_apply_builtin_frame_sid_w)))
    (should (eq (car (funcall names (nth 2 split))) 'nl_apply_builtin_frame_part_1_w))
    ;; A shared specialization goes with the first leaf that reaches it; one no
    ;; leaf reaches stays emitted, with the first leaf.
    (let ((p0 (funcall names (nth 1 split))) (p1 (funcall names (nth 2 split))))
      (should (memq 'h_unused__af9 p0))
      (should (or (and (memq 'h_shared__af1 p1) (memq 'nl_apply_builtin_frame_leaf_66_w p1))
                  (and (memq 'h_shared__af1 p0) (memq 'nl_apply_builtin_frame_leaf_66_w p0)))))))

(ert-deftest nelisp-applyfn-split-routes-every-key ()
  (let* ((keys '(20 31 47 52 66 70))
         (split (nelisp-standalone--applyfn-split-units (nelisp-applyfn-split-test--source keys) 2))
         (forms (apply #'append (mapcar #'cdr (cdr split))))
         (find (lambda (n) (cl-find-if (lambda (f) (eq (cadr f) n)) forms)))
         (sid (funcall find 'nl_apply_builtin_frame_sid_w))
         (part1 (funcall find 'nl_apply_builtin_frame_part_1_w)))
    ;; Evaluate the routers' key trees: each key reaches its own leaf, others 2.
    (cl-labels ((run (form key)
                  (pcase form
                    (`(if (= nl_dk ,k) ,then ,else) (if (= key k) (run then key) (run else key)))
                    (`(if (< nl_dk ,k) ,then ,else) (if (< key k) (run then key) (run else key)))
                    (`(nl_apply_builtin_frame_part_1_w . ,_) (run (nth 3 part1) key))
                    (`(,leaf nl_dk . ,_) leaf)
                    (_ form))))
      (dolist (k keys)
        (should (eq (run (nth 2 (nth 3 sid)) k)
                    (intern (format "nl_apply_builtin_frame_leaf_%d_w" k)))))
      (should (equal (run (nth 2 (nth 3 sid)) 21) 2))
      (should (equal (run (nth 2 (nth 3 sid)) 99) 2)))))

(ert-deftest nelisp-applyfn-split-without-leaves-is-identity ()
  (let ((source '(seq (defun a (x) x) (defun b (y) (a y)))))
    (should (equal (nelisp-standalone--applyfn-split-units source 2) (list source)))))

(ert-deftest nelisp-applyfn-split-deep-donor-private-data-scan ()
  (let ((max-lisp-eval-depth 1600)
        (body '(+ x 1)))
    (dotimes (_ 800) (setq body `(if (= x 0) 0 ,body)))
    (should-not (nelisp-standalone--inline-static-data-p body))
    (should (nelisp-standalone--inline-static-data-p
             (list 'seq body '(data-addr donor_private)))))
  ;; Preserve the old scanner's literal-tree behavior, including quotes and
  ;; dotted forms; the provider's candidate and ambiguity rules stay unchanged.
  (dolist (form '((quote (data-addr private)) (x . (data-addr private))))
    (should (nelisp-standalone--inline-static-data-p form))))

(ert-deftest nelisp-applyfn-split-donor-cycles-rejected ()
  (let ((max-lisp-eval-depth 1600))
    (dolist (shape '(car cdr mixed))
      (let ((node (list 'quote)) (other (list 'body)))
        (pcase shape
          ('car (setcar node node))
          ('cdr (setcdr node node))
          ('mixed (setcar node other) (setcdr other node)))
        (let ((condition (should-error (nelisp-standalone--inline-static-data-p node)
                                      :type 'error)))
          (should (equal condition '(error "Cyclic inline donor form"))))))))

(ert-deftest nelisp-applyfn-split-shared-deep-donor-graphs ()
  (let ((max-lisp-eval-depth 1600) (shared '(+ x 1)))
    (dotimes (_ 800) (setq shared `(if (= x 0) 0 ,shared)))
    (let ((dag (list 'seq shared shared)))
      (should-not (nelisp-standalone--inline-static-data-p dag))
      (should (eq (cadr dag) (caddr dag))))
    (let ((dag (list 'seq shared shared '(quote (data-addr private)))))
      (should (nelisp-standalone--inline-static-data-p dag))
      (should (eq (cadr dag) (caddr dag))))))

(ert-deftest nelisp-applyfn-split-donor-search-order ()
  (let ((direct (list 'data-addr)))
    (setcdr direct direct)
    (should (nelisp-standalone--inline-static-data-p direct)))
  (let ((cycle (list 'quote)))
    (setcdr cycle cycle)
    ;; Match the old car-before-cdr short circuit: later malformed input
    ;; is not inspected once a direct data reference has already been found.
    (should (nelisp-standalone--inline-static-data-p
             (list 'seq '(data-addr private) cycle)))
    (should-error (nelisp-standalone--inline-static-data-p
                   (list 'seq cycle '(data-addr private))) :type 'error)))

(provide 'nelisp-applyfn-split-test)
;;; nelisp-applyfn-split-test.el ends here
