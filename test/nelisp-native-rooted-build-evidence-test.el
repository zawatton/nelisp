;;; nelisp-native-rooted-build-evidence-test.el --- Active input capture controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-rooted-build-evidence)

(defun nelisp-native-rooted-build-evidence-test--units ()
  (list (list :name "rootstack.o" :sections (list (cons 'text (unibyte-string 195)))
              :symbols (list (list :name "ticket" :value 0 :section 'text :bind 'global :type 'func))
              :relocs nil)
        (list :name "arena-base.o" :sections (list (cons 'bss 4096))
              :symbols (list (list :name "nl_arena_base" :value 0 :section 'bss :bind 'global :type 'object)))))

(ert-deftest nelisp-root-active-build-captures-units-and-refuses-stale-output ()
  (let* ((root (make-temp-file "root-active-build-" t))
         (builder (expand-file-name "builder.el" root))
         (output (expand-file-name "generation" root)))
    (unwind-protect
        (progn
          (with-temp-file builder (insert ";;; Genuine test build source\n"))
          (let* ((result (nelisp-native-rooted-build-evidence-write
                          (nelisp-native-rooted-build-evidence-test--units) builder root output))
                 (manifest (json-read-file (plist-get result :manifest))))
            (should (equal (alist-get 'builder-source manifest) "builder.el"))
            (should (= (length (alist-get 'units manifest)) 2))
            (should-error (nelisp-native-rooted-build-evidence-write
                           (nelisp-native-rooted-build-evidence-test--units) builder root output))))
      (delete-directory root t))))

(ert-deftest nelisp-root-active-build-refuses-missing-duplicate-and-changing-source ()
  (let* ((root (make-temp-file "root-active-negative-" t))
         (builder (expand-file-name "builder.el" root))
         (output (expand-file-name "generation" root))
         (original (symbol-function 'nelisp-native-rooted-build-evidence-source-hash)))
    (unwind-protect
        (progn
          (should-error (nelisp-native-rooted-build-evidence-write
                         (nelisp-native-rooted-build-evidence-test--units) builder root output))
          (with-temp-file builder (insert ";;; Original builder\n"))
          (let* ((units (nelisp-native-rooted-build-evidence-test--units))
                 (duplicate (copy-tree (car units))))
            (plist-put duplicate :name "duplicate.o")
            (should-error (nelisp-native-rooted-build-evidence-write
                           (cons duplicate units) builder root output)))
          (delete-directory output t)
          (let ((changed nil))
            (cl-letf (((symbol-function 'nelisp-native-rooted-build-evidence-source-hash)
                       (lambda (path limit)
                         (let ((digest (funcall original path limit)))
                           (when (and (equal path builder) (not changed))
                             (setq changed t)
                             (with-temp-file builder (insert ";;; Changed builder\n")))
                           digest))))
              (should-error (nelisp-native-rooted-build-evidence-write
                             (nelisp-native-rooted-build-evidence-test--units) builder root output))))
          (should-not (file-exists-p (expand-file-name "active-build.json" output))))
      (delete-directory root t))))
