;;; nelisp-native-lean-test.el --- Tier 0 dependency and fence controls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-native-cache)

(defconst nelisp-native-lean-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-native-lean-cache-load-excludes-optimizers ()
  (let ((root nelisp-native-lean-test--root))
    (with-temp-buffer
      (should
       (= 0 (call-process
             invocation-name nil t nil "-Q" "--batch" "-L" (expand-file-name "lisp" root)
             "-L" (expand-file-name "src" root) "--eval"
             "(progn (require 'nelisp-native-cache) (dolist (f '(nelisp-aot-compiler nelisp-bytecode-ir nelisp-bytecode-native-rooted-cfg-plan nelisp-bytecode-native-rooted-cfg-emit)) (when (featurep f) (error \"Tier 1 loaded: %s\" f))))"))))))

(ert-deftest nelisp-native-lean-preparation-refuses-optimizer ()
  (let ((features (cons 'nelisp-aot-compiler features)))
    (should-error (nelisp-native-cache-prepare-cold-template))))

(ert-deftest nelisp-native-lean-fence-retains-independent-data ()
  (let ((nelisp-native-template--source-fence nil)
        (nelisp-native-cache--build-source-identity "built")
        (nelisp-native-template-fragment-abi (copy-tree nelisp-native-template-fragment-abi)))
    (nelisp-native-template-prepare-source-fence)
    (should (funcall nelisp-native-template--source-fence))
    (setcar nelisp-native-template-fragment-abi :mutated)
    (should-error (funcall nelisp-native-template--source-fence))))

(ert-deftest nelisp-native-lean-fence-refuses-cyclic-identity ()
  (let ((nelisp-native-template--source-fence nil)
        (nelisp-native-cache--build-source-identity "built")
        (nelisp-native-template-fragment-abi (copy-tree nelisp-native-template-fragment-abi)))
    (nelisp-native-template-prepare-source-fence)
    (setcdr (last nelisp-native-template-fragment-abi) nelisp-native-template-fragment-abi)
    (should-error (funcall nelisp-native-template--source-fence))))
