;;; nelisp-bytecode-native-rooted-stack-cold-compare.el --- compare stored raw artifact -*- lexical-binding: t; -*-

;;; Code:

(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-package)
(require 'nelisp-bytecode-native-rooted-stack)

(defun nelisp-bytecode-native-rooted-stack-cold-equivalent-p (result stored-path)
  "Compare inert STORED-PATH with authenticated rooted-stack RESULT.
Return non-nil only when all manifest data match except generated :source
and dependent :artifact-sha256.  This is pairwise equivalence, not standalone
provenance authentication.  Never maps or executes STORED-PATH."
  (condition-case nil
      (let* ((fresh-path (plist-get result :artifact-path))
             (runtime-sha (plist-get result :runtime-binary-sha256))
             (fresh-before (nelisp-bytecode-native-package-raw-file-sha256 fresh-path))
             (stored-before (nelisp-bytecode-native-package-raw-file-sha256 stored-path))
             (fresh (nelisp-native-load-manifest fresh-path))
             (stored (nelisp-native-load-manifest stored-path))
             (fresh-problems (nelisp-native-load-raw-v2-check
                              fresh "nl_native_stack_probe_v1"))
             (stored-problems (nelisp-native-load-raw-v2-check
                               stored "nl_native_stack_probe_v1"))
             (fresh-after (nelisp-bytecode-native-package-raw-file-sha256 fresh-path))
             (stored-after (nelisp-bytecode-native-package-raw-file-sha256 stored-path)))
        (and (nelisp-bytecode-native-rooted-stack-authenticated-result-p result)
             (stringp stored-path) (file-regular-p stored-path)
             (equal runtime-sha (nelisp-native-load-running-binary-sha256))
             (equal fresh-before (plist-get result :artifact-sha256))
             (equal fresh-before fresh-after)
             (equal stored-before stored-after)
             (null fresh-problems) (null stored-problems)
             (equal (plist-get fresh :binary-sha256) runtime-sha)
             (equal (plist-get stored :binary-sha256) runtime-sha)
             (stringp (plist-get fresh :source))
             (stringp (plist-get stored :source))
             (stringp (plist-get fresh :source-sha256))
             (stringp (plist-get stored :source-sha256))
             (stringp (plist-get fresh :compiled-source-sha256))
             (stringp (plist-get stored :compiled-source-sha256))
             (let ((a (copy-sequence fresh)) (b (copy-sequence stored)))
               (setq a (plist-put a :source nil)
                     b (plist-put b :source nil)
                     a (plist-put a :artifact-sha256 nil)
                     b (plist-put b :artifact-sha256 nil))
               (equal a b))))
    (error nil)))

(provide 'nelisp-bytecode-native-rooted-stack-cold-compare)
;;; nelisp-bytecode-native-rooted-stack-cold-compare.el ends here
