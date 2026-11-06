;;; emacs-load-test-support.el --- Shared setup for the emacs-load ERT files  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loads emacs-load.el under the host-Emacs guards the load tests need and
;; defines their shared helpers.  It lives in its own feature so that
;; emacs-load-elc-test.el can share the setup without requiring
;; emacs-load-test.el itself; that made the umbrella run load the test
;; file twice, and ERT rejects the second definition of each test.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-load-test--source-file
  (expand-file-name "packages/emacs-lib-core/src/emacs-load.el"
                    (expand-file-name ".." emacs-load-test--root)))

(defvar emacs-load-test--stream-order nil)

;; Load the source implementation directly so the tests do not depend on a
;; potentially stale compiled .elc.
(let ((host-locate-library (symbol-function 'locate-library))
      (host-load (symbol-function 'load))
      (host-load-file (symbol-function 'load-file))
      (native-comp-enable-subr-trampolines nil))
  (let ((emacs-version 1))
    (funcall host-load-file emacs-load-test--source-file))
  (fset 'load host-load)
  (fset 'load-file host-load-file)
  (fset 'locate-library host-locate-library))

(defmacro emacs-load-test--with-fresh-native-cache (&rest body)
  "Run BODY with a fresh native artifact cache."
  `(let ((emacs-load--artifact-native-hash-cache
          (make-hash-table :test 'equal)))
     ,@body))

(defmacro emacs-load-test--without-native-read-all (&rest body)
  "Run BODY without `nelisp--read-all-from-string-native' if it exists."
  `(let ((original-native (when (fboundp 'nelisp--read-all-from-string-native)
                            (symbol-function 'nelisp--read-all-from-string-native))))
     (unwind-protect
         (progn
           (when original-native
             (fmakunbound 'nelisp--read-all-from-string-native))
           ,@body)
       (when original-native
         (fset 'nelisp--read-all-from-string-native original-native)))))

(defun emacs-load-test--standalone-active-p ()
  "Non-nil when the standalone emacs-load override is active."
  (or (fboundp 'rdf)
      (fboundp 'nelisp--eval-source-string)
      (fboundp 'emacs-load--artifact-load-or-compile)))

(defun emacs-load-test--v5-native-section
    (text-base64 extern-symbols reloc-data defuns heavy-char)
  "Return a raw v5 native section fixture."
  (let ((symbols (mapcar (lambda (entry) (plist-get entry :name)) defuns)))
    (list :native-section-version 5
          :serialized-char-size 0
          :runtime-prefix
          (vector 2
                  0
                  "x86_64"
                  symbols
                  text-base64
                  'indexed-plt32-v1
                  (/ (length reloc-data) 3)
                  reloc-data
                  extern-symbols
                  defuns)
          :object-format 'nelisp-aot-elf-v1
          :object-base64 (make-string 20000 heavy-char)
          :compile-report
          (list (list :name (or (car symbols) "probe") :native t)))))


(provide 'emacs-load-test-support)
;;; emacs-load-test-support.el ends here
