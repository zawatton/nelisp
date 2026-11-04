;;; compall2-unit.el --- offline pre-build gate: compile fxir-native.o, size its metadata  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/compall2-unit.el
;; Optional: COMPALL2_META_CEILING (bytes this unit's metadata entry may take; default
;; `compall2-unit-ceiling'), COMPALL2_UNIT_DATA (a data file to lower instead of
;; lisp/nelisp-fxir-native-data.el).
;;
;; Lowers the native rows (as the build does when it loads nelisp-cc-fxir-native) and
;; compiles the fxir-native.o unit the way the build does (tools/ai/compall-unit.el), then
;; serialises the unit's symbols and relocations exactly as
;; lisp/nelisp-native-rooted-build-evidence.el writes its entry of
;; active-unit-metadata.json (`json-encode' of NAME PATH UNIT-SHA256 SYMBOLS
;; RELOCATIONS).  The whole file is bounded by 4 MiB; the other units' entries are the
;; fixed remainder (v35: the file's 3,732,013 bytes less this unit's entry, 900,930 bytes
;; as computed here for v35's sources -- the same 519 symbols and 9,450 relocations as the
;; v35 build -- is 2,831,083), so this unit's entry must stay below 4,194,304 - 2,831,083
;; = 1,363,221 bytes, less a margin for the concurrent lanes' growth of the other units.
;; Prints
;;   LOWER ROWS SECS
;;   UNIT ok|fail SECS SYMBOLS RELOCS TEXT-BYTES
;;   META BYTES CEILING
;;   META-FIT yes|no
;;   RELOC-TOP SYMBOL COUNT     (the ten most relocated symbols)
;;   ERR ...
(setq load-prefer-newer t max-lisp-eval-depth 200000)
(require 'json)
(require 'cl-lib)
(defconst compall2-unit-ceiling (- 1363221 150000)
  "Default bound on this unit's metadata entry: the 4 MiB file bound minus the
other units' v35 entries, less 150,000 bytes kept for the other lanes' growth.")
(require 'nelisp-standalone-build)
(let* ((t0 (float-time))
       (ok (condition-case e
               (progn (require 'nelisp-cc-fxir-native)
                      (when (getenv "COMPALL2_UNIT_DATA")
                        (setq nelisp-cc-fxir-native--data-files
                              (list (cons (expand-file-name (getenv "COMPALL2_UNIT_DATA"))
                                          'nelisp-fxir-native-data)
                                    (cons (expand-file-name "lisp/nelisp-fxir-native-corpus-data.el")
                                          'nelisp-fxir-native-corpus-data))))
                      (setq nelisp-cc-fxir-native--source (nelisp-cc-fxir-native-source))
                      (setq nelisp-cc-fxir-native--boundaries
                            (append nelisp-cc-fxir-native--fixed-boundaries
                                    nelisp-cc-fxir-native--wrapper-names))
                      (setq nelisp-standalone--inline-boundaries
                            (append nelisp-standalone--inline-boundaries
                                    nelisp-cc-fxir-native--wrapper-names))
                      t)
             (error (message "ERR lower %S" e) nil))))
  (message "LOWER %d %.1f" (if ok (length (nelisp-cc-fxir-native-all-rows)) 0) (- (float-time) t0))
  (when ok
    (let* ((t1 (float-time))
           (r (condition-case e
                  (let ((nelisp-standalone--inline-import-sources
                         (nelisp-standalone--unit-inline-imports "fxir-native.o")))
                    (nelisp-standalone--compile-to-unit "fxir-native.o" nelisp-cc-fxir-native--source))
                (error (message "ERR unit %S" e) nil)))
           (syms (and (consp r) (plist-get r :symbols)))
           (relocs (and (consp r) (plist-get r :relocs)))
           (text (cdr (assq 'text (and (consp r) (plist-get r :sections))))))
      (message "UNIT %s %.1f %d %d %d" (if r "ok" "fail") (- (float-time) t1)
               (length syms) (length relocs) (if (stringp text) (string-bytes text) -1))
      (unless (and syms relocs) (message "ERR unit keys %S" (and (consp r) (cl-loop for (k _v) on r by #'cddr collect k))))
      (when (and syms relocs)
        (let* ((entry (list :name "fxir-native.o" :path "fxir-native.o.unit"
                            :unit-sha256 (make-string 64 ?0)
                            :symbols (vconcat syms) :relocations (vconcat relocs)))
               (bytes (+ 1 (string-bytes (json-encode entry))))
               (ceiling (if (getenv "COMPALL2_META_CEILING")
                            (string-to-number (getenv "COMPALL2_META_CEILING"))
                          compall2-unit-ceiling))
               (count (make-hash-table :test 'equal)) (top nil))
          (message "META %d %d" bytes ceiling)
          (message "META-FIT %s" (if (<= bytes ceiling) "yes" "no"))
          (when (getenv "COMPALL2_UNIT_META_OUT")
            (with-temp-file (getenv "COMPALL2_UNIT_META_OUT")
              (insert (json-encode entry))))
          (dolist (x relocs) (cl-incf (gethash (format "%s" (plist-get x :symbol)) count 0)))
          (maphash (lambda (k v) (push (cons k v) top)) count)
          (setq top (sort top (lambda (a b) (> (cdr a) (cdr b)))))
          (dotimes (i (min 10 (length top)))
            (message "RELOC-TOP %s %d" (car (nth i top)) (cdr (nth i top)))))))))
