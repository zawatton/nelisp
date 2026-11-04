;;; applyfn2-unitsize.el --- offline compile of the split reader applyfn units, sized as the rooted build bounds them  -*- lexical-binding: t; -*-
;; NELISP_READER_DYNAMIC=1 NELISP_STANDALONE_TARGET=linux-x86_64 \
;;   emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/applyfn2-unitsize.el
;; (Win64: NELISP_STANDALONE_TARGET=windows-x86_64, no NELISP_READER_DYNAMIC, as the
;; cross build.)  APPLYFN2_WALK=all admits every walk-eligible handler
;; (`nelisp-argframe-walk-names' nil), otherwise the build's setting is used.
;; Compiles the main unit and the argv-entry units exactly as
;; `nelisp-standalone-build-reader' does (same split, direct-only defuns and
;; inline imports) and prints, per unit, the bytes the rooted build-evidence
;; writer bounds (lisp/nelisp-native-rooted-build-evidence.el, not modified):
;;   UNITSIZE NAME encoded=B meta=B text=B relocs=N symbols=N secs=S
;; encoded: the hex-encoded .unit file (<= 16 MiB per unit); meta: the unit's
;; element of active-unit-metadata.json (the whole file <= 4 MiB).
(setq load-prefer-newer t max-lisp-eval-depth 1600)
(require 'cl-lib)
(require 'json)
(require 'nelisp-standalone-build)

(defun applyfn2-unitsize--encoded (unit)
  "Bytes of UNIT's .unit file, written as `nelisp-native-rooted-build-evidence-write' does."
  (let ((encoded (copy-sequence unit)))
    (plist-put encoded :sections
               (mapcar (lambda (section)
                         (if (stringp (cdr section))
                             (list (car section) :nelisp-cache-bytes-hex
                                   (mapconcat (lambda (b) (format "%02x" b)) (cdr section) ""))
                           section))
                       (plist-get unit :sections)))
    (with-temp-buffer
      (let ((print-circle t) (print-length nil) (print-level nil))
        (prin1 encoded (current-buffer)))
      (string-bytes (buffer-string)))))

(defun applyfn2-unitsize--meta (unit)
  "Bytes of UNIT's element of active-unit-metadata.json (json-encode, as the writer)."
  (let ((name (plist-get unit :name)))
    (length (json-encode (list :name name :path (concat name ".unit")
                               :unit-sha256 (make-string 64 ?0)
                               :symbols (vconcat (plist-get unit :symbols))
                               :relocations (vconcat (plist-get unit :relocs)))))))

(let* ((nelisp-argframe-walk-names (if (equal (getenv "APPLYFN2_WALK") "all") nil
                                     nelisp-argframe-walk-names))
       (t0 (float-time))
       (split (nelisp-standalone--applyfn-split-units
               (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source))
               (let ((p (getenv "APPLYFN2_PARTS"))) (and p (> (length p) 0) (string-to-number p)))))
       (main (let ((nelisp-aot-compiler--x86-direct-only-defuns
                    nelisp-standalone--direct-only-reader-defuns)
                   (nelisp-standalone--inline-import-sources
                    (nelisp-standalone--unit-inline-imports "applyfn-reader.o")))
               (nelisp-standalone--compile-to-unit "applyfn-reader.o" (car split))))
       (t1 (float-time))
       (index -1)
       (frames (mapcar (lambda (source)
                         (setq index (1+ index))
                         (let ((nelisp-aot-compiler--x86-direct-only-defuns
                                nelisp-standalone--direct-only-reader-defuns)
                               (nelisp-standalone--inline-import-sources nil))
                           (nelisp-standalone--compile-to-unit
                            (nelisp-standalone--applyfn-unit-name index) source)))
                       (cdr split)))
       (t2 (float-time))
       (meta 0) (enc 0))
  (dolist (u (cons main frames))
    (let ((e (applyfn2-unitsize--encoded u)) (m (applyfn2-unitsize--meta u)))
      (setq meta (+ meta m 1) enc (+ enc e))
      (princ (format "UNITSIZE %s encoded=%d meta=%d text=%d relocs=%d symbols=%d\n"
                     (plist-get u :name) e m
                     (string-bytes (or (cdr (assq 'text (plist-get u :sections))) ""))
                     (length (plist-get u :relocs)) (length (plist-get u :symbols)))))
      ;; The relocation targets, most frequent first, and how many name a main-unit symbol.
      (let ((h (make-hash-table :test #'equal)) (own (make-hash-table :test #'equal)) (inmain 0) l)
        (dolist (s (plist-get main :symbols)) (puthash (plist-get s :name) t own))
        (dolist (r (plist-get u :relocs))
          (let ((n (plist-get r :symbol)))
            (puthash n (1+ (gethash n h 0)) h)
            (when (and (not (eq u main)) (gethash n own)) (setq inmain (1+ inmain)))))
        (maphash (lambda (k v) (push (cons k v) l)) h)
        (setq l (sort l (lambda (a b) (> (cdr a) (cdr b)))))
        (princ (format "  RELOCS %s to-main=%d distinct=%d top=%S\n" (plist-get u :name) inmain (length l)
                       (cl-subseq l 0 (min 25 (length l))))))
      ;; Per function of an argv-entry unit: relocations and text bytes (the
      ;; function a relocation's offset falls in), for the cost of each entry.
      (unless (eq u main)
        (let* ((syms (sort (cl-remove-if-not (lambda (s) (eq (plist-get s :section) 'text))
                                             (copy-sequence (plist-get u :symbols)))
                           (lambda (a b) (< (plist-get a :value) (plist-get b :value)))))
               (vals (vconcat (mapcar (lambda (s) (plist-get s :value)) syms)))
               (names (vconcat (mapcar (lambda (s) (plist-get s :name)) syms)))
               (text (string-bytes (or (cdr (assq 'text (plist-get u :sections))) "")))
               (counts (make-vector (length vals) 0)))
          (dolist (r (plist-get u :relocs))
            (let ((off (plist-get r :offset)) (lo 0) (hi (1- (length vals))))
              (when (eq (plist-get r :section) 'text)
                (while (< lo hi)
                  (let ((mid (/ (+ lo hi 1) 2)))
                    (if (<= (aref vals mid) off) (setq lo mid) (setq hi (1- mid)))))
                (aset counts lo (1+ (aref counts lo))))))
          (dotimes (i (length vals))
            (princ (format "  FN %s relocs=%d text=%d\n" (aref names i) (aref counts i)
                           (- (if (< (1+ i) (length vals)) (aref vals (1+ i)) text) (aref vals i))))))))
  (princ (format "UNITSUM target=%s walk=%s units=%d encoded=%d meta=%d main-secs=%.0f frame-secs=%.0f\n"
                 nelisp-standalone--target (or (getenv "APPLYFN2_WALK") "build")
                 (1+ (length frames)) enc meta (- t1 t0) (- t2 t1))))
