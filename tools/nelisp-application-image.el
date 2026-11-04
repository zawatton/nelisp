;;; nelisp-application-image.el --- saved application image record and check  -*- lexical-binding: t; -*-

;; Doc 240 D1/D2.  This file is loaded into the producing process before the
;; generated driver's preparation forms.  It stays resident, so it (and the
;; record it builds) is part of the dumped heap.
;;
;; Producer side: `nelisp-application-image-record-and-dump' runs after the
;; preparation (pins, preflight, install, registration checks) succeeded.
;; Consumer side: MAIN's first form is `(nelisp-application-image-verify)'.
;; Nothing here swallows an error; every failure signals.

(defconst nelisp-application-image-format "nelisp-app-image-v1")

(defvar nelisp-application-image--record nil
  "Plist describing the prepared application heap; nil outside an image.")

(defun nelisp-application-image--installed ()
  (unless (and (boundp 'nelisp-runtime-driver-preparation--installed-identities)
               (consp nelisp-runtime-driver-preparation--installed-identities))
    (error "Application image: no installed identities (preparation did not run)"))
  nelisp-runtime-driver-preparation--installed-identities)

(defun nelisp-application-image--file-size (path)
  (let ((attrs (file-attributes path 'integer)))
    (unless attrs (error "Application image: cannot stat %s" path))
    (file-attribute-size attrs)))

(defun nelisp-application-image-record-and-dump (out-path meta)
  "Record the prepared application in the heap and dump it to OUT-PATH.
META is a plist from the producer script: :driver-sha256 :prep-sha256 :pins
:rows :row (all required except :row).  :pins is the quoted `pins' binding
the preparation form itself verified; the script extracts it from the driver
text.  The dump goes to OUT-PATH.tmp, its byte count is checked against the
file size, and only then is it renamed to OUT-PATH.  On any failure the .tmp
is deleted and the error propagates."
  (unless (and (stringp out-path) (> (length out-path) 0))
    (error "Application image: bad output path"))
  (dolist (key '(:driver-sha256 :prep-sha256 :pins :rows))
    (unless (plist-get meta key)
      (error "Application image: META lacks %S" key)))
  (let* ((installed (nelisp-application-image--installed))
         (names (mapcar #'car installed))
         (cells nil)
         (producer (getenv "NELISP_PRODUCER_IMAGE_SHA256"))
         (tmp (concat out-path ".tmp"))
         (ok nil))
    (dolist (identity installed)
      (let ((name (car identity)))
        (unless (and (fboundp name) (eq (symbol-function name) (cdr identity)))
          (error "Application image: installed identity not live: %S" name))
        (setq cells (cons (cons name (symbol-function name)) cells))))
    (setq cells (nreverse cells))
    (unless (and (stringp producer) (= (length producer) 64))
      (error "Application image: NELISP_PRODUCER_IMAGE_SHA256 missing"))
    (unless (or (null (plist-get meta :row))
                (member (plist-get meta :row) (plist-get meta :rows)))
      (error "Application image: :row not in :rows"))
    (setq nelisp-application-image--record
          (list :format nelisp-application-image-format
                :names names
                :cells cells
                :row-set names
                :rows (plist-get meta :rows)
                :producer-image-sha256 producer
                :pins (plist-get meta :pins)
                :driver-sha256 (plist-get meta :driver-sha256)
                :prep-sha256 (plist-get meta :prep-sha256)
                :nelisp-version (and (boundp 'nelisp-version) nelisp-version)))
    (when (file-exists-p tmp) (delete-file tmp))
    (unwind-protect
        (let ((bytes (nelisp--arena-dump-image-stream tmp)))
          (unless (and (integerp bytes) (> bytes 0)
                       (= bytes (nelisp-application-image--file-size tmp)))
            (error "Application image: dump byte count %S differs from file size" bytes))
          (rename-file tmp out-path t)
          (setq ok t)
          bytes)
      (unless ok
        (when (file-exists-p tmp) (delete-file tmp))))))

(defun nelisp-application-image-verify (&optional row)
  "Signal unless the restored heap is the recorded prepared application.
Checks the record and format, that every recorded name still has the recorded
function cell (`eq'), and that ROW (default BENCH_ROW) is a row this image
was produced for and whose kernel name is among the recorded names."
  (let ((record nelisp-application-image--record)
        (row (or row (getenv "BENCH_ROW"))))
    (unless (consp record)
      (error "Application image: record missing"))
    (unless (equal (plist-get record :format) nelisp-application-image-format)
      (error "Application image: unsupported format %S" (plist-get record :format)))
    (unless (and (consp (plist-get record :cells))
                 (equal (plist-get record :names)
                        (mapcar #'car (plist-get record :cells)))
                 (equal (plist-get record :names) (plist-get record :row-set)))
      (error "Application image: inconsistent record"))
    (dolist (cell (plist-get record :cells))
      (unless (and (fboundp (car cell)) (eq (symbol-function (car cell)) (cdr cell)))
        (error "Application image: function cell of %S was redefined" (car cell))))
    (unless (and (stringp row) (member row (plist-get record :rows)))
      (error "Application image: row %S is not covered by this image %S"
             row (plist-get record :rows)))
    (unless (memq (intern (concat "bench-" row)) (plist-get record :row-set))
      (error "Application image: kernel of row %S is not in the recorded name set" row))
    t))

(provide 'nelisp-application-image)
;;; nelisp-application-image.el ends here
