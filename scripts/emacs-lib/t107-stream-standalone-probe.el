;;; t107-stream-standalone-probe.el --- standalone T107 smoke probe -*- lexical-binding: t; -*-

;; Run this with the project standalone reader after its normal bootstrap,
;; with `src' on the load path and without loading a user init file.  It is
;; intentionally independent of ERT and exits non-zero through `error'.

(require 'emacs-buffer-builtins)
(let ((root (getenv "T107_SOURCE_ROOT")))
  (when root
    ;; Exercise repeated source loads; the private helper captures must not
    ;; wrap themselves on the second load.
    (load (expand-file-name "src/emacs-buffer-builtins.el" root) nil t t)
    (load (expand-file-name "src/emacs-buffer-builtins.el" root) nil t t)
    (load (expand-file-name "src/nelisp-emacs-compat-fileio.el" root) nil t t)
    (load (expand-file-name "src/files-standalone-buffer.el" root) nil t t)))

(defvar t107-probe--passed 0)

(defun t107-probe--check (condition message)
  (unless condition (error "T107 probe: %s" message))
  (setq t107-probe--passed (1+ t107-probe--passed)))

(let ((buf (generate-new-buffer " *t107-probe*")))
  (unwind-protect
      (progn
        (with-current-buffer buf
          (insert "α (ok) tail")
          (goto-char 3))
        (t107-probe--check (equal (read buf) '(ok)) "read buffer stream")
        (with-current-buffer buf
          (t107-probe--check (= (point) 7) "buffer point advance"))
        (with-current-buffer buf
          (erase-buffer)
          (insert "α (one) (two) tail")
          (narrow-to-region 3 14)
          (goto-char 9))
        (t107-probe--check (equal (read buf) '(two)) "read narrowed stream")
        (with-current-buffer buf
          (t107-probe--check (= (point) 14) "narrowed point advance"))
        (with-current-buffer buf
          (let ((marker (copy-marker 3)))
            (widen)
            (narrow-to-region 9 14)
            (goto-char 14)
            (t107-probe--check (equal (read marker) '(one))
                               "read marker ignores narrowing")
            (t107-probe--check (= (marker-position marker) 8)
                               "marker read advance")))
        (with-current-buffer buf
          (widen)
          (goto-char 9)
          (let ((marker (point-marker)))
            (with-current-buffer (generate-new-buffer " *t107-other* ")
              (princ "X" marker)
              (kill-buffer (current-buffer)))
            (t107-probe--check (= (marker-position marker) 10)
                               "marker relocation")
            (let ((outside (copy-marker 3))
                  (before (buffer-string)))
              (narrow-to-region 10 15)
              (goto-char 10)
              (t107-probe--check
               (condition-case nil (progn (princ "X" outside) nil) (error t))
               "marker output outside narrowing")
              (t107-probe--check (equal (buffer-string) "(two)")
                                 "outside output leaves text unchanged")
              (t107-probe--check (= (point) 10)
                                 "outside output leaves point unchanged")
              (widen)
              (t107-probe--check (equal (buffer-string) before)
                                 "outside output preserves full text"))
            (t107-probe--check (equal (prin1-to-string buf)
                                       (format "#<buffer %s>" (buffer-name buf)))
                               "buffer printer")
            (set-marker marker nil)
            (t107-probe--check (equal (prin1-to-string marker)
                                       "#<marker in no buffer>")
                               "detached marker printer")
            (t107-probe--check
             (condition-case nil (progn (read marker) nil) (error t))
             "detached marker read")
            (let ((path (make-temp-file "t107-stream-")))
              (unwind-protect
                  (progn
                    (nelisp-ec-widen)
                    (let ((m1 (copy-marker 3))
                          (m2 (copy-marker 8)))
                      (write-region m1 m2 path))
                    (t107-probe--check
                     (equal (nelisp-ec--read-raw-bytes path nil nil) "(one)")
                     "write-region marker/position"))
                (ignore-errors (delete-file path))))
            (kill-buffer buf))
        nil)
    (kill-buffer buf))))

(unless (= t107-probe--passed 15)
  (error "T107 probe: only %d/15 assertions ran" t107-probe--passed))
(princ "T107 standalone stream probe: ok\n")

;;; t107-stream-standalone-probe.el ends here
