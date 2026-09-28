;;; nelisp-file-attributes-parity-test.el --- file-attributes stat-field parity -*- lexical-binding: t; -*-
;;
;; Compares this runtime's `file-attributes' -- plus the accessor
;; functions and `time-less-p'/`time-equal-p' that read/compare its
;; timestamps -- against host Emacs, on a fixed fixture set created
;; once by the companion driver script and never modified between the
;; host run and the standalone run: a regular file, a directory, a
;; plain symlink, a dangling symlink, and a path that is never created.
;;
;; `NELISP_FILE_ATTRS_FIXTURE_DIR' must name a directory holding
;; "regular.txt", "subdir/", "link.txt" (a symlink to "regular.txt")
;; and "dangling.txt" (a symlink to a target that does not exist);
;; "missing.txt" under it must not exist.
;;
;; atime (element 4 / `file-attribute-access-time') is intentionally
;; left out of the compared shape: merely stat-ing the same files
;; across two separate process runs can shift it (relatime and
;; similar mount options), and it is not part of the bug this test
;; guards -- see the differential comment above `file-attributes' in
;; scripts/nelisp-stdlib-prelude.el.  mtime/ctime ARE compared exactly,
;; since the fixture files are never written to between the two runs.
(defun nelisp-fa-test--shape (path)
  "Return a comparable subset of `file-attributes' fields for PATH."
  (let ((a (file-attributes path)))
    (if (null a)
        nil
      (list (file-attribute-type a)
            (file-attribute-link-number a)
            (file-attribute-user-id a)
            (file-attribute-group-id a)
            (file-attribute-modification-time a)
            (file-attribute-status-change-time a)
            (file-attribute-size a)
            (file-attribute-modes a)
            (file-attribute-inode-number a)
            (file-attribute-device-number a)))))

(let* ((dir (or (getenv "NELISP_FILE_ATTRS_FIXTURE_DIR")
                 (error "NELISP_FILE_ATTRS_FIXTURE_DIR is unset")))
       (reg (expand-file-name "regular.txt" dir))
       (subdir (expand-file-name "subdir" dir))
       (link (expand-file-name "link.txt" dir))
       (dangling (expand-file-name "dangling.txt" dir))
       (missing (expand-file-name "missing.txt" dir))
       (mt (file-attribute-modification-time (file-attributes reg)))
       ;; One microsecond later than REG's own mtime -- used only to
       ;; exercise `time-less-p' in both directions without depending
       ;; on `time-add' (not available on this runtime).
       (later (list (nth 0 mt) (1+ (nth 1 mt)) (nth 2 mt) (nth 3 mt))))
  (princ (format "%S\n"
                 (list :regular (nelisp-fa-test--shape reg)
                       :dir (nelisp-fa-test--shape subdir)
                       :symlink (nelisp-fa-test--shape link)
                       :dangling (nelisp-fa-test--shape dangling)
                       :missing (nelisp-fa-test--shape missing)
                       :time-less (time-less-p mt later)
                       :time-less-rev (time-less-p later mt)
                       :time-equal (time-equal-p mt mt)))))

;;; nelisp-file-attributes-parity-test.el ends here
