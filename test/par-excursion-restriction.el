;;; par-excursion-restriction.el --- Buffer scope and marker edits  -*- lexical-binding: t; -*-

(defmacro er-row (name &rest body)
  `(princ (format "%s %S\n" ,name
                  (condition-case err
                      (progn ,@body)
                    (error (list 'error (car err)))))))

(er-row "excursion restores accessible point"
  (with-temp-buffer
    (insert "0123456789") (goto-char 3)
    (save-excursion (narrow-to-region 5 8) (goto-char 7))
    (list (point) (point-min) (point-max))))

(er-row "unrestricted restore follows the whole buffer"
  (with-temp-buffer
    (insert "0123456789")
    (save-restriction
      (narrow-to-region 2 7) (goto-char 3) (insert "XY"))
    (list (point-min) (point-max) (point))))

(er-row "moving restriction restores in original buffer"
  (let ((other (generate-new-buffer " *er-other*" t)))
    (unwind-protect
        (with-temp-buffer
          (let ((original (current-buffer)))
            (insert "0123456789") (narrow-to-region 3 8)
            (save-restriction
              (widen)
              (goto-char 1) (insert "X")
              (goto-char 9) (insert "YZ")
              (set-buffer other))
            (list (eq (current-buffer) other)
                  (with-current-buffer original
                    (list (point-min) (point-max) (buffer-string))))))
      (kill-buffer other))))

(er-row "labeled widening survives nested restoration"
  (with-temp-buffer
    (insert "0123456789")
    (let ((inside
           (with-restriction 3 9 :label 'outer
             (with-restriction 4 8 :label 'inner
               (let ((temporarily-wider
                      (without-restriction :label 'inner
                        (list (point-min) (point-max)))))
                 (list temporarily-wider (point-min) (point-max)
                       (without-restriction :label 'inner
                         (list (point-min) (point-max)))))))))
      (list inside (point-min) (point-max)))))

(er-row "killing a narrowed buffer during unwind"
  (let ((original (generate-new-buffer " *er-dead*" t)))
    (with-temp-buffer
      (let ((other (current-buffer)))
        (set-buffer original)
        (insert "0123456789") (narrow-to-region 3 8)
        (list (catch 'done
                (save-restriction
                  (set-buffer other) (kill-buffer original) (throw 'done 42)))
              (eq (current-buffer) other) (buffer-live-p original))))))

(er-row "killing a base buffer detaches its indirect markers"
  (let* ((base (generate-new-buffer " *er-base*" t))
         (peer (make-indirect-buffer base " *er-peer*" nil t))
         (marker (with-current-buffer peer (point-marker))))
    (unwind-protect
        (with-current-buffer peer
          (let ((value (save-excursion (kill-buffer base) 'body)))
            (list value (buffer-live-p base) (buffer-live-p peer)
                  (marker-buffer marker) (marker-position marker))))
      (when (buffer-live-p peer) (kill-buffer peer))
      (when (buffer-live-p base) (kill-buffer base)))))

(er-row "temporary buffers suppress both kill hooks"
  (let (buffer events)
    (let ((kill-buffer-query-functions
           (list (lambda () (push 'query events) t)))
          (kill-buffer-hook (list (lambda () (push 'kill events)))))
      (with-temp-buffer (setq buffer (current-buffer))
                        (add-hook 'kill-buffer-hook
                                  (lambda () (push 'local events)) nil t)))
    (list (buffer-live-p buffer) (nreverse events))))

(er-row "inhibition survives the buffer name and existing lookup"
  (let* ((buffer (generate-new-buffer "er-inhibited" t))
         (normal (generate-new-buffer "er-normal"))
         (created (get-buffer-create "er-created" t))
         events)
    (let ((buffer-list-update-hook (list (lambda () (push 'rename events)))))
      (with-current-buffer buffer (rename-buffer "er-renamed")))
    (get-buffer-create "er-normal" t)
    (let ((kill-buffer-hook (list (lambda () (push (buffer-name) events)))))
      (kill-buffer buffer)
      (kill-buffer created)
      (kill-buffer normal))
    (nreverse events)))

(er-row "set-marker moves and detaches"
  (let* ((a (generate-new-buffer " *er-marker-a*" t))
         (b (generate-new-buffer " *er-marker-b*" t))
         (marker (with-current-buffer a (insert "abcd") (copy-marker 2 t))))
    (unwind-protect
        (with-current-buffer b
          (insert "xyz")
          (let ((moved (eq (set-marker marker 99) marker)))
            (list moved (eq (marker-buffer marker) b) (marker-position marker)
                  (marker-insertion-type marker)
                  (progn (set-marker marker (make-marker))
                         (marker-position marker)))))
      (kill-buffer a) (kill-buffer b))))

(er-row "copy-marker preserves source buffer despite narrowing"
  (let ((marker (with-temp-buffer (make-marker))))
    (with-temp-buffer
      (insert "0123456789") (set-marker marker 2)
      (narrow-to-region 4 8)
      (let ((copy (with-temp-buffer (copy-marker marker t))))
        (list (eq (marker-buffer copy) (current-buffer))
              (marker-position copy) (marker-insertion-type copy))))))

(er-row "marker arguments reject bignums"
  (with-temp-buffer
    (let ((wide (expt 2 70)) (marker (point-marker)))
      (list (condition-case err (copy-marker wide)
              (wrong-type-argument (cadr err)))
            (condition-case err (set-marker marker wide)
              (wrong-type-argument (cadr err)))
            (marker-position marker)))))

(er-row "marker adjustment for insertion and deletion"
  (with-temp-buffer
    (insert "0123456789") (goto-char 4)
    (let ((before (copy-marker 3)) (left (copy-marker 4))
          (right (copy-marker 4 t)) (after (copy-marker 6)))
      (insert "A") (goto-char 4) (insert-before-markers "BC")
      (let ((inserted (mapcar #'marker-position (list before left right after))))
        (delete-region 2 8)
        (list inserted (mapcar #'marker-position (list before left right after)))))))

(er-row "set-buffer accepts names and rejects killed buffers"
  (let ((buffer (generate-new-buffer " *er-select*" t))
        (origin (current-buffer)))
    (let ((matched (save-current-buffer (eq (set-buffer (buffer-name buffer)) buffer))))
      (kill-buffer buffer)
      (list matched (eq (current-buffer) origin)
            (condition-case err (set-buffer buffer) (error (car err)))))))

;;; par-excursion-restriction.el ends here
