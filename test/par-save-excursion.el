;;; -*- lexical-binding: t; -*-
;; save-excursion and marker-constructor rows (F424).  Run under GNU Emacs 31.1 for the .gnu reference.
(defvar sx--n 0)
(defmacro sx-row (label &rest body)
  `(progn (setq sx--n (1+ sx--n))
          (princ (format "S%d %s %S\n" sx--n ,label (condition-case e (progn ,@body) (error (list 'ERR (car e))))))))

(sx-row "insert before point shifts restored point"
  (with-temp-buffer (insert "xxxxxx") (goto-char 6)
    (save-excursion (goto-char 1) (insert "XXX")) (point)))
(sx-row "delete spanning point"
  (with-temp-buffer (insert "0123456789") (goto-char 5)
    (save-excursion (delete-region 3 8)) (point)))
(sx-row "delete before point"
  (with-temp-buffer (insert "0123456789") (goto-char 8)
    (save-excursion (delete-region 2 5)) (point)))
(sx-row "buffer switch restored"
  (let ((a (generate-new-buffer "sx-a")) (b (generate-new-buffer "sx-b")))
    (prog1 (with-current-buffer a (insert "abc") (goto-char 2)
             (save-excursion (set-buffer b) (insert "zzzz"))
             (list (eq (current-buffer) a) (point)))
      (kill-buffer a) (kill-buffer b))))
(sx-row "throw restores"
  (with-temp-buffer (insert "abcdef") (goto-char 3)
    (catch 'tag (save-excursion (goto-char 6) (throw 'tag 1))) (point)))
(sx-row "error restores"
  (with-temp-buffer (insert "abcdef") (goto-char 3)
    (condition-case nil (save-excursion (goto-char 6) (error "boom")) (error nil)) (point)))
(sx-row "error restores buffer"
  (let ((a (generate-new-buffer "sx-a")) (b (generate-new-buffer "sx-b")))
    (prog1 (with-current-buffer a
             (condition-case nil (save-excursion (set-buffer b) (error "boom")) (error nil))
             (eq (current-buffer) a))
      (kill-buffer a) (kill-buffer b))))
(sx-row "saved buffer killed inside body"
  (let ((a (generate-new-buffer "sx-a")) (b (generate-new-buffer "sx-b")))
    (prog1 (with-current-buffer b
             (list (progn (set-buffer a) (insert "abc") (goto-char 2)
                          (save-excursion (set-buffer b) (kill-buffer a) 'body))
                   (eq (current-buffer) b) (buffer-live-p a)))
      (kill-buffer b))))
(sx-row "insertion at saved point stays before (type nil)"
  (with-temp-buffer (insert "abcdef") (goto-char 4)
    (save-excursion (insert "XY")) (list (point) (buffer-string))))
(sx-row "insert at point from other position"
  (with-temp-buffer (insert "abcdef") (goto-char 4)
    (save-excursion (goto-char 4) (insert "XY") (goto-char 1)) (point)))
(sx-row "body value and empty body"
  (with-temp-buffer (list (save-excursion 1 2 3) (save-excursion))))
(sx-row "nested"
  (with-temp-buffer (insert "abcdef") (goto-char 3)
    (save-excursion (goto-char 5) (save-excursion (goto-char 1) (insert "Q")) (list (point)))))
(sx-row "no marker left behind"
  (with-temp-buffer (insert "abc") (goto-char 2)
    (let ((m (point-marker)))
      (save-excursion (goto-char 3))
      (list (marker-position m) (eq (marker-buffer m) (current-buffer)) (point)))))
(sx-row "point-marker type/pos/buffer"
  (with-temp-buffer (insert "abcdef") (goto-char 4)
    (let ((m (point-marker)))
      (list (markerp m) (marker-position m) (eq (marker-buffer m) (current-buffer))
            (marker-insertion-type m)))))
(sx-row "point-marker advances not on insert at it"
  (with-temp-buffer (insert "abcdef") (goto-char 4)
    (let ((m (point-marker))) (insert "ZZ") (marker-position m))))
(sx-row "point-marker follows earlier insert"
  (with-temp-buffer (insert "abcdef") (goto-char 4)
    (let ((m (point-marker))) (goto-char 1) (insert "ZZ") (marker-position m))))
(sx-row "point-min/max-marker"
  (with-temp-buffer (insert "abcdef")
    (let ((a (point-min-marker)) (b (point-max-marker)))
      (list (marker-position a) (marker-position b) (marker-insertion-type a) (marker-insertion-type b)
            (eq (marker-buffer a) (current-buffer))))))
(sx-row "copy-marker integer"
  (with-temp-buffer (insert "abcdef")
    (let ((m (copy-marker 3))) (list (marker-position m) (marker-insertion-type m) (eq (marker-buffer m) (current-buffer))))))
(sx-row "copy-marker type t advances"
  (with-temp-buffer (insert "abcdef") (goto-char 3)
    (let ((m (copy-marker (point) t))) (insert "ZZ") (list (marker-position m) (marker-insertion-type m)))))
(sx-row "copy-marker of marker"
  (with-temp-buffer (insert "abcdef")
    (let* ((a (copy-marker 5)) (c (copy-marker a))) (list (marker-position c) (eq (marker-buffer c) (current-buffer)) (eq a c)))))
(sx-row "copy-marker nil / nowhere"
  (let ((m (make-marker))) (list (marker-position (copy-marker nil)) (marker-buffer (copy-marker nil)) (marker-position (copy-marker m)))))
(sx-row "make-marker"
  (let ((m (make-marker))) (list (markerp m) (marker-position m) (marker-buffer m) (marker-insertion-type m))))
