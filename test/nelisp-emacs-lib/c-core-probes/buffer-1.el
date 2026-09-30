(barf-if-buffer-read-only
 (with-temp-buffer (condition-case nil (progn (barf-if-buffer-read-only) nil) (error nil)))
 (with-temp-buffer (setq buffer-read-only t) (condition-case nil (progn (barf-if-buffer-read-only) nil) (error nil))) )
(buffer-last-name
 (or (stringp (buffer-last-name)) (null (buffer-last-name)))
 (let ((b (generate-new-buffer "buffer-1-last"))) (unwind-protect (progn (with-current-buffer b (rename-buffer "buffer-1-renamed")) (or (stringp (buffer-last-name b)) (null (buffer-last-name b)))) (kill-buffer b))) )
(buffer-swap-text
 (let ((a (generate-new-buffer "buffer-1-a")) (b (generate-new-buffer "buffer-1-b"))) (unwind-protect (progn (with-current-buffer a (insert "alpha")) (with-current-buffer b (insert "beta")) (with-current-buffer a (buffer-swap-text b)) (list (with-current-buffer a (buffer-string)) (with-current-buffer b (buffer-string)))) (kill-buffer a) (kill-buffer b)))
 (condition-case e (buffer-swap-text "bad") (error (list (car e) (cdr e)))) )
(bury-buffer-internal
 (let ((b (get-buffer-create "buffer-1-bury"))) (bury-buffer-internal b) (eq (car (last (buffer-list))) b))
 (condition-case e (bury-buffer-internal "bad") (error (list (car e) (cdr e)))) )
(delete-all-overlays
 (with-temp-buffer (insert "abc") (make-overlay 1 3) (delete-all-overlays) (or (null (overlays-in 1 3)) t))
 (let ((b (generate-new-buffer "buffer-1-overlays"))) (unwind-protect (progn (with-current-buffer b (insert "abc") (make-overlay 1 3)) (delete-all-overlays b) (with-current-buffer b (or (null (overlays-in 1 3)) t))) (kill-buffer b))) )
(find-buffer
 (let ((b (generate-new-buffer "buffer-1-find")) (v 'major-mode)) (unwind-protect (progn (with-current-buffer b (setq major-mode 'text-mode)) (or (eq (find-buffer v 'text-mode) b) (null (find-buffer v 'text-mode)))) (kill-buffer b)))
 (condition-case e (find-buffer 4 nil) (error (car e))) )
(get-truename-buffer
 (condition-case e (get-truename-buffer "") (error (car e)))
 (let ((b (generate-new-buffer "buffer-1-file")) (f (make-temp-file "buffer-1"))) (unwind-protect (progn (with-current-buffer b (set-visited-file-name f)) (eq (get-truename-buffer f) b)) (kill-buffer b) (delete-file f))) )
(internal--set-buffer-modified-tick
 (condition-case e (internal--set-buffer-modified-tick "bad") (error (car e)))
 (let ((b (generate-new-buffer "buffer-1-tick"))) (unwind-protect (progn (condition-case nil (internal--set-buffer-modified-tick 123 b) (error nil)) t) (kill-buffer b))) )
(other-buffer
 (let ((b (get-buffer-create "buffer-1-other"))) (bufferp (other-buffer (current-buffer))))
 (let ((b (generate-new-buffer "buffer-1-visible"))) (unwind-protect (condition-case nil (let ((result (other-buffer b t))) (or (bufferp result) (null result))) (error nil)) (kill-buffer b))) )
(set-buffer-major-mode
 (let ((b (generate-new-buffer "buffer-1-mode"))) (unwind-protect (progn (set-buffer-major-mode b) (symbolp (buffer-local-value 'major-mode b))) (kill-buffer b)))
 (condition-case e (set-buffer-major-mode nil) (error (list (car e) (cdr e)))) )
