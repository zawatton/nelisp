;;; census-process-01.el --- canonical probes  -*- lexical-binding: t; -*-
(accept-process-output
 (accept-process-output nil 0)
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (accept-process-output p 0 nil 1) (delete-process p)))
 (condition-case e (accept-process-output 7 0) (error e))
)
(call-process
 (call-process "true" nil nil nil)
 (with-temp-buffer (call-process "true" nil t nil) (buffer-string))
 (condition-case e (call-process 7) (error e))
)
(call-process-region
 (with-temp-buffer (insert "abc") (call-process-region (point-min) (point-max) "true" nil nil nil))
 (with-temp-buffer (insert "abc") (call-process-region (point-min) (point-max) "true" t nil nil) (buffer-string))
 (condition-case e (call-process-region 1 1 7) (error e))
)
(dbus--fd-close
 (let ((file (make-temp-file "cc-census-fd-")) (fd nil)) (unwind-protect (progn (setq fd (dbus--fd-open file)) (prog1 (dbus--fd-close fd) (setq fd nil))) (when fd (dbus--fd-close fd)) (delete-file file)))
 (let ((file (make-temp-file "cc-census-fd-")) (fd nil)) (unwind-protect (progn (setq fd (dbus--fd-open file)) (progn (dbus--fd-close fd) (prog1 (dbus--fd-close fd) (setq fd nil)))) (when fd (dbus--fd-close fd)) (delete-file file)))
 (condition-case e (dbus--fd-close "bad") (error e))
)
(dbus--fd-open
 (let ((file (make-temp-file "cc-census-fd-")) (fd nil)) (unwind-protect (progn (setq fd (dbus--fd-open file)) (integerp fd)) (when fd (dbus--fd-close fd)) (delete-file file)))
 (let ((file (make-temp-file "cc-census-fd-")) (fd nil)) (unwind-protect (progn (with-temp-file file (insert "abc")) (setq fd (dbus--fd-open file)) (integerp fd)) (when fd (dbus--fd-close fd)) (delete-file file)))
 (condition-case e (dbus--fd-open 7) (error e))
)
(dbus--init-bus
 (condition-case e (dbus--init-bus 7) (error e))
 (condition-case e (dbus--init-bus) (error (car e)))
)
(dbus--registered-fds
 (listp (dbus--registered-fds))
 (let ((file (make-temp-file "cc-census-fd-")) (fd nil)) (unwind-protect (progn (setq fd (dbus--fd-open file)) (not (null (assq fd (dbus--registered-fds))))) (when fd (dbus--fd-close fd)) (delete-file file)))
 (condition-case e (dbus--registered-fds 7) (error (car e)))
)
(dbus-get-unique-name
 (condition-case e (dbus-get-unique-name 7) (error e))
 (condition-case e (dbus-get-unique-name) (error (car e)))
)
(dbus-message-internal
 (condition-case e (dbus-message-internal "bad" 7 "service" "/path" "interface" "member") (error e))
 (condition-case e (dbus-message-internal) (error (car e)))
)
(delete-process
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (progn (delete-process p) (process-status p)) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (progn (delete-process p) (delete-process p)) (delete-process p)))
 (condition-case e (delete-process 7) (error e))
)
(emacs-pid
 (integerp (emacs-pid))
 (= (emacs-pid) (emacs-pid))
 (condition-case e (emacs-pid 7) (error (car e)))
)
(format-network-address
 (format-network-address [127 0 0 1 8080])
 (format-network-address [127 0 0 1 8080] t)
 (format-network-address [1 2])
)
(get-buffer-process
 (with-temp-buffer (get-buffer-process (current-buffer)))
 (with-temp-buffer (let ((p (make-pipe-process :name "cc-census-buffer" :buffer (current-buffer) :noquery t))) (unwind-protect (eq (get-buffer-process (current-buffer)) p) (delete-process p))))
 (condition-case e (get-buffer-process 7) (error e))
)
(get-process
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (eq (get-process (process-name p)) p) (delete-process p)))
 (get-process "cc-census-no-such-process")
 (condition-case e (get-process 7) (error e))
)
(getenv-internal
 (let ((process-environment (quote ("CC_CENSUS=alpha")))) (getenv-internal "CC_CENSUS"))
 (getenv-internal "CC_CENSUS" (quote ("CC_CENSUS")))
 (condition-case e (getenv-internal 7) (error e))
)
(kill-process
 (condition-case e (kill-process "cc-census-no-such-process") (error e))
 (condition-case e (kill-process 7) (error e))
)
(make-network-process
 (condition-case e (make-network-process :name 7) (error e))
 (condition-case e (make-network-process :name "cc-census-net" :family (quote bogus)) (error e))
)
(make-pipe-process
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (processp p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t :stop t))) (unwind-protect (process-status p) (delete-process p)))
 (condition-case e (make-pipe-process :name 7) (error e))
)
(mutex-lock
 (let ((m (make-mutex "cc-census"))) (unwind-protect (mutex-lock m) (mutex-unlock m)))
 (let ((m (make-mutex "cc-census"))) (mutex-lock m) (unwind-protect (unwind-protect (mutex-lock m) (mutex-unlock m)) (mutex-unlock m)))
 (condition-case e (mutex-lock 7) (error e))
)
(mutex-unlock
 (let ((m (make-mutex "cc-census"))) (mutex-lock m) (mutex-unlock m))
 (let ((m (make-mutex "cc-census"))) (mutex-lock m) (mutex-lock m) (unwind-protect (mutex-unlock m) (mutex-unlock m)))
 (condition-case e (mutex-unlock 7) (error e))
)
(num-processors
 (let ((n (num-processors))) (and (integerp n) (> n 0)))
 (let ((n (num-processors (quote current)))) (and (integerp n) (> n 0)))
 (let ((n (num-processors (quote all)))) (and (integerp n) (> n 0)))
)
(process-attributes
 (listp (process-attributes (emacs-pid)))
 (process-attributes -1)
 (condition-case e (process-attributes "bad") (error e))
)
(process-buffer
 (with-temp-buffer (let ((p (make-pipe-process :name "cc-census-buffer" :buffer (current-buffer) :noquery t))) (unwind-protect (eq (process-buffer p) (current-buffer)) (delete-process p))))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (process-buffer p) (delete-process p)))
 (condition-case e (process-buffer 7) (error e))
)
(process-coding-system
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t :coding 'utf-8-unix))) (unwind-protect (process-coding-system p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t :coding '(no-conversion . utf-8-unix)))) (unwind-protect (process-coding-system p) (delete-process p)))
 (condition-case e (process-coding-system 7) (error e))
)
(process-command
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (process-command p) (delete-process p)))
 (let ((p (start-process "cc-census-command" nil "true"))) (unwind-protect (equal (process-command p) (quote ("true"))) (delete-process p)))
 (condition-case e (process-command 7) (error e))
)
(process-contact
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (process-contact p) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t ))) (unwind-protect (process-contact p :name) (delete-process p)))
 (condition-case e (process-contact 7) (error e))
)
(process-filter
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t :filter nil))) (unwind-protect (null (process-filter p)) (delete-process p)))
 (let ((p (make-pipe-process :name "cc-census-pipe" :buffer nil :noquery t :filter 'ignore))) (unwind-protect (eq (process-filter p) (quote ignore)) (delete-process p)))
 (condition-case e (process-filter 7) (error e))
)
