(process-inherit-coding-system-flag
 (let ((p (make-pipe-process :name "p2-inherit")))
   (prog1 (process-inherit-coding-system-flag p) (delete-process p)))
 (condition-case e (process-inherit-coding-system-flag nil)
   (error (list (car e) (cdr e)))))
(process-running-child-p
 (let ((p (start-process "p2-child" nil "sleep" "1")))
   (unwind-protect (process-running-child-p p) (delete-process p)))
 (let ((p (make-pipe-process :name "p2-child-pipe")))
   (unwind-protect
       (condition-case e (process-running-child-p p)
         (error (list (car e) (cdr e))))
     (delete-process p))))
(process-thread
 (let ((p (make-pipe-process :name "p2-thread")))
   (prog1 (if (process-thread p) t nil) (delete-process p)))
 (condition-case e (process-thread nil) (error (list (car e) (cdr e)))))
(process-tty-name
 (let ((p (make-pipe-process :name "p2-tty")))
   (prog1 (list (process-tty-name p) (process-tty-name p 'stdout))
     (delete-process p)))
 (let ((p (make-pipe-process :name "p2-tty-error")))
   (unwind-protect (condition-case e (process-tty-name p 'invalid)
                     (error (list (car e) (cdr e))))
     (delete-process p))))
(process-type
 (let* ((p (make-pipe-process :name "p2-type"))
        (b (get-buffer-create "*p2-type*")))
   (set-process-buffer p b)
   (prog1 (list (process-type p) (eq (process-type b) 'pipe))
     (delete-process p) (kill-buffer b)))
 (condition-case e (process-type "p2-no-such-process")
   (error (list (car e) (cdr e)))))
(quit-process
 (let ((p (make-pipe-process :name "p2-quit")))
   (unwind-protect (condition-case e (quit-process p)
                     (error (list (car e) (cdr e))))
     (delete-process p)))
 (condition-case e (quit-process nil) (error (list (car e) (cdr e)))))
(serial-process-configure
 (let ((p (make-pipe-process :name "p2-serial")))
   (unwind-protect
       (condition-case e (serial-process-configure :process p :speed 9600)
         (error (list (car e) (cdr e))))
     (delete-process p)))
 (condition-case e (serial-process-configure :speed 19200)
   (error (list (car e) (cdr e))))
 )
(set-network-process-option
 (let ((p (make-pipe-process :name "p2-net-option")))
   (unwind-protect
       (condition-case e (set-network-process-option p :broadcast t t)
         (error (list (car e) (cdr e))))
     (delete-process p)))
 (condition-case e (set-network-process-option nil :broadcast nil)
   (error (list (car e) (cdr e)))))
(set-process-datagram-address
 (let ((p (make-pipe-process :name "p2-dgram")))
   (unwind-protect (set-process-datagram-address p '("127.0.0.1" . 9000))
     (delete-process p)))
 (condition-case e (set-process-datagram-address nil nil)
   (error (list (car e) (cdr e)))))
(set-process-inherit-coding-system-flag
 (let ((p (make-pipe-process :name "p2-set-inherit")))
   (unwind-protect
       (list (set-process-inherit-coding-system-flag p t)
             (process-inherit-coding-system-flag p))
     (delete-process p)))
 (condition-case e (set-process-inherit-coding-system-flag nil nil)
   (error (list (car e) (cdr e)))))
(set-process-thread
 (let ((p (make-pipe-process :name "p2-set-thread")))
   (unwind-protect
       (condition-case e (set-process-thread p 'not-a-thread)
         (error (list (car e) (cdr e))))
     (delete-process p)))
 (condition-case e (set-process-thread nil nil) (error (list (car e) (cdr e)))))
(set-process-window-size
 (let ((p (make-pipe-process :name "p2-size")))
   (unwind-protect (set-process-window-size p 31 97) (delete-process p)))
 (let ((p (make-pipe-process :name "p2-size-error")))
   (unwind-protect
       (condition-case e (set-process-window-size p 31 "wide")
         (error (list (car e) (cdr e))))
     (delete-process p))))
