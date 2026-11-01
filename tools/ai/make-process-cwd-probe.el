;;; make-process-cwd-probe.el --- standalone probe for process cwd defects  -*- lexical-binding: t; -*-
;; Prints one "PROBE <name> PASS|FAIL <detail>" line per check.  Run after
;; the runtime's process adapter is loaded (e.g. after anvil's standalone
;; loader with ANVIL_STANDALONE_TEST_NO_SERVER=1).
(defun mpc-probe--report (name ok detail)
  (princ (format "PROBE %s %s %S\n" name (if ok "PASS" "FAIL") detail)))

(mpc-probe--report "coding-alist-bound" (boundp 'process-coding-system-alist)
                   (boundp 'process-coding-system-alist))

(let ((buf (generate-new-buffer " mpc-probe")))
  (condition-case err
      (mpc-probe--report "get-buffer-process" (null (get-buffer-process buf)) "nil")
    (error (mpc-probe--report "get-buffer-process" nil err))))

(let* ((dir (file-name-as-directory (make-temp-file "mpc-probe-" t)))
       (buf (generate-new-buffer " mpc-out"))
       (default-directory dir))
  (condition-case err
      (let ((proc (make-process :name "mpc-pwd" :buffer buf :command '("pwd")
                                :connection-type 'pipe :sentinel #'ignore)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.1)))
        ;; Exit status is visible before the last chunk is drained; pump once more.
        (accept-process-output proc 0.2)
        (let ((out (string-trim (with-current-buffer buf (buffer-string)))))
          (mpc-probe--report "make-process-cwd"
                             (equal (file-name-as-directory out) dir)
                             (list :want dir :got out))))
    (error (mpc-probe--report "make-process-cwd" nil err))))
