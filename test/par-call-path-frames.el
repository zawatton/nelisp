;;; par-call-path-frames.el --- Rooting and observation of Subr argv -*- lexical-binding: t; -*-

(defconst par-call-path-frame-call (make-byte-code 771 (unibyte-string 34 135) [] 3))
(defvar par-call-path-binding 'outside)
(defvar par-call-path-retained nil)

(defun par-call-path-frame-callback (value)
  (garbage-collect)
  (let* ((frame (backtrace-frame 0 'mapcar))
         (arguments (nth 3 frame)))
    (list (eq value (car par-call-path-retained))
          (eq arguments par-call-path-retained)
          (car frame) (nth 1 frame) par-call-path-binding)))

(defun par-call-path-frame-throw (value)
  (throw 'par-call-path-stop (par-call-path-frame-callback value)))

(let* ((par-call-path-retained (list (vector 'unaliased)))
       (cleanup nil)
       (result
        (catch 'par-call-path-stop
          (let ((par-call-path-binding 'inside))
            (unwind-protect
                (funcall par-call-path-frame-call 'mapcar #'par-call-path-frame-throw
                         par-call-path-retained)
              (setq cleanup par-call-path-binding))))))
  (prin1 (list 'frame result cleanup par-call-path-binding
               (funcall par-call-path-frame-call 'mapcar #'par-call-path-frame-callback
                        par-call-path-retained)))
  (terpri))
