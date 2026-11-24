;;; nelisp-process-buffer-test.el --- Buffered process contracts -*- lexical-binding: t; -*-
(require 'ert)

;; These contracts are for the standalone runtime, where markers are NeLisp
;; markers.  `nelisp-marker--make' alone does not say that: a host Emacs that
;; loaded src/nelisp-buffer.el defines it too while `markerp',
;; `make-pipe-process' and `process-mark' stay the host's.
(defun nelisp-process-buffer-test--nelisp-markers-p ()
  "Non-nil when this runtime's markers are NeLisp markers."
  (and (fboundp 'nelisp-marker--make) (markerp (nelisp-marker--make))))


(ert-deftest nelisp-process-buffer-marker-follows-edits ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (with-temp-buffer
    (insert "abc")
    (let* ((channel (make-pipe-process :name "marker" :buffer (current-buffer)))
           (marker (process-mark channel)))
      (unwind-protect
          (progn
            (should (markerp marker))
            (should (eq marker (process-mark channel)))
            (should (eq (marker-buffer marker) (current-buffer)))
            (set-marker marker 2)
            (goto-char 1) (insert "X")
            (should (= (marker-position marker) 3))
            (goto-char 1)
            (process-send-string channel "Z")
            (should (accept-process-output channel 0))
            (should (equal (buffer-string) "XaZbc"))
            (should (= (point) 1))
            (should (= (marker-position marker) 4)))
        (delete-process channel)))))

(ert-deftest nelisp-process-buffer-metadata-and-eof ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (with-temp-buffer
    (let ((channel (make-pipe-process :name "pipe" :buffer (current-buffer))))
      (unwind-protect
          (progn
            (should (processp channel))
            (should (eq channel (get-buffer-process (current-buffer))))
            (process-put channel 'key 'value)
            (should (eq (process-get channel 'key) 'value))
            (process-send-string channel "日本語\n")
            (should (eq (process-send-eof channel) channel))
            (should (equal (buffer-string) "日本語\n"))
            (should-not (process-live-p channel))
            (should-error (process-send-string channel "late")))
        (delete-process channel)))))

(ert-deftest nelisp-process-buffer-marker-current-buffer-and-clamp ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (let ((first (generate-new-buffer " *marker-first*"))
        (second (generate-new-buffer " *marker-second*"))
        (marker (nelisp-marker--make)))
    (unwind-protect
        (progn
          (with-current-buffer first (insert "abc") (set-marker marker 2))
          (with-current-buffer second
            (insert "x") (set-marker marker 999)
            (should (eq (marker-buffer marker) second))
            (should (= (marker-position marker) 2))
            (set-marker marker nil)
            (should-not (marker-position marker))))
      (kill-buffer first) (kill-buffer second))))

(ert-deftest nelisp-process-buffer-native-streams-are-separated ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (let ((out (generate-new-buffer " *process-out*"))
        (err (generate-new-buffer " *process-err*")) proc spool delivered)
    (unwind-protect
        (progn
          (setq proc (make-process
                      :name "separate" :buffer out :stderr err
                      :command '("sh" "-c" "printf out; printf 'diagnostic 日本語\\n' >&2")
                      :sentinel (lambda (&rest _) (setq delivered t))))
          (setq spool (process-get proc :stderr-spool))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not delivered) (< (float-time) deadline))
              (accept-process-output proc 0.01)))
          (should delivered)
          (should (equal (with-current-buffer out (buffer-string)) "out"))
          (should (equal (with-current-buffer err (buffer-string)) "diagnostic 日本語\n"))
          (should-not (file-exists-p spool)))
      (when proc (delete-process proc))
      (kill-buffer out) (kill-buffer err))))

(ert-deftest nelisp-process-buffer-filter-owning-insertion ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (with-temp-buffer
    (let ((seen nil)
          (channel (make-pipe-process :name "filter" :buffer (current-buffer))))
      (unwind-protect
          (progn
            (set-process-filter channel (lambda (_ text) (setq seen text)))
            (process-send-string channel "filtered")
            (should (accept-process-output channel 0))
            (should (equal seen "filtered"))
            (should (equal (buffer-string) "")))
        (delete-process channel)))))

(ert-deftest nelisp-process-buffer-invalid-metadata-object ()
  (should-not (processp 42))
  (should-error (process-get 42 'key) :type 'wrong-type-argument)
  (should-error (process-put 42 'key 'value) :type 'wrong-type-argument))

(ert-deftest nelisp-process-buffer-spawn-error-cleans-spool ()
  (skip-unless (nelisp-process-buffer-test--nelisp-markers-p))
  (let ((before nelisp-process-buffer--channels) spool)
    (cl-letf (((symbol-function 'nelisp-process-start)
               (lambda (&rest command)
                 (setq spool (nth 4 command))
                 (when spool (should (file-exists-p spool)))
                 (error "Injected native spawn failure"))))
      (should-error (make-process :name "failure" :stderr nil
                                  :command '("sh" "-c" "exit 0")))
      (with-temp-buffer
        (let ((failure
               (should-error (make-process :name "failure" :stderr (current-buffer)
                                           :command '("sh" "-c" "exit 0")))))
          (should (equal (cadr failure) "Injected native spawn failure")))))
    (should spool)
    (should-not (file-exists-p spool))
    (should (eq before nelisp-process-buffer--channels))))

(provide 'nelisp-process-buffer-test)
