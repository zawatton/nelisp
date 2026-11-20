;;; gnu58-process-owner-publication-1.el --- process owner publication -*- lexical-binding: t; -*-

(process-exit-status
 (let ((before (length (process-list))) child)
   (unwind-protect
       (let* ((outcome
               (condition-case err
                   (progn
                     (setq child (make-process :name "invalid-coding-audit"
                                               :command '("cat")
                                               :connection-type 'pipe
                                               :coding 'ccore-invalid-coding
                                               :noquery t))
                     'accepted)
                 (error (car err))))
              (delta (- (length (process-list)) before)))
         (list outcome delta))
     (when child (ignore-errors (delete-process child)))))
 (let* ((shell (executable-find "sh"))
        (filter-data "")
        (filter-count 0)
        (sentinel-events nil)
        (filter (lambda (_process text)
                  (setq filter-count (1+ filter-count)
                        filter-data (concat filter-data text))))
        (sentinel (lambda (_process event) (push event sentinel-events)))
        process)
   (unwind-protect
       (progn
         (setq process
               (make-process :name "owner-delivery-audit"
                             :command (list shell "-c" "printf x; exit 0")
                             :connection-type 'pipe :noquery t
                             :filter filter :sentinel sentinel))
         (let ((deadline (+ (float-time) 2)))
           (while (and (< (float-time) deadline)
                       (or (process-live-p process)
                           (= filter-count 0)
                           (null sentinel-events)))
             (accept-process-output process 0.02)))
         (dotimes (_ 5) (accept-process-output process 0.02))
         (list :filter-owner (eq (process-filter process) filter)
               :sentinel-owner (eq (process-sentinel process) sentinel)
               :filter-count filter-count
               :filter-data (string-to-list filter-data)
               :terminal-sentinel-count (length sentinel-events)
               :terminal (not (process-live-p process))))
     (when process (ignore-errors (delete-process process))))))
