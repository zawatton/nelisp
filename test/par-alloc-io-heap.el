;;; par-alloc-io-heap.el --- Read buffer boundaries and heap reuse -*- lexical-binding: t; -*-

(let ((path (make-temp-file "par-alloc-io-"))
      (coding-system-for-write 'no-conversion)
      (sizes '(0 4095 4096 4097 8191 8192 8193 131072))
      (survivor (list (vector 'live) (cons 'key 'value))))
  (unwind-protect
      (progn
        (prin1
         (cons :reads
               (mapcar
                (lambda (size)
                  (let ((text (make-string size ?a)))
                    (when (> size 0)
                      (aset text (1- size) ?z))
                    (write-region text nil path nil 'silent)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally path)
                      (and (= (buffer-size) size)
                           (equal text (buffer-string))
                           (or (= size 0)
                               (= (char-before (point-max))
                                  (if (getenv "PAR_ALLOC_IO_REVERSE") ?a ?z)))))))
                sizes)))
        (terpri)
        (let ((garbage (make-vector 12000 nil)))
          (dotimes (i (length garbage))
            (aset garbage i (cons i (vector i))))
          (setq garbage nil)
          (garbage-collect))
        (let ((fresh (make-vector 12000 nil)))
          (dotimes (i (length fresh))
            (aset fresh i (cons i (vector (+ i 5)))))
          (prin1 (list :reuse survivor (aref fresh 0) (aref fresh 11999)))
          (terpri)))
    (delete-file path)))
