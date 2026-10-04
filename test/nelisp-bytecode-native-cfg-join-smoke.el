;;; nelisp-bytecode-native-cfg-join-smoke.el --- Raw CFG join ELF smoke -*- lexical-binding: t; -*-

;;; Code:

(defconst nelisp-bytecode-native-cfg-join-smoke--source
  (or load-file-name buffer-file-name))
(defconst nelisp-bytecode-native-cfg-join-smoke--lisp-dir
  (expand-file-name "../lisp"
                    (file-name-directory
                     nelisp-bytecode-native-cfg-join-smoke--source)))

(add-to-list 'load-path nelisp-bytecode-native-cfg-join-smoke--lisp-dir)
(require 'nelisp-native-unit)
(load (expand-file-name "nelisp-bytecode-ir.el"
                        nelisp-bytecode-native-cfg-join-smoke--lisp-dir)
      nil t)
(load (expand-file-name "nelisp-bytecode-frame-ir.el"
                        nelisp-bytecode-native-cfg-join-smoke--lisp-dir)
      nil t)
(load (expand-file-name "nelisp-bytecode-native-cfg.el"
                        nelisp-bytecode-native-cfg-join-smoke--lisp-dir)
      nil t)

(defun nelisp-bytecode-native-cfg-join-smoke--one (condition export expected)
  "Publish one source-free raw CFG artifact and compare its result to GNU VM."
  (let* ((code (unibyte-string 192 193 194 131 10 0
                               195 130 11 0 196 135))
         (constants (vector 5 10 condition 20 30))
         (frame (nelisp-bytecode-frame-ir-build code constants))
         (lowered (nelisp-bytecode-native-cfg-lower code constants))
         (artifact (make-temp-file "nelisp-raw-join-" nil ".nelr"))
         (manifest
          (nelisp-bytecode-native-cfg-write-raw-v1
           lowered artifact nelisp-bytecode-native-cfg-join-smoke--source export))
         (staged (nelisp-native-unit-stage artifact nil (list export))))
    (unwind-protect
        (progn
          (unless (and (eq (plist-get frame :status) 'complete)
                       (eq (plist-get lowered :status) 'complete))
            (error "raw-join: lowering failure: frame=%S lower=%S"
                   (plist-get frame :reason) (plist-get lowered :reason)))
          (unless (eq (plist-get staged :status) 'staged)
            (error "raw-join: native stage failed: %S" staged))
          (let* ((unit-id (plist-get staged :unit-id))
                 (published (nelisp-native-unit-publish
                             (plist-get staged :candidate-id))))
            (unless (eq (plist-get published :status) 'published)
              (error "raw-join: native publish failed: %S" published))
            (let ((native (nelisp-native-unit-call unit-id export nil)))
              (unless (= native expected)
                (error "raw-join: native/GNU-VM golden mismatch for %s: golden=%S native=%S"
                       export expected native))
              (list export expected native
                    (plist-get manifest :binary-sha256)
                    (plist-get manifest :artifact-sha256)))))
      (when (file-exists-p artifact) (delete-file artifact)))))

(let ((false-result
       (nelisp-bytecode-native-cfg-join-smoke--one nil "bc_raw_join_nil" 30))
      (true-result
       (nelisp-bytecode-native-cfg-join-smoke--one t "bc_raw_join_true" 20)))
  (message "raw-join: PASS native-ELF vs GNU-VM golden nil=%s t=%s"
           (nth 2 false-result) (nth 2 true-result))
  (list :status 'passed :false-path false-result :true-path true-result))

;;; nelisp-bytecode-native-cfg-join-smoke.el ends here
