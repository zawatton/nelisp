;;; nelisp-bytecode-native-cfg-switch-smoke.el --- Bswitch ELF smoke -*- lexical-binding: t; -*-

(defconst nelisp-bytecode-native-cfg-switch-smoke--source
  (or load-file-name buffer-file-name))

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                nelisp-bytecode-native-cfg-switch-smoke--source)))
(require 'nelisp-native-unit)
(let ((lisp-dir (expand-file-name "../lisp"
                                  (file-name-directory
                                   nelisp-bytecode-native-cfg-switch-smoke--source))))
  (load (expand-file-name "nelisp-bytecode-ir.el" lisp-dir) nil t)
  (load (expand-file-name "nelisp-bytecode-frame-ir.el" lisp-dir) nil t)
  (load (expand-file-name "nelisp-bytecode-native-cfg.el" lisp-dir) nil t))

(let* ((code (unibyte-string 137 192 183 130 10 0 193 135 194 135 195 135))
       (table (make-hash-table :test 'eq))
       (constants (vector table 10 20 30))
       (_ (progn (puthash 1 6 table) (puthash 2 8 table)))
       (frame (nelisp-bytecode-frame-ir-build code constants 1))
       (lowered (nelisp-bytecode-native-cfg-lower code constants 1 '(raw-i64)))
       (native-results nil)
       (artifact (make-temp-file "nelisp-bswitch-" nil ".nelr"))
       (manifest (nelisp-bytecode-native-cfg-write-raw-v1
                  lowered artifact
                  nelisp-bytecode-native-cfg-switch-smoke--source
                  "bc_cfg_bswitch"))
       (staged (nelisp-native-unit-stage artifact nil '("bc_cfg_bswitch"))))
  (unwind-protect
      (progn
        (unless (and (eq (plist-get frame :status) 'complete)
                     (eq (plist-get lowered :status) 'complete))
          (error "Bswitch lowering failed: frame=%S lower=%S"
                 (plist-get frame :reason) (plist-get lowered :reason)))
        (unless (eq (plist-get staged :status) 'staged)
          (error "Bswitch native stage failed: %S" staged))
        (let* ((unit-id (plist-get staged :unit-id))
               (published (nelisp-native-unit-publish
                           (plist-get staged :candidate-id))))
          (unless (eq (plist-get published :status) 'published)
            (error "Bswitch native publish failed: %S" published))
          (dolist (case '((1 . 10) (2 . 20) (7 . 30)))
            (let* ((input (car case)) (expected (cdr case))
                   (native (nelisp-native-unit-call
                            unit-id "bc_cfg_bswitch" (list input))))
              (unless (= native expected)
                (error "Bswitch native/GNU-VM golden mismatch for %s: expected=%S native=%S"
                       input expected native))
              (push (cons input native) native-results)))
          (message "native-bswitch: PASS export=bc_cfg_bswitch GNU-VM/native=10,20,30 sha256=%s"
                   (plist-get manifest :artifact-sha256))
          (list :status 'passed :export "bc_cfg_bswitch"
                :gnu-vm-golden '((1 . 10) (2 . 20) (7 . 30))
                :native-results (nreverse native-results)
                :artifact-sha256 (plist-get manifest :artifact-sha256))))
    (when (file-exists-p artifact) (delete-file artifact))))

;;; nelisp-bytecode-native-cfg-switch-smoke.el ends here
