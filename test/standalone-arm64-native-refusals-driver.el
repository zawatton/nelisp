;;; standalone-arm64-native-refusals-driver.el --- F1 cache refusals -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)
(let* ((decoded (cdr (assq 'f1-fixture (nelisp-bytecode-native-consumer-read-elc-functions (getenv "F1_FIXTURE")))))
       (file (or (getenv "F1_PRODUCER_FILE") (error "Producing cache locator required")))
       (root (getenv "NELISP_NATIVE_CACHE"))
       (original (nelisp-native-linux-private-file-bytes file))
       (first (read-from-string original))
       (manifest (car (read-from-string original (cdr first))))
       (recipe (plist-get (plist-get manifest :native-rooted-cfg-contract) :input-recipe))
       ;; GNU #$ metadata resolves against the reader's current source file.
       ;; Replay ONLY that diagnostic provenance from the producer. Every
       ;; semantic field remains decoded from the genuine GNU fixture, and the
       ;; complete recipe must match before any mutation or positive mapping.
       (function (apply #'make-byte-code
                        (append (list (aref decoded 0) (aref decoded 1)
                                      (aref decoded 2) (aref decoded 3))
                                (when (> (length decoded) 4) (list (plist-get recipe :metadata)))
                                (when (> (length decoded) 5) (list (aref decoded 5))))))
       (_ (unless (and (equal (nelisp-native-cache--recipe function) recipe)
                       (equal (nelisp-native-cache-file function) file))
            (error "Producing GNU recipe differs beyond source provenance")))
       (base (nelisp-native-cache-load function))
       (map-owner (symbol-function 'nelisp-native-load--mmap))
       (maps 0) (count 0))
  (unless base (error "Refusal controls need a genuine positive cache load"))
  (dolist (case '(target source binary imports literals ranges interrupted writable foreign))
    (let* ((first (read-from-string original)) (header (car first))
           (manifest (car (read-from-string original (cdr first))))
           (native (plist-get manifest :native)) (refused nil))
      (cond ((eq case 'target) (plist-put manifest :target '(:os gnu/linux :arch x86_64)))
            ((eq case 'source) (plist-put header :input (make-string 64 ?0)))
            ((eq case 'binary) (plist-put manifest :binary-sha256 (make-string 64 ?0)))
            ((eq case 'imports) (plist-put (car (plist-get native :imports)) :name "foreign_import"))
            ((eq case 'literals)
             (let ((text (base64-decode-string (plist-get native :text-base64)))
                   (offset 0) (literal nil))
               ;; Mutate an actual MOVZ immediate while retaining opcode/register.
               (while (and (not literal) (<= (+ offset 4) (length text)))
                 (when (= (logand (nelisp-native-arm64-u32 text offset) #xff800000) #xd2800000)
                   (setq literal offset))
                 (setq offset (+ offset 4)))
               (unless literal (error "F1 control needs a real encoded literal"))
               (aset text literal (logxor (aref text literal) 32))
               (plist-put native :text-base64 (base64-encode-string text t))))
            ((eq case 'ranges) (plist-put (car (plist-get native :relocs)) :offset 134217728)))
      ;; Re-sign identity/range mutations to exercise structural checks rather
      ;; than only the stored checksum. Text/import corruption keeps the old pin.
      (when (memq case '(target binary ranges))
        (plist-put manifest :artifact-sha256
                   (nelisp-native-load-sha256
                    (prin1-to-string (nelisp-native-load--raw-plist-without manifest :artifact-sha256)))))
      (write-region (if (eq case 'interrupted) (concat (nelisp-native-cache--print header) "\n")
                      (concat (nelisp-native-cache--print header) "\n"
                              (nelisp-native-cache--print manifest) "\n")) nil file nil 'silent)
      (when (eq case 'writable) (set-file-modes root 511))
      (when (eq case 'foreign) (setenv "NELISP_NATIVE_CACHE" "/proc"))
      (unwind-protect
          (cl-letf (((symbol-function 'nelisp-native-load--mmap)
                     (lambda (size executable) (setq maps (+ maps 1)) (funcall map-owner size executable))))
            (setq refused (condition-case nil (progn (nelisp-native-cache-load function) nil) (error t))))
        (setenv "NELISP_NATIVE_CACHE" root)
        (set-file-modes root 448)
        (write-region original nil file nil 'silent))
      (unless (and refused (= maps 0)) (error "ARM64 refusal %S mapped or admitted" case))
      (setq count (+ count 1))
      (princ (format "ARM64-REFUSAL-PASS case=%S maps=0\n" case))))
  (unless (nelisp-native-cache-load function) (error "Refusals damaged the producing cache"))
  (princ (format "ARM64-REFUSALS-PASS cases=%d maps=0\n" count)))
