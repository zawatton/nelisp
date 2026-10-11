;;; nelisp-native-rooted-build-evidence.el --- Capture actual prelink inputs -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)

(defun nelisp-native-rooted-build-evidence-source-hash (path limit)
  "Hash a bounded existing generation input before reading it."
  (unless (and (file-regular-p path)
               (<= (file-attribute-size (file-attributes path)) limit))
    (error "Missing or oversized rooted build input"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-native-rooted-build-evidence--hex (bytes)
  "Encode exact unit BYTES without character decoding."
  (unless (and (stringp bytes) (not (multibyte-string-p bytes)))
    (error "Rooted unit payload is not exact bytes"))
  (mapconcat (lambda (byte) (format "%02x" byte)) bytes ""))

(defun nelisp-native-rooted-build-evidence-write (units builder root directory)
  "Capture exact active non-driver UNITS and BUILDER beneath ROOT.
Return manifest, metadata and generated data paths. This does not issue a
runtime certificate; the caller must decode the units and generate startup
evidence before compiling the driver. Unknown or ambiguous inputs are refused."
  (unless (and (proper-list-p units) (<= 1 (length units) 128)
               (file-directory-p root))
    (error "Rooted active unit input bound"))
  (let* ((relative (file-relative-name (file-truename builder) (file-truename root)))
         (builder-hash (nelisp-native-rooted-build-evidence-source-hash builder 4194304))
         (names (make-hash-table :test #'equal))
         (symbols (make-hash-table :test #'equal))
         (total 0) (encoded-total 0) records metadata generated-data)
    (when (or (file-name-absolute-p relative) (string-prefix-p "../" relative))
      (error "Rooted builder source escapes generation root"))
    (when (and (file-directory-p directory)
               (directory-files directory nil directory-files-no-dot-files-regexp))
      (error "Rooted generation output is not fresh"))
    (make-directory directory t)
    (dolist (unit units)
      (let* ((name (plist-get unit :name))
             (sections (plist-get unit :sections))
             (text (cdr (assq 'text sections)))
             (bss (cdr (assq 'bss sections)))
             (unit-symbols (plist-get unit :symbols))
             (size 0))
        (unless (and (stringp name) (<= 1 (length name) 256)
                     (equal name (file-name-nondirectory name))
                     (not (gethash name names)) (proper-list-p unit-symbols)
                     (<= (length unit-symbols) 20000)
                     (proper-list-p sections) (<= (length sections) 16))
          (error "Ambiguous or malformed active unit"))
        (puthash name t names)
        (dolist (symbol unit-symbols)
          (let ((symbol-name (plist-get symbol :name)))
            (when (and (memq (plist-get symbol :section) '(text rodata data bss))
                       (equal (plist-get symbol :bind) 'global))
              (when (gethash symbol-name symbols)
                (error "Duplicate active symbol owner"))
              (puthash symbol-name name symbols))))
        (dolist (section sections)
          (when (stringp (cdr section))
            (setq size (+ size (string-bytes (cdr section))))))
        (setq total (+ total size))
        (unless (and (<= size 16777216) (<= total 33554432))
          (error "Active unit byte input bound"))
        (let* ((encoded (copy-sequence unit))
               (path (expand-file-name (concat name ".unit") directory)))
          (plist-put encoded :sections
                     (mapcar (lambda (section)
                               (if (stringp (cdr section))
                                   (list (car section) :nelisp-cache-bytes-hex
                                         (nelisp-native-rooted-build-evidence--hex (cdr section)))
                                 section)) sections))
          (with-temp-file path
            (let ((print-circle t) (print-length nil) (print-level nil))
              (prin1 encoded (current-buffer))))
          (setq encoded-total (+ encoded-total (file-attribute-size (file-attributes path))))
          (unless (<= encoded-total 33554432) (error "Encoded active unit aggregate bound"))
          (let ((hash (nelisp-native-rooted-build-evidence-source-hash path 16777216)))
            (push (list :name name :path (file-name-nondirectory path) :unit-sha256 hash) records)
            (when (and (stringp text) (> (length text) 0))
              (push (list :name name :path (file-name-nondirectory path) :unit-sha256 hash
                          :symbols (vconcat unit-symbols)
                          :relocations (vconcat (plist-get unit :relocs))) metadata))))
        (when (and (integerp bss) (> bss 0)
                   (cl-find "nl_arena_base" unit-symbols
                            :key (lambda (symbol) (plist-get symbol :name)) :test #'equal))
          (when generated-data (error "Ambiguous generated arena owner"))
          (setq generated-data
                (list :unit name :owner-source-sha256 builder-hash
                      :owner-forms-sha256 (secure-hash 'sha256 (prin1-to-string unit))
                      :bss-size bss :symbols (vconcat unit-symbols)))
          (when (and (fboundp 'nelisp-native-load--windows-p) (nelisp-native-load--windows-p))
            (setq generated-data (append generated-data
                                   (list :target "windows-x86_64" :initial-chunk-bytes 67108864)))))))
    (unless generated-data (error "Missing generated arena owner"))
    (unless (equal builder-hash (nelisp-native-rooted-build-evidence-source-hash builder 4194304))
      (error "Builder source changed during rooted generation"))
    (let ((manifest (expand-file-name "active-build.json" directory))
          (meta (expand-file-name "active-unit-metadata.json" directory))
          (data (expand-file-name "generated-data-owner.json" directory)))
      (with-temp-file meta (insert (json-encode (vconcat (nreverse metadata))) "\n"))
      (with-temp-file data (insert (json-encode generated-data) "\n"))
      (with-temp-file manifest
        (insert (json-encode
                 (list :domain "nelisp-rooted-active-build-v1" :builder-source relative
                       :builder-sha256 builder-hash :units (vconcat (nreverse records))
                       :metadata-sha256 (nelisp-native-rooted-build-evidence-source-hash meta 4194304)
                       :generated-data-sha256 (nelisp-native-rooted-build-evidence-source-hash data 1048576))) "\n"))
      (list :manifest manifest :metadata meta :generated-data data))))

(provide 'nelisp-native-rooted-build-evidence)
