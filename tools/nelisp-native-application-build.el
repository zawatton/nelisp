;;; nelisp-native-application-build.el --- bounded AOT intake -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Parse only explicitly selected lexical source modules. No form is evaluated.
;; The result supplies names and IDs to the existing build-time nc1 harvester.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'nelisp-cc-fxir-admission)

(define-error 'nelisp-native-application-build-host-discard-required
  "Activation was attempted; discard this compiler host" 'error)

(defconst nelisp-native-application-build-max-files 32)
(defconst nelisp-native-application-build-max-definitions 256)
(defconst nelisp-native-application-build-max-file-bytes 1048576)
(defconst nelisp-native-application-build-max-total-bytes 8388608)

(defun nelisp-native-application-build--lexical-header-p (bytes)
  "Return non-nil only for one unambiguous supported first-line lexical header.
Do not interpret file-local values or evaluate source."
  (let* ((source (decode-coding-string bytes 'utf-8-unix))
         (first-line (car (split-string source "\n")))
         (start 0) (count 0) opener opener-end closer block)
    (while (string-match "lexical-binding[ \t]*:" source start)
      (setq count (1+ count) start (match-end 0)))
    (setq opener (and (string-prefix-p ";;;" first-line)
                      (string-match "-\\*-" first-line))
          opener-end (and opener (match-end 0))
          closer (and opener-end (string-match "-\\*-" first-line opener-end))
          block (and closer (substring first-line opener-end closer)))
    (and (= count 1) block
         (string-match-p "\\`[ \t]*lexical-binding:[ \t]*t;?[ \t]*\\'" block))))

(defun nelisp-native-application-build--output-plan (discovery outputs)
  "Preflight all five OUTPUT paths against each other and pinned sources."
  (unless (and (proper-list-p outputs) (= (length outputs) 5)
               (cl-every #'stringp outputs))
    (error "Application build requires five output paths"))
  (when (cl-some (lambda (path) (or (file-exists-p path) (file-symlink-p path))) outputs)
    (error "Application output already exists"))
  (let* ((canonical (mapcar #'file-truename outputs))
         (sources (mapcar (lambda (pin) (plist-get pin :file))
                          (plist-get discovery :modules)))
         (parents (mapcar #'file-name-directory canonical)))
    (unless (= (length canonical) (length (delete-dups (copy-sequence canonical))))
      (error "Application output paths alias each other"))
    (when (cl-some (lambda (path) (member path sources)) canonical)
      (error "Application output aliases a pinned source"))
    (unless (cl-every #'file-directory-p parents)
      (error "Application output directory is missing"))
    canonical))

(defun nelisp-native-application-build-discover (modules &optional exclusions baseline-rows)
  "Inspect explicit lexical MODULES and return pinned harvest inputs.
EXCLUSIONS are named defuns to omit and record. BASELINE-ROWS defaults to
currently admitted rows and defines the name and ID collision domain."
  (unless (and (proper-list-p modules) (<= 1 (length modules)
                                           nelisp-native-application-build-max-files)
               (cl-every #'stringp modules))
    (error "Application module count/type bound"))
  (unless (and (or (null exclusions) (proper-list-p exclusions))
               (cl-every #'symbolp exclusions)
               (= (length exclusions) (length (delete-dups (copy-sequence exclusions)))))
    (error "Invalid application exclusion list"))
  (let* ((files (mapcar #'file-truename modules))
         (baseline (or baseline-rows (nelisp-cc-fxir-native-all-rows)))
         (occupied-names (make-hash-table :test 'eq))
         (occupied-ids (make-hash-table :test 'eql))
         (seen (make-hash-table :test 'eq))
         (pins nil) (total 0) defun-count definitions rejections)
    (when (/= (length files) (length (delete-dups (copy-sequence files))))
      (error "Duplicate canonical application module path"))
    (dolist (row baseline)
      (when (and (symbolp (car-safe row)) (integerp (cadr row)))
        (when (gethash (cadr row) occupied-ids)
          (error "Duplicate baseline native ID: %d" (cadr row)))
        (puthash (car row) t occupied-names)
        (puthash (cadr row) t occupied-ids)))
    (dolist (file files)
      (let* ((bytes (nelisp-cc-fxir-admission--bytes
                     file nelisp-native-application-build-max-file-bytes))
             (forms (nelisp-cc-fxir-admission--forms bytes))
             (pin (list :file file :sha256 (secure-hash 'sha256 bytes)
                        :limit nelisp-native-application-build-max-file-bytes)))
        (setq total (+ total (string-bytes bytes)))
        (when (> total nelisp-native-application-build-max-total-bytes)
          (error "Application module total-byte bound"))
        (push pin pins)
        (unless (nelisp-native-application-build--lexical-header-p bytes)
          (error "Application module must declare lexical binding: %s" file))
        (dolist (form forms)
          (when (eq (car-safe form) 'defun)
            (setq defun-count (1+ (or defun-count 0)))
            (when (> defun-count nelisp-native-application-build-max-definitions)
              (error "Application definition count bound"))
            (let ((name (cadr form)))
              (unless (and (symbolp name) name (not (keywordp name))
                           (proper-list-p form) (>= (length form) 4)
                           (listp (nth 2 form))
                           (not (string-match-p "[ \t\r\n]" (symbol-name name))))
                (error "Malformed top-level defun in %s" file))
              (when (gethash name seen) (error "Duplicate application definition: %S" name))
              (puthash name t seen)
              (when (gethash name occupied-names)
                (error "Application definition conflicts with admitted name: %S" name))
              (if (memq name exclusions)
                  (push (list :name name :reason 'excluded-by-request
                              :source pin :form-sha256 (nelisp-cc-fxir-admission--digest form))
                        rejections)
                (push (list :name name :source pin
                            :form-sha256 (nelisp-cc-fxir-admission--digest form)) definitions)))))))
    (setq definitions (nreverse definitions)
          rejections (nreverse rejections)
          pins (nreverse pins))
    (let ((next (if (> (hash-table-count occupied-ids) 0)
                    (1+ (apply #'max (let (ids)
                                      (maphash (lambda (id _value) (push id ids)) occupied-ids)
                                      ids)))
                  20000)))
      (when (> (+ next (length definitions)) 1073741824)
        (error "Application native ID range exhausted"))
      (list :schema 1 :kind :application-intake
            :id0 next
            :baseline-sha256 (nelisp-cc-fxir-admission--digest baseline)
            :modules pins :definitions definitions
            :names (mapcar (lambda (d) (plist-get d :name)) definitions)
            :rejections rejections))))

(defun nelisp-native-application-build-write-inputs
    (discovery names-file manifest-file row-file receipt-file report-file)
  "Preflight the five output paths, then write DISCOVERY's immutable inputs."
  (unless (eq (plist-get discovery :kind) :application-intake)
    (error "Invalid application discovery snapshot"))
  (nelisp-cc-fxir-admission--check-pins (plist-get discovery :modules))
  (unless (equal (plist-get discovery :baseline-sha256)
                 (nelisp-cc-fxir-admission--digest (nelisp-cc-fxir-native-all-rows)))
    (error "Admission baseline changed after discovery"))
  (let* ((names (plist-get discovery :names))
         (outputs (nelisp-native-application-build--output-plan
                   discovery (list names-file manifest-file row-file receipt-file report-file)))
         (names-path (nth 0 outputs)) (manifest-path (nth 1 outputs))
         temps)
    (unless (and (proper-list-p names) (cl-every #'symbolp names)
                 (<= 1 (length names) nelisp-native-application-build-max-definitions))
      (error "Invalid application discovery snapshot"))
    (let* ((names-text (mapconcat (lambda (name) (concat (symbol-name name) "\n")) names ""))
           (manifest-text (concat (nelisp-cc-fxir-admission--print discovery) "\n"))
           (names-temp (make-temp-file (expand-file-name ".nnab-names-" (file-name-directory names-path))))
           (manifest-temp (make-temp-file (expand-file-name ".nnab-manifest-"
                                                            (file-name-directory manifest-path)))))
      (setq temps (list names-temp manifest-temp))
      (unwind-protect
          (progn
            (with-temp-file names-temp (insert names-text))
            (with-temp-file manifest-temp (insert manifest-text))
            (rename-file names-temp names-path)
            (condition-case err
                (rename-file manifest-temp manifest-path)
              (error
               (when (equal (secure-hash 'sha256 names-text)
                            (secure-hash 'sha256 (nelisp-cc-fxir-admission--bytes names-path)))
                 (delete-file names-path))
               (signal (car err) (cdr err))))
            (list :names-file names-path :manifest-file manifest-path
                  :names-sha256 (secure-hash 'sha256 names-text)
                  :manifest-sha256 (secure-hash 'sha256 manifest-text)
                  :output-paths outputs :id0 (plist-get discovery :id0)
                  :names (copy-sequence names)))
        (dolist (file temps)
          (when (file-exists-p file) (delete-file file)))))))

(defun nelisp-native-application-build--require-complete-harvest
    (discovery inputs row-file constant producer)
  "Require an exact successful harvest before existing admission validators.
Reject skipped, missing, malformed, duplicate, or unknown status rows, and
require the raw rows to contain every selected name exactly once with its
expected sequential ID. This adjacent gate intentionally leaves the legacy
partial-result validator unchanged."
  (let* ((names (plist-get discovery :names))
         (stdout-file (plist-get producer :stdout))
         (names-file (plist-get inputs :names-file)))
    (unless (and (eq (plist-get discovery :kind) :application-intake)
                 (proper-list-p names) names
                 (= (length names) (length (delete-dups (copy-sequence names))))
                 (stringp names-file) (stringp stdout-file) (stringp row-file)
                 (not (file-symlink-p names-file))
                 (not (file-symlink-p stdout-file))
                 (not (file-symlink-p row-file))
                 (file-regular-p names-file) (file-regular-p stdout-file)
                 (file-regular-p row-file))
      (error "Missing, aliased, or non-regular complete-harvest artifact"))
    (nelisp-cc-fxir-admission--check-pins
     (list (list :file names-file :sha256 (plist-get inputs :names-sha256)
                 :limit nelisp-native-application-build-max-file-bytes)))
    (unless (equal (nelisp-cc-fxir-admission--bytes names-file)
                   (mapconcat (lambda (name) (concat (symbol-name name) "\n")) names ""))
      (error "Selected names changed before complete-harvest validation"))
    (let ((lines (split-string (nelisp-cc-fxir-admission--bytes stdout-file 134217728)
                               "\n" t))
          (seen (make-hash-table :test 'eq))
          (next (plist-get discovery :id0)))
      (dolist (line lines)
        (when (string-match-p "\\_<HARVEST\\_>" line)
          (unless (string-match "\\`HARVEST[ \t]+\\([^ \t]+\\)\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
            (error "Malformed harvester status line"))
          (let* ((name-text (match-string 1 line))
                 (name (cl-find name-text names :key #'symbol-name :test #'equal))
                 (status (match-string 2 line)))
            (unless name (error "Unexpected harvester status for %s" name-text))
            (when (gethash name seen) (error "Duplicate harvester status for %S" name))
            (puthash name t seen)
            (unless (and status
                         (string-match
                          "\\`ok id=\\([0-9]+\\) words=\\([1-9][0-9]*\\)\\'" status)
                         (= (string-to-number (match-string 1 status)) next))
              (error "Incomplete or non-deterministic harvest for %S" name))
            (setq next (1+ next)))))
      (unless (= (hash-table-count seen) (length names))
        (error "Missing selected harvester status")))
    (let* ((pin (nelisp-cc-fxir-admission--pin row-file))
           (rows (nelisp-cc-fxir-admission--read-rows row-file constant
                                                       (plist-get pin :sha256)))
           (row-names (mapcar #'car rows))
           (row-ids (mapcar #'cadr rows))
           (expected-ids (number-sequence (plist-get discovery :id0)
                                          (1- (+ (plist-get discovery :id0)
                                                 (length names))))))
      (unless (and (equal row-names names)
                   (= (length row-ids) (length (delete-dups (copy-sequence row-ids))))
                   (equal row-ids expected-ids))
        (error "Rows are not the exact selected harvest")))
    t))

(defconst nelisp-native-application-build--request-fields
  '("constant" "id0" "mode" "names_sha256" "output_parent" "pins" "schema"))
(defconst nelisp-native-application-build--pin-roles
  '("application_manifest" "build_manifest" "cold_image" "guard" "harvester"
    "helper" "launcher" "names" "owned_runner" "prelude" "producer"
    "producer_image" "repository"))

(defun nelisp-native-application-build--json-keys (object)
  "Return OBJECT's JSON object keys without imposing their serialized order."
  (let (keys)
    (unless (hash-table-p object) (error "Expected a JSON object"))
    (maphash (lambda (key _value)
               (unless (stringp key) (error "JSON object key is not a string"))
               (push key keys))
             object)
    (sort keys #'string<)))

(defun nelisp-native-application-build--json-hash (pairs)
  "Build a JSON object hash table from PAIRS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair pairs) (puthash (car pair) (cdr pair) table))
    table))

(defconst nelisp-native-application-build--max-image-pin-bytes 67108864
  "Maximum bytes hashed for a producer executable or cold image pin.
Source, metadata, row and literal readers retain their existing limits.")

(defconst nelisp-native-application-build--max-prelude-pin-bytes 8388608
  "Maximum bytes hashed for the fixed prelude pin role.")

(defun nelisp-native-application-build--sha256-file (file &optional limit)
  "Hash literal FILE bytes, bounded by LIMIT (default one MiB).
Read at most one byte beyond the bound to reject growth without an
unbounded allocation, and reject files whose attributes change while read."
  (let* ((limit (or limit 1048576))
         (before (file-attributes file 'integer)))
    (unless (and (integerp limit) (> limit 0) before
                 (not (file-symlink-p file)) (file-regular-p file)
                 (<= (file-attribute-size before) limit))
      (error "Missing or oversized pin input: %s" file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 (1+ limit))
      (let ((after (file-attributes file 'integer)))
        ;; Reading may update atime; identity, size, mtime and ctime must agree.
        (when after (setf (nth 4 before) nil (nth 4 after) nil))
        (unless (and (<= (buffer-size) limit)
                     (= (buffer-size) (file-attribute-size before))
                     (not (file-symlink-p file)) (file-regular-p file)
                     (equal before after))
          (error "Pin input changed while hashing: %s" file)))
      (secure-hash 'sha256 (current-buffer)))))

(defun nelisp-native-application-build--canonical-file-pin (path expected-hash &optional role)
  "Require canonical regular PATH with EXPECTED-HASH and return its truename.
Only prelude permits 8 MiB; producer_image and cold_image permit 64 MiB."
  (unless (and (stringp path) (file-name-absolute-p path)
               (not (file-symlink-p path)) (file-regular-p path)
               (equal path (file-truename path))
               (stringp expected-hash)
               (string-match-p "\\`[0-9a-f]\\{64\\}\\'" expected-hash)
               (condition-case failure
                   (equal expected-hash
                          (nelisp-native-application-build--sha256-file
                           path (cond
                                 ((equal role "prelude")
                                  nelisp-native-application-build--max-prelude-pin-bytes)
                                 ((member role '("producer_image" "cold_image"))
                                  nelisp-native-application-build--max-image-pin-bytes)
                                 (t 1048576))))
                 (error (error "Request file pin role %S: %s"
                               role (error-message-string failure)))))
    (error "Request file pin is missing, aliased, or mismatched: %S" path))
  path)

(defun nelisp-native-application-build--validate-request
    (discovery inputs producer-spec row-file constant)
  "Validate producer request, fixed pins, and agreement with DISCOVERY."
  (let* ((request (plist-get producer-spec :request))
         (pins (and (hash-table-p request) (gethash "pins" request)))
         (mode (and (hash-table-p request) (gethash "mode" request)))
         (names-file (plist-get inputs :names-file))
         (manifest-file (plist-get inputs :manifest-file))
         (names-hash (plist-get inputs :names-sha256))
         (manifest-hash (plist-get inputs :manifest-sha256)))
    (unless (and (hash-table-p request)
                 (equal (nelisp-native-application-build--json-keys request)
                        nelisp-native-application-build--request-fields)
                 (equal (gethash "schema" request) "nelisp-native-application-harvest/v1")
                 (member mode '("strict" "selected-definitions"))
                 (equal (gethash "names_sha256" request) names-hash)
                 (equal (gethash "id0" request) (plist-get discovery :id0))
                 (not (eq (gethash "id0" request) t))
                 (equal (gethash "constant" request) (symbol-name constant))
                 (hash-table-p pins)
                 (equal (nelisp-native-application-build--json-keys pins)
                        nelisp-native-application-build--pin-roles)
                 (stringp (gethash "output_parent" request))
                 (file-name-absolute-p (gethash "output_parent" request))
                 (file-directory-p (gethash "output_parent" request))
                 (not (file-symlink-p (gethash "output_parent" request)))
                 (equal (gethash "output_parent" request)
                        (file-truename (gethash "output_parent" request)))
                 (symbolp constant) (not (keywordp constant))
                 (equal (file-truename row-file)
                        (nth 2 (plist-get inputs :output-paths))))
      (error "Invalid or inconsistent canonical producer request"))
    (let* ((names-pin (gethash "names" pins))
           (manifest-pin (gethash "application_manifest" pins))
           (producer-pin (gethash "producer" pins))
           (image-pin (gethash "producer_image" pins))
           (adapter (plist-get producer-spec :adapter))
           (adapter-hash (plist-get producer-spec :adapter-sha256))
           (python (plist-get producer-spec :python)))
      (dolist (role nelisp-native-application-build--pin-roles)
        (let ((pin (gethash role pins)))
          (unless (and (hash-table-p pin)
                       (equal (nelisp-native-application-build--json-keys pin)
                              '("path" "sha256")))
            (error "Malformed request pin role %s" role))
          (if (equal role "repository")
              (let ((path (gethash "path" pin)))
                (unless (and (stringp path) (file-name-absolute-p path)
                             (file-directory-p path) (not (file-symlink-p path))
                             (equal path (file-truename path))
                             (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                                             (gethash "sha256" pin)))
                  (error "Malformed repository tree pin")))
            (nelisp-native-application-build--canonical-file-pin
             (gethash "path" pin) (gethash "sha256" pin) role))))
      (unless (and (equal (gethash "path" names-pin) (file-truename names-file))
                   (equal (gethash "sha256" names-pin) names-hash)
                   (equal (gethash "path" manifest-pin) (file-truename manifest-file))
                   (equal (gethash "sha256" manifest-pin) manifest-hash)
                   (stringp (gethash "path" producer-pin))
                   (stringp (gethash "path" image-pin))
                   (stringp adapter) (file-name-absolute-p adapter)
                   (not (file-symlink-p adapter)) (file-regular-p adapter)
                   (equal adapter (file-truename adapter))
                   (equal adapter-hash
                          (nelisp-native-application-build--sha256-file adapter))
                   (stringp python) (file-name-absolute-p python)
                   (not (file-symlink-p python)) (file-executable-p python)
                   (equal python (file-truename python))
                   (plist-member producer-spec :condition-facts)
                   (memq (plist-get producer-spec :condition-facts) '(nil t)))
        (error "Producer spec pins or host condition option do not match request"))
      (list :request request :pins pins :mode mode :adapter adapter
            :adapter-sha256 adapter-hash :python python))))

(defun nelisp-native-application-build--invoke-adapter
    (python adapter request-file stdout-file stderr-file)
  "Invoke PYTHON ADAPTER directly with one absolute request-file argument."
  (let (status)
    (with-temp-buffer
      (setq status
            (call-process python nil (list t stderr-file) nil adapter request-file))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) stdout-file nil 'silent)))
    status))

(defun nelisp-native-application-build--read-json-file (file)
  "Read one bounded JSON object from FILE."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        (json-false :false)
        (json-null nil))
    (json-parse-string
     (decode-coding-string (nelisp-cc-fxir-admission--bytes file 1048576) 'utf-8-unix)
     :object-type 'hash-table :array-type 'list :null-object nil :false-object :false)))

(defun nelisp-native-application-build--check-child-lifecycle
    (lifecycle request-info output child)
  "Verify the owned runner's actual identity, argv, environment, deadline and cleanup."
  (let* ((request (plist-get request-info :request))
         (pins (plist-get request-info :pins))
         (environment (gethash "environment" lifecycle))
         (child-id (gethash "child" lifecycle))
         (controller-id (gethash "controller" lifecycle))
         (cleanup (gethash "cleanup" lifecycle))
         (producer-image (gethash "path" (gethash "producer_image" pins)))
         (manifest (gethash "path" (gethash "application_manifest" pins)))
         (repository (gethash "path" (gethash "repository" pins)))
         (driver (expand-file-name "producer-driver.el" output))
         (expected-argv (list producer-image "--load" driver))
         (expected-env
          (nelisp-native-application-build--json-hash
           (list (cons "LC_ALL" "C")
                 (cons "NC1_CONST" (gethash "constant" request))
                 (cons "NC1_ID0" (number-to-string (gethash "id0" request)))
                 (cons "NC1_NAMES" (expand-file-name "names.txt" output))
                 (cons "NC1_OUT" (expand-file-name "rows.el" output))
                 (cons "NELISP_APPLICATION_INSTALL_RECEIPT"
                       (expand-file-name "install-receipt.el" output))
                 (cons "NELISP_APPLICATION_MANIFEST" manifest)
                 (cons "NELISP_APPLICATION_MANIFEST_SHA256"
                       (gethash "sha256" (gethash "application_manifest" pins)))
                 (cons "NELISP_APPLICATION_MODE" (gethash "mode" request))
                 (cons "NELISP_APPLICATION_NAMES_SHA256" (gethash "names_sha256" request))
                 (cons "NELISP_APPLICATION_ROOT" repository)
                 (cons "NELISP_FXIR_ROOT" repository)
                 (cons "NELISP_PRODUCER_IMAGE_SHA256"
                       (gethash "sha256" (gethash "producer_image" pins)))))))
    (unless (and (equal (gethash "argv" lifecycle) expected-argv)
                 (stringp (gethash "cwd" lifecycle))
                 (equal (gethash "cwd" lifecycle) (expand-file-name "cwd" child))
                 (file-directory-p (gethash "cwd" lifecycle))
                 (not (file-symlink-p (gethash "cwd" lifecycle)))
                 (hash-table-p environment)
                 (equal (nelisp-native-application-build--json-keys environment)
                        (append (nelisp-native-application-build--json-keys expected-env)
                                '("PATH")))
                 (equal (gethash "PATH" environment)
                        (or (getenv "PATH") "/usr/bin:/bin")))
      (error "Owned child argv, working directory, or environment changed"))
    (maphash (lambda (key value)
               (unless (equal (gethash key environment) value)
                 (error "Owned child environment changed at %s" key)))
             expected-env)
    (unless (and (hash-table-p child-id) (hash-table-p controller-id)
                 (and (integerp (gethash "pid" child-id))
                      (> (gethash "pid" child-id) 0)
                      (integerp (gethash "session" child-id))
                      (> (gethash "session" child-id) 0)
                      (integerp (gethash "start_ticks" child-id))
                      (> (gethash "start_ticks" child-id) 0)
                      (integerp (gethash "uid" child-id))
                      (>= (gethash "uid" child-id) 0))
                 (and (integerp (gethash "pid" controller-id))
                      (> (gethash "pid" controller-id) 0)
                      (integerp (gethash "session" controller-id))
                      (> (gethash "session" controller-id) 0)
                      (integerp (gethash "start_ticks" controller-id))
                      (> (gethash "start_ticks" controller-id) 0)
                      (integerp (gethash "uid" controller-id))
                      (>= (gethash "uid" controller-id) 0))
                 (= (gethash "pid" child-id) (gethash "session" child-id))
                 (hash-table-p cleanup)
                 (eq (gethash "verified" cleanup) t)
                 (null (gethash "errors" cleanup))
                 (null (gethash "initial" cleanup))
                 (null (gethash "remaining" cleanup)))
      (error "Owned runner identity, deadline, or cleanup evidence is invalid"))))

(defun nelisp-native-application-build--check-install-receipt
    (file discovery inputs pins mode staged-names-file)
  "Authenticate raw producer FILE receipt against discovery and request PINS.
STAGED-NAMES-FILE is the exact authenticated adapter-owned staging path."
  (unless (and (not (file-symlink-p file)) (file-regular-p file))
    (error "Raw producer receipt is missing or aliased"))
  (let* ((forms (nelisp-cc-fxir-admission--forms
                 (nelisp-cc-fxir-admission--bytes file 1048576)))
         (receipt (car forms))
         (definitions (plist-get receipt :definitions))
         (names (plist-get discovery :names))
         (module-pins (plist-get discovery :modules))
         (pin-hash (lambda (role) (gethash "sha256" (gethash role pins))))
         (expected-mode (if (equal mode "selected-definitions")
                            :selected-definitions :strict)))
    (unless (and (= (length forms) 1)
                 (eq (plist-get receipt :status) :installed)
                 (null (plist-get receipt :native-qualified))
                 (eq (plist-get receipt :module-initialization-executed) nil)
                 (eq (plist-get receipt :whole-module-initialization-equivalence) nil)
                 (equal (plist-get receipt :names) names)
                 (equal (plist-get receipt :manifest-file)
                        (gethash "path" (gethash "application_manifest" pins)))
                 (equal (plist-get receipt :manifest-sha256)
                        (plist-get inputs :manifest-sha256))
                 (equal (plist-get receipt :names-file) staged-names-file)
                 (equal (plist-get receipt :names-sha256) (plist-get inputs :names-sha256))
                 (equal (plist-get receipt :source-modules) module-pins)
                 (equal (plist-get receipt :baseline-sha256)
                        (plist-get discovery :baseline-sha256))
                 (eq (plist-get receipt :mode) expected-mode)
                 (equal (plist-get receipt :omitted-definition-names)
                        (mapcar (lambda (entry) (plist-get entry :name))
                                (plist-get discovery :rejections)))
                 (equal (plist-get receipt :producer-image-sha256)
                        (funcall pin-hash "producer_image"))
                 (equal (plist-get receipt :producer-source-sha256)
                        (funcall pin-hash "producer"))
                 (equal (plist-get receipt :helper-sha256) (funcall pin-hash "helper"))
                 (equal (plist-get receipt :prelude-sha256) (funcall pin-hash "prelude"))
                 (equal (mapcar (lambda (entry) (plist-get entry :name)) definitions) names))
      (error "Raw producer receipt does not authenticate selected discovery"))
    (cl-mapc
     (lambda (definition expected)
       (unless (and (eq (plist-get definition :installed-identity-match) t)
                    (eq (plist-get definition :installed-structure-match) t)
                    (eq (plist-get definition :arguments-authenticated) t)
                    (eq (plist-get definition :body-authenticated) t)
                    (equal (plist-get definition :source-form-sha256)
                           (plist-get expected :form-sha256)))
         (error "Raw producer receipt changed source identity for %S"
                (plist-get expected :name))))
     definitions (plist-get discovery :definitions))
    receipt))

(defun nelisp-native-application-build--check-owned-capture
    (response request-file request-json request-info)
  "Validate successful RESPONSE, raw capture, and child output ownership."
  (unless (and (hash-table-p response)
               (equal (nelisp-native-application-build--json-keys response)
                      '("capture" "output" "status"))
               (equal (gethash "status" response) "RAW_CAPTURED_NOT_ADMITTED"))
    (error "Adapter did not return one raw-capture result"))
  (let* ((output (gethash "output" response))
         (capture-file (gethash "capture" response))
         (output-parent (file-name-directory request-file)))
    (unless (and (stringp output) (file-name-absolute-p output)
                 (file-directory-p output) (not (file-symlink-p output))
                 (equal output (file-truename output))
                 (equal (file-name-directory output) output-parent)
                 (equal capture-file (expand-file-name "capture.json" output)))
      (error "Adapter output path is foreign, aliased, or malformed"))
    (let* ((capture (nelisp-native-application-build--read-json-file capture-file))
           (required '("child_lifecycle" "outputs" "owned_report" "pins"
                       "rejected" "request_sha256" "runner_ast_sha256" "status"))
           (child (expand-file-name "child" output))
           (paths (mapcar (lambda (name) (expand-file-name name output))
                          '("names.txt" "producer-driver.el" "install-receipt.el" "rows.el")))
           (child-files (mapcar (lambda (name) (expand-file-name name child))
                                '("command.json" "launch.json" "stdout" "stderr" "rc"))))
      (unless (and (hash-table-p capture)
                   (equal (nelisp-native-application-build--json-keys capture) required)
                   (equal (gethash "status" capture) "RAW_CAPTURED_NOT_ADMITTED")
                   (eq (gethash "rejected" capture) :false)
                   (equal (gethash "request_sha256" capture)
                          (nelisp-native-application-build--sha256-file request-file))
                   (equal (nelisp-cc-fxir-admission--bytes request-file 1048576)
                          (encode-coding-string request-json 'utf-8-unix))
                   (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                                   (gethash "runner_ast_sha256" capture))
                   (equal (nelisp-native-application-build--json-keys
                           (gethash "pins" capture))
                          nelisp-native-application-build--pin-roles)
                   (equal (nelisp-native-application-build--json-keys
                           (gethash "outputs" capture))
                          '("command.json" "install-receipt.el" "launch.json"
                            "names.txt" "producer-driver.el" "rc" "rows.el"
                            "stderr" "stdout"))
                   (hash-table-p (gethash "child_lifecycle" capture))
                   (hash-table-p (gethash "owned_report" capture)))
        (error "Adapter capture did not authenticate the canonical request"))
      (dolist (role nelisp-native-application-build--pin-roles)
        (let ((captured-pin (gethash role (gethash "pins" capture)))
              (request-pin (gethash role (plist-get request-info :pins))))
          (unless (and (hash-table-p captured-pin)
                       (equal (nelisp-native-application-build--json-keys captured-pin)
                              '("path" "sha256"))
                       (equal (gethash "path" captured-pin)
                              (gethash "path" request-pin))
                       (equal (gethash "sha256" captured-pin)
                              (gethash "sha256" request-pin)))
            (error "Adapter capture changed pin role %s" role))))
      (dolist (path (append (list capture-file) paths child-files))
        (unless (and (not (file-symlink-p path)) (file-regular-p path)
                     (file-in-directory-p path output))
          (error "Adapter produced a missing, symlinked, or foreign artifact: %s" path)))
      (dolist (name '("command.json" "install-receipt.el" "launch.json" "names.txt"
                      "producer-driver.el" "rc" "rows.el" "stderr" "stdout"))
        (let* ((path (if (member name '("command.json" "launch.json" "rc" "stderr" "stdout"))
                         (expand-file-name name child)
                       (expand-file-name name output)))
               (expected (gethash name (gethash "outputs" capture))))
          (unless (and (stringp expected)
                       (string-match-p "\\`[0-9a-f]\\{64\\}\\'" expected)
                       (equal expected
                              (nelisp-native-application-build--sha256-file path)))
            (error "Owned output hash mismatch: %s" name))))
      (let* ((lifecycle (gethash "child_lifecycle" capture))
             (child (expand-file-name "child" output)))
        (unless (and (hash-table-p lifecycle)
                     (integerp (gethash "rc" lifecycle))
                     (= (gethash "rc" lifecycle) 0)
                     (eq (gethash "timed_out" lifecycle) :false)
                     (eq (gethash "cancelled" lifecycle) :false)
                     (= (gethash "timeout" lifecycle) 45))
          (error "Owned runner identity, deadline, or cleanup evidence is invalid: %S"
                 lifecycle)))
      (nelisp-native-application-build--check-child-lifecycle
       (gethash "child_lifecycle" capture) request-info output child)
      (unless (and (equal (string-trim
                           (nelisp-cc-fxir-admission--bytes
                            (expand-file-name "rc" child) 64)) "0")
                   (zerop (string-bytes
                           (nelisp-cc-fxir-admission--bytes
                            (expand-file-name "stderr" child) 1048576))))
        (error "Owned child status or stderr is invalid"))
      (list :output output :capture capture :rows (nth 3 paths)
            :install-receipt (nth 2 paths)
            :stdout (expand-file-name "stdout" child)
            :stderr (expand-file-name "stderr" child)
            :request-file request-file))))

(defun nelisp-native-application-build--host-condition-facts ()
  "Return the effective M2 option, defaulting to the direct-build OFF state."
  (and (boundp 'nelisp-cc-fxir-native-condition-facts)
       nelisp-cc-fxir-native-condition-facts))

(defun nelisp-native-application-build--check-condition-option (requested)
  "Require REQUESTED host M2 option to equal the already-effective option."
  (unless (and (memq requested '(nil t))
               (eq requested
                   (nelisp-native-application-build--host-condition-facts)))
    (error "Requested condition-facts option mismatches the effective host option")))

(defun nelisp-native-application-build--activation-inputs
    (discovery inputs planned-outputs owned-output)
  "Authenticate adapter-owned output mapping and copy INPUTS for activation."
  (let* ((planned (plist-get inputs :output-paths))
         (names-file (plist-get inputs :names-file))
         (manifest-file (plist-get inputs :manifest-file))
         (directory (plist-get owned-output :output))
         (rows-file (plist-get owned-output :rows))
         (receipt-file (expand-file-name "admission-receipt.sexp" directory))
         (report-file (expand-file-name "admission-report.sexp" directory))
         (install-receipt (plist-get owned-output :install-receipt))
         (new-outputs (list names-file manifest-file rows-file receipt-file report-file))
         (canonical (mapcar #'file-truename new-outputs)))
    (unless (and (equal planned planned-outputs)
                 (equal (nth 0 planned) (file-truename names-file))
                 (equal (nth 1 planned) (file-truename manifest-file))
                 (equal canonical new-outputs)
                 (= (length canonical) (length (delete-dups (copy-sequence canonical))))
                 (not (file-exists-p receipt-file)) (not (file-symlink-p receipt-file))
                 (not (file-exists-p report-file)) (not (file-symlink-p report-file))
                 (equal rows-file (expand-file-name "rows.el" directory))
                 (equal install-receipt (expand-file-name "install-receipt.el" directory))
                 (not (file-symlink-p rows-file)) (file-regular-p rows-file)
                 (not (file-symlink-p install-receipt)) (file-regular-p install-receipt)
                 (file-in-directory-p rows-file directory)
                 (file-in-directory-p receipt-file directory)
                 (file-in-directory-p report-file directory)
                 (not (cl-some (lambda (path)
                                 (member path (mapcar (lambda (pin) (plist-get pin :file))
                                                      (plist-get discovery :modules))))
                               canonical)))
      (error "Owned adapter output does not match the planned activation inputs"))
    (let ((activation-inputs (copy-sequence inputs)))
      (setq activation-inputs
            (plist-put activation-inputs :output-paths canonical))
      (setq activation-inputs
            (plist-put activation-inputs :owned-output-mapping
                       (list :schema 1 :planned-output-paths (copy-sequence planned)
                             :output-directory directory :raw-rows rows-file
                             :raw-rows-sha256
                             (nelisp-native-application-build--sha256-file rows-file)
                             :raw-install-receipt install-receipt
                             :admission-receipt receipt-file :report report-file)))
      activation-inputs)))

(defun nelisp-native-application-build--delete-owned-tree (directory)
  "Remove DIRECTORY without traversing any symlink found beneath it."
  (when (file-symlink-p directory) (error "Owned directory became a symlink"))
  (when (file-directory-p directory)
    (dolist (name (directory-files directory nil directory-files-no-dot-files-regexp t))
      (let ((path (expand-file-name name directory)))
        (cond ((file-symlink-p path) (delete-file path))
              ((file-directory-p path)
               (nelisp-native-application-build--delete-owned-tree path))
              ((file-exists-p path) (delete-file path)))))
    (delete-directory directory)))

(defun nelisp-native-application-build--condition-source-hash ()
  "Regenerate the native source under the current option and return its hash."
  (unless (and (fboundp 'nelisp-cc-fxir-native-source)
               (boundp 'nelisp-cc-fxir-native--source))
    (error "Native source/configuration is not loaded in this host"))
  (let ((fresh (nelisp-cc-fxir-native-source)))
    (unless (equal fresh nelisp-cc-fxir-native--source)
      (error "Cached native source does not match the effective condition option"))
    (secure-hash 'sha256 (nelisp-cc-fxir-admission--print fresh))))

(defun nelisp-native-application-build-run-owned
    (discovery inputs producer-spec row-file constant receipt-file report-file)
  "Run one owned pinned producer/harvester, then activate in this host.

PRODUCER-SPEC is a plist with :adapter (canonical script path),
:adapter-sha256, :python (resolved interpreter path), :request (a seven-field
JSON object in a hash table, with the exact 13 canonical pin roles), and
:condition-facts (explicit nil or t, separate from the Python request).
The request's names and raw Lisp application_manifest pins must equal INPUTS;
its mode, names hash, ID0, and constant must match DISCOVERY and this call.
ROW-FILE, RECEIPT-FILE, and REPORT-FILE are the original planned destinations
from `write-inputs'; the adapter's dynamic raw rows and new host receipt/report
are mapped explicitly into a copied activation input. The original INPUTS and
its planned outputs remain unchanged. The return value is the existing
activation result for the same live host, before its caller loads the builder.
If this function signals `nelisp-native-application-build-host-discard-required',
the caller must discard this compiler host and must not hand it to the builder."
  (let* ((planned-outputs (mapcar #'file-truename
                                  (list (plist-get inputs :names-file)
                                        (plist-get inputs :manifest-file)
                                        row-file receipt-file report-file)))
         (request-info (nelisp-native-application-build--validate-request
                        discovery inputs producer-spec row-file constant))
         (condition-facts (plist-get producer-spec :condition-facts))
         (base-output-parent
          (gethash "output_parent" (plist-get request-info :request)))
         (repository (gethash "path"
                              (gethash "repository" (plist-get request-info :pins))))
         run-root request-file stdout-file stderr-file request-json request-hash
         adapter-output owned-output activation-inputs activation source-hash
         activation-attempted metadata result)
    (unless (and (equal (plist-get inputs :output-paths) planned-outputs)
                 (equal (nth 0 planned-outputs) (file-truename (plist-get inputs :names-file)))
                 (equal (nth 1 planned-outputs) (file-truename (plist-get inputs :manifest-file)))
                 (not (file-in-directory-p base-output-parent repository)))
      (error "Original input output plan is inconsistent or inside the repository"))
    (nelisp-native-application-build--check-condition-option condition-facts)
    (setq run-root
          (make-temp-file (expand-file-name ".nnab-owned-" base-output-parent) t))
    (condition-case failure
        (progn
          (setq request-info (plist-put request-info :request
                                        (copy-hash-table (plist-get request-info :request))))
          (puthash "output_parent" run-root (plist-get request-info :request))
          (setq request-file (expand-file-name "request.json" run-root)
                stdout-file (expand-file-name "adapter.stdout" run-root)
                stderr-file (expand-file-name "adapter.stderr" run-root)
                request-json (concat (json-serialize (plist-get request-info :request)) "\n"))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file request-file (insert request-json)))
          (setq request-hash
                (nelisp-native-application-build--sha256-file request-file))
          ;; Keep the exact request data shape; only its required output_parent
          ;; is scoped to this fresh private invocation directory.
          (dolist (role nelisp-native-application-build--pin-roles)
            (unless (equal role "repository")
              (let ((pin (gethash role (plist-get request-info :pins))))
                (nelisp-native-application-build--canonical-file-pin
                 (gethash "path" pin) (gethash "sha256" pin) role))))
          (nelisp-cc-fxir-admission--check-pins (plist-get discovery :modules))
          (unless (equal (nelisp-native-application-build--sha256-file
                          (plist-get request-info :adapter))
                         (plist-get request-info :adapter-sha256))
            (error "Pinned adapter changed before invocation"))
          (let ((status (nelisp-native-application-build--invoke-adapter
                         (plist-get request-info :python)
                         (plist-get request-info :adapter)
                         request-file stdout-file stderr-file)))
            (unless (and (integerp status) (= status 0)
                         (file-regular-p stdout-file) (not (file-symlink-p stdout-file))
                         (file-regular-p stderr-file) (not (file-symlink-p stderr-file))
                         (= (string-bytes
                             (nelisp-cc-fxir-admission--bytes stderr-file 1048576)) 0))
              (error "Pinned harvest adapter failed (status %S): %s"
                     status
                     (if (file-regular-p stderr-file)
                         (string-trim
                          (nelisp-cc-fxir-admission--bytes stderr-file 4096))
                       "stderr unavailable")))
            (setq adapter-output
                  (nelisp-native-application-build--read-json-file stdout-file)))
          (setq owned-output
                (nelisp-native-application-build--check-owned-capture
                 adapter-output request-file request-json request-info))
          (let ((staged-names-file
                 (expand-file-name "names.txt" (plist-get owned-output :output))))
            (unless (equal (nelisp-cc-fxir-admission--bytes
                            staged-names-file
                            nelisp-native-application-build-max-file-bytes)
                           (nelisp-cc-fxir-admission--bytes
                            (plist-get inputs :names-file)
                            nelisp-native-application-build-max-file-bytes))
              (error "Adapter-owned names copy differs from the Lisp input"))
            (nelisp-native-application-build--check-install-receipt
             (plist-get owned-output :install-receipt) discovery inputs
             (plist-get request-info :pins) (plist-get request-info :mode)
             staged-names-file))
          ;; Python revalidates all explicit pins and the repository tree;
          ;; Lisp rechecks every source module embedded only in the raw plist.
          (nelisp-cc-fxir-admission--check-pins (plist-get discovery :modules))
          (dolist (role nelisp-native-application-build--pin-roles)
            (unless (equal role "repository")
              (let ((pin (gethash role (plist-get request-info :pins))))
                (nelisp-native-application-build--canonical-file-pin
                 (gethash "path" pin) (gethash "sha256" pin) role))))
          (unless (equal (nelisp-native-application-build--sha256-file
                          (plist-get request-info :adapter))
                         (plist-get request-info :adapter-sha256))
            (error "Pinned adapter changed after child completion"))
          (nelisp-native-application-build--check-condition-option condition-facts)
          (nelisp-native-application-build--require-complete-harvest
           discovery inputs (plist-get owned-output :rows) constant
           (list :stdout (plist-get owned-output :stdout)))
          (setq activation-inputs
                (nelisp-native-application-build--activation-inputs
                 discovery inputs planned-outputs owned-output))
          (setq source-hash (nelisp-native-application-build--condition-source-hash))
          (let* ((configuration
                  (list :schema 1 :kind :native-application-host-configuration
                        :condition-facts condition-facts
                        :request-sha256 request-hash
                        :adapter-sha256 (plist-get request-info :adapter-sha256)
                        :generated-source-sha256 source-hash))
                 (configuration-text
                  (concat (nelisp-cc-fxir-admission--print configuration) "\n"))
                 (configuration-hash (secure-hash 'sha256 configuration-text))
                 (configuration-file
                  (expand-file-name "host-configuration.sexp"
                                    (plist-get owned-output :output))))
            (when (or (file-exists-p configuration-file) (file-symlink-p configuration-file))
              (error "Host configuration metadata path is already owned"))
            (with-temp-file configuration-file (insert configuration-text))
            ;; The unchanged validator can publish globals before a later
            ;; report write fails, so any error from this call requires the
            ;; caller to discard the host.
            (setq activation-attempted t)
            (setq activation
                  (nelisp-native-application-build-prepare-and-activate
                   discovery activation-inputs (plist-get owned-output :rows) constant
                   (list :binary (gethash "path"
                                           (gethash "producer_image"
                                                    (plist-get request-info :pins)))
                         :cold (gethash "path"
                                        (gethash "cold_image"
                                                 (plist-get request-info :pins)))
                         :build-manifest (gethash "path"
                                                  (gethash "build_manifest"
                                                           (plist-get request-info :pins)))
                         :stdout (plist-get owned-output :stdout))
                   (plist-get (plist-get activation-inputs :owned-output-mapping)
                              :admission-receipt)
                   (plist-get (plist-get activation-inputs :owned-output-mapping) :report)))
            (nelisp-native-application-build--check-condition-option condition-facts)
            (let* ((activated-source-hash
                    (nelisp-native-application-build--condition-source-hash))
                   (activation-file
                    (expand-file-name "host-activation.sexp"
                                      (plist-get owned-output :output)))
                   (activation-metadata
                    (list :schema 1 :kind :native-application-host-activation
                          :condition-facts condition-facts
                          :configuration-sha256 configuration-hash
                          :generated-source-sha256 activated-source-hash
                          :request-sha256 request-hash
                          :raw-rows-sha256
                          (plist-get (plist-get activation-inputs :owned-output-mapping)
                                     :raw-rows-sha256))))
              (when (or (file-exists-p activation-file) (file-symlink-p activation-file))
                (error "Host activation metadata path is already owned"))
              (with-temp-file activation-file
                (insert (nelisp-cc-fxir-admission--print activation-metadata) "\n"))
              (setq result
                    (append activation
                            (list :owned-output-mapping
                                  (plist-get activation-inputs :owned-output-mapping)
                                  :raw-capture (plist-get owned-output :capture)
                                  :request-sha256 request-hash
                                  :host-configuration-file configuration-file
                                  :host-activation-file activation-file
                                  :condition-facts condition-facts)))))
          result)
      ((error quit)
       (let (cleanup-errors)
         (condition-case cleanup
             (nelisp-native-application-build--delete-owned-tree run-root)
           ((error quit) (push cleanup cleanup-errors)))
         (cond
          (activation-attempted
           (signal 'nelisp-native-application-build-host-discard-required
                   (list :activation-attempted t :cause failure
                         :owned-cleanup-errors (nreverse cleanup-errors))))
          (cleanup-errors
           (signal (car failure)
                   (append (cdr failure)
                           (list :owned-cleanup-errors (nreverse cleanup-errors)))))
          (t (signal (car failure) (cdr failure)))))))))

(defun nelisp-native-application-build-prepare-and-activate
    (discovery inputs row-file constant producer receipt-file report-file)
  "Validate completed PRODUCER artifacts, prepare admission, then activate it.

Call after the existing nc1 harvester finishes and before loading the
standalone builder. DISCOVERY is the pinned result from `discover'. Every
candidate receives one status in REPORT-FILE, including harvester declines.
Activation retains the existing code/IR/callee/arity checks and is not a
native execution witness."
  (unless (and (eq (plist-get discovery :kind) :application-intake)
               (stringp (plist-get inputs :manifest-file))
               (stringp (plist-get inputs :names-file))
               (symbolp constant) (stringp row-file) (stringp receipt-file)
               (stringp report-file))
    (error "Invalid application preparation request"))
  (let* ((outputs (plist-get inputs :output-paths))
         (requested (mapcar #'file-truename
                            (list (plist-get inputs :names-file) (plist-get inputs :manifest-file)
                                  row-file receipt-file report-file)))
         (sources (mapcar (lambda (pin) (plist-get pin :file))
                          (plist-get discovery :modules)))
         (receipt-path (nth 3 requested))
         (report-path (nth 4 requested))
         (producer-paths
          (delq nil (mapcar (lambda (role)
                              (let ((path (plist-get producer role)))
                                (and (stringp path) (file-truename path))))
                            '(:binary :cold :build-manifest :stdout))))
         (all-paths (append requested producer-paths)))
    (unless (and (proper-list-p outputs) (= (length outputs) 5)
                 (equal outputs requested)
                 (= (length all-paths) (length (delete-dups (copy-sequence all-paths))))
                 (not (cl-some (lambda (path) (member path sources)) outputs)))
      (error "Application output plan changed or aliases an input"))
    (unless (and (file-exists-p (nth 0 requested)) (file-exists-p (nth 1 requested))
                 (file-exists-p (nth 2 requested))
                 (not (or (file-exists-p receipt-path) (file-symlink-p receipt-path)
                          (file-exists-p report-path) (file-symlink-p report-path)))
                 (equal (plist-get discovery :baseline-sha256)
                        (nelisp-cc-fxir-admission--digest
                         (nelisp-cc-fxir-native-all-rows))))
      (error "Application inputs, output ownership, or admission baseline changed")))
  (nelisp-cc-fxir-admission--check-pins
   (list (list :file (plist-get inputs :manifest-file)
               :sha256 (plist-get inputs :manifest-sha256) :limit 1048576)
         (list :file (plist-get inputs :names-file)
               :sha256 (plist-get inputs :names-sha256) :limit 1048576)))
  (unless (and (equal (nelisp-cc-fxir-admission--forms
                       (nelisp-cc-fxir-admission--bytes (plist-get inputs :manifest-file)))
                      (list discovery))
               (equal (nelisp-cc-fxir-admission--bytes (plist-get inputs :names-file))
                      (mapconcat (lambda (name) (concat (symbol-name name) "\n"))
                                 (plist-get discovery :names) "")))
    (error "Application discovery inputs changed after harvest preparation"))
  (nelisp-cc-fxir-admission--check-pins (plist-get discovery :modules))
  (let* ((stdout (nelisp-cc-fxir-admission--bytes (plist-get producer :stdout) 134217728))
         (lines (split-string stdout "\n" t))
         (row-pin (nelisp-cc-fxir-admission--pin row-file))
         (next (plist-get discovery :id0)) statuses accepted rejected)
    (dolist (name (plist-get discovery :names))
      (let* ((prefix (format "HARVEST %s " (regexp-quote (symbol-name name))))
             (matches (cl-remove-if-not (lambda (line) (string-match-p (concat "^" prefix) line)) lines)))
        (unless (= (length matches) 1)
          (error "Missing or duplicate harvester status for %S" name))
        (let ((line (car matches)))
          (if (string-match (concat "^HARVEST " (regexp-quote (symbol-name name))
                                    " ok id=\\([0-9]+\\) words=[0-9]+$") line)
              (let ((id (string-to-number (match-string 1 line))))
                (unless (= id next) (error "Non-deterministic native ID for %S" name))
                (setq next (1+ next))
                (push (list :name name :status 'harvested :id id) statuses)
                (push name accepted))
            (unless (string-match (concat "^HARVEST " (regexp-quote (symbol-name name))
                                          " skip \\(.*\\)$") line)
              (error "Malformed harvester status for %S" name))
            (let* ((definition (cl-find name (plist-get discovery :definitions)
                                         :key (lambda (d) (plist-get d :name)) :test #'eq))
                   (entry (list :name name :status 'rejected :reason (match-string 1 line)
                                :source (plist-get definition :source)
                                :form-sha256 (plist-get definition :form-sha256))))
              (push entry statuses)
              (push entry rejected))))))
    (setq statuses (nreverse statuses) accepted (nreverse accepted) rejected (nreverse rejected))
    (let* ((rows (nelisp-cc-fxir-admission--read-rows
                  row-file constant (plist-get row-pin :sha256)))
           (row-names (mapcar #'car rows)))
      (unless (equal row-names accepted)
        (error "Harvested rows do not match successful producer statuses")))
    (let ((summary
           (list :schema 1 :kind :application-intake-result
                 :discovery-sha256 (nelisp-cc-fxir-admission--digest discovery)
                 :definitions
                 (mapcar (lambda (definition)
                           (let* ((name (plist-get definition :name))
                                  (status (cl-find name statuses :key (lambda (s) (plist-get s :name))))
                                  (identity (list :name name :source (plist-get definition :source)
                                                  :form-sha256 (plist-get definition :form-sha256))))
                             (append identity status)))
                         (plist-get discovery :definitions))
                 :excluded (plist-get discovery :rejections)
                 :rows-sha256 (plist-get row-pin :sha256)
                 :producer-stdout-sha256 (secure-hash 'sha256 stdout)
                 :native-qualified nil)))
      (with-temp-file report-file
        (insert (nelisp-cc-fxir-admission--print summary) "\n"))
      (let* ((sources (mapcar (lambda (pin) (plist-get pin :file))
                              (plist-get discovery :modules)))
             (receipt (nelisp-cc-fxir-admission-capture-receipt
                       row-file constant sources producer receipt-file))
             (snapshot (nelisp-cc-fxir-admission-prepare
                        row-file constant (plist-get row-pin :sha256) receipt))
             (activation (nelisp-cc-fxir-admission-activate snapshot)))
        (setq summary (append summary (list :activation activation)))
        (with-temp-file report-file
          (insert (nelisp-cc-fxir-admission--print summary) "\n"))
        (list :snapshot snapshot :activation activation :report-file (file-truename report-file))))))

(provide 'nelisp-native-application-build)
;;; nelisp-native-application-build.el ends here
