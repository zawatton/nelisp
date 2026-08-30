;;; nelisp-pkg.el --- package manifests checked against the real graph -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; NeLisp has packages but no package system.  `packages/<name>/' is a
;; layout convention, the Makefile puts every `src' directory on the load
;; path, and `require' finds whatever is there.  That works until two
;; questions need answering: what does this package depend on, and in
;; what order may these be loaded.  Neither is written down anywhere
;; today, so both are answered by trying it.
;;
;; This library derives the answer from the code and lets a package
;; declare it, then checks the declaration against the derivation.  The
;; derivation is the source of truth about what the code does; the
;; manifest is a statement of intent, and the interesting output is
;; where the two disagree.
;;
;; Deriving first is deliberate.  A manifest format adopted before the
;; graph is known would be 34 files of guesses, and a package system
;; whose metadata is unverified is a package system that lies -- which
;; is worse than none, because it is believed.
;;
;; A manifest is a plist in `packages/<name>/manifest.el':
;;
;;     (:name "nelisp-http"
;;      :version "1.0"
;;      :requires ("nelisp-process" "nelisp-state" "nelisp-sys"))
;;
;; Findings use the same shape as `nl-check' and `nl-ns':
;; `(:kind KIND :subject S ...)'.
;;
;; | kind                       | meaning                                    |
;; |----------------------------+--------------------------------------------|
;; | pkg-cycle                  | packages that require each other in a ring |
;; | pkg-undeclared-dependency  | the code requires it, the manifest does not|
;; | pkg-stale-dependency       | the manifest requires it, the code does not|
;; | pkg-missing-manifest       | no manifest.el (informational for now)     |
;; | pkg-invalid-manifest       | manifest.el is unreadable or malformed     |
;; | pkg-unresolved-require     | required feature provided nowhere in tree  |
;;
;; `pkg-unresolved-require' is the low-severity carrier: a require that
;; nothing in the tree provides is usually a host Emacs library
;; (`cl-lib', `dom') or a runtime builtin, not an error.  It is reported
;; so that an unexpected one is visible, not to be driven to zero.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Reading -------------------------------------------------------------

(defun nelisp-pkg--read-forms (file)
  "Return every top-level form in FILE, or nil when it cannot be read."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((forms nil))
          (condition-case nil
              (while t (push (read (current-buffer)) forms))
            (end-of-file nil))
          (nreverse forms)))
    (error nil)))

(defun nelisp-pkg--walk (form fn)
  "Call FN on every cons cell in FORM.

The spine is walked iteratively and only the cars are recursed into,
for two reasons.  Source files contain dotted pairs -- alist literals
like `(let . bindings-and-body)' are everywhere in this tree -- so a
`dolist' over a form is wrong the moment it meets one.  And a long data
literal is a long spine, which recursion down the cdr would turn into
stack depth."
  (let ((tail form))
    (while (consp tail)
      (funcall fn tail)
      (when (consp (car tail))
        (nelisp-pkg--walk (car tail) fn))
      (setq tail (cdr tail)))))

(defun nelisp-pkg--feature-arg (form head)
  "Return the feature symbol when FORM is (HEAD \\='FEATURE ...).
Only a quoted literal counts: a computed feature name is not a
dependency anyone can resolve statically, and pretending otherwise
would put a guess into the graph."
  (when (and (consp form)
             (eq (car form) head)
             (consp (cdr form)))
    (let ((arg (nth 1 form)))
      (cond
       ((and (consp arg) (eq (car arg) 'quote) (symbolp (nth 1 arg)))
        (nth 1 arg))
       ((symbolp arg) nil)))))

(defun nelisp-pkg-file-features (file)
  "Return (PROVIDES . REQUIRES) for FILE, each a list of symbols."
  (let ((provides nil)
        (requires nil))
    (dolist (form (nelisp-pkg--read-forms file))
      (nelisp-pkg--walk
       form
       (lambda (node)
         (let ((p (nelisp-pkg--feature-arg node 'provide))
               (r (nelisp-pkg--feature-arg node 'require)))
           (when p (cl-pushnew p provides))
           (when r (cl-pushnew r requires))))))
    (cons (nreverse provides) (nreverse requires))))

;;;; Manifests -----------------------------------------------------------

(defconst nelisp-pkg-manifest-keys '(:name :requires)
  "Keys a manifest must carry.

`:version' is deliberately not among them.  Nothing consumes a version
yet -- there is no resolver constraint, no distribution, no
compatibility rule -- so requiring one would mean stamping 34 invented
numbers into the tree and calling it metadata.  Require what is
checked; add `:version' to a package when it starts to mean something.")

(defun nelisp-pkg-validate-manifest (manifest)
  "Return a list of problem strings for MANIFEST, empty when it is valid."
  (let ((problems nil))
    (if (not (and manifest (listp manifest)))
        (push "manifest is not a plist" problems)
      (dolist (key nelisp-pkg-manifest-keys)
        (unless (plist-member manifest key)
          (push (format "missing %s" key) problems)))
      (let ((name (plist-get manifest :name))
            (version (plist-get manifest :version))
            (requires (plist-get manifest :requires)))
        (unless (stringp name) (push ":name must be a string" problems))
        (when (and (plist-member manifest :version) (not (stringp version)))
          (push ":version must be a string when present" problems))
        (unless (listp requires) (push ":requires must be a list" problems))
        (dolist (r (and (listp requires) requires))
          (unless (stringp r)
            (push (format ":requires entry %S must be a string" r) problems)))))
    (nreverse problems)))

(defun nelisp-pkg-read-manifest (dir)
  "Return (MANIFEST . PROBLEMS) for DIR, or nil when it has no manifest."
  (let ((file (expand-file-name "manifest.el" dir)))
    (when (file-readable-p file)
      (let ((forms (nelisp-pkg--read-forms file)))
        (if (null forms)
            (cons nil (list "manifest.el could not be read"))
          (let ((manifest (car forms)))
            (cons manifest (nelisp-pkg-validate-manifest manifest))))))))

(defun nelisp-pkg-manifest-render (name requires &optional existing)
  "Return the text of a manifest for NAME requiring REQUIRES.

EXISTING, when given, is the manifest already on disk; every key in it
other than `:name' and `:requires' is carried over, so regenerating a
package's dependencies never silently drops a `:version' or anything
else its author put there."
  (let ((extra nil)
        (tail existing))
    (while (and tail (cdr tail))
      (let ((key (car tail))
            (value (cadr tail)))
        (unless (memq key '(:name :requires))
          (setq extra (append extra (list key value)))))
      (setq tail (cddr tail)))
    (concat
     "(:name " (prin1-to-string name) "\n"
     (let ((rendered ""))
       (while (and extra (cdr extra))
         (setq rendered (concat rendered " " (symbol-name (car extra))
                                " " (prin1-to-string (cadr extra)) "\n"))
         (setq extra (cddr extra)))
       rendered)
     " :requires "
     (if requires
         (concat "(" (mapconcat #'prin1-to-string requires " ") ")")
       "()")
     ")\n")))

(defun nelisp-pkg-actual-requires (package packages)
  "Return the package names PACKAGE actually depends on, sorted."
  (let ((providers (nelisp-pkg-provider-table packages))
        (name (plist-get package :name))
        (deps nil))
    (dolist (feature (plist-get package :requires))
      (let ((owner (gethash feature providers)))
        (when (and owner (not (equal owner name)))
          (cl-pushnew owner deps :test #'equal))))
    (sort deps #'string<)))

;;;; Scanning ------------------------------------------------------------

(defun nelisp-pkg-scan-package (dir)
  "Return a plist describing the package rooted at DIR."
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
         (src (expand-file-name "src" dir))
         (files (and (file-directory-p src)
                     (directory-files src t "\\.el\\'")))
         (provides nil)
         (requires nil)
         (per-file nil))
    (dolist (file files)
      (let ((pair (nelisp-pkg-file-features file)))
        (push (list :file file :provides (car pair) :requires (cdr pair))
              per-file)
        (dolist (p (car pair)) (cl-pushnew p provides))
        (dolist (r (cdr pair)) (cl-pushnew r requires))))
    (setq per-file (nreverse per-file))
    (let ((manifest (nelisp-pkg-read-manifest dir)))
      (list :name name
            :dir dir
            :files (length files)
            :file-features per-file
            :provides (nreverse provides)
            :requires (nreverse requires)
            :manifest (car manifest)
            :manifest-problems (cdr manifest)
            :has-manifest (and manifest t)))))

(defun nelisp-pkg-scan (&optional root)
  "Scan `packages/' below ROOT (default `default-directory')."
  (let* ((base (expand-file-name "packages" (or root default-directory)))
         (dirs (and (file-directory-p base)
                    (cl-remove-if-not #'file-directory-p
                                      (directory-files base t "\\`[^.]")))))
    (mapcar #'nelisp-pkg-scan-package dirs)))

(defun nelisp-pkg-core-features (&optional root dirs)
  "Return the features provided outside `packages/'.
DIRS defaults to src and lisp below ROOT."
  (let ((features nil))
    (dolist (dir (or dirs '("src" "lisp")))
      (let ((full (expand-file-name dir (or root default-directory))))
        (when (file-directory-p full)
          (dolist (file (directory-files full t "\\.el\\'"))
            (dolist (p (car (nelisp-pkg-file-features file)))
              (cl-pushnew p features))))))
    features))

(defun nelisp-pkg--require-noerror-p (form)
  "Return non-nil when FORM is a `require\=' that tolerates a missing file.
`(require \='X nil t)\=' is a question and answers nil where X is absent.
`(require \='X)\=' is a demand, and a demand for a file the tree does not
contain aborts the load of whatever names it."
  (and (consp form) (eq (car form) 'require) (nth 3 form) t))

(defun nelisp-pkg--sort-host-entries (entries)
  "Sort (FEATURE . FILE) ENTRIES by feature, then file."
  (sort entries
        (lambda (a b)
          (if (eq (car a) (car b))
              (string< (cdr a) (cdr b))
            (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun nelisp-pkg-core-host-requires (packages &optional root dirs)
  "Return what src/ and lisp/ require that nothing in the tree provides.
Value is (:hard ((FEATURE . FILE) ...) :optional ((FEATURE . FILE) ...)).

`packages/\=' has a manifest each and a gate over them; src/ and lisp/ have
neither, and they are where every host-library dependency that broke the
standalone runtime lived -- subr-x and url-parse first, then macroexp, seq
and pcase (2026-08-19).  Each was found by bisecting a require by hand
after something failed a long way from it: the last one left the native
compiler unavailable and every hot defun on the bytecode path, and the
manifest said only \"native compiler unavailable\".

The split is the actionable half.  Off-host an optional require answers
nil; a hard one stops the file, and everything that requires that file
after it."
  (let ((provided (nelisp-pkg-core-features root dirs))
        (providers (nelisp-pkg-provider-table packages))
        (hard nil)
        (optional nil))
    (dolist (dir (or dirs '("src" "lisp")))
      (let ((full (expand-file-name dir (or root default-directory))))
        (when (file-directory-p full)
          (dolist (file (directory-files full t "\\.el\\'"))
            (dolist (form (nelisp-pkg--read-forms file))
              (nelisp-pkg--walk
               form
               (lambda (node)
                 (let ((feature (nelisp-pkg--feature-arg node 'require)))
                   (when (and feature
                              (not (memq feature provided))
                              (not (gethash feature providers)))
                     (let ((entry (cons feature
                                        (file-name-nondirectory file))))
                       (if (nelisp-pkg--require-noerror-p node)
                           (cl-pushnew entry optional :test #'equal)
                         (cl-pushnew entry hard :test #'equal))))))))))))
    (list :hard (nelisp-pkg--sort-host-entries hard)
          :optional (nelisp-pkg--sort-host-entries optional))))

;;;; The graph -----------------------------------------------------------

(defun nelisp-pkg-provider-table (packages)
  "Return a hash of FEATURE -> package name for PACKAGES."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (package packages)
      (dolist (feature (plist-get package :provides))
        (puthash feature (plist-get package :name) table)))
    table))

(defun nelisp-pkg-edges (packages)
  "Return ((FROM . TO) ...) for cross-package requires in PACKAGES."
  (let ((providers (nelisp-pkg-provider-table packages))
        (edges nil))
    (dolist (package packages)
      (let ((from (plist-get package :name)))
        (dolist (feature (plist-get package :requires))
          (let ((to (gethash feature providers)))
            (when (and to (not (equal to from)))
              (cl-pushnew (cons from to) edges :test #'equal))))))
    (nreverse edges)))

(defun nelisp-pkg-resolve (packages)
  "Return (:order NAMES :cycles NAMES) for PACKAGES.

The order is a load order: every package appears after the ones it
requires.  Names that could not be ordered are returned as `:cycles',
which is the only structural failure this library treats as fatal --
a ring of requires has no load order at all, so no amount of care at
the call site can work around it."
  (let* ((edges (nelisp-pkg-edges packages))
         (names (mapcar (lambda (p) (plist-get p :name)) packages))
         (pending (copy-sequence names))
         (order nil)
         (progress t))
    (while (and pending progress)
      (setq progress nil)
      (dolist (name (copy-sequence pending))
        (let ((deps (cl-remove-if-not
                     (lambda (edge) (equal (car edge) name))
                     edges)))
          (when (cl-every (lambda (edge) (member (cdr edge) order))
                          deps)
            (setq order (append order (list name)))
            (setq pending (delete name pending))
            (setq progress t)))))
    (list :order order :cycles pending)))

;;;; Load sequences -------------------------------------------------------

;; Why this exists: on the standalone runtime, dependencies are loaded by
;; explicit path rather than through `require', and every smoke and
;; recipe that does so carries a hand-written list of files in a
;; hand-worked-out order.  Add a dependency and each of those lists is
;; silently wrong until someone runs it.  The order is derivable from the
;; same provide/require data as the package graph, so it should be
;; derived.

(defun nelisp-pkg--order-files (entries package-name)
  "Order ENTRIES (from `:file-features') so providers come first.

Only intra-package features are ordered on: a require satisfied outside
this package is somebody else's file, and is handled by ordering the
packages themselves.  Files whose intra-package requires cannot all be
satisfied are appended in their original order rather than dropped --
a load list that silently omits a file is worse than one in a doubtful
order, because the failure moves somewhere else."
  (let* ((within (make-hash-table :test #'eq))
         (pending (copy-sequence entries))
         (ordered nil)
         (done (make-hash-table :test #'eq))
         (progress t))
    (dolist (entry entries)
      (dolist (feature (plist-get entry :provides))
        (puthash feature t within)))
    (while (and pending progress)
      (setq progress nil)
      (dolist (entry (copy-sequence pending))
        (let ((needed (cl-remove-if-not
                       (lambda (feature) (gethash feature within))
                       (plist-get entry :requires))))
          (when (cl-every (lambda (feature)
                            (or (gethash feature done)
                                (memq feature (plist-get entry :provides))))
                          needed)
            (setq ordered (append ordered (list entry)))
            (dolist (feature (plist-get entry :provides))
              (puthash feature t done))
            (setq pending (delq entry pending))
            (setq progress t)))))
    (ignore package-name)
    (append ordered pending)))

(defun nelisp-pkg-load-sequence (name packages)
  "Return the absolute file paths to load for package NAME, in order.

Dependencies first, each package's own files ordered so that a file
comes after the ones providing what it requires.  Signals when NAME is
not among PACKAGES, or when the packages it needs form a cycle -- in
both cases there is no sequence, and returning a plausible one would
be the failure this function exists to prevent."
  (let* ((by-name (let ((table (make-hash-table :test #'equal)))
                    (dolist (package packages)
                      (puthash (plist-get package :name) package table))
                    table))
         (package (gethash name by-name))
         (resolution (nelisp-pkg-resolve packages))
         (edges (nelisp-pkg-edges packages)))
    (unless package
      (error "nelisp-pkg: no package named %s" name))
    (when (member name (plist-get resolution :cycles))
      (error "nelisp-pkg: %s is in a dependency cycle; no load order exists"
             name))
    ;; Transitive dependencies, in the global load order.
    (let ((needed (list name))
          (frontier (list name)))
      (while frontier
        (let ((current (pop frontier)))
          (dolist (edge edges)
            (when (equal (car edge) current)
              (unless (member (cdr edge) needed)
                (push (cdr edge) needed)
                (push (cdr edge) frontier))))))
      (let ((sequence nil))
        (dolist (candidate (plist-get resolution :order))
          (when (member candidate needed)
            (let ((entry (gethash candidate by-name)))
              (dolist (file (nelisp-pkg--order-files
                             (plist-get entry :file-features)
                             candidate))
                (setq sequence (append sequence
                                       (list (plist-get file :file))))))))
        sequence))))

;;;; Hand-written load lists ----------------------------------------------

;; The standalone smokes load their dependencies by explicit path, in an
;; order someone worked out once.  Those lists cannot be replaced by a
;; generated one -- generating it inside the standalone runtime would
;; mean bootstrapping this library there first -- but they can be
;; checked, which is the part that matters: a stale list fails silently,
;; because `(load "missing.el" nil t)' returns t in that runtime instead
;; of signalling.

(defun nelisp-pkg-load-list (file)
  "Return the `load' calls in FILE as ((:path P :literal BOOL) ...).

A computed path is recorded with `:literal' nil rather than dropped:
knowing that a list is partly unreadable is different from believing it
was fully checked."
  (let ((calls nil))
    (dolist (form (nelisp-pkg--read-forms file))
      (nelisp-pkg--walk
       form
       (lambda (node)
         (when (and (consp node) (eq (car node) 'load) (consp (cdr node)))
           (let ((arg (nth 1 node)))
             (push (list :path (and (stringp arg) arg)
                         :literal (stringp arg))
                   calls))))))
    (nreverse calls)))

(defun nelisp-pkg-check-load-list (file &optional root)
  "Return findings for the hand-written load list in FILE.

ROOT is the directory the paths are relative to, default
`default-directory'.

Two things are checked, and both are failures that produce no message
at run time:

  load-list-missing-file  the path does not exist, so the load is a
                          silent no-op and nothing it should define is
                          defined;
  load-list-out-of-order  an earlier file requires a feature that a
                          later one provides.

Pairs are compared by actual dependency, not by position in a derived
order, so two unrelated files in either order are not a finding."
  (let* ((root (file-name-as-directory (or root default-directory)))
         (calls (nelisp-pkg-load-list file))
         (entries nil)
         (findings nil))
    (dolist (call calls)
      (if (not (plist-get call :literal))
          (push (list :kind 'load-list-computed-path :subject file) findings)
        (let* ((path (plist-get call :path))
               (full (expand-file-name path root)))
          (if (not (file-readable-p full))
              (push (list :kind 'load-list-missing-file
                          :subject file :path path)
                    findings)
            (let ((features (nelisp-pkg-file-features full)))
              (push (list :path path
                          :provides (car features)
                          :requires (cdr features))
                    entries))))))
    (setq entries (nreverse entries))
    (let ((tail entries))
      (while tail
        (let ((earlier (car tail)))
          (dolist (later (cdr tail))
            (dolist (feature (plist-get earlier :requires))
              (when (memq feature (plist-get later :provides))
                (push (list :kind 'load-list-out-of-order
                            :subject file
                            :path (plist-get earlier :path)
                            :after (plist-get later :path)
                            :feature feature)
                      findings)))))
        (setq tail (cdr tail))))
    (list :findings (nreverse findings) :checked (length entries))))

;;;; Checking ------------------------------------------------------------

(defun nelisp-pkg-check (packages &optional core-features)
  "Return findings for PACKAGES, given CORE-FEATURES from outside packages/."
  (let* ((providers (nelisp-pkg-provider-table packages))
         (resolution (nelisp-pkg-resolve packages))
         (findings nil))
    (dolist (name (plist-get resolution :cycles))
      (push (list :kind 'pkg-cycle :subject name) findings))
    (dolist (package packages)
      (let* ((name (plist-get package :name))
             (declared (plist-get (plist-get package :manifest) :requires))
             (actual (delete-dups
                      (delq nil
                            (mapcar (lambda (feature)
                                      (let ((owner (gethash feature providers)))
                                        (and owner (not (equal owner name)) owner)))
                                    (plist-get package :requires))))))
        (dolist (problem (plist-get package :manifest-problems))
          (push (list :kind 'pkg-invalid-manifest :subject name :detail problem)
                findings))
        (if (not (plist-get package :has-manifest))
            (push (list :kind 'pkg-missing-manifest :subject name) findings)
          (dolist (dep actual)
            (unless (member dep declared)
              (push (list :kind 'pkg-undeclared-dependency
                          :subject name :dependency dep)
                    findings)))
          (dolist (dep declared)
            (unless (member dep actual)
              (push (list :kind 'pkg-stale-dependency
                          :subject name :dependency dep)
                    findings))))
        (dolist (feature (plist-get package :requires))
          (unless (or (gethash feature providers)
                      (memq feature core-features)
                      (memq feature (plist-get package :provides)))
            (push (list :kind 'pkg-unresolved-require
                        :subject name :feature feature)
                  findings)))))
    (nreverse findings)))

(defun nelisp-pkg-findings-of-kind (findings kind)
  "Return the FINDINGS whose `:kind' is KIND."
  (cl-remove-if-not (lambda (f) (eq (plist-get f :kind) kind)) findings))

(defun nelisp-pkg-summary (findings)
  "Return ((KIND . COUNT) ...) for FINDINGS, most frequent first."
  (let ((table (make-hash-table :test #'eq))
        (rows nil))
    (dolist (finding findings)
      (let ((kind (plist-get finding :kind)))
        (puthash kind (1+ (gethash kind table 0)) table)))
    (maphash (lambda (k v) (push (cons k v) rows)) table)
    (sort rows (lambda (a b) (> (cdr a) (cdr b))))))

(defun nelisp-pkg-report (packages findings)
  "Return a human-readable report string."
  (let ((resolution (nelisp-pkg-resolve packages))
        (edges (nelisp-pkg-edges packages)))
    (concat
     (format "nelisp-pkg: %d package(s), %d cross-package edge(s)\n"
             (length packages) (length edges))
     (mapconcat (lambda (row) (format "%6d  %s\n" (cdr row) (car row)))
                (nelisp-pkg-summary findings)
                "")
     (if (plist-get resolution :cycles)
         (format "  CYCLE: %s\n"
                 (mapconcat #'identity (plist-get resolution :cycles) ", "))
       ""))))

(provide 'nelisp-pkg)

;;; nelisp-pkg.el ends here
