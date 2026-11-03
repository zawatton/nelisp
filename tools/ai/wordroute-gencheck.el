;;; wordroute-gencheck.el --- wordroute S2: argv word entries for list-walking handlers, offline  -*- lexical-binding: t; -*-
;; emacs --batch -Q -L lisp -L src -L scripts -l tools/ai/wordroute-gencheck.el
;; Prints one CHECK line per property and GENCHECK bad=N; exit 0 iff bad = 0.
;; - walker: `nelisp-argframe-walk-lower' exists (the list-walk lowering).
;; - admit: the 197 handlers HEAD admits plus every one `nelisp-argframe-walk-names' lets
;;   the walk admit (the build keeps the applyfn unit within its bound); of the 115
;;   builtins callpath2 listed without a word entry, the walk-names ones get one.
;; - sid: every admitted name has a static id (no admitted handler silently
;;   loses its argv entry).
;; - hot: max/min lower to indexed reads with no fresh list; gethash, equal, get,
;;   append, puthash cons a list only on their Lisp-callback branch (one site).
;; - gen: the reader applyfn source generates (frame leaves and the specialized
;;   helpers pass the AOT parse), with >= 200 frame leaves (HEAD 193).
;; - listside: the list-route leaf bodies are those of HEAD (01c6b97fc), only
;;   re-keyed by the appended static ids.
;; - env: no lowered body rebinds `env' around a rooted fresh list.
;; - size: the applyfn source stays well under the unit size bound (build A hit it).
(setq max-lisp-eval-depth 20000)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(defvar wr-bad 0)
(defun nelisp-argframe-admitted-names-indexed (table)
  (let (names)
    (dolist (e table names)
      (when (and (eq (cadr e) :word)
                 (condition-case nil (progn (nelisp-argframe--lower-indexed (cddr e) 'args '(- _pad 1) '_pad) t)
                   (nelisp-argframe-ineligible nil)))
        (push (cadar e) names)))))
(defun wr-check (name ok &optional detail)
  (unless ok (setq wr-bad (1+ wr-bad)))
  (princ (format "CHECK %s %s%s\n" name (if ok "ok" "BAD") (if detail (format " %s" detail) ""))))
(defun wr-count (x form)
  (cond ((eq x form) 1) ((consp form) (+ (wr-count x (car form)) (wr-count x (cdr form)))) (t 0)))
(defun wr-binds (var form)
  (cond ((atom form) nil)
        ((and (memq (car form) '(let let*))
              (cl-some (lambda (b) (eq (if (consp b) (car b) b) var)) (cadr form))) t)
        (t (or (wr-binds var (car form)) (wr-binds var (cdr form))))))
(defconst wr-noword-v34
  '("/" "/=" "alist-get" "all-completions" "append" "assoc" "assoc-string" "bool-vector" "bool-vector-p"
    "buffer-substring" "buffer-substring-no-properties" "byte-code" "elt" "equal" "error" "exit" "float-time"
    "format" "garbage-collect" "get" "load" "logior" "logxor" "make-bool-vector" "make-byte-code"
    "make-char-table" "mapcar" "max" "min" "mod" "nelisp--arena-dump-image-stream" "nelisp--debug-switch"
    "nelisp--exit-process" "nelisp--gb-char" "nelisp--gb-column" "nelisp--gb-count-lines" "nelisp--gb-count-nl"
    "nelisp--gb-delete" "nelisp--gb-find-char" "nelisp--gb-insert" "nelisp--gb-insert-file" "nelisp--gb-move"
    "nelisp--gb-overwrite" "nelisp--gb-reserve" "nelisp--gb-substring" "nelisp--gc-diag" "nelisp--native-format"
    "nelisp--native-subr-create" "nelisp--native-substring" "nelisp--native-unbox-reference"
    "nelisp--read-all-from-string-native" "nelisp--readdir-scan-raw" "nelisp--record-expand"
    "nelisp--syscall-lstat-buf" "nelisp--syscall-mount" "nelisp--syscall-path" "nelisp--syscall-path-int"
    "nelisp--syscall-path2" "nelisp--syscall-pivot-root" "nelisp--syscall-read-file" "nelisp--syscall-readdir-names"
    "nelisp--syscall-readlink" "nelisp--syscall-stat-buf" "nelisp--syscall-stat-field" "nelisp--syscall-statx-buf"
    "nelisp--syscall-unshare" "nelisp--syscall-utimes" "nelisp--write-stderr-line" "nelisp--write-stdout-bytes"
    "nelisp-portable-syscall" "nelisp-process-call-process" "nelisp-process-close-stdin" "nelisp-process-poll"
    "nelisp-process-read-output" "nelisp-process-start" "nelisp-process-start-process" "nelisp-process-write"
    "nelisp-socket-accept" "nelisp-socket-close" "nelisp-socket-connect" "nelisp-socket-connect-error"
    "nelisp-socket-listen" "nelisp-socket-poll" "nelisp-socket-recv" "nelisp-socket-send" "nelisp-thread-atomic-add"
    "nelisp-thread-atomic-read" "nelisp-thread-gc-inhibit" "nelisp-thread-join" "nelisp-thread-shared-alloc"
    "nelisp-thread-spawn" "nl--nthcdr" "nl-write-file" "obarrayp" "ptr-read-bytes" "ptr-write-bytes" "put"
    "rdf" "read-stdin-bytes" "record" "require" "set" "setplist" "slen" "sort" "str-count-nl"
    "str-filter-prefix-lines" "str-kv-line" "str-line-start" "symbol-plist" "test-completion" "try-completion"
    "unibyte-string" "unintern" "wrf")
  "The 115 builtins without any word entry on v34 (callpath2's list, census-head.txt).")
;; Sorted list-route leaf body hashes of HEAD 01c6b97fc (target/wordroute/head-export).
(defconst wr-listside-head "2dc550ed7ebf7941c79c8637813a7af20ef0325dd80e51c52225fe246dc69625")

(let* ((nelisp-standalone--target 'linux-x86_64)
       (table (nelisp-standalone--applyfn-guarded-reader-table))
       (adm (nelisp-argframe-admitted-names table))
       (walker (fboundp 'nelisp-argframe-walk-lower)))
  (wr-check "walker" walker)
  (let* ((names (delete-dups (mapcar #'cadar (cl-remove-if-not (lambda (e) (eq (cadr e) :word)) table))))
         (out (cl-remove-if (lambda (n) (member n adm)) names))
         (left (cl-remove-if-not (lambda (n) (and (member n wr-noword-v34)
                                                  (not (member n adm))
                                                  (not (assoc n nelisp-standalone--directcall-specs))))
                                 wr-noword-v34)))
    ;; Every handler the build lets the walk admit (`nelisp-argframe-walk-names')
    ;; is admitted, plus the 197 the indexed pass admitted on HEAD.
    (defvar nelisp-argframe-walk-names nil)
    (wr-check "admit" (and (= (length wr-noword-v34) 115)
                          (if (null nelisp-argframe-walk-names)
                              ;; applyfn2 admits all 361 structurally eligible
                              ;; handlers, including the complete frozen 115.
                              (and (= (length adm) 361) (null left))
                            (and
                             (cl-every (lambda (n) (member n adm)) nelisp-argframe-walk-names)
                             (= (length adm)
                                (+ 197 (length
                                        (cl-remove-if
                                         (lambda (n) (member n (nelisp-argframe-admitted-names-indexed table)))
                                         nelisp-argframe-walk-names)))))))
              (format "admitted=%d walk-names=%d noword-left=%d" (length adm) (length nelisp-argframe-walk-names) (length left))))
  (wr-check "sid" (cl-every (lambda (n) (let ((sid (nelisp-standalone--sid n))) (and sid (< sid 4095)))) adm)
            (format "nosid=%S" (cl-remove-if #'nelisp-standalone--sid adm)))
  (when walker
    (let ((counts nil) (ok t))
      (dolist (spec '(("max" 0) ("min" 0) ("gethash" 1) ("equal" 1) ("get" 1) ("append" 1) ("puthash" 1)))
        (let* ((e (cl-find-if (lambda (e) (equal (cadar e) (car spec))) table))
               (low (condition-case nil
                        (let ((nelisp-argframe-generated (cons nil (make-hash-table :test #'equal))))
                          (nelisp-argframe-lower (cddr e) 'args 'argc))
                      (nelisp-argframe-ineligible nil)))
               (n (and low (wr-count 'nl_argframe_fresh_list low))))
          (push (format "%s=%s" (car spec) n) counts)
          (unless (and low (eql n (cadr spec)) (> (wr-count 'ptr-read-u64 low) 0)
                       (= 0 (wr-count 'nl_cons_cdr_word low)))
            (setq ok nil))))
      (wr-check "hot" ok (mapconcat #'identity (nreverse counts) " ")))))

(let* ((nelisp-standalone--target 'linux-x86_64)
       (src (condition-case err (nelisp-standalone--applyfn-source)
              (error (princ (format "GEN-ERROR %S\n" err)) nil)))
       (defs (cl-remove-if-not (lambda (f) (eq (car-safe f) 'defun)) (cdr src)))
       (frame (cl-remove-if-not (lambda (f) (string-match-p "^nl_apply_builtin_frame_leaf_" (symbol-name (cadr f)))) defs))
       (spec (cl-remove-if-not (lambda (f) (string-match-p "__af[0-9_]+\\'" (symbol-name (cadr f)))) defs))
       (list-leaves (cl-remove-if-not (lambda (f) (string-match-p "^nl_apply_builtin_leaf_" (symbol-name (cadr f)))) defs))
       (print-length nil) (print-level nil))
  (wr-check "gen" (and src (>= (length frame) 200) (>= (length spec) 1))
            (format "frame-leaves=%d specialized=%d" (length frame) (length spec)))
  ;; The applyfn unit must stay under the 16 MiB rooted build-input bound
  ;; (nelisp-native-rooted-build-evidence.el).  Proxy: printed source size;
  ;; HEAD 1333657 chars -> 13.59 MB unit, a 1887176-char source -> 16.97 MB
  ;; (build A, rejected), about 6.1 bytes of unit per added char.
  ;; Integrated v35 (all 164 admitted): Linux unit 15.50 MB, Win64 17.68 MB.  Each
  ;; target's applyfn source may grow at most 25000 chars over HEAD's (about 0.2 MB
  ;; of unit: the Win64 unit stays under 15 MiB with the other lanes' share).
  ;; applyfn2: the bound is per link unit; the argv entries (frame leaves, their
  ;; specializations and routers) are compiled as units of their own
  ;; (`nelisp-standalone--applyfn-split-units'), so the gate measures the main unit,
  ;; and tools/ai/applyfn2-gencheck.el gates the argv-entry units.
  (let* ((main-chars (lambda (source)
                       (apply #'+ (mapcar (lambda (f) (length (prin1-to-string f)))
                                          (cdr (if (fboundp 'nelisp-standalone--applyfn-split-units)
                                                   (car (nelisp-standalone--applyfn-split-units source))
                                                 source))))))
         (chars (funcall main-chars src))
         (win (let ((nelisp-standalone--target 'windows-x86_64))
                (funcall main-chars (nelisp-standalone--applyfn-source)))))
    (wr-check "size" (and (<= chars (+ 1333657 25000)) (<= win (+ 1406419 25000)))
              (format "linux-chars=%d (+%d) win64-chars=%d (+%d)" chars (- chars 1333657) win (- win 1406419))))
  (let ((h (secure-hash 'sha256 (mapconcat #'identity
                                          (sort (mapcar (lambda (f) (secure-hash 'sha256 (prin1-to-string (cddr f)))) list-leaves)
                                                #'string<) ","))))
    (princ (format "LISTSIDE %s\n" h))
    (wr-check "listside" (equal h wr-listside-head) (format "leaves=%d" (length list-leaves))))
  (wr-check "env" (cl-notany (lambda (f) (and (> (wr-count 'nl_argframe_fresh_list f) 0) (wr-binds 'env f)))
                             (append frame spec))))

(princ (format "GENCHECK bad=%d\n" wr-bad))
(kill-emacs (if (= wr-bad 0) 0 1))
