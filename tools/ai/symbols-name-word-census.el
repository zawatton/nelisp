;;; symbols-name-word-census.el --- the name cells of symbol objects hold String WORDs -*- lexical-binding: t; -*-

;; Decision item A (tools/ai/symbols-name-word-progress.org).  One file, several modes, run by
;; tools/ai/symbols-name-word-check.sh:
;;
;;   BIN --load tools/ai/symbols-name-word-census.el                    SNW_MODE=producers (default)
;;   BIN --load ...                                                     SNW_MODE=heap
;;   BIN --load ...                                      SNW_IMAGE=PATH SNW_MODE=dump
;;   BIN --cold-load-from PATH --load ...                               SNW_MODE=verify
;;   BIN --cold-load-from PATH --load ...                SNW_IMAGE=P2   SNW_MODE=dump2
;;   BIN --cold-load-from P2 --load ...                                 SNW_MODE=verify2
;;   GNU -Q --batch -l ...                                              (SEM lines only)
;;
;; SEM lines state what GNU 31.1 states (name text, multibyte flag, eq of repeated `symbol-name' answers,
;; make-symbol sharing its argument, intern-soft); the check compares them with GNU's.  REP lines exist only
;; on NeLisp: the representation of the name cell, read through the debug arms of `nelisp--debug-switch'
;; (50 state of an interned symbol, 51 class of a WORD: 205 and 214 are a multibyte and a unibyte String
;; WORD, 54 the raw name cell of a symbol object as a value, 55 the live symbol_block cells counted by the
;; class of their name cell).  SNW_BASELINE=1 uses only what the 63ec9640e binaries have (the class of what
;; `symbol-name' answers): there the interned names are legacy boxes and the census must report them.
;; The last line is "SNW-CENSUS mode=M checks=N failures=F legacy=L PASS|FAIL" and the exit status 0 or 1.

;; setq, not defvar: a restored image already holds these variables from the run that dumped it.
(defvar snw-engine nil)
(defvar snw-mode nil)
(defvar snw-baseline nil)
(defvar snw-checks 0)
(defvar snw-failures 0)
(defvar snw-legacy 0)
(setq snw-engine (if (fboundp 'nelisp--debug-switch) 'nelisp 'gnu)
      snw-mode (or (getenv "SNW_MODE") "producers")
      snw-baseline (and (getenv "SNW_BASELINE") t)
      snw-checks 0 snw-failures 0 snw-legacy 0)

(defun snw-nelisp-p () (eq snw-engine 'nelisp))
(defun snw-class (w) (nelisp--debug-switch 51 w))
(defun snw-cell (sym) (nelisp--debug-switch 54 sym))
(defun snw-state (sym) (nelisp--debug-switch 50 sym))
(defun snw-check (ok label &rest detail)
  (setq snw-checks (1+ snw-checks))
  (unless ok
    (setq snw-failures (1+ snw-failures))
    (princ (format "REP FAIL %s %s\n" label (if detail (format "%S" detail) "")))))
(defun snw-sem (label &rest kv) (princ (format "SEM %s %s\n" label (mapconcat (lambda (x) (format "%S" x)) kv " "))))
(defun snw-string-word-class-p (c) (or (eql c 205) (eql c 214)))

;; -------------------------------------------------------------------- the producer paths
;; Each entry: (LABEL MAKER NAME MULTIBYTE INTERNED).  MAKER answers the symbol.  NAME is what its name
;; must be, MULTIBYTE whether that name is a multibyte string, INTERNED whether intern-soft finds it.
(defvar snw-obarray-extra nil)
(defvar snw-shared-string nil)
(defun snw-make-paths ()
  (setq snw-shared-string (copy-sequence "snw-shared"))
  (list
   (list "intern-new-ascii" (lambda () (intern (copy-sequence "snw-new-ascii"))) "snw-new-ascii" nil t)
   (list "intern-new-literal" (lambda () (intern "snw-new-literal")) "snw-new-literal" nil t)
   (list "intern-new-multibyte" (lambda () (intern "snw-新-é")) "snw-新-é" t t)
   (list "intern-multibyte-ascii" (lambda () (intern (string-to-multibyte "snw-mbascii"))) "snw-mbascii" t t)
   (list "intern-unibyte-raw" (lambda () (intern (unibyte-string 200 201 65))) (unibyte-string 200 201 65) nil t)
   (list "intern-empty" (lambda () (intern "")) "" nil t)
   (list "intern-long" (lambda () (intern (concat "snw-long-" (make-string 300 ?y)))) (concat "snw-long-" (make-string 300 ?y)) nil t)
   (list "intern-existing-image" (lambda () (intern "car")) "car" nil t)
   (list "intern-existing-runtime" (lambda () (intern "snw-new-literal")) "snw-new-literal" nil t)
   (list "intern-soft-existing" (lambda () (intern-soft "cdr")) "cdr" nil t)
   (list "reader-new" (lambda () (car (read-from-string "snw-reader-new"))) "snw-reader-new" nil t)
   (list "reader-existing" (lambda () (car (read-from-string "car"))) "car" nil t)
   (list "reader-multibyte-source" (lambda () (car (read-from-string (string-to-multibyte "snw-mbsrc")))) "snw-mbsrc" t t)
   (list "reader-multibyte-chars" (lambda () (car (read-from-string "snw-読-é"))) "snw-読-é" t t)
   (list "reader-keyword" (lambda () (car (read-from-string ":snw-kw"))) ":snw-kw" nil t)
   (list "reader-escape" (lambda () (car (read-from-string "snw\\ space\\,comma"))) "snw space,comma" nil t)
   (list "reader-uninterned" (lambda () (car (read-from-string "#:snw-unint"))) "snw-unint" nil nil)
   (list "reader-uninterned-multibyte" (lambda () (car (read-from-string "#:snw-読-un"))) "snw-読-un" t nil)
   (list "make-symbol-fresh" (lambda () (make-symbol (copy-sequence "snw-ms"))) "snw-ms" nil nil)
   (list "make-symbol-literal" (lambda () (make-symbol "snw-ms-literal")) "snw-ms-literal" nil nil)
   (list "make-symbol-shared" (lambda () (make-symbol snw-shared-string)) "snw-shared" nil nil)
   (list "make-symbol-multibyte" (lambda () (make-symbol (copy-sequence "é-λ-名"))) "é-λ-名" t nil)
   (list "make-symbol-multibyte-ascii" (lambda () (make-symbol (string-to-multibyte "snw-ms-mb"))) "snw-ms-mb" t nil)
   (list "make-symbol-unibyte-raw" (lambda () (make-symbol (unibyte-string 200 201))) (unibyte-string 200 201) nil nil)
   (list "make-symbol-empty" (lambda () (make-symbol "")) "" nil nil)
   (list "make-symbol-long" (lambda () (make-symbol (make-string 300 ?z))) (make-string 300 ?z) nil nil)
   (list "make-symbol-of-name" (lambda () (make-symbol (symbol-name 'car))) "car" nil nil)
   (list "gensym" (lambda () (let ((gensym-counter 7)) (gensym "snw"))) "snw7" nil nil)
   (list "obarray-custom" (lambda () (intern "snw-ob" (obarray-make))) "snw-ob" nil nil)
   (list "unintern-then-intern" (lambda ()
                                  (intern "snw-un")
                                  (unintern "snw-un" obarray)
                                  (intern "snw-un"))
         "snw-un" nil t)))

(defun snw-check-symbol (entry sym)
  "The REP and SEM lines of one producer path."
  (let* ((label (nth 0 entry)) (name (nth 2 entry)) (mb (nth 3 entry)) (interned (nth 4 entry))
         (n1 (symbol-name sym)) (n2 (symbol-name sym)))
    (snw-sem label 'name n1 'mb (and (multibyte-string-p n1) t) 'eq2 (eq n1 n2)
             'soft (if interned (eq sym (intern-soft name)) (null (intern-soft name))))
    (snw-check (equal n1 name) (concat label " name") n1 name)
    (snw-check (eq (and (multibyte-string-p n1) t) mb) (concat label " multibyte flag") (multibyte-string-p n1) mb)
    (snw-check (eq n1 n2) (concat label " eq of repeated symbol-name"))
    (when (snw-nelisp-p)
      (let ((rc (snw-class n1)))
        (when (memq rc '(5 14)) (setq snw-legacy (1+ snw-legacy)))
        (snw-check (snw-string-word-class-p rc) (concat label " symbol-name answers a String WORD") rc)
        (unless snw-baseline
          (let* ((cell (snw-cell sym)) (cc (snw-class cell)))
            (snw-check (snw-string-word-class-p cc) (concat label " name cell is a String WORD") cc)
            (snw-check (eq cell n1) (concat label " symbol-name answers the cell's WORD itself"))
            (snw-check (eql cc (if mb 205 214)) (concat label " the String is flagged as expected") cc mb)))))))

(defun snw-run-producers ()
  (let ((paths (snw-make-paths)))
    (dolist (entry paths)
      (let ((sym (condition-case err (funcall (nth 1 entry))
                   (error (snw-check nil (concat (nth 0 entry) " producer signalled") err) nil))))
        (when sym (snw-check-symbol entry sym))))
    ;; The intern policy (REP, not compared with GNU): intern keeps a private copy of its argument, as
    ;; the baseline does, while make-symbol shares it as GNU's Fmake_symbol does.
    (when (snw-nelisp-p)
      (let* ((a (copy-sequence "snw-private")) (s (intern a)))
        (aset a 0 ?X)
        (snw-check (equal (symbol-name s) "snw-private") "intern keeps a private copy of its argument")
        (snw-check (not (eq (symbol-name s) a)) "the name of an interned symbol is not the argument")))
    (let* ((a (copy-sequence "snw-sharing")) (s (make-symbol a)))
      (snw-sem "make-symbol-shares-its-argument" 'eq (eq (symbol-name s) a))
      (snw-check (eq (symbol-name s) a) "make-symbol stores its argument String itself")
      (aset a 0 ?X)
      (snw-sem "make-symbol-name-follows-aset" 'name (symbol-name s)))
    ;; A name read twice is one symbol and one String.
    (snw-sem "two-names" 'eq (eq (symbol-name 'ab) (symbol-name 'ab)))
    (snw-check (eq (symbol-name 'ab) (symbol-name 'ab)) "(eq (symbol-name 'ab) (symbol-name 'ab))")
    (snw-sem "same-symbol-twice" 'eq (eq (intern "snw-twice") (intern "snw-twice")))
    ;; cl-gensym exists on NeLisp only (GNU 31.1 does not define it without cl-lib).
    (when (and (snw-nelisp-p) (fboundp 'cl-gensym))
      (let ((s (cl-gensym "snw")))
        (snw-check (equal (substring (symbol-name s) 0 3) "snw") "cl-gensym name")
        (unless snw-baseline
          (snw-check (snw-string-word-class-p (snw-class (snw-cell s))) "cl-gensym name cell is a String WORD"))))
    ;; defvar and defun intern through the reader.
    (defvar snw-defvar-made 1)
    (snw-check (equal (symbol-name 'snw-defvar-made) "snw-defvar-made") "defvar-made symbol name")
    (when (and (snw-nelisp-p) (not snw-baseline))
      (snw-check (snw-string-word-class-p (snw-class (snw-cell 'snw-defvar-made))) "defvar-made symbol name cell"))))

;; -------------------------------------------------------------------- the heap
(defun snw-table-census ()
  "(STRING-WORD NIL-NAME NO-OBJECT NOT-INTERNED OTHER TOTAL) over the intern table."
  (let ((sw 0) (nilname 0) (noobj 0) (notint 0) (other 0) (total 0))
    (dolist (s (nelisp--intern-table-symbols))
      (setq total (1+ total))
      (let ((st (snw-state s)))
        (cond ((eql st 0) (setq noobj (1+ noobj)))
              ((eql st 2) (setq nilname (1+ nilname)))
              ((eql st 3) (setq notint (1+ notint)))
              ((eql st 1)
               (let ((c (snw-class (snw-cell s))))
                 (cond ((snw-string-word-class-p c) (setq sw (1+ sw)))
                       (t (setq other (1+ other))
                          (when (memq c '(5 14)) (setq snw-legacy (1+ snw-legacy)))))))
              (t (setq other (1+ other))))))
    (list sw nilname noobj notint other total)))

(defun snw-block-census () (nelisp--debug-switch 55))

(defun snw-heap-check (label)
  (let ((tc (snw-table-census)) (bc (snw-block-census)))
    (princ (format "REP %s table=%S blocks=%S\n" label tc bc))
    (snw-check (> (nth 0 tc) 5000) (concat label " table: named objects") tc)
    (snw-check (= (nth 3 tc) 0) (concat label " table: no entry that is not an interned object") tc)
    (snw-check (= (nth 4 tc) 0) (concat label " table: no legacy or unknown name cell") tc)
    (snw-check (<= (nth 1 tc) 300) (concat label " table: at most 300 unnamed objects") tc)
    (snw-check (and (listp bc) (= (length bc) 5)) (concat label " blocks: the census arm answers five counts") bc)
    (when (and (listp bc) (= (length bc) 5))
      (snw-check (= (nth 1 bc) 0) (concat label " blocks: no legacy name box") bc)
      (snw-check (= (nth 3 bc) 0) (concat label " blocks: no unknown name word") bc)
      (snw-check (<= (nth 2 bc) 300) (concat label " blocks: at most 300 unnamed cells") bc)
      (snw-check (> (nth 0 bc) 0) (concat label " blocks: String WORD names are counted") bc))
    (list tc bc)))

(defvar snw-control nil)
(defvar snw-sample nil)
(defun snw-run-heap ()
  (snw-heap-check "boot")
  ;; Positive control of the census arm: three collector self-test cells hold legacy words in their name
  ;; cells (debug arm 40), so the arm must count exactly three more legacy cells while they live.
  (let* ((before (nth 1 (snw-block-census))))
    (setq snw-control (nelisp--debug-switch 40 3))
    (let ((during (nth 1 (snw-block-census))))
      (snw-check (= during (+ before 3)) "the block census sees the three legacy control cells" before during))
    (setq snw-control nil)
    (garbage-collect) (garbage-collect)
    (snw-check (= (nth 1 (snw-block-census)) 0) "the control cells are gone after two collections"))
  ;; A churn of new symbols through every producer, with collections between.
  (let ((i 0))
    (while (< i 50000)
      (intern (format "snw-churn-%d" i))
      (when (= (% i 25) 0) (setq snw-sample (cons (cons (intern (format "snw-churn-%d" i)) (format "snw-churn-%d" i)) snw-sample)))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 20000)
      (make-symbol (format "snw-un-%d" i))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 5000)
      (car (read-from-string (format "snw-read-%d" i)))
      (setq i (1+ i))))
  (garbage-collect) (garbage-collect)
  ;; String memory churn, so a name String freed by mistake would be reused and caught.
  (let ((junk nil) (i 0))
    (while (< i 60000)
      (setq junk (cons (format "snw-junk-%d-%s" i (make-string (% i 40) ?j)) junk))
      (when (= (% i 6000) 0) (setq junk nil) (garbage-collect))
      (setq i (1+ i))))
  (garbage-collect)
  (let ((bad 0))
    (dolist (p snw-sample)
      (unless (and (equal (symbol-name (car p)) (cdr p)) (eq (symbol-name (car p)) (symbol-name (car p))))
        (setq bad (1+ bad))))
    (snw-check (= bad 0) "the names of 2,000 symbols survive the collections and the string churn" bad (length snw-sample)))
  (snw-heap-check "churn"))

;; -------------------------------------------------------------------- the image
(defvar snw-img-data nil)
(defun snw-image-check-all (label)
  (dolist (row snw-img-data)
    (let ((entry (list (concat label " " (nth 0 row)) nil (nth 2 row) (nth 3 row) (nth 4 row))))
      (snw-check-symbol entry (nth 1 row))))
  (snw-heap-check label))

(defun snw-run-image ()
  (cond
   ((equal snw-mode "dump")
    (setq snw-img-data nil)
    (dolist (entry (snw-make-paths))
      (let ((sym (funcall (nth 1 entry))))
        (push (list (nth 0 entry) sym (nth 2 entry) (nth 3 entry) (nth 4 entry)) snw-img-data)))
    (garbage-collect)
    (snw-image-check-all "dump")
    (snw-check (> (nelisp--arena-dump-image-stream (getenv "SNW_IMAGE")) 0) "dump"))
   ((equal snw-mode "verify")
    (garbage-collect)
    (snw-image-check-all "verify")
    (garbage-collect)
    (snw-image-check-all "verify after a collection"))
   ((equal snw-mode "dump2")
    (garbage-collect)
    (snw-image-check-all "dump2")
    (let ((extra (make-symbol (copy-sequence "snw-second-generation"))))
      (push (list "second-generation" extra "snw-second-generation" nil nil) snw-img-data))
    (push (list "second-generation-interned" (intern "snw-second-interned") "snw-second-interned" nil t) snw-img-data)
    (garbage-collect)
    (snw-check (> (nelisp--arena-dump-image-stream (getenv "SNW_IMAGE")) 0) "second-generation dump"))
   ((equal snw-mode "verify2")
    (garbage-collect)
    (snw-image-check-all "verify2"))))

;; -------------------------------------------------------------------- main
(cond
 ((and (equal snw-mode "producers") (snw-nelisp-p) (not snw-baseline))
  ;; The census arms must exist: a binary without them answers the diagnostics list, not five counts.
  (let ((bc (snw-block-census)))
    (snw-check (and (listp bc) (= (length bc) 5)) "debug arm 55 exists" bc))
  (snw-run-producers))
 ((equal snw-mode "producers") (snw-run-producers))
 ((and (equal snw-mode "heap") (snw-nelisp-p)) (snw-run-heap))
 ((and (member snw-mode '("dump" "verify" "dump2" "verify2")) (snw-nelisp-p)) (snw-run-image)))

(princ (format "SNW-CENSUS mode=%s%s checks=%d failures=%d legacy=%d %s\n" snw-mode (if snw-baseline " baseline" "")
               snw-checks snw-failures snw-legacy (if (= snw-failures 0) "PASS" "FAIL")))
(kill-emacs (if (= snw-failures 0) 0 1))
