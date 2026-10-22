;;; gc-port-symcells.el --- Doc 226 D9 stage 3 mirror record root self-test -*- lexical-binding: t; -*-

;; The mirror table (the globals record: table in slot 0, whose slot 1 is the
;; buckets vector, slot 2 the entry count and slot 3 the symbols' cells vector,
;; grown by doubling in `nl_symcell_reserve') is what symbol allocation must
;; reach with no env in hand.  BSS `nl_symcells_root' (inside `nl_typed_alloc',
;; +480) holds the mirror record WORD; `nl_symcells_get' computes the cells
;; vector from it.  This drives the debug arms that expose it:
;;   (nelisp--debug-switch 60)        the cells vector computed from the root,
;;                                    nil when there is none;
;;   (nelisp--debug-switch 61 N)      a mirror-shaped table kept only by the
;;                                    root (the mirror record, the table, a
;;                                    buckets vector and a cells vector of N
;;                                    fixnums each), two collections, and the
;;                                    answer: how many buckets and cells still
;;                                    hold their values (2 N intact), or a
;;                                    negative code when a record or vector is
;;                                    gone;
;;   (nelisp--debug-switch 62 N)      the same with the root word's low bit
;;                                    set, so the collector reads an
;;                                    immediate (the negative control);
;;   (nelisp--debug-switch 63 (list N PATH))
;;                                    the table of 61 around an image dump to
;;                                    PATH: the same answer, -101 when the dump
;;                                    failed (the builtin takes one argument
;;                                    after the code, hence the list);
;; Arms 61-63 restore the root before they return.  The evaluator reads the
;; root (symbol compares, the store canonicalization, interning) once
;; interned symbols are objects, so no evaluator work may run while it names a
;; table that is not the globals record: the whole window is one native call.
;;   (nelisp--debug-switch 64)        1 when the root is the record of the
;;                                    evaluator's globals, else 0, -1 outside
;;                                    a form;
;;   (nelisp--debug-switch 65)        re-derive the root from the evaluator;
;;   (nelisp--debug-switch 66)        clear the root.
;; GCSC_MODE selects the run:
;;   run      boot, growth, survival of a table only the root keeps (with the
;;            conservative scan off), the controls;
;;   dump     run, then dump the heap to GCSC_IMAGE with a table only the root
;;            keeps;
;;   restore  under --cold-load-from GCSC_IMAGE: the root names the restored
;;            globals record, its buckets and cells work, growth and survival
;;            work on the restored heap.
;; Prints "GCSC MODE checks=N failures=F PASS|FAIL" last.
;; A restored image carries the dumping run's values: reset the per-run state
;; with setq, and keep only the population (`defvar' leaves it).
(defvar gcsc-mode nil)
(defvar gcsc-checks 0)
(defvar gcsc-failures 0)
(defvar gcsc-len 0)
(setq gcsc-mode (or (getenv "GCSC_MODE") "run") gcsc-checks 0 gcsc-failures 0)
(defun gcsc-check (ok label)
  (setq gcsc-checks (1+ gcsc-checks))
  (unless ok
    (setq gcsc-failures (1+ gcsc-failures))
    (princ (format "GCSC FAIL %s\n" label))))
(defun gcsc-cells () (nelisp--debug-switch 60))
(defun gcsc-survive (n) (nelisp--debug-switch 61 n))
(defun gcsc-hidden (n) (nelisp--debug-switch 62 n))
(defun gcsc-dump (n path) (nelisp--debug-switch 63 (list n path)))
(defun gcsc-same () (nelisp--debug-switch 64))
(defun gcsc-attach () (nelisp--debug-switch 65))
(defun gcsc-clear () (nelisp--debug-switch 66))
(defun gcsc-gc () (garbage-collect) (garbage-collect))
(defun gcsc-grow (prefix n)
  ;; Give N fresh names an entry each, so the table needs more cells.
  (let ((i 0))
    (while (< i n)
      (set (intern (format "%s%d" prefix i)) i)
      (setq i (1+ i)))))
(defun gcsc-survival (tag)
  ;; The conservative scan is off, so no stale native word can keep the table:
  ;; only the root does.  Each arm builds the table, collects twice and
  ;; restores the root natively; the checks run with the real root.
  (nelisp--debug-switch 28)
  (gcsc-check (= (gcsc-survive 3000) 6000)
              (format "%s: a table kept only by the root: its record, table, buckets and cells survive two collections" tag))
  (gcsc-check (= (gcsc-same) 1) (format "%s: the root is re-derived" tag))
  ;; Control: the same table, hidden from the collector, is not kept.
  (gcsc-check (/= (gcsc-hidden 3000) 6000)
              (format "%s: control, the hidden table does not survive" tag))
  (gcsc-check (= (gcsc-same) 1) (format "%s: the root is re-derived after the control" tag))
  (nelisp--debug-switch 29))

(if (equal gcsc-mode "restore")
    (progn
      (gcsc-check (= (gcsc-same) 1) "after the cold load the root is the restored globals record")
      (gcsc-check (and (vectorp (gcsc-cells)) (= (length (gcsc-cells)) gcsc-len))
                  (format "the restored cells vector has the dumped length %d" gcsc-len))
      (gcsc-check (= (symbol-value (intern "gcsc-a5999")) 5999)
                  "names given entries before the dump still resolve")
      (gcsc-gc)
      (gcsc-check (= (gcsc-same) 1) "the root survives collections after the cold load")
      (gcsc-survival "restore")
      (let ((before (length (gcsc-cells))))
        (gcsc-grow "gcsc-b" 30000)
        (gcsc-check (> (length (gcsc-cells)) before) "growth after the cold load")
        (gcsc-check (= (gcsc-same) 1) "the root still names the globals record after growth"))
      (gcsc-check (= (symbol-value (intern "gcsc-a5999")) 5999) "old names still resolve after growth")
      (gcsc-check (= (symbol-value (intern "gcsc-b29999")) 29999) "new names resolve")
      (gcsc-clear)
      (gcsc-check (= (gcsc-same) 0) "control: a cleared root is reported")
      (gcsc-attach)
      (gcsc-check (= (gcsc-same) 1) "the root is re-derived at the end"))
  (gcsc-check (= (gcsc-same) 1) "after boot the root is the evaluator's globals record")
  (gcsc-check (and (vectorp (gcsc-cells)) (>= (length (gcsc-cells)) 4096)) "a cells vector of at least 4096")
  (let ((before (length (gcsc-cells))))
    (gcsc-grow "gcsc-a" 6000)
    (gcsc-check (> (length (gcsc-cells)) before) "6000 more names grew the cells vector")
    (gcsc-check (= (gcsc-same) 1) "the root still names the globals record after growth"))
  (gcsc-gc)
  (gcsc-check (= (gcsc-same) 1) "the root survives collections")
  (gcsc-check (= (symbol-value (intern "gcsc-a5999")) 5999) "names resolve through the grown cells")
  (gcsc-survival "run")
  (gcsc-clear)
  (gcsc-check (= (gcsc-same) 0) "control: a cleared root is reported")
  (gcsc-attach)
  (gcsc-check (= (gcsc-same) 1) "the root is re-derived at the end")
  (when (equal gcsc-mode "dump")
    (setq gcsc-len (length (gcsc-cells)))
    (gcsc-gc)
    ;; The dump runs with a table only the root keeps: its marking and its
    ;; relocation walk must agree, and the image must load.
    (nelisp--debug-switch 28)
    (gcsc-check (= (gcsc-dump 500 (getenv "GCSC_IMAGE")) 1000)
                "a table kept only by the root: the image dump succeeds and the table is intact after it")
    (nelisp--debug-switch 29)
    (gcsc-check (= (gcsc-same) 1) "the root is re-derived after the dump")
    (gcsc-gc)
    (gcsc-check (= (gcsc-same) 1) "the root survives collections after the dump")))
(princ (format "GCSC %s checks=%d failures=%d %s\n" gcsc-mode gcsc-checks gcsc-failures
               (if (= gcsc-failures 0) "PASS" "FAIL")))
nil
